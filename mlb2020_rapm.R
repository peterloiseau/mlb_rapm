library(baseballr)
library(dplyr)
library(tidyr)
library(glmnet)
library(tidyverse)
library(ggthemes)
daily<-data.frame()
range<-seq(as.Date("2019/03/20"),as.Date("2020/10/03"),by="day")
for(i in c(1:length(range))) {
  try(
  daily <-
    rbind(
      daily,
      scrape_statcast_savant(start_date = range[i], end_date = range[i]) %>% select(
        pitch_type,
        game_date,
        game_pk,
        release_speed,
        release_pos_x,
        release_pos_z,
        player_name,
        batter,
        pitcher,
        events,
        description,
        plate_x,
        plate_z,
        stand,
        p_throws,
        hit_location,
        balls,
        strikes,
        on_3b,
        on_2b,
        on_1b,
        outs_when_up,
        inning,
        inning_topbot,
        hc_x,
        hc_y,
        hit_distance_sc,
        launch_speed,
        launch_angle,
        effective_speed,
        release_spin_rate,
        release_extension,
        release_pos_y,
        at_bat_number,
        pitch_number,
        bat_score,
        fld_score,
        estimated_woba_using_speedangle,
        woba_value,
        fielder_2_1,
        fielder_3,
        fielder_4,
        fielder_5,
        fielder_6,
        fielder_7,
        fielder_8,
        fielder_9,
        if_fielding_alignment,
        of_fielding_alignment,
        home_team,
        away_team,
        game_year
      )
    ))
}
write.csv(daily%>%filter(game_year==2020),'C:/Users/peter/Documents/baseball-databases/savant_pitches/2020.csv',row.names=F)
write.csv(daily%>%filter(game_year==2019),'C:/Users/peter/Documents/baseball-databases/savant_pitches/2019.csv',row.names=F)
                    
plate_appearance <-
  daily %>% filter(!is.na(events)) %>% mutate(
    xwoba = ifelse(
      is.na(estimated_woba_using_speedangle),
      woba_value,
      estimated_woba_using_speedangle
    ),
    pa_id = paste0(game_pk, "_", at_bat_number)
  )

pa_gather_2020 <-
  plate_appearance %>% filter(game_year=='2020') %>%
  select(pa_id, batter, pitcher, xwoba) %>% gather("var", "id", batter:pitcher) %>%
  mutate(val = ifelse(var == 'batter', 1, -1),
         id = paste0(id, "_", var)) %>% select(-var)

pa_spread_2020 <-
  pa_gather_2020 %>% spread(id,val)

pa_spread_2020[is.na(pa_spread_2020)] <- 0

####MODEL
cv_rapm_2020 <-
  cv.glmnet(
    x = pa_spread_2020 %>% select(-c(pa_id, xwoba)) %>% data.matrix(),
    y = pa_spread_2020$xwoba,
    alpha = 0
  )

player_parameters_2020 <-
  as.data.frame(as.matrix(coef(cv_rapm_2020))) %>% rownames_to_column('id') %>%
  left_join(pa_gather_2020 %>% group_by(id) %>% summarise(pa = n())) %>%
  separate('id', c('id', 'cat'), sep = '_') %>% filter(!is.na(cat))


names_2020 <-
  do.call(rbind, lapply(player_parameters_2020$id, function(x)
    playername_lookup(x))) %>%
  mutate(name = paste(name_first, name_last))
  
player_parameters_fin_2020 <-
  player_parameters_2020 %>% cbind(data.frame(
    name = names_2020$name,
    fg_key = as.character(names_2020$key_fangraphs)
  ))

pitcher_2020 <-
  player_parameters_fin_2020 %>% filter(cat=='pitcher')
write.csv(pitcher_2020,'pitchers_2020.csv',row.names = F)

batter_2020 <- 
  player_parameters_fin_2020 %>% filter(cat=='batter')
write.csv(batter_2020,'batters_2020.csv',row.names = F)

#################2019
pa_gather_2019 <-
  plate_appearance %>% filter(game_year=='2019') %>%
  select(pa_id, batter, pitcher, xwoba) %>% gather("var", "id", batter:pitcher) %>%
  mutate(val = ifelse(var == 'batter', 1, -1),
         id = paste0(id, "_", var)) %>% select(-var)

pa_spread_2019 <-
  pa_gather_2019 %>% spread(id,val)

pa_spread_2019[is.na(pa_spread_2019)] <- 0

####MODEL
cv_rapm_2019 <-
  cv.glmnet(
    x = pa_spread_2019 %>% select(-c(pa_id, xwoba)) %>% data.matrix(),
    y = pa_spread_2019$xwoba,
    alpha = 0
  )

player_parameters_2019 <-
  as.data.frame(as.matrix(coef(cv_rapm_2019))) %>% rownames_to_column('id') %>%
  left_join(pa_gather_2019 %>% group_by(id) %>% summarise(pa = n())) %>%
  separate('id', c('id', 'cat'), sep = '_') %>% filter(!is.na(cat))

names_2019 <-
  do.call(rbind, lapply(player_parameters_2019$id, function(x)
    playername_lookup(x))) %>%
  mutate(name = paste(name_first, name_last))

player_parameters_fin_2019 <-
  player_parameters_2019 %>% cbind(data.frame(
    name = names_2019$name,
    fg_key = as.character(names_2019$key_fangraphs)
  )) 


pitcher_2019 <-
  player_parameters_fin_2019 %>% filter(cat=='pitcher')
write.csv(pitcher_2019,'pitchers_2019.csv',row.names = F)

batter_2019 <- 
  player_parameters_fin_2019 %>% filter(cat=='batter')
write.csv(batter_2019,'batters_2019.csv',row.names = F)

################TEST comparison to wOBA for hitters and FIP- for pitchers
#batter_2020<-read.csv('batters_2020.csv')
#pitcher_2020<-read.csv('pitchers_2020.csv')
#batter_2019<-read.csv('batters_2019.csv')
#pitcher_2019<-read.csv('pitchers_2019.csv')
#plate_appearance<-read.csv('C:/Users/peter/Documents/baseball-databases/savant_pitches/2020.csv')%>%rbind(read.csv('C:/Users/peter/Documents/baseball-databases/savant_pitches/2019.csv'))
#rescale
batter<-batter_2020%>%mutate(Season='2020')%>%rbind(batter_2019%>%mutate(Season='2019'))
pitcher<-pitcher_2020%>%mutate(Season='2020')%>%rbind(pitcher_2019%>%mutate(Season='2019'))
names(batter)[3]<-'1'
names(pitcher)[3]<-'1'

batter <-
  batter %>% left_join(batter %>% filter((log(pa, exp(
    .5
  )) / log(max(
    pa
  ) * 5, exp(
    .5
  ))) > .01) %>% group_by(Season) %>%
    summarise(min = min(`1`), mean = mean(`1` - min(`1`)))) %>%
  mutate(prop = (log(pa, exp(.5)) / log(max(pa) * 5, exp(.5)))) %>% group_by(Season) %>% mutate(raw_rapm = ((`1` - min) * 100 / mean),
                                                                                                rapm =
                                                                                                  ((`1` - min) * 100 *
                                                                                                     prop / mean) + ((1 - prop) * mean((`1` - min) * 100 / mean)))

ggplot(batter,aes(pa,prop))+geom_point()+theme_economist()+labs(x='Plate Appearances',y='Proportion of Raw RAPM not Regressed',title = 'Regression to the Mean Weighting Function')

pitcher <-
  pitcher %>% left_join(pitcher %>% filter((log(pa, exp(
    .5
  )) / log(max(
    pa
  ) * 5, exp(
    .5
  ))) > .01) %>% group_by(Season) %>%
    summarise(min = min(`1`), mean = mean(`1` - min(`1`)))) %>%
  mutate(prop = (log(pa, exp(.5)) / log(max(pa) * 5, exp(.5)))) %>% group_by(Season) %>% mutate(raw_rapm = ((`1` - min) * 100 / mean),
                                                                                                rapm =
                                                                                                  ((`1` - min) * 100 *
                                                                                                     prop / mean) + ((1 - prop) * mean((`1` - min) * 100 / mean)))

fg_bat<-fg_bat_leaders(2019,2020,qual='n',ind=1)%>%filter(PA>0)
fg_pitch<-fg_pitch_leaders(2019,2020,qual='n',ind=1)%>%filter(TBF>0)

batter_fg <-
  batter %>% left_join(
    fg_bat %>% select(playerid, Season, Name, wOBA),
    by = c('Season', 'fg_key' = 'playerid')
  ) %>% left_join(
    fg_bat %>% select(playerid, Season, Name, wOBA),
    by = c('Season', 'name' = 'Name')
  ) %>% mutate(wOBA = ifelse(is.na(wOBA.x), wOBA.y, wOBA.x))


fix_double<-function(data){
  pitcher_double_fin<-data%>%filter(fg_key==playerid)
  row<-data%>%filter(!(id%in%pitcher_double_fin$id)&!(playerid==pitcher_double_fin$playerid))
  pitcher_double_fin<-pitcher_double_fin%>%rbind(row)  
}

batter_double <- batter_fg %>% group_by(id,Season) %>% filter(n() > 1)
batter_double_nest <- batter_double %>% ungroup() %>% nest(-c(name,Season))
batter_double_fin <-
  batter_double_nest %>% mutate(un = map(data,  ~ fix_double(.x))) %>% select(-data) %>%
  unnest(un)

if(nrow(batter_double_nest)!=0){
  batter_fg_fin <- batter_fg %>%
  group_by(id,Season) %>% filter(n() == 1) %>%
  rbind(batter_double_fin) %>%
    filter(!is.na(wOBA))
}else{
  batter_fg_fin <- batter_fg %>%
    filter(!is.na(wOBA))
}

pitcher_fg <-
  pitcher %>% left_join(
    fg_pitch %>% select(playerid, Season, Name, `FIP-`),
    by = c('Season', 'fg_key' = 'playerid')
  ) %>% left_join(
    fg_pitch %>% select(playerid, Season, Name, `FIP-`),
    by = c('Season', 'name' = 'Name')
  ) %>% mutate(`FIP-` = ifelse(is.na(`FIP-.x`), `FIP-.y`, `FIP-.x`))

pitcher_double <- pitcher_fg %>% group_by(Season,id) %>% filter(n() > 1)
pitcher_double_nest <- pitcher_double %>% ungroup() %>% nest(-c(Season,name))
pitcher_double_fin <-
  pitcher_double_nest %>% mutate(un = map(data,  ~ fix_double(.x))) %>% select(-data) %>%
  unnest(un)

if(nrow(pitcher_double_nest)!=0){
  pitcher_fg_fin <- pitcher_fg %>%
    group_by(id,Season) %>% filter(n() == 1) %>%
    rbind(pitcher_double_fin) %>%
    filter(!is.na(`FIP-`))
}else{
  pitcher_fg_fin <- pitcher_fg %>%
    filter(!is.na(`FIP-`))
}

cor(pitcher_fg_fin$raw_rapm,pitcher_fg_fin$`FIP-`)
#.650
cor(batter_fg_fin$wOBA,batter_fg_fin$raw_rapm)
#.852
###########Is opponent quality accounted for?
plate_appearance_pbq <-
  plate_appearance %>% left_join(pitcher_fg_fin %>% select(id, `FIP-`), by =
                                   c('pitcher' = 'id')) %>% left_join(batter_fg_fin %>% select(id, Season, wOBA),
                                                                      by = c('batter' = 'id', 'game_year' = 'Season'))

batter_oppq<-plate_appearance_pbq%>%group_by(game_year,batter)%>%summarise(`FIP-`= median(`FIP-`,na.rm=T))
pitcher_oppq<-plate_appearance_pbq%>%group_by(game_year,pitcher)%>%summarise(wOBA = median(wOBA,na.rm=T))

pitcher_fin<-pitcher_fg_fin%>%left_join(pitcher_oppq,by=c('id'='pitcher','Season'='game_year'))
batter_fin<-batter_fg_fin%>%left_join(batter_oppq,by=c('id'='batter','Season'='game_year'))

pitcher_opp_pval<-data.frame()
for(i in 1:max(pitcher_fin$pa)){
  opp_pval<-summary(lm(rapm~wOBA+`FIP-`,pitcher_fin%>%filter(pa>i)))$coefficients[2,4]
  pitcher_opp_pval<-pitcher_opp_pval%>%rbind(data.frame(pa=i,pval=opp_pval))  
}

ggplot(pitcher_opp_pval, aes(pa, pval)) + geom_point() + geom_smooth() + geom_hline(yintercept=.05)+labs(x='Plate Appearance Cut Off',y='P-Value',title='Significance Level of Opponent wOBA in Predicting RAPM')+theme_economist()

summary(lm(rapm~wOBA+`FIP-`,pitcher_fin))

batter_opp_pval<-data.frame()
for(i in 1:max(batter_fin$pa)){
  opp_pval<-summary(lm(rapm~wOBA+`FIP-`,batter_fin%>%filter(pa>i)))$coefficients[3,4]
  batter_opp_pval<-batter_opp_pval%>%rbind(data.frame(pa=i,pval=opp_pval))  
}

ggplot(batter_opp_pval, aes(pa, pval)) + geom_point() + geom_smooth() + geom_hline(yintercept=.05)+labs(x='Plate Appearance Cut Off',y='P-Value',title='Significance Level of Opponent FIP- in Predicting RAPM')+theme_economist()

summary(lm(rapm~wOBA+`FIP-`,batter_fin))

#it appears we do have something like a model which shows a player's talent with respect to their opponents
#how does it predict future performance?
cor_diff_bat<-data.frame()
for(i in 1:max(batter_fin$pa)){
  bat_pred <-
    batter_fin %>% filter(pa>i) %>% select(id, Season, name, rapm, wOBA) %>% group_by(id, name) %>%
    summarise(f_rapm = rapm[2],
              f_woba = wOBA[2],
              s_woba = wOBA[1]) %>% na.omit() %>% ungroup()
  cor_diff<-cor(bat_pred%>%select(-c(id,name)))[3,1]-cor(bat_pred%>%select(-c(id,name)))[3,2]
  cor_diff_bat<-cor_diff_bat%>%rbind(data.frame(pa=i,n=nrow(bat_pred),cor_r=cor(bat_pred%>%select(-c(id,name)))[3,1],cor_p=cor(bat_pred%>%select(-c(id,name)))[3,2],cor_diff=cor_diff))
}

ggplot(cor_diff_bat%>%na.omit(), aes(pa, cor_diff)) + geom_point() + geom_smooth() + geom_hline(yintercept=0) + labs(title = 'Predicting Future wOBA with RAPM vs. wOBA', x = "Plate Appearance Cutoff", y = 'Correlation Difference')+theme_economist()
summary((cor_diff_bat%>%na.omit())$cor_diff)
cor_diff_bat%>%na.omit()%>%summarise(mean_cor_r=mean(cor_r),mean_cor_p=mean(cor_p),mean_cor_diff=mean(cor_diff))

cor_diff_pitch<-data.frame()
for(i in 1:max(pitcher_fin$pa)){
 pitch_pred <-
    pitcher_fin %>% filter(pa>i) %>% select(id, Season, name, rapm, `FIP-`) %>% group_by(id, name) %>%
    summarise(f_rapm = rapm[2],
              `f_FIP-` = `FIP-`[2],
              `s_FIP-` = `FIP-`[1]) %>% na.omit() %>% ungroup()
  cor_diff<-cor(pitch_pred%>%select(-c(id,name)))[3,1]+cor(pitch_pred%>%select(-c(id,name)))[3,2]
  cor_diff_pitch<-cor_diff_pitch%>%rbind(data.frame(pa=i,n=nrow(pitch_pred),cor_r=cor(pitch_pred%>%select(-c(id,name)))[3,1],cor_p=cor(pitch_pred%>%select(-c(id,name)))[3,2],cor_diff=cor_diff))
}

ggplot(cor_diff_pitch%>%na.omit(), aes(pa, cor_diff)) + geom_point() + geom_smooth() + geom_hline(yintercept=0) + labs(title = 'Predicting Future FIP- with RAPM vs. FIP-', x = "Plate Appearance Cutoff", y = 'Correlation Difference')+theme_economist()
summary((cor_diff_pitch%>%na.omit())$cor_diff)
cor_diff_pitch%>%na.omit()%>%summarise(mean_cor_r=mean(cor_r),mean_cor_p=mean(cor_p),mean_cor_diff=mean(cor_diff))
