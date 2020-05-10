library(tidyverse)
library(ggplot2)
library(reshape)
library(lubridate)

# Open the File
kick_raw1 = readr::read_csv('./kickstart.csv')
kick_raw2 = readr::read_csv('./kickstart_19.csv')

#remove unneeded columns from kick_raw1 and kick_raw2
kick_raw1_v2 = kick_raw1 %>% select(.,-1,-5,-7,-9,-11,-12,-13)
kick_raw2_v2 = kick_raw2 %>% select(.,2,5,4,6,7,9,15,20)       

#rename the columns for both to match each other and make it easier to read
kick_raw1_v3 = kick_raw1_v2 %>% dplyr::rename(., sub_category = category, status = state, 
                                       pledged_usd = usd_pledged_real, goal_usd = usd_goal_real)
kick_raw2_v3 = kick_raw2_v2 %>% dplyr::rename(., launched = launched_at, pledged_usd = usd_pledged )

#combine both dataframes and remove duplicated projects
kick_v1 = rbind(kick_raw1_v3, kick_raw2_v3)
kick_v2 = kick_v1 %>% distinct(name, .keep_all = TRUE) 

#change format of deadline and launched to date
kick_v2$deadline = as_date(kick_v2$deadline, '%Y-%m-%d') 
kick_v2$launched = as_date(kick_v2$launched, "%Y-%m-%d")
kick_v2 = kick_v2[!(kick_v2$launched =="1970-01-01"),]


#create group by duration of project launched
kick_v3 = kick_v2 %>% mutate(., duration = deadline-launched)%>%
  mutate(., year = format(as.Date(kick_v2$launched, format="%d/%m/%Y"),"%Y"))
kick_v4 = kick_v3 %>% mutate(., quarter =  ifelse(as.numeric(kick_v3$duration)<=15,'Q1',
                                       ifelse(as.numeric(kick_v3$duration) > 15 & as.numeric(kick_v3$duration) <= 30,'Q2',
                                              ifelse(as.numeric(kick_v3$duration) > 30 & as.numeric(kick_v3$duration) <= 45,'Q3','Q4'))))

#check if there are stray values for status
table(kick_v4$status)

#there are still some under live which should be classified to either failed or successful
#remove undefined and suspended 
kick_live = filter(kick_v4, status == 'live')
kick_live$status = ifelse(kick_live$pledged_usd >= kick_live$goal_usd, 'successful','undefined')

kick_nolive = kick_v4%>% filter(., status != 'live')
table(kick_nolive$status)

kick_v5 = full_join(kick_nolive,kick_live)
table(kick_v5$status)

kick_v6 = kick_v5%>% filter(., status %in% c('canceled','failed', 'successful'))
table(kick_v6$status)

kick_v7 = kick_v6 %>% mutate(., profit = ifelse(goal_usd <= pledged_usd, 
                                    ((pledged_usd-goal_usd)/goal_usd), NA))

kick_v7$main_category = tolower(kick_v7$main_category)
kick_v7$sub_category = tolower(kick_v7$sub_category)

kick_v7$duration = as.numeric(substr(kick_v7$duration,1,2))


write.csv(kick_v7,file = "kickstarter_df2.csv")

kickstarter_art = kick_v7 %>%
  filter(main_category == "art") %>%
  group_by(year)

kickstarter_comics = kick_v7 %>%
  filter(main_category == "comics") %>%
  group_by(year)

kickstarter_crafts = kick_v7 %>%
  filter(main_category == "crafts") %>%
  group_by(year)

kickstarter_design = kick_v7 %>%
  filter(main_category == "design") %>%
  group_by(year)

kickstarter_dance = kick_v7 %>%
  filter(main_category == "dance") %>%
  group_by(year)

kickstarter_fashion = kick_v7 %>%
  filter(main_category == "fashion") %>%
  group_by(year)

kickstarter_film = kick_v7 %>%
  filter(main_category == "film & video") %>%
  group_by(year)

kickstarter_food = kick_v7 %>%
  filter(main_category == "food") %>%
  group_by(year)

kickstarter_games = kick_v7 %>%
  filter(main_category == "games") %>%
  group_by(year)

kickstarter_journalism = kick_v7 %>%
  filter(main_category == "journalism") %>%
  group_by(year)

kickstarter_music = kick_v7 %>%
  filter(main_category == "music") %>%
  group_by(year)

kickstarter_photography = kick_v7 %>%
  filter(main_category == "photography") %>%
  group_by(year)

kickstarter_publishing = kick_v7 %>%
  filter(main_category == "publishing") %>%
  group_by(year)

kickstarter_technology = kick_v7 %>%
  filter(main_category == "technology") %>%
  group_by(year)

kickstarter_theater = kick_v7 %>%
  filter(main_category == "theater") %>%
  group_by(year)

write.csv(kickstarter_art,file = "kickstarter_art.csv")
write.csv(kickstarter_comics,file = "kickstarter_comics.csv")
write.csv(kickstarter_crafts,file = "kickstarter_crafts.csv")
write.csv(kickstarter_dance,file = "kickstarter_dance.csv")
write.csv(kickstarter_design,file = "kickstarter_design.csv")
write.csv(kickstarter_fashion,file = "kickstarter_fashion.csv")
write.csv(kickstarter_food,file = "kickstarter_food.csv")
write.csv(kickstarter_film,file = "kickstarter_film.csv")
write.csv(kickstarter_games,file = "kickstarter_games.csv")
write.csv(kickstarter_journalism,file = "kickstarter_journalism.csv")
write.csv(kickstarter_music,file = "kickstarter_music.csv")
write.csv(kickstarter_photography,file = "kickstarter_photography.csv")
write.csv(kickstarter_publishing,file = "kickstarter_publishing.csv")
write.csv(kickstarter_technology,file = "kickstarter_technology.csv")
write.csv(kickstarter_theater,file = "kickstarter_theater.csv")

