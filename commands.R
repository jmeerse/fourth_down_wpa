library(tidyverse)
library(nflfastR)

pbp21 <- load_pbp(2021)

bal21 <- pbp21 %>% filter(down == 4, posteam == "BAL", 
                          play_type == "run" | play_type == "pass")

bal21 <- bal21 %>% select(posteam, play_type, ydstogo,
                          touchdown, fourth_down_converted, 
                          fourth_down_failed,
                          wp, wpa, series_result, 
                          home_team, away_team, result, game_id)

bal21$win <- if_else((bal21$result > 0 & bal21$home_team == "BAL") |
                       (bal21$result < 0 & bal21$away_team == "BAL"), 
                     1, 0)
bal21$lose <- if_else(bal21$win == 1, 0, 1)
