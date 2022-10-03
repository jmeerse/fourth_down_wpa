library(tidyverse)
library(nflfastR)

pbp <- load_pbp(2020:2022) %>% filter(down == 4, 
                                      play_type == "run" | play_type == "pass") %>% 
  select(posteam, play_type, ydstogo,
         touchdown, fourth_down_converted, 
         fourth_down_failed,
         wp, wpa, series_result, 
         home_team, away_team, result, game_id)



pbp$winner <- if_else(pbp$result > 0, pbp$home_team, pbp$away_team) 
pbp$win <- if_else(pbp$winner == pbp$posteam, 1, 0)
pbp$lose <- if_else(pbp$win == 1, 0, 1)


fourth <- pbp %>% group_by(posteam) %>% 
  summarise(convert_pct = sum(fourth_down_converted) / n(),
            count = n(),
            wpa_convert = sum(wpa * fourth_down_converted),
            wpa_fail = sum(wpa * fourth_down_failed),
            tot_wpa = wpa_convert + wpa_fail
            )

fourth <- left_join(fourth, teams_colors_logos, by = c("posteam" = "team_abbr"))

fourth %>% ggplot(aes(y = reorder(posteam, tot_wpa), 
                      x = tot_wpa)) + 
  geom_col(color = fourth$team_color2,
           fill = fourth$team_color) +
  labs(title = "Total WPA Going on 4th Down",
       subtitle = "Since start of 2020 season",
       x = "Total WPA",
       y = "",
       caption = "@jmeerse | data: nflfastR") 

#Just Baltimore below
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

bal21 %>% group_by(game_id) %>% 
  summarise(convert_pct = sum(fourth_down_converted) / n(),
            count = n(),
            wpa_convert = sum(wpa * fourth_down_converted),
            wpa_fail = sum(wpa * fourth_down_failed),
            tot_wpa = wpa_convert + wpa_fail,
            win = if_else(last(win == 1), "win", "lose"),
            margin = last(abs(result)))
