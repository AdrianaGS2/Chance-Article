## Load libraries

library(tidyverse)
library(ggrepel) 
library(ggimage)
library(nflfastR)
library(dplyr)
library(ggpubr)
library(knitr)
library(mgcv)
library(MatchIt)
library(cobalt)
library(gt)

## Extract Data nflfastR and Clean it

Data <- nflfastR::load_pbp(1999:2021)

Clean_Data <- Data %>%
  mutate(defteam = ifelse(is.na(defteam), "NOTHING", defteam),
         timeout_team = ifelse(is.na(timeout_team), "NOTHING", timeout_team)) %>%
  group_by(game_id) %>%
  mutate(prev_def_team_timeout = as.numeric(defteam != "NOTHING") * 
           as.numeric(defteam == lag(timeout_team)),
         prev_def_team_timeout = ifelse(is.na(prev_def_team_timeout), 0,
                                        prev_def_team_timeout)) %>%
  ungroup() %>%
  mutate(is_iced_kick = prev_def_team_timeout * field_goal_attempt) %>%
  filter(play_type == "field_goal" | timeout == 1) %>%
  select(play_id, game_id, old_game_id, qtr, game_half, game_seconds_remaining,
         quarter_end, sp, goal_to_go,  yrdln, yardline_100, ydstogo,
         play_type,kick_distance, score_differential, field_goal_attempt,
         kicker_player_name, kicker_player_id, end_clock_time, result, success,
         special, wp, ep, vegas_wp, weather, roof, temp, wind, surface, posteam, defteam,
         timeout,timeout_team, desc, half_seconds_remaining, field_goal_result,
         prev_def_team_timeout, is_iced_kick, stadium, game_stadium)

Clean_Data$season <-gsub(".*?([0-9]+).*", "\\1",Clean_Data$game_id)

Clean_Data$kicker_id_season <- paste(Clean_Data$kicker_player_id,
                                     Clean_Data$season, sep="_")

data_without_timeouts <- Clean_Data %>%
  filter(play_type == "field_goal")
