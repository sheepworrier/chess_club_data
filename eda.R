library(Microsoft365R)
library(readxl)
library(jsonlite)
library(lubridate)
library(tidyr)
library(tidylog)
library(purrr)
library(furrr)
library(dplyr)
library(tictoc)
# Connect to SharePoint
sites_list <- list_sharepoint_sites()
sp_site <- get_sharepoint_site(site_id = sites_list[[1]]$properties$id)
# Specify club to analyse on chess.com
club_name_in_url <- "1-day-per-move-club"
# Get the club profile using the API
club_profile_json <- read_json(paste0("https://api.chess.com/pub/club/",
                                      club_name_in_url))
club_admin_urls <- tibble(`@id` = pluck(club_profile_json, "admin") %>%
                            as.character()) %>%
  mutate(is_admin = TRUE)
club_profile_df <- as_tibble(club_profile_json) %>%
  select(-admin) %>%
  distinct()
# Get the club members using the API
club_members_json <- read_json(paste0("https://api.chess.com/pub/club/",
                                      club_name_in_url,
                                      "/members"))
club_members_df <- tibble(weekly_players = club_members_json$weekly) %>%
  unnest_wider(weekly_players) %>%
  mutate(activity_level = "weekly") %>%
  bind_rows(tibble(monthly_players = club_members_json$monthly) %>%
              unnest_wider(monthly_players) %>%
              mutate(activity_level = "monthly")) %>%
  bind_rows(tibble(inactive_players = club_members_json$all_time) %>%
              unnest_wider(inactive_players) %>%
              mutate(activity_level = "inactive")) %>%
  mutate(joined_club = as_datetime(joined)) %>%
  select(-joined)
# Get the player profile information
get_player_profile <- function(username) {
  print(paste("Processing profile:", username))
  player_profile_json <- read_json(paste0("https://api.chess.com/pub/player/",
                                          username))
  as_tibble(player_profile_json) %>%
    mutate(last_online = as_datetime(last_online),
           joined = as_datetime(joined))
}
# Get the player profiles of all the club members
plan(multisession)
tic()
club_member_profiles_df <-
  future_map_dfr(club_members_df$username, get_player_profile) %>%
  inner_join(club_members_df, by = "username") %>%
  left_join(club_admin_urls)
toc()
# Get the daily stats of a player
get_player_daily_stats <- function(username) {
  print(paste("Processing stats:", username))
  player_stats_json <- read_json(paste0("https://api.chess.com/pub/player/",
                                        username,
                                        "/stats"))
  if(!is.na(player_stats_json$chess_daily)) {
    as_tibble(player_stats_json$chess_daily$last) %>%
      bind_cols(as_tibble(player_stats_json$chess_daily$record)) %>%
      mutate(username = !!username)
  }
}
# Get the player stats of all the club members
tic()
club_member_daily_stats <- map_dfr(club_members_df$username,
                                   get_player_daily_stats)
toc()