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
library(googlesheets4)
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
  if("chess_daily" %in% names(player_stats_json)) {
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
# Get details of club matches
club_matches_json <- read_json(paste0("https://api.chess.com/pub/club/",
                                      club_name_in_url,
                                      "/matches"))
club_matches_df <- tibble(matches = pluck(club_matches_json, "finished")) %>%
  unnest_wider(matches) %>%
  bind_rows(tibble(matches = pluck(club_matches_json, "in_progress")) %>%
              unnest_wider(matches)) %>%
  bind_rows(tibble(matches = pluck(club_matches_json, "registered")) %>%
              unnest_wider(matches)) %>%
  mutate(start_time = as_datetime(start_time))

match_details_json <- read_json(club_matches_df$`@id`[1])
# as_tibble(match_details_json)

# Test writing something to Google sheets
gs4_auth(email = "deanjohnperry@gmail.com")
ss <- gs4_find("1-day-per-move-club-data")
sheet_write(club_member_profiles_df, ss = ss, sheet = "players")
sheet_write(club_matches_df, ss = ss, sheet = "matches")
