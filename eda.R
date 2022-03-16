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
library(stringr)
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
# rm(api_url, my_club_name, match_details_json, match_details_df, board_details_df)
# board_url <- "https://api.chess.com/pub/match/1337937/3"
# my_player <- "samsam06"
# rm(board_url, my_player)
# Function to get details of the two matches played on each board
get_board_match_details <- function(board_url, my_player) {
  print(paste("Processing board", board_url, "for player", my_player))
  board_json <- read_json(board_url)
  tibble(games = board_json$games) %>%
    hoist(games,
          end_time = "end_time",
          white_username = c("white", "username"),
          white_result = c("white", "result"),
          black_username = c("black", "username"),
          black_result = c("black", "result")) %>%
    mutate(end_time = as_datetime(end_time),
           my_player_played_as = if_else(str_to_lower(white_username) == !!my_player,
                                         "white",
                                         "black"),
           my_player_result = if_else(str_to_lower(white_username) == !!my_player,
                                      white_result,
                                      black_result),
           board_url = !!board_url) %>%
    select(-games, -starts_with("black_"), -starts_with("white_"))
}
# Function to get details of club matches
get_club_match_details <- function(api_url, my_club_name) {
  print(paste("Processing match:", api_url))
  match_details_json <- read_json(api_url)
  # Rectangle the JSON into a tidy data frame - one row per board
  match_details_df <- tibble(`match_@id` = match_details_json$`@id`,
         match_name = match_details_json$name,
         match_start_time = as_datetime(match_details_json$start_time),
         match_rules = match_details_json$settings$rules,
         match_time_class = match_details_json$settings$time_class,
         match_time_control = match_details_json$settings$time_control,
         match_min_team_players = match_details_json$settings$min_team_players,
         match_max_team_players = match_details_json$settings$max_team_players,
         match_min_rating = match_details_json$settings$min_rating,
         match_max_rating = match_details_json$settings$max_rating,
         match_min_req_games = match_details_json$settings$min_required_games,
         match_autostart = match_details_json$settings$autostart,
         teams = match_details_json$teams) %>%
    unnest_wider(teams) %>%
    unnest_longer(players) %>%
    unnest_wider(players) %>%
    filter(name == !!my_club_name) %>%
    mutate(days_per_move = if_else(match_time_class == "daily",
                                   str_remove(match_time_control, "1/") %>%
                                     as.numeric() / 86400 %>%
                                     as.integer(),
                                   NA_real_))
  # Get the details of both matches played on each board
  board_details_df <- map2_dfr(match_details_df$board,
                               match_details_df$username,
                               get_board_match_details)
  # Join together and return the results
  match_details_df %>%
    inner_join(board_details_df, by = c("board" = "board_url"))
}
# Loop through the club matches and get the details of each board and then both
# matches under each
club_match_details_df <- map2_dfr(club_matches_df$`@id`,
                                  list(club_profile_df$name),
                                  get_club_match_details)


library(repurrrsive)
tibble(disc = discog) %>%
  unnest_wider(disc)
# as_tibble(match_details_json)

# Test writing something to Google sheets
gs4_auth(email = TRUE)
ss <- gs4_find("1-day-per-move-club-data")
sheet_write(club_member_profiles_df, ss = ss, sheet = "players")
sheet_write(club_matches_df, ss = ss, sheet = "matches")
