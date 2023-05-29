#' This function gets the FanGraphs Player IDs for the 
#' top n players for a given team and year.
#'
#' @param tm Character value indicating the team.
#' @param yr A numeric value indicating the four digit year.
#' @return A tibble with player name and id, and wRC
#' @export
get_top_batters <- function(tm, yr, num_players = 7) {
  # Find top wRC players on a team
  all_batters <- baseballr::fg_batter_leaders(x = yr, y = yr, qual = 0)
  
  # Get top n players on a team
  team_top <- all_batters %>%
    filter(Team == tm) %>%
    select(playerid, Name, wRC) %>%
    arrange(wRC, decreasing = TRUE) %>%
    top_n(num_players, wt = wRC) %>%
    select(Name, playerid)
  
  return(team_top)
}

#' This function gets the daily wRC for a given batter for a given year.
#'
#' @param player_id A character value representing a fangraphs playerid.
#' @param yr A numeric value indicating four digit year.
#' @return A tibble containing daily wRC for a season 
#' @export
get_player_daily_wrc <- function(player_id, yr){
  player_wrc <- 
    baseballr::fg_batter_game_logs(playerid = player_id, year = yr) %>%
    select(Date, wRC) %>%
    group_by(Date) %>%
    summarise(wRC = mean(wRC))
  
  player_wrc$playerid <- player_id
  
  return(player_wrc)
}

#' Get dataframe of individual players' wRC by date
#'
#'
#' @param team_top tibble with player_ids
#' @param y A numeric value representing the four digit year
#' @return A tibble with daily wRC for multiple players
#' @export
get_daily_wrcs <- function(team_top, yr) {
  
  # get vector of player ids
  player_ids <- unique(team_top$playerid)
  
  # get daily wRC for each player
  m = lapply(player_ids, get_player_daily_wrc, year=yr)
  
  # get dataframe of all players by day
  tm_wrc_by_date <- do.call(rbind, m) %>%
    left_join(., team_top, by="playerid")
  
  return(yr)
}
