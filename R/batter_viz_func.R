#' This function gets the FanGraphs Player IDs for the 
#' top n players for a given team and year.
#'
#' @param tm Character value indicating the team.
#' @param yr A numeric value indicating the four digit year.
#' @return A character vector of player_ids
#' @export
get_top_batters <- function(tm, yr) {
  # Find top wRC players on a team
  all_batters <- baseballr::fg_batter_leaders(x = yr, y = yr, qual = 0)
  
  # Get top n players on a team
  team_top <- all_batters %>%
    filter(Team == tm) %>%
    select(playerid, Name, wRC) %>%
    arrange(wRC, decreasing = TRUE) %>%
    top_n(num_players) %>%
    pull(playerid)
  
  return(team_top)
}

#' Add two numbers
#'
#' This function adds two numbers and returns the result.
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
  
  player_wrc$player <- player_id
  
  return(player_wrc)
}

#' Get dataframe of individual players' wRC by date
#'
#'
#' @param team_top a vector of player_id values
#' @param y A numeric value representing the four digit year
#' @return A tibble with daily wRC for multiple players
#' @export
add_numbers <- function(team_top, yr) {
  
  m = lapply(team_top, get_player_daily_wrc, year=yr)
  tm_wrc_by_date <- do.call(rbind, m)
  
  return(yr)
}
