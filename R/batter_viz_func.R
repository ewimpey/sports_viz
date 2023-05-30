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
  m = lapply(player_ids, get_player_daily_wrc, yr = yr)
  
  # get dataframe of all players by day
  tm_wrc_by_date <- do.call(rbind, m) %>%
    left_join(., team_top, by="playerid") %>%
    select(-playerid)
  
  return(tm_wrc_by_date)
}


#' Complete and clean a dataframe for visualizing
#'
#'
#' @param tm_wrc_by_date tibble with players and wRC by date
#' @return a tibble with all date and player combinations
#' @export
complete_wrc_df <- function(tm_wrc_by_date) {
  
  # Generate all possible combinations of dates and players
  all_combinations <- expand.grid(
    Date = unique(tm_wrc_by_date$Date),
    Name = unique(tm_wrc_by_date$Name))
  
  # Left join the original dataframe with all possible combinations
  complete_df <- left_join(all_combinations, 
                           tm_wrc_by_date,
                           by = c("Date", "Name"))
  
  # Fill in missing wRC values with 0
  complete_df$wRC[is.na(complete_df$wRC)] <- 0
  
  # Format date
  complete_df$Date <- as.Date(complete_df$Date)
  
  return(complete_df)
}

#' Get color palette for a specific team 
#'
#'
#' @param tm
#' @return a vector with at least 7 hex color codes
#' @export
get_cp <- function(tm) {
  team_colors <- list(
    ATL = c('#1e305f', #dark blue
            '#01528c', #medium blue
            '#ffffff', #white
            '#a01f3d', #scarlet
            '#d62045', #red
            '#8dbae2', #80s blue
            '#32426c', #dark blue2
            '#808080', #medium gray
            '#e7e7e8'), #light gray
    WSN = c('#AB0003', #red
            '#14225A', #navy blue
            '#FFFFFF', #white
            '#5f7080', #blue-gray
            '#808080', #medium gray
            '#c4c2c2', #light gray
            '#ba0c2f' #other red
    ), 
    BAL = c('#DF4601', #orange
            '#000000', #black)
            '#FBD872', #yellow
            '#ac3702', #dark orange
            '#808080', #medium gray
            '#e7e7e8', #light gray
            '#ffffff' #white
    ), 
    HOU = c('#002147', #dark blue
            '#D6492A', #red-orange
            '#E57200', #orange
            '#FFFFFF', #white
            '#002D62', #dark blue
            '#FFCD00', #retro yellow
            '#E87722', #retro orange
            '#97999B', #gray
            
            '#e7e7e8' #light gray
    )
    # Add more teams and their corresponding color schemes here
  )
  
  if (tm %in% names(team_colors)) {
    color_scheme <- team_colors[[tm]]
    return(color_scheme)
  } else {
    color_scheme <- team_colors[["ATL"]]
    message(paste0(tm, "  color palette, not found. Using Braves instead. Feel free to add a color palette to source code!"))
    return(color_scheme)
  }
}


#' Build a visualization 
#'
#'
#' @param tm_wrc_by_date tibble with players and wRC by date
#' @return a tibble with all date and player combinations
#' @export
viz_wrc <- function(complete_df, tm, yr, cp) {
  
  ggplot(complete_df, aes(x = Date, y = wRC, fill = Name)) +
    geom_stream(true_range = "none",
                bw = 0.55) + 
    scale_fill_manual(values = cp) +
    theme(axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position = "bottom",
          legend.justification = "left",
          legend.text = element_text()) +
    labs(title = paste0(yr, " ", tm),
         subtitle = "Offensive Production",
         y = '',
         x = '')
}



#' Generate the offensive wRC plot by player over a season
#'
#'
#' @param tm string representing MLB team using fangraphs standards
#' @param yr four digit numeric year
#' @return a tibble with all date and player combinations
#' @export
generate_viz <- function(tm, yr) {
  
  #get the top batters by wRC for a given team and year
  team_top <- get_top_batters(tm, yr)
  
  # get wrc by date for top players on team
  tm_wrc_by_date <- get_daily_wrcs(team_top = team_top, yr = yr)
  
  # clean and format the extracted date
  clean_df <- complete_wrc_df(tm_wrc_by_date)
  
  # get the team color palette
  cp <- get_cp(tm)
  
  # build the vizualization
  viz <- viz_wrc(clean_df, tm, yr, cp)
  
  return(viz)
  
}
