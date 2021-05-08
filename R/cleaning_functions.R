library(tidyverse)

nfl_field <- sportyR::geom_football(
  'nfl', 
  # the CC at the end gives the field lower alpha
  grass_color = "#196f0cCC"
)

lee <- nflfastR:::load_lees_games() %>%
  # since no tracking data before 2017
  filter(season >= 2017)

schedule <- lee %>%
  select(season, game_id = old_game_id) %>%
  mutate(game_id = as.integer(game_id))

max_year <- lee %>%
  filter(!is.na(result), season >= 2020) %>%
  tail(1) %>%
  pull(season)

load_nflfastr <- function(y) {
  
    .url <- glue::glue("https://github.com/nflverse/nflfastR-data/blob/master/data/play_by_play_{y}.rds?raw=true")
    con <- url(.url)
    pbp <- readRDS(con)
    close(con)
    return(pbp)

}

message(glue::glue("Getting nflfastR data"))
pbp <- map_df(2017:max_year, load_nflfastr) %>%
  dplyr::rename(nflfastr_game_id = game_id, game_id = old_game_id) %>%
  dplyr::select(
    nflfastr_game_id, 
    game_id, 
    play_id, 
    posteam, 
    home_team, 
    away_team, 
    down, 
    ydstogo, 
    yardline_100,
    qtr, 
    epa, 
    yards_gained, 
    air_yards, 
    desc, 
    pass,
    rush,
    play_type_nfl
  ) %>%
  dplyr::mutate(game_id = as.integer(game_id))


# get team colors and logo for joining 
colors <- nflfastR::teams_colors_logos %>%
  select(team_name = team_abbr, team_color, team_color2, team_logo_espn)

wrapper <- function(df) {
  df %>%
    add_info() %>%
    rotate_to_ltr() %>%
    return()
}

# creates team_name, defense, and adds some play info from nflfastr
add_info <- function(df) {
  
  # make column names look reasonable
  df <- df %>%
    janitor::clean_names()
  
  # NGS highlights use "frame" instead of "frame_id" so make frame_id for these
  if (!"frame_id" %in% names(df) & "frame" %in% names(df)) {
    df <- df %>%
      dplyr::rename(frame_id = frame)
  }
  
  # NGS highlights have home_team_flag instead of team
  if (!"team" %in% names(df)) {
    
    df <- df %>%
      mutate(
        team = case_when(
          home_team_flag == 1 ~ "home",
          home_team_flag == 0 ~ "away",
          is.na(home_team_flag) ~ "football"
        )
      )
    
  }
  
  # 2020 bdb used "orientation" instead of "o"
  if ("orientation" %in% names(df)) {
    df <- df %>%
      dplyr::rename(o = orientation)
  }
  
  # 2020 big data bowl has weird play IDs where game_id is pre-pended
  if (max(nchar(df$play_id)) > 10) {
    
    df <- df %>%
      mutate(
        play_id = substr(play_id, 11, 14) %>% as.integer(),
        game_id = as.integer(game_id),
        
        # since bdb only has handoffs and doesn't have event, put in the event
        event = "handoff"
      )
    
  }
  
  
  df %>%
    # get rid of the columns we're joining so no join duplicates
    select(-tidyselect::any_of(c(
      "posteam", "home_team", "away_team", 
      "down", "ydstogo", "qtr", "yardline_100", "epa",
      "yards_gained", "air_yards", "desc", "pass", "rush", "play_type_nfl"
      ))) %>%
    left_join(pbp, by = c("game_id", "play_id")) %>%
    mutate(
      team_name = case_when(
        team == "home" ~ home_team,
        team == "away" ~ away_team,
        # for the football ("football")
        TRUE ~ "football",
      ),
      defense = case_when(
        posteam == home_team & team == "away" ~ 1,
        posteam == away_team & team == "home" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    left_join(colors, by = "team_name") %>%
    mutate(team_color = ifelse(team_name == "football", "#663300", team_color)) %>%
    return()
  
}

# rotate field so all plays are left to right
# affects x, y, o, dir
rotate_to_ltr <- function(df) {
  
  if (!"play_direction" %in% names(df)) {
    message("Can't find play direction. Inferring from offense & defense locations at snap")
    
    df <- df %>% 
      filter(event == "ball_snap", team != "football") %>% 
      group_by(game_id, play_id, defense) %>%
      summarize(mean_x = mean(x, na.rm = T)) %>%
      pivot_wider(names_from = defense, values_from = mean_x, names_prefix = "x_") %>%
      ungroup() %>%
      mutate(
        play_direction = 
          ifelse(
            # if defense has bigger x than offense, it's left to right
            x_1 > x_0, 
            "right",
            "left"
            )
        ) %>%
    select(game_id, play_id, play_direction) %>%
    inner_join(df,  by = c("game_id", "play_id"))

  }
  
  # now we're ready to flip everything on left-moving plays
  df <- df %>%
    mutate(
      # standardize all plays so they are left to right
      to_left = ifelse(play_direction == "left", 1, 0),
      
      # reflect x & y
      x = ifelse(to_left == 1, 120 - x, x),
      y = ifelse(to_left == 1, 160/3 - y, y)
    )
  
  # if orientation is in df, standardize it
  if ("o" %in% names(df)) {
    df <- df %>%
      mutate(
        # rotate 180 degrees for the angles
        o = ifelse(to_left == 1, o + 180, o), 
        
        # make sure measured 0 to 360
        o = ifelse(o > 360, o - 360, o),
        
        # convert to radians
        o_rad = pi * (o / 180),
        
        # get orientation and direction in x and y direction
        # NA checks are for the ball
        o_x = ifelse(is.na(o), NA_real_, sin(o_rad)),
        o_y = ifelse(is.na(o), NA_real_, cos(o_rad))
      )
  }
  
  # if dir is in df, standardize it
  if ("dir" %in% names(df)) {
    df <- df %>%
      mutate(
        # rotate 180 degrees for the angles
        dir = ifelse(to_left == 1, dir + 180, dir),
        
        # make sure measured 0 to 360
        dir = ifelse(dir > 360, dir - 360, dir),
        
        # convert to radians
        dir_rad = pi * (dir / 180),
        
        # get orientation and direction in x and y direction
        # NA checks are for the ball
        dir_x = ifelse(is.na(dir), NA_real_, sin(dir_rad)),
        dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad))
      )
  }
  
  return(df)
  
}



# compute angle difference between x and y and 
# some prefix_x and prefix_y,
# returning o_to_prefix
compute_o_diff <- function(df, prefix = "qb") {
  
  name_x <- sym(paste0(prefix, "_x"))
  name_y <- sym(paste0(prefix, "_y"))
  
  new_column <- paste0("o_to_", prefix)
  
  df <- df %>%
    mutate(
      # compute distances
      dis_x = {{name_x}} - x, 
      dis_y = {{name_y}} - y,
      
      # get atan2 in degrees
      tmp = atan2(dis_y, dis_x) * (180 / pi),
      
      # set clockwise (360 - tmp) with 0 on top instead of east (+ 90)
      # https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space
      tmp = (360 - tmp) + 90,
      
      # make sure 0 to 360
      tmp = case_when(tmp < 0 ~ tmp + 360, 
                      tmp > 360 ~ tmp - 360 ,
                      TRUE ~ tmp),
      
      # difference in angles
      diff = abs(o - tmp),
      
      # angle to qb
      !!new_column := pmin(360 - diff, diff)
    )
  
  return(df)
  
}
