#' Standardize direction and add play information
#'
#' @description Standardize direction and add play information.
#' @param df A dataframe of player tracking data obtained from a Big Data Bowl
#' or NGS highlights
#' @return The original data with the columns below appended. Note that all returned columns will have
#' cleaned names, including the original columns in `df` (e.g. play_id rather than playId), to end the tyranny of weird Big Data Bowl
#' column names.
#' \describe{
#' \item{team_name}{Values of home team (eg "SEA"), away team (eg "GB"), or "football"}
#' \item{defense}{Whether player is on defense (football has 0 here)}
#' \item{play}{Unique play identifier in format "gameid_playid" with gameid old GSIS format. Ex: "2018091000_1101".}
#' \item{nflfastr_game_id}{Game ID in nflfastR format. Ex: "2018_01_ATL_PHI"}
#' \item{week}{Week of season}
#' \item{posteam}{Possession team}
#' \item{home_team}{Home team (e.g. "PHI")}
#' \item{away_team}{Away team (e.g. "ATL")}
#' \item{down}{Down}
#' \item{ydstogo}{Yards to go}
#' \item{yardline_100}{Distance from opponent end zone}
#' \item{qtr}{Quarter}
#' \item{epa}{Expected Points Added gained on play from nflfastR}
#' \item{yards_gained}{Yards gained on play}
#' \item{air_yards}{Air yards (when applicable)}
#' \item{desc}{Play description}
#' \item{pass}{Was it a dropback? From nflfastR}
#' \item{rush}{Was it a designed rush attempt? From nflfastR}
#' \item{play_type_nfl}{Play type from NFL data. E.g. "PASS", "PENALTY", "RUSH", "SACK", "PUNT", etc.}
#' \item{team_color}{Primary team color. Useful for play animations}
#' \item{team_color2}{Secondary team color. Useful for play animations}
#' \item{team_logo_espn}{URL of team logo}
#' \item{los_x}{x location of line of scrimmage (e.g. 20 means own 10 yard line)}
#' \item{dist_from_los}{Distance of player from line of scirmmage in x direction}
#' \item{o_x}{Orientation of player in x direction}
#' \item{o_y}{Orientation of player in y direction}
#' \item{dir_x}{Direction of player in x direction}
#' \item{dir_y}{Direction of player in y direction}
#' \item{s_x}{Speed of player in x direction}
#' \item{s_y}{Speed of player in y direction}
#' \item{a_x}{Acceleration of player in x direction}
#' \item{a_y}{Acceleration of player in y direction}
#' }
#' @export
clean_and_rotate <- function(df) {

  original_cols <- df %>%
    janitor::clean_names() %>%
    names()

  added_cols <- c(
    "team_name",
    "defense",
    "play",
    "nflfastr_game_id",
    "week",
    "posteam",
    "home_team",
    "away_team",
    "down", "ydstogo", "yardline_100", "qtr", "epa", "yards_gained",
    "air_yards", "desc", "pass", "rush", "play_type_nfl",
    "team_color", "team_color2", "team_logo_espn",
    "los_x", "dist_from_los", "o_x", "o_y", "dir_x", "dir_y",
    "s_x", "s_y", "a_x", "a_y"
    )
  df %>%
    add_info() %>%
    rotate_to_ltr() %>%
    dplyr::select(tidyselect::any_of(c(original_cols, added_cols)))
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
      "posteam", "home_team", "away_team", "week",
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
    mutate(
      team_color = ifelse(team_name == "football", "#663300", team_color),
      play = paste0(game_id, "_", play_id)
      ) %>%
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
      y = ifelse(to_left == 1, 160/3 - y, y),

      # get x value of line of scrimmage
      los_x = 110 - yardline_100,
      dist_from_los = x - los_x
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
        dir_y = ifelse(is.na(dir), NA_real_, cos(dir_rad)),

        s_x = dir_x * s,
        s_y = dir_y * s,

        a_x = dir_x * a,
        a_y = dir_y * a
      )
  }

  return(df)

}



#' Compute orientation difference
#'
#' @description Compute difference in orientation between direction player is currently facing and
#' orientation if player were facing towards a given x and y location.
#' @param df A dataframe containing x, y, o, "prefix"_x, and "prefix"_y
#' @param prefix (default = "qb"). Columns prefix_x and prefix_y must be contained in `df`. These columns
#' contain the x and y locations that will be used to calculate orientation difference.
#' @return Original dataframe with o_to_"prefix" added, which is the difference in orientation
#' in degrees between the way the player is facing and where the "prefix" player is (0 is facing
#' directly at the "prefix" player, 180 is directly away).
#' @export
#' @examples
#' df <- tibble::tibble("x" = 20, "y" = 30, "o" = 270, "qb_x" = 10, "qb_y" = 25)
#' df <- compute_o_diff(df)
#' str(df)
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
    ) %>%
    select(-diff, -tmp)

  return(df)

}

#' Trim plays based on events
#'
#' @description Trim frames for a play and/or remove plays based on how quickly provided events happen in the play.
#' @param df A dataframe containing player tracking data with `event`, `frame_id`, and `play` with the latter uniquely identifying plays.
#' @param end_events Events designated as play end events. Defaults are when a pass is thrown or QB's involvement ends in some
#' other way (sack, strip sack, shovel pass, etc).
#' @param time_after_event Number of frames to keep after the `end_events` (default: 0).
#' Note that there are 10 frames in each second so providing 10 would keep one additional second after a pass was thrown
#' when using the default end events.
#' @param throw_frame If not NULL, for plays when one of the `end_events` happens before this frame,
#' these plays will be removed from the returned df (default: 25, ie 1.5 seconds
#' into the play). To not employ play dropping, provide throw_frame = NULL and all of the plays provided in original
#' `df` will be returned.
#' @return The original df with trimmed frames (and if throw_frame not NULL, the shorter plays removed).
#' @export
cut_plays <- function(df,

  # cut off anything that happens after this event
  end_events = c("pass_forward", "qb_sack", "qb_strip_sack", "qb_spike", "tackle", "pass_shovel"),
  # keep this many frames after the end event
  time_after_event = 0,
  # remove plays with throws before this frame
  throw_frame = 25) {

  # default truncates data at pass
  if (!is.null(end_events)) {

    mins <- df %>%
      arrange(play, frame_id) %>%
      group_by(play) %>%
      mutate(
        end_event = cumsum(event %in% end_events)
      ) %>%
      filter(end_event > 0) %>%
      dplyr::slice(1) %>%
      ungroup() %>%
      # if throw happens on frame 36 and user wants 5 frames, keep 36 - 40
      mutate(end_frame = frame_id + time_after_event - 1) %>%
      select(play, end_frame)

    df <- df %>%
      left_join(mins, by = c("play")) %>%
      filter(frame_id <= end_frame)


  }

  # if the play ends before throw_frame, throw out the play
  # frame 25 is 1.5 seconds into play
  if (!is.null(throw_frame)) {

    df <- df %>%
      arrange(play, frame_id) %>%
      group_by(play) %>%
      mutate(max_frame = max(frame_id)) %>%
      filter(max_frame >= throw_frame) %>%
      ungroup()

  }

  return(df)

}



#' Prepare a week of data from the 2021 Big Data Bowl
#'
#' @description Prepare a week of data from the 2021 Big Data Bowl (data from 2018 season). To use this, you'll need to have
#' the BDB data saved and unzipped somewhere in a directory on your computer.
#' @param week Get and prepare this week of data (1-17)
#' @param dir Location of directory where BDB data lives. Default is unzipped to adjacent directory
#' (default = "../nfl-big-data-bowl-2021/input")
#' @param trim_frame If a throw, sack, etc happens before this frame, drop the play (default = 25; i.e. before
#' 1.5 seconds into the play).
#' @param frames_after_throw If a frame happened more than this many frames after throw, drop the frame.
#' @param keep_frames Keep these frames. Default: NULL (ie keep all frames).
#' @param drop_positions Drop these positions from the returned data (default = "QB").
#' @details Loads raw .csvs from 2021 BDB, cleans, rotates, applies frame trimming, calculates orientation to QB,
#' drops plays without at least 3 offensive and defensive players.
#' @export
prepare_bdb_week <- function(
  week,
  dir = "../nfl-big-data-bowl-2021/input",
  trim_frame = 25,
  frames_after_throw = 10,
  keep_frames = NULL,
  drop_positions = c("QB")
  ) {
  df <- readr::read_csv(glue::glue("{dir}/week{week}.csv")) %>%

    # do all the cleaning
    clean_and_rotate() %>%
    # stop plays at pass forward
    # and remove short (< 1 seconds) plays
    cut_plays(
      # if throw happens before this frame (1.5 seconds after snap), discard
      throw_frame = trim_frame,
      # keep this many frames after the throw
      time_after_event = frames_after_throw
    )

  # get qb location
  # first, throw out plays with 2 qbs
  n_qbs <- df %>%
    filter(position == "QB") %>%
    group_by(game_id, play_id, frame_id) %>%
    summarize(qbs = n()) %>%
    group_by(game_id, play_id) %>%
    summarise(qbs = max(qbs)) %>%
    filter(qbs == 1) %>%
    ungroup()

  # now get the location of the QB
  qbs <- df %>%
    filter(position == "QB") %>%
    dplyr::select(
      game_id,
      play_id,
      frame_id,
      qb_x = x,
      qb_y = y
    ) %>%
    inner_join(n_qbs, by = c("game_id", "play_id")) %>%
    select(-qbs)

  # add qb location
  df <- df %>%
    # inner_join(labels, by = c("game_id", "play_id")) %>%
    left_join(qbs, by = c("game_id", "play_id", "frame_id")) %>%
    compute_o_diff("qb") %>%
    # scale 0 to 1
    mutate(o_to_qb = o_to_qb / 180) %>%
    dplyr::filter(
      position != drop_positions,
      !is.na(position),
      !is.na(o_to_qb)
    ) %>%
    dplyr::select(
      game_id,
      play_id,
      week,
      frame_id,
      nfl_id,
      play,
      defense,
      # coverage,
      x,
      y,
      s_x,
      s_y,
      a_x,
      a_y,
      o,
      o_to_qb,
      los_x,
      dist_from_los
    ) %>%
    # this slice is probably not necessary anymore after getting rid of plays
    # with 2 qbs
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    dplyr::slice(1) %>%
    # for getting rid of plays without any defense or offense players
    group_by(game_id, play_id, frame_id) %>%
    mutate(
      n_defenders = sum(defense),
      n_offense = sum(1 - defense)
    ) %>%
    ungroup() %>%
    filter(
      n_defenders > 2 & n_offense > 2,
      n_defenders <= 11 & n_offense <= 11
    )

  if (!is.null(keep_frames)) {
    df <- df %>%
      filter(
        frame_id %in% keep_frames
      )
  }

  df %>%
    select(-game_id, -play_id, -n_defenders, -n_offense) %>%
    arrange(
      play, frame_id, defense, nfl_id
    )

}


