# https://www.kaggle.com/jccampos/nfl-2020-winner-solution-the-zoo
# 
library(tidyverse)
library(torch)
source("R/cleaning_functions.R")
set.seed(2013)

# if you just want to keep one frame from each play
# frame_number is (frame_number - 10) seconds into the play
one_frame = FALSE
frame_number <- 25

# all frames will be in this range
# cut play off here (frame_number - 10) frames after snap
start_frame_number <- 0
end_frame_number <- 45

keep_frames <- c(5, 10, 15, 20, 25, 30, 35)

# pull week 1 through this week:
final_week <- 17

labels <- readRDS("data-raw/coverage_labels.rds")

get_bdb <- function(w) {
  df <- read_csv(glue::glue("../nfl-big-data-bowl-2021/input/week{w}.csv")) %>%
    
    # do all the cleaning
    wrapper() %>%
    # stop plays at pass forward
    # and remove short (< 1 seconds) plays
    cut_plays(throw_frame = 20) %>%
    mutate(play = paste0(game_id, "_", play_id))
  
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
    inner_join(labels, by = c("game_id", "play_id")) %>%
    left_join(qbs, by = c("game_id", "play_id", "frame_id")) %>%
    compute_o_diff("qb") %>%
    # scale 0 to 1
    mutate(o_to_qb = o_to_qb / 180) %>%
    dplyr::filter(
      position != "QB",
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
      coverage,
      x, 
      y,
      s_x,
      s_y,
      o_to_qb,
      los_x,
      dist_from_los
    ) %>%
    # this slice is probably not necessary anymore after getting rid of plays
    # with 2 qbs
    group_by(game_id, play_id, frame_id, nfl_id) %>%
    dplyr::slice(1) %>%
    # for getting plays without any defense or offense players
    group_by(game_id, play_id, frame_id) %>%
    mutate(
      n_defenders = sum(defense),
      n_offense = sum(1 - defense)
    ) %>%
    ungroup() %>%
    filter(
      n_defenders > 2 & n_offense > 2
    )
  
  if (one_frame) {
    df <- df %>% filter(frame_id == frame_number)
  }
  
  df <- df %>%
    filter(
      # cut play off based on stuff above
      between(frame_id, start_frame_number, end_frame_number)
      ) %>%
    group_by(play) %>%
    filter(
      frame_id %in% keep_frames | frame_id == max(frame_id)
    ) %>%
    ungroup() %>%
    # make frame IDs start at 1
    mutate(frame_id = frame_id - min(frame_id) + 1) %>%
    select(-game_id, -play_id, -n_defenders, -n_offense) %>%
    arrange(
      play, frame_id, defense, nfl_id
    )
  
  return(df)
  
}

df <- map_df(1:final_week, get_bdb)

df

# number of features that only depend on def player
def_only_features <- 6
# number of features that depend on defense and offense player
off_def_features <- 4

n_features <- def_only_features + off_def_features

object.size(df) %>% format("MB")



# using week 1 for testing so see how long this is
# assumes data ordered with week 1 first
test_length <- df %>% filter(week == 1) %>% select(play) %>% unique() %>% nrow()

# offense: 4-5 players
# defense: 5-11 players

# def x, y
# def Sx, y
# def o to qb
# def distance from los
# off x, y - def x, y
# off Sx, y - def Sx, y

# input shape (time steps (t) * n_features (10) * n_defenders (11) * n_non-qb-offense (5))
# features 1-6 x, y, Sx, Sy, o_to_qb, dist_from_los, 
# (7) off x - def x, (8) off y - def, (9) off Sx - def, (10) off Sy - def

play_indices <- df %>%
  select(play, frame_id) %>%
  unique() %>%
  # get play index for 1 : n_plays
  mutate(
    i = as.integer(as.factor(play))
  ) %>%
  # get time step indices
  group_by(play) %>%
  mutate(f = 1 : n()) %>%
  ungroup()

n_frames <- n_distinct(play_indices$f)
plays <- n_distinct(df$play)
plays
test_length
n_frames

# for accessing later
if (one_frame) {
  tibble::tibble(
    "test_length" = test_length,
    "plays" = plays
  ) %>%
    saveRDS("data/data_sizes_one_frame.rds")
} else {
  tibble::tibble(
    "test_length" = test_length,
    "plays" = plays
  ) %>%
    saveRDS("data/data_sizes.rds")
}

# i, features, def, off, 
train_x = torch_empty(plays, n_frames, n_features, 11, 5)

# for testing
# row <- play_indices %>%
#   dplyr::slice(1)

fill_row <- function(row) {
  
  # indices for putting in tensor
  i = row$i
  f = row$f
  
  # play info for extracting from df
  playid = row$play
  frameid = row$frame_id
  
  offense_df <- df %>%
    filter(defense == 0, play == playid, frame_id == frameid) %>%
    select(play, o_x = x, o_y = y, o_s_x = s_x, o_s_y = s_y, los_x)
  
  # if only 4 offensive players, make a fake one standing at los
  # not moving anywhere
  # adding a little noise bc otherwise things were breaking with all the zeros
  if (nrow(offense_df) == 4) {
    dummy_player <- tibble::tibble(
      "play" = offense_df$play[1],
      "o_x" = offense_df$los_x[1] + rnorm(1, sd = .1),
      "o_y" = 160/6 + rnorm(1, sd = .1),
      "o_s_x" = pmax(0, rnorm(1, sd = .1)),
      "o_s_y" = pmax(0, rnorm(1, sd = .1))
    )
    
    offense_df <- bind_rows(
      offense_df,
      dummy_player
    )
  }
  
  defense_df <- df %>%
    filter(defense == 1, play == playid, frame_id == frameid)
  
  defenders <- nrow(defense_df)
  
  # fill in dummy defenders
  if (defenders < 11) {
    
    # standing on los, not moving, facing qb
    dummy_player <- tibble::tibble(
      "play" = defense_df$play[1],
      "frame_id" = defense_df$frame_id[1],
      "x" = defense_df$los_x[1] + rnorm(1, sd = .1),
      "defense" = 1,
      "y" = 160/6 + rnorm(1, sd = .1),
      "s_x" = pmax(0, rnorm(1, sd = .1)),
      "s_y" = pmax(0, rnorm(1, sd = .1)),
      "o_to_qb" = pmax(0, rnorm(1, sd = .1)),
      "dist_from_los" = x - defense_df$los_x[1]
    ) %>%
      # why does this work? who knows
      # https://statisticsglobe.com/repeat-rows-of-data-frame-n-times-in-r
      slice(rep(1, each = 11 - defenders))
    
    defense_df <- bind_rows(
      defense_df,
      dummy_player
    )
    
  }
  
  # test
  big_df <- defense_df %>%
    left_join(offense_df, by = "play") %>%
    mutate(diff_x = o_x - x, diff_y = o_y - y, diff_s_x = o_s_x - s_x, diff_s_y = o_s_y - s_y) %>%
    select(x, y, s_x, s_y, o_to_qb, dist_from_los, starts_with("diff_"))
  
  # fill in info for each defender
  for (j in 1:11) {
    
    # message(j)
    
    start <- 1 + 5 * (j-1)
    end <- 5 * j
    
    train_x[i, f, 1:n_features, j, ] <- big_df %>% 
      dplyr::slice(start:end) %>%
      as.matrix() %>%
      t()
    
  }
  
  # return(train_x)
  
}

# test
# fill_row(play_indices %>% dplyr::slice(1))
# train_x[1, 1, , , ]
# train_x[1, ..]

future::plan("multicore")
# build the tensor for train and test data
furrr::future_walk(1 : nrow(play_indices), ~{
  if(.x %% 250 == 0) {
    message(glue::glue("{.x} of {nrow(play_indices)}"))
  }
  fill_row(play_indices %>% dplyr::slice(.x))
}, .options = furrr_options(seed = TRUE))


train_y <- torch_zeros(plays, dtype = torch_long())

train_y[1:plays] <- df %>%
  mutate(coverage = as.factor(coverage) %>% as.integer()) %>%
  group_by(play) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  pull(coverage)

dim(train_x)
dim(train_y)

if (one_frame) {

  message("saving one frame dataframe")
  
  # get rid of singleton time dimension
  train_x <- train_x %>% torch_squeeze()
  
  torch_save(train_x, "data/train_x_one_frame.pt")
  torch_save(train_y, "data/train_y_one_frame.pt")
  
} else {
  torch_save(train_x, "data/train_x.pt")
  torch_save(train_y, "data/train_y.pt")
}


