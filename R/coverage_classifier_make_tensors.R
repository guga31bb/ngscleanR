# https://www.kaggle.com/jccampos/nfl-2020-winner-solution-the-zoo
# 
library(tidyverse)
library(torch)
source("R/cleaning_functions.R")
source("R/coverage_classifier_functions.R")
set.seed(2013)

# if you just want to keep one frame from each play
# frame_number is (frame_number - 10) seconds into the play
one_frame = FALSE
frame_number <- 28

# number of features that only depend on def player
# dist_from_los, y, s_x, s_y, a_x, a_y, o_to_qb
def_only_features <- 7
# number of features that depend on defense and offense player
# rel x, rel y, rel sx, rel sy, rel ax, rel ay
off_def_features <- 6
n_features <- def_only_features + off_def_features

# all frames will be in this range
# cut play off here (frame_number - 10) frames after snap
start_frame_number <- 0
end_frame_number <- 45

# frames to keep
# note that later on frame_id will be re-coded to be 1, ... , F
keep_frames <- seq(10, 44, by = 2)

# pull week 1 through this week:
final_week <- 17

# get labels
labels <- readRDS("data-raw/coverage_labels.rds")

# get_bdb is a function in coverage_classifier_functions.R
df <- map_df(1:final_week, get_bdb)

df

# put together the df of defense players relative to offense players
offense_df <- df %>%
  filter(defense == 0) %>%
  select(play, frame_id, o_x = x, o_y = y, o_s_x = s_x, o_s_y = s_y, o_a_x = a_x, o_a_y = a_y)

defense_df <- df %>%
  filter(defense == 1) %>%
  select(play, frame_id, nfl_id, x, y, s_x, s_y, a_x, a_y, o_to_qb, dist_from_los)

rel_df <- defense_df %>%
  left_join(offense_df, by = c("play", "frame_id")) %>%
  mutate(diff_x = o_x - x, diff_y = o_y - y, diff_s_x = o_s_x - s_x, diff_s_y = o_s_y - s_y, diff_a_x = o_a_x - a_x, diff_a_y = o_a_y - a_y) %>%
  select(play, frame_id, nfl_id, dist_from_los, y, s_x, s_y, a_x, a_y, o_to_qb, starts_with("diff_"))

rel_df


object.size(df) %>% format("MB")
object.size(rel_df) %>% format("MB")

# using weeks 1 and 2 for testing so see how long this is
# assumes data ordered with week 1 first
test_length <- df %>% filter(week <= 2) %>% select(play) %>% unique() %>% nrow()

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
  select(play, frame_id, play, week) %>%
  unique() %>%
  # get play index for 1 : n_plays
  mutate(
    i = as.integer(as.factor(play))
  ) %>%
  # get time step indices
  group_by(play) %>%
  mutate(f = 1 : n()) %>%
  ungroup()

play_indices

play_indices %>%
  saveRDS("data/valid_plays.rds")

n_frames <- n_distinct(play_indices$f)
plays <- n_distinct(df$play)
plays
n_frames

# i, features, def, off, 
train_x = torch_empty(plays, n_frames, n_features, 11, 5)

# for testing
# row <- play_indices %>%
#   dplyr::slice(1)

# test
# fill_row(play_indices %>% dplyr::slice(1))
# train_x[1, 1, , , ]
# train_x[1, ..]

# future::plan("multicore")
# build the tensor for train and test data
walk(1 : nrow(play_indices), ~{
  if(.x %% 250 == 0) {
    message(glue::glue("{.x} of {nrow(play_indices)}"))
  }
  fill_row(rel_df, play_indices %>% dplyr::slice(.x))
})

train_y <- torch_zeros(plays, dtype = torch_long())

train_y[1:plays] <- df %>%
  mutate(coverage = as.factor(coverage) %>% as.integer()) %>%
  group_by(play) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  pull(coverage)

dim(train_x)
dim(train_y)

torch_save(train_y, "data/train_y.pt")

if (one_frame) {

  message("saving one frame dataframe")
  # get rid of singleton time dimension
  train_x <- train_x %>% torch_squeeze()
  
  torch_save(train_x, "data/train_x_one_frame.pt")

} else {
  torch_save(train_x, "data/train_x.pt")
}


