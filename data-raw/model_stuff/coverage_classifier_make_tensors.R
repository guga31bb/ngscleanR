
# need to run this if not installed
# devtools::install_github("guga31bb/ngscleanR")

library(tidyverse)
library(torch)
set.seed(2013)

# number of features that only depend on def player
# dist_from_los, y, s_x, s_y, a_x, a_y, o_to_qb
def_only_features <- 7
# number of features that depend on defense and offense player
# rel x, rel y, rel sx, rel sy, rel ax, rel ay
off_def_features <- 6
n_features <- def_only_features + off_def_features

# pull week 1 through this week:
final_week <- 17

# get labels
labels <- readRDS("data-raw/coverage_labels.rds") %>%
  mutate(
    play = paste0(game_id, "_", play_id)
  ) %>%
  filter(!is.na(coverage)) %>%
  select(play, coverage)

# make sure ngscleanR installed (see top)
df <- map_df(1:final_week, ~{
  ngscleanR::prepare_bdb_week(
    week = .x,
    # where is your big data bowl data saved?
    dir = "../nfl-big-data-bowl-2021/input",
    # any throw that happens before 1.5 seconds after snap is thrown away
    trim_frame = 25,
    # all frames coming more than 1 second after pass released are thrown away
    frames_after_throw = 10,
    # let's keep these frames for fun (every 3 frames starting at snap for 16 frames)
    keep_frames = seq(11, 57, by = 3)
  )
  }) %>%
  left_join(labels, by = "play")

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

# offense: 4-5 players
# defense: 5-11 players
# input shape (time steps (t) * n_features (13) * n_defenders (11) * n_non-qb-offense (5))

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

# i, f, features, def, off,
train_x = torch_empty(plays, n_frames, n_features, 11, 5)

fill_row <- function(df, row) {

  # indices for putting in tensor
  i = row$i
  f = row$f

  # play info for extracting from df
  playid = row$play
  frameid = row$frame_id

  play_df <- df %>%
    filter(play == playid, frame_id == frameid) %>%
    select(-play, -frame_id)

  defenders <- n_distinct(play_df$nfl_id)
  n_offense <- nrow(play_df) / defenders

  play_df <- play_df %>% select(-nfl_id)

  train_x[i, f, , 1:defenders, 1:n_offense] <-
    torch_tensor(t(play_df))$view(c(-1, defenders, n_offense))

}

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
torch_save(train_x, "data/train_x.pt")



