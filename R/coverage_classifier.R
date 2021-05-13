# https://www.kaggle.com/jccampos/nfl-2020-winner-solution-the-zoo
# 
library(tidyverse)
library(torch)
source("R/cleaning_functions.R")

get_bdb <- function(w) {
  read_csv(glue::glue("../nfl-big-data-bowl-2021/input/week{w}.csv"))
}

s_bdb21 <- map_df(1:5, get_bdb)

labels <- readRDS("data-raw/coverage_labels.rds")

df <- s_bdb21 %>%
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
  
  # for this first pass
  filter(frame_id == 30) %>%
  
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
    # yardline_100,
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
  filter(n_defenders > 2 & n_offense > 2) %>%
  select(-game_id, -play_id, -n_defenders, -n_offense) %>%
  arrange(
    play, frame_id, defense, nfl_id
  )

df


# using week 1 for testing so see how long this is
# assumes data ordered with week 1 first
test_length <- df %>% filter(week == 1) %>% select(play) %>% unique() %>% nrow()

# offense: 4-5 players
# defense: 5-11 players

# def Sx, y
# def o to qb
# def distance from los
# off x, y - def x, y
# off Sx, y - def Sx, y

# input shape (n_features (8) * n_defenders (11) * n_non-qb-offense (5))
# features 1-4 Sx, Sy, o_to_qb, dist_from_los, 
# (5) off x - def x, (6) off y - def, (7) off Sx - def, (8) off Sy - def

plays <- n_distinct(df$play)
plays
test_length

# i, features, def, off, 
train_x = torch_zeros(plays, 8, 11, 5)

play_indices <- df %>%
  select(play) %>%
  unique() %>%
  mutate(i = 1 : n())

# for testing
# row <- play_indices %>%
#   dplyr::slice(1798)

fill_row <- function(row) {
  
  i = row$i
  playid = row$play
  
  offense_df <- df %>%
    filter(defense == 0, play == playid) %>%
    select(play, o_x = x, o_y = y, o_s_x = s_x, o_s_y = s_y, los_x)
  
  # if only 4 offensive players, make a fake one standing at los
  # not moving anywhere
  # adding a little noise bc otherwise things were breaking with all the zeros
  if (nrow(offense_df) == 4) {
    dummy_player <- tibble::tibble(
      "play" = offense_df$play[1],
      "o_x" = offense_df$los_x[1],
      "o_y" = 160/6,
      "o_s_x" = 0 + rnorm(1, sd = .1),
      "o_s_y" = 0 + rnorm(1, sd = .1)
    )
    
    offense_df <- bind_rows(
      offense_df,
      dummy_player
    )
  }
  
  defense_df <- df %>%
    filter(defense == 1, play == playid)
  
  defenders <- nrow(defense_df)
  
  # fill in dummy defenders
  if (defenders < 11) {
    
    # standing on los, not moving, facing qb
    dummy_player <- tibble::tibble(
      "play" = defense_df$play[1],
      "frame_id" = defense_df$frame_id[1],
      "x" = defense_df$los_x[1],
      "defense" = 1,
      "y" = 160/6,
      "s_x" = 0 + rnorm(1, sd = .1),
      "s_y" = 0 + rnorm(1, sd = .1),
      "o_to_qb" = 0 + rnorm(1, sd = 1),
      "dist_from_los" = 0
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
    select(s_x, s_y, o_to_qb, dist_from_los, starts_with("diff_"))
  
  # fill in info for each defender
  for (j in 1:11) {
    
    # message(j)
    
    start <- 1 + 5 * (j-1)
    end <- 5 * j
    
    train_x[i, 1:8, j, ] <- big_df %>% 
      dplyr::slice(start:end) %>%
      as.matrix() %>%
      t()
    
  }
  
  return(train_x)
  
}

# build the tensor for train and test data
walk(1 : nrow(play_indices), ~{
  if(.x %% 50 == 0) {
    message(glue::glue("{.x} of {nrow(play_indices)}"))
  }
  fill_row(play_indices %>% dplyr::slice(.x))
  })

train_x

train_y <- torch_zeros(plays, dtype = torch_long())

train_y[1:plays] <- df %>%
  mutate(coverage = as.factor(coverage) %>% as.integer()) %>%
  group_by(play) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  pull(coverage)

rm(df)
gc()

# right now we have tensors for train_x and train_y that also include test data (week 1)
dim(train_x)
dim(train_y)

# split into test and train
test_x <- train_x[1:test_length, , ]
train_x <- train_x[(test_length + 1) : plays, , ]

test_y <- train_y[1:test_length]
train_y <- train_y[(test_length + 1) : plays]

# make plays the length of train data 
plays <- dim(train_y)

# define dataloader
tracking_dataset <- dataset(
  name = "tracking_dataset",
  
  initialize = function(x_tensor, y_tensor) {
    
    self$data_x <- x_tensor
    self$data_y <- y_tensor
    
  },
  
  .getitem = function(i) {
    list(self$data_x[i,], self$data_y[i])
  },
  
  .length = function() {
    self$data_y$size()[[1]]
  }
)

# split into train and validation
train_id <- sample(1:plays, ceiling(0.80 * plays))
valid_id <- setdiff(1:plays, train_id)

# use dataloaders for train and validation
train_ds <- tracking_dataset(train_x[train_id, , , ], train_y[train_id])
valid_ds <- tracking_dataset(train_x[valid_id, , , ], train_y[valid_id])

# Dataloaders
train_dl <- train_ds %>%
  dataloader(batch_size = 64, shuffle = TRUE)

valid_dl <- valid_ds %>%
  dataloader(batch_size = 64, shuffle = FALSE)

length(train_dl)
length(valid_dl)

net <- nn_module(
  "Net",
  
  initialize = function() {
    
    self$conv_block_1 <- nn_sequential(
      nn_conv2d(
        in_channels = 8,
        out_channels = 128,
        kernel_size = 1
      ),
      nn_relu(),
      nn_conv2d(
        in_channels = 128,
        out_channels = 160,
        kernel_size = 1
      ),
      nn_relu(),
      nn_conv2d(
        in_channels = 160,
        out_channels = 128,
        kernel_size = 1
      ),
      nn_relu()
    )
    
    self$conv_block_2 <- nn_sequential(
      nn_conv1d(
        in_channels = 128,
        out_channels = 160,
        kernel_size = 1
      ),
      nn_relu(),
      nn_batch_norm1d(160),
      nn_conv1d(
        in_channels = 160,
        out_channels = 96,
        kernel_size = 1
      ),
      nn_relu(),
      nn_batch_norm1d(96),
      nn_conv1d(
        in_channels = 96,
        out_channels = 96,
        kernel_size = 1
      ),
      nn_relu(),
      nn_batch_norm1d(96)
    )
    
    self$linear_block <- nn_sequential(
      nn_linear(96, 96),
      nn_relu(),
      nn_batch_norm1d(96),
      
      nn_linear(96, 256),
      nn_relu(),
      # nn_batch_norm1d(256),
      
      nn_layer_norm(256),
      nn_dropout(p = 0.3),
      
      nn_linear(256, 9),
      
      nn_softmax(2)
    )

  },
  
  forward = function(x) {
    
    # first conv layer
    x <- self$conv_block_1(x)
    
    # first pool layer
    avg <- nn_avg_pool2d(kernel_size = c(1, 5))(x) %>%
      torch_squeeze()
    max <- nn_max_pool2d(kernel_size = c(1, 5))(x) %>%
      torch_squeeze()
    
    x <- 0.7 * avg + 0.3 * max
    
    x <- nn_batch_norm1d(128)(x)
    
    # second conv layer
    x <- self$conv_block_2(x)
    
    # second pool layer
    avg <- nn_avg_pool1d(kernel_size = 11)(x) %>%
      torch_squeeze()
    max <- nn_max_pool1d(kernel_size = 11)(x) %>%
      torch_squeeze()
    
    x <- 0.7 * avg + 0.3 * max
    
    x <- self$linear_block(x)
    
    x
    
  }
)

model <- net()


# test_batch <- enumerate(train_dl)[[10]]
# model(test_batch[[1]])

# For fitting, we use Adam optimizer with a one cycle scheduler over a total of 50 epochs for each fit 
# with lower lr being 0.0005 and upper lr being 0.001 and 64 batch size

optimizer <- optim_adam(model$parameters, lr = 0.001)


for (epoch in 1:50) {
  
  train_losses <- c()
  valid_losses <- c()
  
  model$train()
  
  for (b in enumerate(train_dl)) {
    # skip if the batch is too small
    if (dim(b[[1]])[1] < 10) {
      # message(paste("batch too small"))
      next
    }
    
    x <- b[[1]]
    y <- b[[2]]
    
    optimizer$zero_grad()
    output <- model(x)
    loss <- nnf_cross_entropy(output, y)
    loss$backward()
    optimizer$step()
    train_losses <- c(train_losses, loss$item())
  }
  
  model$eval()
  
  for (b in enumerate(valid_dl)) {
    # skip if the batch is too small
    if (dim(b[[1]])[1] < 10) {
      # message(paste("batch too small"))
      next
    }
    
    x <- b[[1]]
    y <- b[[2]]
    
    output <- model(x)
    loss <- nnf_cross_entropy(output, y)
    valid_losses <- c(valid_losses, loss$item())
  }
  
  # print(l)
  
  cat(sprintf("\nLoss at epoch %d: training: %1.5f, validation: %1.5f\n", epoch, mean(train_losses), mean(valid_losses)))
}




model$eval()

# test_batch <- enumerate(valid_dl)[[1]]
# x <- test_x

labels <- test_y %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

# label frequency
labels %>% group_by(label) %>% summarize(n=n())

output <- model(test_x)
predictions <- as.matrix(output) 

predictions <- predictions %>% 
  as_tibble() %>%
  transform(prediction = max.col(predictions)) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0))

predictions %>% group_by(label) %>% summarize(n=n())
predictions %>% group_by(prediction) %>% summarize(n=n())

mean(predictions$correct)

# coverage         n
# <chr>        <int>
# 1 Bracket         94
# 2 Cover 0 Man    459
# 3 Cover 1 Man   5870
# 4 Cover 2 Man    612
# 5 Cover 2 Zone  2490
# 6 Cover 3 Zone  7312
# 7 Cover 4 Zone  2079
# 8 Cover 6 Zone  1458
# 9 Prevent        110
