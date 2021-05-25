# number of features that only depend on def player
# dist_from_los, y, s_x, s_y, a_x, a_y, o_to_qb
#       .     subtract .  -1   .    -1     .

# number of features that depend on defense and offense player
# rel x, rel y, rel sx, rel sy, rel ax, rel ay
#   .     -1      .       -1       .      -1

# dist_from_los
# y          
# s_x        
# s_y        
# a_x        
# a_y        
# o_to_qb    
# diff_x     
# diff_y     
# diff_s_x   
# diff_s_y   
# diff_a_x   
# diff_a_y  

augment_data <- function(df, flip_indices, subtract_indices) {
  
  # testing
  # df <- train_data
  
  # x, y, sx, sy, o_qb, los_dist, xdiff, ydiff, sxdiff, sydiff
  # 1, XX, 1, -1, 1, 1, 1, -1, 1, -1
  
  # indices of the elements that need to be flipped
  
  t <- torch_ones_like(df)
  t[, flip_indices, , ] <- -1
  
  # first fix: multiply by -1 where needed
  flipped <- df * t
  
  # now flip y coordinates: 2nd feature dimension
  t <- torch_zeros_like(df)
  t[, subtract_indices, , ] <- 160/3
  
  # flip around y
  flipped[, subtract_indices, , ] <- t[, subtract_indices, , ] - flipped[, subtract_indices, , ]
  
  return(flipped)
  
  # df[1, 2, ..]
  # flipped[1, 2, ..]
  # 
  # df[1, 4, ..]
  # flipped[1, 4, ..]
}


# define dataloader
tracking_dataset <- dataset(
  name = "tracking_dataset",
  
  initialize = function(x_tensor, y_tensor) {
    
    self$data_x <- x_tensor
    self$data_y <- y_tensor
    
  },
  
  .getitem = function(i) {
    list("x" = self$data_x[i,], "y" = self$data_y[i])
  },
  
  .length = function() {
    self$data_y$size()[[1]]
  }
)

# define dataloader
tracking_dataset_t <- dataset(
  name = "tracking_dataset",
  
  initialize = function(x_tensor, y_tensor, dims) {
    
    self$data_x <- x_tensor
    self$data_y <- y_tensor
    self$dims <- dims
    
  },
  
  .getitem = function(i) {
    list(self$data_x[i,], self$data_y[i], self$dims[i])
  },
  
  .length = function() {
    self$data_y$size()[[1]]
  }
)



ltsm <- nn_module(
  "Outer",
  
  initialize = function(hidden_size = 32, layers = 1, bi_dir = TRUE) {
    
    self$hidden_size <- hidden_size
    self$layers <- layers
    self$bi_dir <- bi_dir
    
    self$rnn = nn_lstm(9, self$hidden_size, batch_first=TRUE,
                       num_layers=self$layers, bidirectional=self$bi_dir)
    
    self$linear_block <- nn_sequential(
      nn_linear(self$layers * (1 + self$bi_dir) * (self$hidden_size), 9),
      nn_relu(inplace = TRUE),
      nn_softmax(2)
    )
    
  },
  
  forward = function(x, dims) {
    
    batch_size <- dim(x)[1]

    x <- torch::nn_utils_rnn_pack_padded_sequence(
      x, dims$to(device = "cpu"), batch_first=TRUE, enforce_sorted=FALSE
    )
    
    # check which part to pull
    x <- self$rnn(x)[[2]][[1]]
    
    x <- x$view(c(self$layers, (1 + self$bi_dir), batch_size, -1))
    # B, 1, 2 for bidirectional, output layers
    x <- x$permute(c(3, 1, 2, 4))
    x <- x$reshape(c(batch_size, -1))
    
    x <- self$linear_block(x)
    
    x
 
  }
)

net <- nn_module(
  "Net",
  
  initialize = function() {
    
    self$conv_block_1 <- nn_sequential(
      nn_conv2d(
        in_channels = input_channels,
        out_channels = 128,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_conv2d(
        in_channels = 128,
        out_channels = 160,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_conv2d(
        in_channels = 160,
        out_channels = 128,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
    )
    
    self$conv_block_2 <- nn_sequential(
      nn_batch_norm1d(128),
      nn_conv1d(
        in_channels = 128,
        out_channels = 160,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(160),
      nn_conv1d(
        in_channels = 160,
        out_channels = 96,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(96),
      nn_conv1d(
        in_channels = 96,
        out_channels = 96,
        kernel_size = 1
      ),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(96)
    )
    
    self$linear_block <- nn_sequential(
      nn_linear(96, 96),
      nn_relu(inplace = TRUE),
      nn_batch_norm1d(96),
      
      nn_linear(96, 256),
      nn_relu(inplace = TRUE),
      
      # breaks on current kaggle version
      # nn_batch_norm1d(256),
      
      nn_layer_norm(256),
      nn_dropout(p = 0.3),
      
      nn_linear(256, 9)
      
    )
    
  },
  
  forward = function(x) {
    
    # first conv layer
    x <- self$conv_block_1(x)
    
    # first pool layer
    avg <- nn_avg_pool2d(kernel_size = c(1, 5))(x) %>%
      torch_squeeze(-1)
    max <- nn_max_pool2d(kernel_size = c(1, 5))(x) %>%
      torch_squeeze(-1)
    
    x <- 0.7 * avg + 0.3 * max
    
    # second conv layer
    x <- self$conv_block_2(x)
    
    # second pool layer
    avg <- nn_avg_pool1d(kernel_size = 11)(x) %>%
      torch_squeeze(-1)
    max <- nn_max_pool1d(kernel_size = 11)(x) %>%
      torch_squeeze(-1)
    
    x <- 0.7 * avg + 0.3 * max
    
    x <- self$linear_block(x)
    
    x
    
  }
)


full_model <- nn_module(
  "Full",
  
  initialize = function() {
    
    self$cnn <- net()
    self$ltsm <- ltsm()
    
  },
  
  forward = function(x, dims) {
    
    # flatten out time steps and do cnn
    orig_dim = dim(x)
    x <- x$view(c(-1, orig_dim[3:5]))
    
    x <- self$cnn(x)
    
    x <- x$view(c(orig_dim[1:2], -1))
    
    # do rnn
    x <- self$ltsm(x, dims)
    
    # return
    x
    
  }
)



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
      a_x,
      a_y,
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
    # no longer needed bc we specify frames
    # filter(between(frame_id, start_frame_number, end_frame_number)) %>%
    group_by(play) %>%
    filter(
      frame_id %in% keep_frames
    ) %>%
    ungroup() %>%
    select(-game_id, -play_id, -n_defenders, -n_offense) %>%
    arrange(
      play, frame_id, defense, nfl_id
    )
  
  df
  
}

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



