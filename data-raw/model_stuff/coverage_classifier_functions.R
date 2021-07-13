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

augment_data <- function(df,
                         # stuff that will be multiplied by -1 (eg Sy)
                         flip_indices = c(4, 6, 9, 11, 13),
                         # raw y location
                         subtract_indices = c(2)) {

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
    list("x" = self$data_x[i,], "y" = self$data_y[i], "t" = self$dims[i])
  },

  .length = function() {
    self$data_y$size()[[1]]
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



