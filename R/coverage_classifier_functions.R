library(tidyverse)
library(torch)

augment_data <- function(df) {
  
  # testing
  # df <- train_data
  
  # x, y, sx, sy, o_qb, los_dist, xdiff, ydiff, sxdiff, sydiff
  # 1, XX, 1, -1, 1, 1, 1, -1, 1, -1
  
  # indices of the elements that need to be flipped
  
  flip_idx <- c(4, 8, 10)
  t <- torch_ones_like(df)
  t[, flip_idx, , ] <- -1
  
  # first fix: multiply by -1 where needed
  flipped <- df * t
  
  # now flip y coordinates: 2nd feature dimension
  t <- torch_zeros_like(df)
  t[, 2, , ] <- 160/3
  
  # flip around y
  flipped[, 2, , ] <- t[, 2, , ] - flipped[, 2, , ]
  
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
    list(self$data_x[i,], self$data_y[i])
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
      torch_squeeze(-1)
    max <- nn_max_pool2d(kernel_size = c(1, 5))(x) %>%
      torch_squeeze(-1)
    
    x <- 0.7 * avg + 0.3 * max
    
    x <- nn_batch_norm1d(128)(x)
    
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

