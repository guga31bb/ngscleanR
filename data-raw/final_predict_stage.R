library(tidyverse)
library(torch)

test_weeks <- c(1)
n_kfold <- 10

augment_data <- function(df,
                         # stuff that will be multiplied by -1 (eg Sy)
                         flip_indices = c(4, 6, 9, 11, 13),
                         # raw y location
                         subtract_indices = c(2)) {


  # indices of the elements that need to be flipped
  t <- torch_ones_like(df)
  t[ , , flip_indices, , ] <- -1

  # first fix: multiply by -1 where needed (stuff like speed in Y direction)
  flipped <- df * t

  # for flipping Y itself, need to do 160/3 - y
  t <- torch_zeros_like(df)
  t[ , , subtract_indices, , ] <- 160 / 3

  # second fix: flip around y
  flipped[ , , subtract_indices, , ] <- t[ , , subtract_indices, , ] - flipped[ , , subtract_indices, , ]

  return(flipped)
}

# have to download these from kaggle after training there (or wherever they're trained)
models <- map(1:n_kfold, ~ {
  torch_load(glue::glue("data-raw/model_stuff/data/models/best_model_{.x}.pt"))
})

train_x <- torch_load("data-raw/model_stuff/data/train_x.pt")
train_y <- torch_load("data-raw/model_stuff/data/train_y.pt")

# created in coverage_classifier_make_tensors.R
frame_lengths <- readRDS("data-raw/model_stuff/data/valid_plays.rds") %>%
  group_by(i) %>% summarize(n_frames = n()) %>% ungroup() %>%
  pull(n_frames)

# created in coverage_classifier_make_tensors.R
test_idx <- readRDS("data-raw/model_stuff/data/valid_plays.rds") %>%
  filter(week %in% test_weeks) %>%
  select(i) %>% distinct() %>% pull(i)

test_x <- train_x[test_idx, ..]
test_y <- train_y[test_idx]
test_dims <- torch_tensor(frame_lengths[test_idx])

n_class <- n_distinct(as.matrix(train_y))

rm(train_x)
rm(train_y)
gc()

message(glue::glue("--------------- TEST STAGE ------------."))

labels <- test_y %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

test_data_augmented <- augment_data(test_x)

# folds x obs x class
output <- torch_zeros(n_kfold, dim(test_x)[1], n_class)

# get augmented prediction for each fold
walk(1:n_kfold, ~ {
  message(glue::glue("Doing number {.x}"))
  output[.x, ..] <- (models[[.x]](test_x, test_dims) + models[[.x]](test_data_augmented, test_dims)) / 2
})

# average prediction over folds
predictions <- (1 / n_kfold) * torch_sum(output, 1)
predictions <- as.matrix(predictions)

predictions <- predictions %>%
  as_tibble() %>%
  transform(prediction = max.col(predictions)) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  as_tibble() %>%
  mutate(high = pmax(V1, V2, V3, V4, V5, V6, V7, V8, V9))

message(glue::glue("Week 1 test: {round(100*mean(predictions$correct), 1)}% correct"))

# 86.8


