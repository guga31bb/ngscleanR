# scheduler 
# https://torchvision.mlverse.org/articles/examples/tinyimagenet-alexnet.html

source("R/coverage_classifier_functions.R")


# get tensors
train_x <- torch_load("data/train_x_one_frame.pt")
train_y <- torch_load("data/train_y_one_frame.pt")

# get pre-saved lengths
lengths <- readRDS("data/data_sizes_one_frame.rds")

test_length <- lengths$test_length
plays <- lengths$plays

input_channels <- dim(train_x)[2]

test_length
plays

input_channels

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


# split into train and validation
train_id <- sample(1:plays, ceiling(0.80 * plays))
valid_id <- setdiff(1:plays, train_id)

train_data <- train_x[train_id, , , ]

# if you want to augment with flipped data
# otherwise comment this out
train_data <- torch_cat(list(train_data, augment_data(train_data)))

# use dataloaders for train and validation
train_ds <- tracking_dataset(train_data, train_y[train_id])
valid_ds <- tracking_dataset(train_x[valid_id, , , ], train_y[valid_id])

# Dataloaders
train_dl <- train_ds %>%
  dataloader(batch_size = 64, shuffle = TRUE)

valid_dl <- valid_ds %>%
  dataloader(batch_size = 64, shuffle = FALSE)

model <- net()

# to test something passing through model
# b <- enumerate(train_dl)[[1]][[1]]
# model(b)

# For fitting, we use Adam optimizer with a one cycle scheduler over a total of 50 epochs for each fit 
# with lower lr being 0.0005 and upper lr being 0.001 and 64 batch size

# if we need to load (currently broken in torch)
# model <- torch_load("data/model.pt")

optimizer <- optim_adam(model$parameters, lr = 0.001)

epochs <- 3
for (epoch in 1:epochs) {
  
  train_losses <- c()
  valid_losses <- c()
  
  # train step
  model$train()
  for (b in enumerate(train_dl)) {
    optimizer$zero_grad()
    loss <- nnf_cross_entropy(model(b[[1]]), b[[2]])
    loss$backward()
    optimizer$step()
    train_losses <- c(train_losses, loss$item())
  }
  
  # validation step
  model$eval()
  for (b in enumerate(valid_dl)) {
    valid_losses <- c(valid_losses, nnf_cross_entropy(model(b[[1]]), b[[2]])$item())
  }
  
  torch_save(model, "data/model.pt")

  cat(sprintf("\nLoss at epoch %d: training: %1.4f, validation: %1.4f", epoch, mean(train_losses), mean(valid_losses)))
  
  # who knows if this does anything
  gc()
}


# evaluate on test set
model$eval()

labels <- test_y %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

output <- model(test_x)
output_augmented <- model(augment_data(test_x))

output <- (output + output_augmented) / 2

predictions <- as.matrix(output) 

predictions <- predictions %>% 
  as_tibble() %>%
  transform(prediction = max.col(predictions)) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  as_tibble() %>%
  mutate(
    label = as.factor(label),
    prediction = as.factor(prediction)
  )

message(glue::glue("Week 1 test: {round(100*mean(predictions$correct), 0)}% correct"))


# confusion matrix
# conf_mat <- caret::confusionMatrix(predictions$prediction, predictions$label)
# conf_mat$table %>%
#   broom::tidy() %>%
#   dplyr::rename(
#     Target = Reference,
#     N = n
#   ) %>%
#   cvms::plot_confusion_matrix(
#     add_sums = TRUE, place_x_axis_above = FALSE,
#     add_normalized = FALSE)

# predictions %>% head(20)


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
