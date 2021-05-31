# scheduler 
# https://torchvision.mlverse.org/articles/examples/tinyimagenet-alexnet.html

source("R/coverage_classifier_functions.R")

# for deciding whether to augment data and saying which features need it
augment = TRUE
device <- if(cuda_is_available()) "cuda" else "cpu"

# get tensors
train_x <- torch_load("data/train_x_one_frame.pt")
train_y <- torch_load("data/train_y.pt")

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
train_label <- train_y[train_id]

# if you want to augment with flipped data
if (augment) {
  
  dim(train_data)
  train_data_augmented <- augment_data(train_data, c(4, 6, 9, 11, 13), c(2))
  
  train_data <- torch_cat(list(train_data, train_data_augmented))
  train_label <- torch_cat(list(train_label, train_label))
  
  dim(train_data)
  dim(train_label)
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

# use dataloaders for train and validation
train_ds <- tracking_dataset(train_data, train_label)
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

# decay by about 50% after 15 epochs
scheduler <- lr_step(optimizer, step_size = 1, 0.95)

# 80% at 8 epochs and 10
# XX at 10

# cb <- luz_callback_lr_scheduler(torch::lr_step, step_size = 1)
# 
# # luz
# fitted <- net %>%
#   setup(
#     loss = nnf_cross_entropy,
#     optimizer = optim_adam
#   ) %>%
#   fit(train_dl, epochs = 1, valid_data = valid_dl)

epochs <- 1
for (epoch in 1:epochs) {
  
  train_losses <- c()
  valid_losses <- c()
  valid_accuracies <- c()
  
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
    
    output <- model(b[[1]]$to(device = device))
    y <- b[[2]]$to(device = device)
    
    valid_losses <- c(valid_losses, nnf_cross_entropy(output, y)$item())
    
    pred <- torch_max(output, dim = 2)[[2]]
    correct <- (pred == y)$sum()$item()
    valid_accuracies <- c(valid_accuracies, correct/length(y))
  }
  
  scheduler$step()
  cat(sprintf("\nLoss at epoch %d: training: %1.4f, validation: %1.4f, validation accuracy %1.4f", epoch, mean(train_losses), mean(valid_losses), mean(valid_accuracies)))
  
  # who knows if this does anything
  gc()
}

# for loading model
# model <- torch_load("data/model.pt")

# evaluate on test set
model$eval()

labels <- test_y %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

output <- model(test_x)


# testing only
# output_augmented <- model(train_data_augmented)
# predictions <- as.matrix(output_augmented)
# end test

predictions <- as.matrix(output) 

predictions <- predictions %>% 
  as_tibble() %>%
  transform(prediction = max.col(predictions)) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  as_tibble() 

message(glue::glue("Week 1 test: {round(100*mean(predictions$correct), 0)}% correct"))

# augmented preds
test_data_augmented <- augment_data(test_x, c(4, 6, 9, 11, 13), c(2))

output_augmented <- model(test_data_augmented)
output <- (output + output_augmented) / 2

predictions <- as.matrix(output$to(device = "cpu")) 

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

message(glue::glue("Week 1 augmented test: {round(100*mean(predictions$correct), 0)}% correct"))

# confusion matrix
# tab <- predictions %>%
#   filter(
#     !label %in% c(1, 9),
#     !prediction %in% c(1, 9)
#     ) %>%
#   mutate(
#     label = as.factor(label),
#     prediction = as.factor(prediction)
#   )
# 
# levels(tab$label) <- 
#   c("C0m", "C1m", "C2m", "C2z", "C3z", "C4z", "C6z")
# levels(tab$prediction) <- 
#   c("C0m", "C1m", "C2m", "C2z", "C3z", "C4z", "C6z")
# 
# conf_mat <- caret::confusionMatrix(tab$prediction, tab$label)
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
