# 
# https://gist.github.com/HarshTrivedi/f4e7293e941b17d19058f6fb90ab0fec

# scheduler 
# https://torchvision.mlverse.org/articles/examples/tinyimagenet-alexnet.html

source("R/coverage_classifier_functions.R")

# for deciding whether to augment data and saying which features need it
augment = FALSE

packageVersion("torch")
device <- if(cuda_is_available()) "cuda" else "cpu"
device

# get tensors
train_x <- torch_load("data/train_x.pt")
train_y <- torch_load("data/train_y.pt")

# get pre-saved lengths
lengths <- readRDS("data/data_sizes.rds")
frame_lengths <- readRDS("data/valid_plays.rds") %>% 
  group_by(i) %>% summarize(n_frames = n()) %>% ungroup() %>%
  pull(n_frames)

test_length <- lengths$test_length
plays <- lengths$plays

input_channels <- dim(train_x)[3]

test_length
plays

input_channels

# right now we have tensors for train_x and train_y that also include test data (week 1)
dim(train_x)
dim(train_y)

# split into test and train
test_x <- train_x[1:test_length, ..]
test_y <- train_y[1:test_length]
test_dims <- torch_tensor(frame_lengths[1:test_length])

train_x <- train_x[(test_length + 1) : plays, ..]
train_y <- train_y[(test_length + 1) : plays]
train_dims <- frame_lengths[(test_length + 1) : plays]

# make plays the length of train data 
plays <- dim(train_y)

# split into train and validation
train_id <- sample(1:plays, ceiling(0.80 * plays))
valid_id <- setdiff(1:plays, train_id)

# get all the data read
valid_data <- train_x[valid_id, ..]
valid_label <- train_y[valid_id]
valid_dims <- torch_tensor(train_dims[valid_id])

train_data <- train_x[train_id, ..]
train_label <- train_y[train_id]
train_dims <- torch_tensor(train_dims[train_id])

# do the function

# use dataloaders for train and validation
train_ds <- tracking_dataset_t(train_data, train_label, train_dims)
valid_ds <- tracking_dataset_t(valid_data, valid_label, valid_dims)

# Dataloaders
train_dl <- train_ds %>%
  dataloader(batch_size = 64, shuffle = TRUE)

valid_dl <- valid_ds %>%
  dataloader(batch_size = 64, shuffle = FALSE)

model <- full_model()
model$to(device = device)

# to test something passing through model
# b <- enumerate(train_dl)[[1]][[1]]
# model(b)

# For fitting, we use Adam optimizer with a one cycle scheduler over a total of 50 epochs for each fit 
# with lower lr being 0.0005 and upper lr being 0.001 and 64 batch size

# if we need to load (currently broken in torch)
# model <- torch_load("data/model.pt")

optimizer <- optim_adam(model$parameters, lr = 0.001)

# decay by about 50% after 15 epochs
scheduler <- lr_step(optimizer, step_size = 1, 0.975)

# 80% at 8 epochs and 10
# XX at 10

epochs <- 1
for (epoch in 1:epochs) {
  
  train_losses <- c()
  valid_losses <- c()
  
  # train step
  model$train()
  for (b in enumerate(train_dl)) {
    optimizer$zero_grad()
    
    # message("trying loss")
    loss <- nnf_cross_entropy(model(b[[1]]$to(device = device), b[[3]]$to(device = device)), b[[2]]$to(device = device))
    
    # message("trying backward")
    loss$backward()
    
    # message("trying step")
    optimizer$step()
    train_losses <- c(train_losses, loss$item())
    
  }
  
  # message("got train losses")
  
  # validation step
  model$eval()
  for (b in enumerate(valid_dl)) {
    
    # message("trying valid losses")
    loss <- nnf_cross_entropy(model(b[[1]]$to(device = device), b[[3]]$to(device = device)), b[[2]]$to(device = device))
    valid_losses <- c(valid_losses, loss$item())
  }
  
  scheduler$step()
  cat(sprintf("\nLoss at epoch %d: training: %1.4f, validation: %1.4f", epoch, mean(train_losses), mean(valid_losses)))
  
  # who knows if this does anything
  gc()
}

# move to cpu for saving
model$to(device = "cpu")
torch_save(model, "model.pt") 

# put back
model$to(device = device)


# for loading model
# model <- torch_load("data/model.pt")

# evaluate on test set
model$eval()

labels <- test_y %>%
  as.matrix() %>%
  as_tibble() %>%
  set_names("label")

output <- model(test_x$to(device = device), test_dims$to(device = device))

# testing only
# output_augmented <- model(train_data_augmented)
# predictions <- as.matrix(output_augmented)
# end test

predictions <- as.matrix(output$to(device = "cpu")) 

predictions <- predictions %>% 
  as_tibble() %>%
  transform(prediction = max.col(predictions)) %>%
  bind_cols(labels) %>%
  mutate(correct = ifelse(prediction == label, 1, 0)) %>%
  as_tibble() 

message(glue::glue("Week 1 test: {round(100*mean(predictions$correct), 0)}% correct"))


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
