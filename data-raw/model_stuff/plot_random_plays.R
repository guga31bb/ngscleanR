library(tidyverse)
library(torch)
source("R/cleaning_functions.R")
source("R/plotting_functions.R")
set.seed(2013)

# if you just want to keep one frame from each play
# frame_number is (frame_number - 10) seconds into the play
one_frame = FALSE

# all frames will be in this range
# cut play off here (frame_number - 10) frames after snap
start_frame_number <- 0
end_frame_number <- 45

keep_frames <- c(1:40)

# pull week 1 through this week:
final_week <- 1

labels <- readRDS("data-raw/coverage_labels.rds") %>% mutate(play = paste0(game_id, "_", play_id))

get_bdb <- function(w) {
  df <- read_csv(glue::glue("../nfl-big-data-bowl-2021/input/week{w}.csv")) %>%
    
    # do all the cleaning
    wrapper() %>%
    
    cut_plays(throw_frame = 20) %>%
    
    mutate(play = paste0(game_id, "_", play_id))
  
  return(df)
  
}

df <- map_df(1:final_week, get_bdb)

df

library(patchwork)


ex <- sample(df$play, 4)

plots <- map(ex, ~{
  lab <- labels %>% filter(play == .x) %>% dplyr::pull(coverage)
  plot <- df %>%
    filter(frame_id == 28, play == .x) %>%
    plot_play(
      animated = FALSE,
      segment_length = 4,
      segment_size = 2,
      dot_size = 4
      
    )
  
  plot + 
    labs(title = lab) +
    theme(plot.title = element_blank(),
          plot.caption = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm")
  )
})

(plots[[1]] + plots[[2]] )/ (plots[[3]] + plots[[4]] )


ggsave("plot.png", width = 10, height = 5, units = "in", dpi = 300)

