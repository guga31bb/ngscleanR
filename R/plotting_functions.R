

g <- bdb20 %>%
  add_info() %>%
  rotate_to_ltr()

g %>%
  filter(season == 2019, home_team == "SEA")

df_track <- g %>% filter(home_team == "SEA", season == 2019, play_id == 290, possession_team == "SEA")


plot_play <- function(
  df_track, 
  orientation = TRUE, 
  dot_size = 6,
  numbers = TRUE,
  animated = TRUE,
  animated_h = 4,
  animated_w = 8,
  animated_res = 200,
  frame = NULL
  ) {
  
  caption <- glue::glue("{df_track$nflfastr_game_id[1]} {df_track$down[1]}&{df_track$ydstogo[1]}: Q{df_track$qtr[1]} {df_track$desc[1]}")
  
  if (!is.null(frame)) {
    
    df_track <- df_track %>% filter(frame_id == frame)
    
  }
  
  fig <- nfl_field +
    # dots
    geom_point(data = df_track, aes(x, y), 
               color = df_track$team_color, 
               shape = ifelse(
                 df_track$team_name == "football" | df_track$defense == 1,
                 19, 1
               ), 
               size = dot_size
    ) +
    labs(
      caption = caption
    ) +
    theme(
      plot.title = element_blank(),
      plot.margin = margin(.1, 0, .5, 0, "cm"),
      plot.caption = element_text(size = 8)
    )
  
  if (orientation == TRUE & "o" %in% names(df_track)) {
    
    fig <- fig +
      # orientation lines
      geom_segment(
        data = df_track,
        aes(x, y, xend = x + 2.5 * o_x, yend = y + 2.5 * o_y),
        color = df_track$team_color, size = 1.5
      )
    
  }
  
  if (numbers) {
    
    fig <- fig +
      geom_text(
        data = df_track,
        mapping = aes(x = x, y = y, label = jersey_number),
        colour = ifelse(df_track$defense == 1, df_track$team_color2, "white"),
        size = 2
      ) 
    
  }
  
  if (animated) {
    
    # if (animated_output == "mp4") {
    #   renderer <- gganimate::gifski_renderer()
    # } else {
    #   renderer <- gganimate::av_renderer()
    # }
    # 
    fig <- fig +
      gganimate::transition_time(df_track$frame_id)
    
    fig <- gganimate::animate(
     fig, 
     # renderer = renderer,
     height = animated_h, width = animated_w, units = "in", 
     res = animated_res,
     nframes = n_distinct(df_track$frame_id),
     start_pause = 5,
     end_pause = 3
    )
    
  }
  
  return(fig)
  
}

animate_play <- function(
  df_track,
  height = 4,
  width = 8,
  res = 200,
  type = "gif"
  ) {
  
  
  
}

# before animation


fig

plot <- fig +
  gganimate::transition_time(df_track$frame_id)

gganimate::animate(plot, 
                   # video
                   renderer = gganimate::av_renderer(),
                   height = 4, width = 8, units = "in", res = 250,
                   nframes = n_distinct(df_track$frame_id)
)

gganimate::anim_save("test.mp4")