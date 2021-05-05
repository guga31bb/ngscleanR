

g <- bdb20 %>%
  add_info() %>%
  rotate_to_ltr()

g %>%
  filter(season == 2019, home_team == "SEA")

df_track <- g %>% filter(home_team == "SEA", season == 2019, play_id == 290, possession_team == "SEA")

caption <- glue::glue("{df_track$nflfastr_game_id[1]} {df_track$down[1]}&{df_track$ydstogo[1]}: Q{df_track$qtr[1]} {df_track$desc[1]}")

# before animation
fig <- nfl_field +
  # dots
  geom_point(data = df_track, aes(x, y), 
             color = df_track$team_color, 
             shape = ifelse(
               df_track$team_name == "football" | df_track$defense == 1,
               19, 1
             ), 
             size = 4
  ) +
  # orientation lines
  geom_segment(
    data = df_track,
    aes(x, y, xend = x + 2.5 * o_x, yend = y + 2.5 * o_y),
    color = df_track$team_color, size = 1.5
  ) +
  # numbers
  geom_text(
    data = df_track,
    mapping = aes(x = x, y = y, label = jersey_number),
    colour = ifelse(df_track$defense == 1, df_track$team_color2, "white"),
    size = 2
  ) +
  labs(
    caption = caption
  ) +
  theme(
    plot.title = element_blank(),
    plot.margin = margin(.1, 0, .5, 0, "cm"),
    plot.caption = element_text(size = 8)
  )

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