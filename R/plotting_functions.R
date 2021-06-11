

#' Plot a play lol
#'
#' @param df_track A df of tracking data
#' @param orientation Show lines representing where player is facing (default = T)
#' @param dot_size Size of player dots (default = 6)
#' @param segment_length Length of orientation segment lines (default = 2.5)
#' @param segment_size Width of orientation segment lines (default = 1.5)
#' @param numbers Show player jersey numbers (default = T)
#' @param animated Whether play is animated or a still frame (default = T)
#' @param animated_h If animated, height of animated image (default = 4)
#' @param animated_w If animated, width of animated image (default = 8)
#' @param animated_res If animated, resolution of animated image (default = 200)
#' @param frame frame_id to plot (default = NULL, ie plot all provided frames).
#' @export
plot_play <- function(
  df_track,
  orientation = TRUE,
  dot_size = 6,
  segment_length = 2.5,
  segment_size = 1.5,
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
        aes(x, y, xend = x + segment_length * o_x, yend = y + segment_length * o_y),
        color = df_track$team_color, size = segment_size
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
     start_pause = 6,
     end_pause = 4
    )

  }

  return(fig)

}


# helper function to not make every table have so many lines of code
make_table <- function(df) {
  df %>%
    gt::gt() %>%
    gt::tab_style(
      style = gt::cell_text(color = "black", weight = "bold"),
      locations = list(
        gt::cells_column_labels(dplyr::everything())
      )
    ) %>%
    gt::tab_options(
      row_group.border.top.width = gt::px(3),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      table_body.hlines.color = "white",
      table.border.top.color = "black",
      table.border.top.width = gt::px(1),
      table.border.bottom.color = "white",
      table.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = gt::px(2),
      row.striping.background_color = '#FFFFFF',
      row.striping.include_table_body = TRUE,
      table.background.color = '#F2F2F2',
      data_row.padding = gt::px(2),
      table.font.size = gt::px(16L)
    ) %>%
    return()
}



