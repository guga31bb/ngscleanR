library(tidyverse)
require(data.table)

# ngs highlights
# https://github.com/asonty/ngs_highlights
df_ngs <- as.data.frame(fread("data-raw/2018_ARI_2018092311_141.tsv.txt")) %>%
  as_tibble()

saveRDS(df_ngs, "data-raw/sample_ngs.rds")

# 2019 bdb
bdb19 <- read_csv(url("https://github.com/nfl-football-ops/Big-Data-Bowl/blob/master/Data/tracking_gameId_2017090700.csv?raw=true"))

bdb19 %>%
  saveRDS("data-raw/sample_bdb_2019.rds")

# 2020 bdb
bdb20 <- read_csv("data-raw/bdb_2020.csv")

bdb20 %>%
  filter(GameId == 2019112411) %>%
  saveRDS("data-raw/sample_bdb_2020.rds")

# 2021 bdb
bdb21 <- suppressMessages(readr::read_csv(glue::glue("../nfl-big-data-bowl-2021/input/week{week}.csv")))

bdb21 %>%
  # filter(gameId == 2018120600) %>%
  saveRDS("data-raw/sample_bdb_2021.rds")

# # #
# create old nflfastR data

load_nflfastr <- function(y) {

  .url <- glue::glue("https://github.com/nflverse/nflfastR-data/blob/master/data/play_by_play_{y}.rds?raw=true")
  con <- url(.url)
  pbp <- readRDS(con)
  close(con)
  return(pbp)

}

message(glue::glue("Getting nflfastR data"))
pbp <- map_df(2017:2020, load_nflfastr) %>%
  dplyr::rename(nflfastr_game_id = game_id, game_id = old_game_id) %>%
  dplyr::select(
    nflfastr_game_id,
    game_id,
    play_id,
    week,
    posteam,
    home_team,
    away_team,
    down,
    ydstogo,
    yardline_100,
    qtr,
    epa,
    yards_gained,
    air_yards,
    desc,
    pass,
    rush,
    play_type_nfl
  ) %>%
  dplyr::mutate(game_id = as.integer(game_id))

# put in use_data instead
# saveRDS(pbp, "data-raw/nflfastr_plays.rds")


# # #
# create coverage labels
labels <- read_csv("../nfl-big-data-bowl-2021/input/coverages_2018.csv") %>%
  mutate(coverage = case_when(
    coverage == "3 Seam" ~ "Cover 3 Zone",
    coverage == "Cover 1 Double" ~ "Cover 1 Man",
    coverage %in% c("Red Zone", "Goal Line") ~ "Red zone / goal line",
    coverage == "Mis" | is.na(coverage) ~ "Other / misc",
    TRUE ~ coverage
  )) %>%
  filter(coverage %in% c(
    "Cover 0 Man",
    "Cover 1 Man",
    "Cover 2 Man",
    "Cover 2 Zone",
    "Cover 3 Zone",
    "Cover 4 Zone",
    "Cover 6 Zone",
    "Bracket",
    "Prevent"
  ))

labels %>%
  saveRDS("data-raw/coverage_labels.rds")

# from telemetry
readr::read_csv("../nfl-big-data-bowl-2021/input/coverages_week1.csv") %>%
  saveRDS("data-raw/coverages_week1.rds")


# get team colors and logo for joining
colors <- nflfastR::teams_colors_logos %>%
  select(team_name = team_abbr, team_color, team_color2, team_logo_espn)

nfl_field <- sportyR::geom_football(
  'nfl',
  # the CC at the end gives the field lower alpha
  grass_color = "#196f0cCC"
)

usethis::use_data(
  pbp, colors, nfl_field,
  internal = TRUE, overwrite = TRUE
)


