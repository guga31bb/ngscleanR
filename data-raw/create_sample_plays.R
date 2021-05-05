library(tidyverse)

df_ngs <- as.data.frame(fread("data-raw/2018_ARI_2018092311_141.tsv.txt")) %>%
  as_tibble()

saveRDS(df_ngs, "data-raw/sample_ngs.rds")

bdb19 <- read_csv(url("https://github.com/nfl-football-ops/Big-Data-Bowl/blob/master/Data/tracking_gameId_2017090700.csv?raw=true"))

bdb19 %>%
  saveRDS("data-raw/sample_bdb_2019.rds")

bdb20 <- read_csv("data-raw/bdb_2020.csv")

bdb20 %>%
  filter(GameId == 2019112411) %>%
  saveRDS("data-raw/sample_bdb_2020.rds")

bdb21 <- suppressMessages(readr::read_csv(glue::glue("../nfl-big-data-bowl-2021/input/week{week}.csv")))

bdb21 %>%
  filter(gameId == 2018120600) %>%
  saveRDS("data-raw/sample_bdb_2021.rds")
