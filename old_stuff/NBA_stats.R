library(tidyverse)
library(rvest)
library(haven)


# This is how I am pulling and getting the data into a tiblle

regular_2021.url <- ("https://www.basketball-reference.com/leagues/NBA_2021_totals.html")
regular_2021 <- regular_2021.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

playoff_2021.url <- ("https://www.basketball-reference.com/playoffs/NBA_2021_totals.html")
playoff_2021 <- playoff_2021.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

regular_2022.url <- ("https://www.basketball-reference.com/leagues/NBA_2022_totals.html")
regular_2022 <- regular_2022.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

playoff_2022.url <- ("https://www.basketball-reference.com/playoffs/NBA_2022_totals.html")
playoff_2022 <- playoff_2022.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

regular_2023.url <- ("https://www.basketball-reference.com/leagues/NBA_2023_totals.html")
regular_2023 <- regular_2023.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

playoff_2023.url <- ("https://www.basketball-reference.com/playoffs/NBA_2023_totals.html")
playoff_2023 <- playoff_2023.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()

playoff_2023

playoff_2020$Age <- parse_integer(playoff_2020$Age)

