library(haven)
library(dplyr)
source("helpers.R")

cook_stat <- read_dta('data/cook_stat.dta')

cook_stat <- cook_stat |>
  mutate(
    white_share = totwht / tractpop,
    black_share = totblk / tractpop,
    hisp_share = tothisp / tractpop,
  )

# 1a

cook_stat |>
  filter(white_share > 1 | black_share > 1 | hisp_share > 1) |>
  count()

cook_stat <- cook_stat |>
  mutate(
    white_share = if_else(white_share > 1, NA_real_, white_share),
    black_share = if_else(black_share > 1, NA_real_, black_share),
    hisp_share = if_else(hisp_share > 1, NA_real_, hisp_share),
  )

mean_racial_shares <- cook_stat |>
  filter(year %in% c(1980, 1990, 2000, 2010)) |>
  group_by(year) |>
  summarise(
    across(
      c(white_share, black_share, hisp_share),
      ~ mean(.x, na.rm = TRUE)
    )
  ) |>
  export_word_table(
    file_name = "1a.docx",
    column_names = c(
      year = "Mean Tract Shares:",
      white_share = "White",
      black_share = "Black",
      hisp_share = "Hispanic"
    ),
    percent_columns = c("white_share", "black_share", "hisp_share"),
    integer_columns = "year"
  )

# 1b

fraction_of_highly_segregated_tracts <- cook_stat |>
  filter(year %in% c(1980, 1990, 2000, 2010)) |>
  group_by(year) |>
  summarise(
    across(
      c(white_share, black_share, hisp_share),
      ~ mean(.x >= 0.8, na.rm = TRUE)
    )
  ) |>
  export_word_table(
    file_name = "1b.docx",
    column_names = c(
      year = "Proportion of Tracts Over 80%:",
      white_share = "White",
      black_share = "Black",
      hisp_share = "Hispanic"
    ),
    percent_columns = c("white_share", "black_share", "hisp_share"),
    integer_columns = "year"
  )

# 1c

dissimilarity_indices <- cook_stat |>
  filter(year %in% c(1980, 1990, 2000, 2010)) |>
  group_by(year) |>
  summarise(
    black_white = sum(abs(totblk / sum(totblk) - totwht / sum(totwht))) / 2,
    hispanic_white = sum(abs(tothisp / sum(tothisp) - totwht / sum(totwht))) / 2
  ) |>
  export_word_table(
    file_name = "1c.docx",
    column_names = c(
      year = "Dissimilarity Indices:",
      black_white = "Black-White",
      hispanic_white = "Hispanic-White"
    ),
    integer_columns = "year",
    digits = 2
  )
