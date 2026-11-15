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

# 2a

cook_stat <- cook_stat |>
  mutate(
    college_share = educ16 / numpers25,
    mean_income = agghhinc / numhh,
  )

cook_stat_2010 <- cook_stat |>
  filter(year == 2010)

mean_education_income <- cook_stat_2010 |>
  summarise(
    across(
      c(college_share, mean_income),
      ~ mean(.x, na.rm = TRUE)
    )
  )

# 2b

export_scatterplot(
  cook_stat_2010,
  x = "black_share",
  y = "mean_income",
  file_name = "2b_income_black_share.png",
  x_label = "Black Population Share",
  y_label = "Mean Household Income",
  x_format = "percent",
  y_format = "dollar"
)

export_scatterplot(
  cook_stat_2010,
  x = "hisp_share",
  y = "mean_income",
  file_name = "2b_income_hispanic_share.png",
  x_label = "Hispanic Population Share",
  y_label = "Mean Household Income",
  x_format = "percent",
  y_format = "dollar"
)

export_scatterplot(
  cook_stat_2010,
  x = "white_share",
  y = "mean_income",
  file_name = "2b_income_white_share.png",
  x_label = "White Population Share",
  y_label = "Mean Household Income",
  x_format = "percent",
  y_format = "dollar"
)

export_scatterplot(
  cook_stat_2010,
  x = "black_share",
  y = "college_share",
  file_name = "2b_college_black_share.png",
  x_label = "Black Population Share",
  y_label = "College-Educated Share",
  x_format = "percent",
  y_format = "percent"
)

export_scatterplot(
  cook_stat_2010,
  x = "hisp_share",
  y = "college_share",
  file_name = "2b_college_hispanic_share.png",
  x_label = "Hispanic Population Share",
  y_label = "College-Educated Share",
  x_format = "percent",
  y_format = "percent"
)

export_scatterplot(
  cook_stat_2010,
  x = "white_share",
  y = "college_share",
  file_name = "2b_college_white_share.png",
  x_label = "White Population Share",
  y_label = "College-Educated Share",
  x_format = "percent",
  y_format = "percent"
)

# 3a

cook_schools_sat_2023 <- read_dta('data/cook_schools_sat_2023.dta')
prices_2022_2023 <- read_dta('data/prices_2022_2023.dta')

summary_variables <- c(
  "percent_lowinc",
  "percent_white",
  "percent_black",
  "percent_hisp",
  "percent_asian",
  "satreadingaveragescore",
  "satmathaveragescore"
)

school_summary <- cook_schools_sat_2023 |>
  filter(!is.na(city)) |>
  mutate(group = if_else(city == "Chicago", "Chicago", "Suburbs")) |>
  tidyr::pivot_longer(
    cols = all_of(summary_variables),
    names_to = "measure",
    values_to = "value"
  ) |>
  group_by(measure, group) |>
  summarise(
    mean = if (all(is.na(value))) NA_real_ else mean(value, na.rm = TRUE),
    n = sum(!is.na(value)),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = group,
    values_from = c(mean, n),
    names_glue = "{.value}_{group}"
  ) |>
  mutate(
    measure = recode(
      measure,
      percent_lowinc = "% Low-Income",
      percent_white = "% White",
      percent_black = "% Black",
      percent_hisp = "% Hispanic",
      percent_asian = "% Asian",
      satreadingaveragescore = "SAT Reading",
      satmathaveragescore = "SAT Math"
    )
  ) |>
  transmute(
    measure,
    Chicago = mean_Chicago,
    Suburbs = mean_Suburbs,
    Chicago_N = n_Chicago,
    Suburbs_N = n_Suburbs
  ) |>
  export_word_table(
    file_name = "3a.docx",
    column_names = c(
      measure = "Measure",
      Chicago = "Chicago Schools Mean",
      Suburbs = "Suburbs Schools Mean",
      Chicago_N = "Chicago N",
      Suburbs_N = "Suburbs N"
    ),
    integer_columns = c("Chicago_N", "Suburbs_N"),
    digits = 1
  )

# 3b

sat_math_income_plot_data <- cook_schools_sat_2023 |>
  filter(
    !is.na(percent_lowinc),
    !is.na(satmathaveragescore),
    !is.na(city)
  ) |>
  mutate(
    group = if_else(city == "Chicago", "Chicago", "Suburbs"),
    low_income_share = percent_lowinc / 100
  ) |>
  export_scatterplot(
    x = "low_income_share",
    y = "satmathaveragescore",
    file_name = "3b.png",
    x_label = "% Low-Income Students",
    y_label = "Avg SAT Math Score",
    x_format = "percent",
    color_by = "group",
    color_label = "School Location"
  )

# 3c

submarket_mapping <- tibble::tribble(
  ~submarket, ~city,
  "Schaumburg", "Schaumburg",
  "Evanston/Skokie", "Evanston",
  "Evanston/Skokie", "Skokie",
  "Calumet City/Harvey", "Calumet City",
  "Calumet City/Harvey", "Harvey"
)

submarket_sat_summary <- cook_schools_sat_2023 |>
  inner_join(submarket_mapping, by = "city") |>
  group_by(submarket) |>
  summarise(
    school_n = n(),
    mean_sat_reading = mean(satreadingaveragescore, na.rm = TRUE),
    mean_sat_math = mean(satmathaveragescore, na.rm = TRUE),
  )

submarket_prices <- prices_2022_2023 |>
  filter(submarket %in% unique(submarket_mapping$submarket)) |>
  select(submarket, median_sales_price = mediansalesprice2022q32023q2)


prices_and_sats <- left_join(submarket_prices, submarket_sat_summary, by = "submarket") |>
  export_word_table(
    file_name = "3c.docx",
    column_names = c(
      submarket = "Housing Submarket",
      median_sales_price = "Median Sales Price",
      mean_sat_reading = "Mean SAT Reading",
      mean_sat_math = "Mean SAT Math",
      school_n = "Schools"
    ),
    dollar_columns = "median_sales_price",
    integer_columns = c("mean_sat_reading", "mean_sat_math", "school_n"),
    digits = 1
  )
