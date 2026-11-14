export_word_table <- function(data, file_name, column_names,
                              percent_columns = character(),
                              integer_columns = character(),
                              digits = 3,
                              percent_digits = 1) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  requested_columns <- c(names(column_names), percent_columns, integer_columns)
  missing_columns <- setdiff(requested_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Columns not found in `data`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  output_directory <- "answers"
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_directory)) {
    stop("Could not create output directory: ", output_directory, call. = FALSE)
  }
  path <- file.path(output_directory, file_name)

  display_data <- data
  for (column in percent_columns) {
    if (!is.numeric(display_data[[column]])) {
      stop("Percent column must be numeric: ", column, call. = FALSE)
    }
    value <- display_data[[column]]
    display_data[[column]] <- ifelse(
      is.na(value),
      "",
      sprintf(paste0("%.", percent_digits, "f%%"), 100 * value)
    )
  }
  for (column in integer_columns) {
    if (!is.numeric(display_data[[column]])) {
      stop("Integer column must be numeric: ", column, call. = FALSE)
    }
    value <- display_data[[column]]
    display_data[[column]] <- ifelse(
      is.na(value),
      "",
      sprintf("%.0f", value)
    )
  }
  numeric_columns <- names(display_data)[
    vapply(display_data, is.numeric, logical(1))
  ]
  for (column in numeric_columns) {
    value <- display_data[[column]]
    display_data[[column]] <- ifelse(
      is.na(value),
      "",
      sprintf(paste0("%.", digits, "f"), value)
    )
  }

  table <- flextable::flextable(display_data)
  table <- flextable::set_header_labels(table, values = column_names)
  table <- flextable::theme_booktabs(table)
  table <- flextable::font(table, fontname = "Times New Roman", part = "all")
  table <- flextable::fontsize(table, size = 12, part = "all")
  table <- flextable::align(table, align = "center", part = "all")
  table <- flextable::set_table_properties(
    table,
    opts_word = list(repeat_headers = FALSE)
  )
  table <- flextable::autofit(table)

  flextable::save_as_docx(table, path = path)
}
