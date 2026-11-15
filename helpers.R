output_directory <- "answers"

export_word_table <- function(data, file_name, column_names,
                              percent_columns = character(),
                              integer_columns = character(),
                              dollar_columns = character(),
                              digits = 3,
                              percent_digits = 1) {

  requested_columns <- c(
    names(column_names),
    percent_columns,
    integer_columns,
    dollar_columns
  )
  missing_columns <- setdiff(requested_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Columns not found in `data`: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

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
  for (column in dollar_columns) {
    if (!is.numeric(display_data[[column]])) {
      stop("Dollar column must be numeric: ", column, call. = FALSE)
    }
    value <- display_data[[column]]
    display_data[[column]] <- ifelse(
      is.na(value),
      "",
      paste0(
        "$",
        format(round(value), big.mark = ",", scientific = FALSE, trim = TRUE)
      )
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

  flextable::save_as_docx(
    table,
    path = file.path(output_directory, file_name)
  )
}

export_scatterplot <- function(data, x, y, file_name, x_label, y_label,
                               x_format = NULL, y_format = NULL) {
  formats <- c("dollar", "percent")
  for (format in list(x_format, y_format)) {
    if (!is.null(format) &&
        (length(format) != 1L || is.na(format) || !format %in% formats)) {
      stop("Axis formats must be NULL, \"dollar\", or \"percent\".", call. = FALSE)
    }
  }

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data[[x]], y = .data[[y]])
  ) +
    ggplot2::geom_point(na.rm = TRUE) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, na.rm = TRUE) +
    ggplot2::labs(x = x_label, y = y_label) +
    ggplot2::theme_minimal(
      base_family = "Times New Roman",
      base_size = 12
    )

  if (!is.null(x_format)) {
    x_labels <- switch(
      x_format,
      dollar = scales::label_dollar(accuracy = 1),
      percent = scales::label_percent()
    )
    plot <- plot + ggplot2::scale_x_continuous(labels = x_labels)
  }
  if (!is.null(y_format)) {
    y_labels <- switch(
      y_format,
      dollar = scales::label_dollar(accuracy = 1),
      percent = scales::label_percent()
    )
    plot <- plot +
      ggplot2::scale_y_continuous(labels = y_labels)
  }

  ggplot2::ggsave(
    filename = file.path(output_directory, file_name),
    plot = plot
  )
}
