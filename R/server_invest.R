# --- 'Invest' Tab UI Output ---
output$invest_charts_area <- renderUI({
  selected_charts <- input$chart_toggles
  if (is.null(selected_charts)) {
    return(helpText("Please select one or more charts from above to view."))
  }

  chart_list <- list()

  if ("heatmap" %in% selected_charts) {
    chart_list <- append(chart_list, list(
      h4("Heatmap (Median Metric by Genre & Budget)"),
      helpText("Display the median values for the selected metrics (ROI, Profit, Revenue) grouped by type and budget."),
      plotOutput("invest_heatmap"),
      hr()
    ))
  }

  if ("barchart" %in% selected_charts) {
    chart_list <- append(chart_list, list(
      h4("Bar Chart (Hit Rate by Genre & Budget)"),
      helpText("Display the percentage of profitable films (Profit > 0)."),
      plotOutput("invest_barchart"),
      hr()
    ))
  }

  if ("boxplot" %in% selected_charts) {
    chart_list <- append(chart_list, list(
      h4("Boxplot (Metric Distribution by Genre)"),
      helpText("Display the distribution of metrics for different types (those you select or all)."),
      plotOutput("invest_boxplot"),
      hr()
    ))
  }

  if ("line" %in% selected_charts) {
    chart_list <- append(chart_list, list(
      h4("Line Graph (Average Metric Over Time)"),
      helpText("Display the trend of the selected indicator over time."),
      plotOutput("invest_line"),
      hr()
    ))
  }

  if ("scatter" %in% selected_charts) {
    chart_list <- append(chart_list, list(
      h4("Scatter Plot (Budget vs. Metric)"),
      helpText("Display the relationship between the budget and the selected metrics."),
      plotOutput("invest_scatter"),
      hr()
    ))
  }

  if ("table" %in% selected_charts) {
    chart_list <- append(chart_list, list(
      h4("Top 10 Movies (Sorted by Selected Metric)"),
      tableOutput("invest_top_movies_table"),
      hr(),
      h5("Genre Details:"),
      verbatimTextOutput("invest_movie_genres_text"),
      hr()
    ))
  }

  tagList(chart_list)
})

# --- 'Invest' Tab Reactives ---

filtered_invest <- reactive({
  req(movie)

  df_invest <- movie %>%
    filter(
      year >= input$year_i[1],
      year <= input$year_i[2],
      budget >= input$budget_i[1],
      budget <= input$budget_i[2],
      runtime >= input$runtime_i[1],
      runtime <= input$runtime_i[2],
      revenue >= input$profit_i[1],
      revenue <= input$profit_i[2]
    )

  if (length(input$genre_i) > 0) {
    df_invest <- df_invest %>%
      filter(rowSums(select(., all_of(input$genre_i))) > 0)
  }

  if (length(input$country_i) > 0) {
    df_invest <- df_invest %>%
      filter(rowSums(select(., all_of(input$country_i))) > 0)
  }

  df_invest <- df_invest %>%
    mutate(
      profit = revenue - budget,
      roi = (revenue - budget) / budget,
      is_hit = ifelse(profit > 0, 1, 0)
    )

  df_invest
})

get_plot_data <- reactive({
  df_filtered_movies <- filtered_invest()
  if (is.null(df_filtered_movies) || nrow(df_filtered_movies) == 0) {
    return(NULL)
  }

  if (input$show_outliers == FALSE) {
    metric_to_plot <- input$metric_i

    q_low <- quantile(df_filtered_movies[[metric_to_plot]], probs = 0.05, na.rm = TRUE)
    q_high <- quantile(df_filtered_movies[[metric_to_plot]], probs = 0.95, na.rm = TRUE)

    df_filtered_movies <- df_filtered_movies %>%
      filter(!!sym(metric_to_plot) >= q_low & !!sym(metric_to_plot) <= q_high)
  }

  if (nrow(df_filtered_movies) == 0) {
    return(NULL)
  }

  selected_genres <- input$genre_i
  if (length(selected_genres) > 0) {
    plot_data <- df_filtered_movies %>%
      tidyr::pivot_longer(
        cols = all_of(selected_genres),
        names_to = "selected_genre_name",
        values_to = "is_present"
      ) %>%
      filter(is_present == 1) %>%
      mutate(selected_genre_name = gsub("^genres_", "", selected_genre_name))
    x_axis_variable <- "selected_genre_name"
    x_axis_label <- "Selected Genre"
  } else {
    plot_data <- df_filtered_movies
    x_axis_variable <- "primary_genre"
    x_axis_label <- "Primary Genre (All)"
  }
  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  return(list(
    data = plot_data,
    x_var = x_axis_variable,
    x_lab = x_axis_label,
    base_data = df_filtered_movies
  ))
})

get_metric_info <- reactive({
  metric_to_plot <- input$metric_i
  metric_label <- names(metric_choices)[metric_choices == metric_to_plot]

  dynamic_scale_y <- if (metric_to_plot == "roi") {
    scale_y_continuous(labels = scales::percent)
  } else if (metric_to_plot == "profit") {
    scale_y_continuous(labels = scales::dollar)
  } else {
    scale_y_log10(labels = scales::dollar)
  }

  dynamic_fill_scale <- if (metric_to_plot == "revenue") {
    scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::dollar)
  } else {
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "darkgreen",
      labels = if (metric_to_plot == "roi") scales::percent else scales::dollar
    )
  }

  return(list(
    var = metric_to_plot,
    lab = metric_label,
    scale_y = dynamic_scale_y,
    scale_fill = dynamic_fill_scale
  ))
})

reactive_top_10_data <- reactive({
  metric_to_sort <- input$metric_i

  filtered_invest() %>%
    arrange(desc(!!sym(metric_to_sort))) %>%
    slice_head(n = 10) %>%
    select(
      title,
      genres_list,
      profit,
      roi,
      revenue
    )
})

# --- 'Invest' Tab Plot/Table Outputs ---

output$invest_heatmap <- renderPlot({
  plot_info <- get_plot_data()
  metric_info <- get_metric_info()
  if (is.null(plot_info)) {
    return(NULL)
  }

  summary_data <- plot_info$data %>%
    group_by(!!sym(plot_info$x_var), budget_band) %>%
    summarise(median_metric = median(!!sym(metric_info$var), na.rm = TRUE), .groups = "drop")

  ggplot(summary_data, aes(x = !!sym(plot_info$x_var), y = budget_band, fill = median_metric)) +
    geom_tile(color = "white") +
    metric_info$scale_fill +
    labs(
      title = paste("Heatmap: Median", metric_info$lab),
      x = plot_info$x_lab, y = "Budget Band", fill = paste("Median", metric_info$lab)
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$invest_barchart <- renderPlot({
  plot_info <- get_plot_data()
  if (is.null(plot_info)) {
    return(NULL)
  }

  summary_data <- plot_info$data %>%
    group_by(!!sym(plot_info$x_var), budget_band) %>%
    summarise(hit_rate = mean(is_hit, na.rm = TRUE), .groups = "drop")

  ggplot(summary_data, aes(x = !!sym(plot_info$x_var), y = hit_rate, fill = budget_band)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Bar Chart: Hit Rate (Revenue > Budget)",
      x = plot_info$x_lab, y = "Hit Rate (%)", fill = "Budget Band"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

output$invest_boxplot <- renderPlot({
  plot_info <- get_plot_data()
  metric_info <- get_metric_info()
  if (is.null(plot_info)) {
    return(NULL)
  }

  ggplot(plot_info$data, aes(x = !!sym(plot_info$x_var), y = !!sym(metric_info$var), fill = !!sym(plot_info$x_var))) +
    geom_boxplot() +
    metric_info$scale_y +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = paste("Boxplot:", metric_info$lab, "Distribution"),
      x = plot_info$x_lab, y = metric_info$lab
    )
})

output$invest_line <- renderPlot({
  plot_info <- get_plot_data()
  metric_info <- get_metric_info()
  if (is.null(plot_info)) {
    return(NULL)
  }

  selected_genres <- input$genre_i

  if (length(selected_genres) > 0) {
    line_data <- plot_info$data %>%
      group_by(year, !!sym(plot_info$x_var)) %>%
      summarise(mean_metric = mean(!!sym(metric_info$var), na.rm = TRUE), .groups = "drop")

    ggplot(line_data, aes(
      x = year,
      y = mean_metric,
      color = !!sym(plot_info$x_var)
    )) +
      geom_line(alpha = 0.8, linewidth = 1.2) +
      geom_point(alpha = 0.8) +
      metric_info$scale_y +
      labs(
        title = paste("Trend:", metric_info$lab, "Over Time by Genre"),
        x = "Year", y = paste("Average", metric_info$lab),
        color = plot_info$x_lab
      ) +
      theme_minimal()
  } else {
    line_data <- plot_info$base_data %>%
      group_by(year) %>%
      summarise(mean_metric = mean(!!sym(metric_info$var), na.rm = TRUE), .groups = "drop")

    ggplot(line_data, aes(x = year, y = mean_metric)) +
      geom_line(color = "steelblue", linewidth = 1.2) +
      geom_point(color = "steelblue") +
      metric_info$scale_y +
      labs(
        title = paste("Line Graph: Average", metric_info$lab, "Over Time (All Genres)"),
        x = "Year", y = paste("Average", metric_info$lab)
      ) +
      theme_minimal()
  }
})

output$invest_scatter <- renderPlot({
  plot_info <- get_plot_data()
  metric_info <- get_metric_info()
  if (is.null(plot_info)) {
    return(NULL)
  }

  ggplot(plot_info$base_data, aes(x = budget, y = !!sym(metric_info$var))) +
    geom_point(alpha = 0.5, color = "blue") +
    scale_x_log10(labels = scales::dollar) +
    metric_info$scale_y +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Scatter Plot: Budget vs.", metric_info$lab),
      x = "Budget (Log Scale)", y = metric_info$lab
    ) +
    theme_minimal()
})

output$invest_top_movies_table <- renderTable(
  {
    table_data <- reactive_top_10_data() %>%
      select(-genres_list)

    table_data %>%
      mutate(
        Profit = scales::dollar(profit, accuracy = 1),
        Revenue = scales::dollar(revenue, accuracy = 1),
        ROI = scales::percent(roi, accuracy = 0.1)
      ) %>%
      select(Title = title, Profit, ROI, Revenue)
  },
  striped = TRUE
)

output$invest_movie_genres_text <- renderText({
  text_data <- reactive_top_10_data()

  movie_genre_strings <- text_data %>%
    mutate(
      genres_clean = gsub("\\[|\\]|'", "", genres_list)
    ) %>%
    rowwise() %>%
    mutate(
      display_text = paste0(title, ": ", genres_clean)
    ) %>%
    pull(display_text)

  paste(movie_genre_strings, collapse = "\n")
})