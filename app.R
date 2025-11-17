library(shiny)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(lubridate)
library(shinyWidgets)
movie <- read.csv("./movies.csv")

movie <- movie %>%
  mutate(
    release_date = as.Date(trimws(as.character(release_date)), format = "%Y-%m-%d"),
    year = year(release_date)
  ) %>%
  filter(!is.na(year)) %>%
  filter(
    vote_count >= 100,
    runtime > 0,
    revenue > 0,
    budget > 10000
  )


movie$primary_genre <- sapply(strsplit(gsub("\\[|\\]|'", "", movie$genres_list), ", "), function(x) {
  if (length(x) > 0 && nchar(x[1]) > 0) {
    return(x[1])
  } else {
    return("Unknown")
  }
})

movie$primary_genre[is.na(movie$primary_genre)] <- "Unknown"

quantiles <- quantile(movie$budget, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
quantiles <- unique(quantiles)

if (length(quantiles) > 1) {
  labels <- if (length(quantiles) == 5) {
    c("Q1 (Low)", "Q2 (Medium-Low)", "Q3 (Medium-High)", "Q4 (High)")
  } else {
    paste0("Bin ", 1:(length(quantiles) - 1)) # Fallback
  }
  movie$budget_band <- cut(movie$budget, breaks = quantiles, labels = labels, include.lowest = TRUE)
} else {
  movie$budget_band <- "Single Value"
}
## year

min_year <- min(movie$year, na.rm = TRUE)
max_year <- max(movie$year, na.rm = TRUE)

## genres
genres_col <- movie %>%
  select(starts_with("genres_"), -"genres_list") %>%
  colnames()
genres_name <- gsub("^genres_", "", genres_col)

## country
country_col <- movie %>%
  select(starts_with("Countries_"), -"Countries_list") %>%
  colnames()
country_name <- gsub("^Countries_", "", country_col)

## runtime
min_runtime <- min(movie$runtime, na.rm = TRUE)
max_runtime <- max(movie$runtime, na.rm = TRUE)
median_runtime <- median(movie$runtime, na.rm = TRUE)

## Languages
original_language <- unique(movie$original_language)

## budget
buget_min <- min(movie$budget, na.rm = TRUE)
buget_max <- max(movie$budget, na.rm = TRUE)

## Profit threshold
min_profit <- min(movie$revenue, na.rm = TRUE)
max_profit <- max(movie$revenue, na.rm = TRUE)

# UI
metric_choices <- c(
  "Profit (Profit)" = "profit",
  "Investment Return (ROI)" = "roi",
  "Total Revenue (Revenue)" = "revenue"
)
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tabsetPanel(
    tabPanel(
      "Discover",
      sidebarLayout(
        sidebarPanel(
          sliderInput("year_d", "Year Range",
            min = min_year, max = max_year,
            value = c(min_year, max_year)
          ),
          selectInput("genre_d", "Genre",
            choices = setNames(genres_col, genres_name),
            multiple = TRUE
          ),
          selectInput("country_d", "Country",
            choices = setNames(country_col, country_name),
            multiple = TRUE
          ),
          sliderInput("runtime_d", "Runtime (min)",
            min = min_runtime, max = max_runtime,
            value = c(min_runtime, max_runtime)
          ),
          selectInput("lang_d", "Original Language",
            choices = original_language,
            multiple = TRUE
          )
        ),
        mainPanel(
          plotOutput("mainPlot"),
          hr(),
          tableOutput("subTable")
        )
      )
    ),
    tabPanel(
      "Invest",
      sidebarLayout(
        sidebarPanel(
          selectInput("metric_i", "Select Metric to Analyze:",
            choices = metric_choices,
            selected = "profit"
          ),
          checkboxInput("show_outliers",
            "Include Extreme Outliers (e.g., Top/Bottom 5%)",
            value = FALSE
          ),
          selectInput("genre_i", "Genre",
            choices = setNames(genres_col, genres_name),
            multiple = TRUE
          ),
          selectInput("country_i", "Country",
            choices = setNames(country_col, country_name),
            multiple = TRUE
          ),
          sliderInput("budget_i", "Budget bands",
            min = buget_min, max = buget_max,
            value = c(buget_min, buget_max)
          ),
          sliderInput("runtime_i", "Runtime(min)",
            min = min_runtime, max = max_runtime,
            value = c(min_runtime, max_runtime)
          ),
          sliderInput("year_i", "Year Range",
            min = min_year, max = max_year,
            value = c(min_year, max_year)
          ),
          sliderInput("profit_i", "Profit threshold",
            min = min_profit, max = max_profit,
            value = c(min_profit, max_profit)
          )
        ),
        mainPanel(
          h4("My Dashboard"),
          checkboxGroupButtons("chart_toggles",
            "Select Charts to Display:",
            choices = c(
              "Heatmap" = "heatmap",
              "Hit Rate" = "barchart",
              "Boxplot" = "boxplot",
              "Trend" = "line",
              "Scatter" = "scatter",
              "Top 10" = "table"
            ),
            selected = c("heatmap", "table"),
            justified = TRUE,
            status = "info"
          ),
          hr(),
          uiOutput("invest_charts_area")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filter_movie_data <- function(df, year_range, genres, countries,
                                runtime_range, languages,
                                budget_range = NULL, profit_range = NULL) {
    df <- df %>%
      filter(
        year >= as.numeric(year_range[1]),
        year <= as.numeric(year_range[2])
      )
    if (length(genres) > 0) {
      for (g in genres) {
        df <- df %>% filter(.data[[g]] == 1)
      }
    }
    if (length(countries) > 0) {
      for (c in countries) {
        df <- df %>% filter(.data[[c]] == 1)
      }
    }
    df <- df %>% filter(
      runtime >= runtime_range[1],
      runtime <= runtime_range[2]
    )
    if (length(languages) > 0) {
      df <- df %>% filter(original_language %in% languages)
    }
    if (!is.null(budget_range)) {
      df <- df %>% filter(
        budget >= budget_range[1],
        budget <= budget_range[2]
      )
    }
    if (!is.null(profit_range)) {
      df <- df %>% filter(
        revenue >= profit_range[1],
        revenue <= profit_range[2]
      )
    }
    df
  }

  filtered_discover <- reactive({
    filter_movie_data(
      df = movie,
      year_range = input$year_d,
      genres = input$genre_d,
      countries = input$country_d,
      runtime_range = input$runtime_d,
      languages = input$lang_d
    )
  })

  output$mainPlot <- renderPlot({
    df <- filtered_discover()
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No movies found under the selected filters")
      return()
    }

    df_top10 <- df %>%
      arrange(desc(vote_average)) %>%
      slice_head(n = 10)

    df_top10_long <- df_top10 %>%
      select(title, budget, revenue) %>%
      pivot_longer(
        cols = c(budget, revenue),
        names_to = "metric",
        values_to = "value"
      )

    ggplot(df_top10_long,
           aes(x = reorder(title, value),
               y = value,
               fill = metric)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(
        title = "Top 10 Movies with Multiple Metrics",
        x = "Movie Title", y = "Value"
      ) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal()
  })

  output$subTable <- renderTable({
    df <- filtered_discover()
    data.frame(
      `Total Movies` = nrow(df),
      `Average Rating` = round(mean(df$vote_average, na.rm = TRUE), 2),
      `Average Revenue` = round(mean(df$revenue, na.rm = TRUE), 2),
      `Average Popularity` = round(mean(df$popularity, na.rm = TRUE), 2)
    )
  })
  # Invest tab
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
  # --- 辅助函数：获取动态指标格式 ---
  get_metric_info <- reactive({
    metric_to_plot <- input$metric_i
    metric_label <- names(metric_choices)[metric_choices == metric_to_plot]

    # (为 Y 轴和图例动态设置格式)
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

  # --- Heatmap ---
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

  # --- Bar Chart (Hit Rate) ---
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

  # --- Boxplot ---
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

  # --- Line Graph---
  output$invest_line <- renderPlot({
    plot_info <- get_plot_data()
    metric_info <- get_metric_info()
    if (is.null(plot_info)) {
      return(NULL)
    }

    # 获取用户选择的 Genres
    selected_genres <- input$genre_i

    if (length(selected_genres) > 0) {
      # 情况 A: 用户【已选择】 Genre (显示对比)
      line_data <- plot_info$data %>%
        # 按年份 和 动态X轴变量 分组)
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
      # 情况 B: 用户【未选择】 Genre (显示总览)
      line_data <- plot_info$base_data %>%
        # 只按年份分组
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

  # --- Scatter Plot ---
  output$invest_scatter <- renderPlot({
    plot_info <- get_plot_data()
    metric_info <- get_metric_info()
    if (is.null(plot_info)) {
      return(NULL)
    }

    # (此图表使用基础数据)
    ggplot(plot_info$base_data, aes(x = budget, y = !!sym(metric_info$var))) +
      geom_point(alpha = 0.5, color = "blue") +
      scale_x_log10(labels = scales::dollar) +
      metric_info$scale_y +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # 基准线
      labs(
        title = paste("Scatter Plot: Budget vs.", metric_info$lab),
        x = "Budget (Log Scale)", y = metric_info$lab
      ) +
      theme_minimal()
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
}

shinyApp(ui = ui, server)
