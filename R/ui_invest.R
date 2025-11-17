ui_invest <- tabPanel(
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