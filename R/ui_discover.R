ui_discover <- tabPanel(
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
)