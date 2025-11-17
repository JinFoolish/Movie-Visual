# --- 'Discover' Tab Reactives ---
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

# --- 'Discover' Tab Outputs ---
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