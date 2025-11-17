# 通用的数据过滤函数
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