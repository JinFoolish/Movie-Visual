# --- 1. 加载库 ---
library(shiny)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(lubridate)
library(shinyWidgets)

# --- 2. 加载和预处理数据 ---
# ... (所有的数据加载和 dplyr 处理代码... )
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


# --- 3. 为 UI 输入定义全局变量 ---
min_year <- min(movie$year, na.rm = TRUE)
max_year <- max(movie$year, na.rm = TRUE)

genres_col <- movie %>%
  select(starts_with("genres_"), -"genres_list") %>%
  colnames()
genres_name <- gsub("^genres_", "", genres_col)

country_col <- movie %>%
  select(starts_with("Countries_"), -"Countries_list") %>%
  colnames()
country_name <- gsub("^Countries_", "", country_col)

min_runtime <- min(movie$runtime, na.rm = TRUE)
max_runtime <- max(movie$runtime, na.rm = TRUE)
median_runtime <- median(movie$runtime, na.rm = TRUE)

original_language <- unique(movie$original_language)

buget_min <- min(movie$budget, na.rm = TRUE)
buget_max <- max(movie$budget, na.rm = TRUE)

min_profit <- min(movie$revenue, na.rm = TRUE)
max_profit <- max(movie$revenue, na.rm = TRUE)

metric_choices <- c(
  "Profit (Profit)" = "profit",
  "Investment Return (ROI)" = "roi",
  "Total Revenue (Revenue)" = "revenue"
)

# --- 4. 加载辅助函数 ---
# 这一行是正确的，因为 utils.R 只定义了一个函数，没有使用 'output'
source("R/utils.R")