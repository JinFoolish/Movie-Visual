
library(shiny)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(lubridate)
movie <- read.csv("./movies.csv")

movie <- movie %>%
  # 转换日期并提取年份
  # 注意：trimws 清除空白, as.character 确保格式一致
  mutate(
    release_date = as.Date(trimws(as.character(release_date)), format = "%Y-%m-%d"),
    year = year(release_date)
  ) %>%
  # 移除 release_date 转换失败 (NA) 的行
  filter(!is.na(year)) %>%
  
  # [cite_start]应用 Proposal [cite: 221, 222] 中的过滤规则
  filter(
    vote_count >= 100,  # [cite: 221]
    runtime > 0,        # [cite: 222]
    revenue > 0,        # [cite: 222]
    budget > 10000         # (创建分档和绘图所必需)
  )

# <b>B. 创建 primary_genre (用于图表分组)</b>
# 从 'genres_list' 中提取第一个类型
movie$primary_genre <- sapply(strsplit(gsub("\\[|\\]|'", "", movie$genres_list), ", "), function(x) {
  if (length(x) > 0 && nchar(x[1]) > 0) {
    return(x[1])
  } else {
    return("Unknown") # 按 proposal [cite: 220] 处理缺失值
  }
})
# 确保没有NA
movie$primary_genre[is.na(movie$primary_genre)] <- "Unknown"

# <b>C. [cite_start]创建 budget_band (按四分位数) [cite: 223]</b>
quantiles <- quantile(movie$budget, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
quantiles <- unique(quantiles) # 确保分界点唯一

if (length(quantiles) > 1) {
  labels <- if (length(quantiles) == 5) {
    c('Q1 (Low)', 'Q2 (Medium-Low)', 'Q3 (Medium-High)', 'Q4 (High)')
  } else {
    paste0("Bin ", 1:(length(quantiles) - 1)) # Fallback
  }
  movie$budget_band <- cut(movie$budget, breaks = quantiles, labels = labels, include.lowest = TRUE)
} else {
  movie$budget_band <- "Single Value" # 不太可能的降级情况
}
## year

min_year <- min(movie$year, na.rm = TRUE)
max_year <- max(movie$year, na.rm = TRUE)

## genres
genres_col <- movie %>% select(starts_with("genres_"),- "genres_list") %>% colnames() 
genres_name <- gsub("^genres_", "", genres_col)

## country
country_col <- movie %>% select(starts_with("Countries_"),- "Countries_list") %>% colnames() 
country_name <- gsub("^Countries_", "", country_col)

## runtime
min_runtime <- min(movie$runtime, na.rm = TRUE)
max_runtime <- max(movie$runtime,na.rm = TRUE)
median_runtime <- median(movie$runtime,na.rm = TRUE)

## Languages
original_language <- unique(movie$original_language)


## plot type
plot_type_col = c("heatmap","line_graph","bar_chart","boxplot","scatter_chart", "top_10_table") # <-- <b>新增</b>
plot_type_name = c("heatmap","line graph","bar chart","boxplot","scatter chart", "Top 10 Table") # <-- <b>新增</b>
names(plot_type_col) <- plot_type_name

## budget
buget_min <- min(movie$budget,na.rm = TRUE)
buget_max <- max(movie$budget,na.rm = TRUE)

## Profit threshold
min_profit <- min(movie$revenue,na.rm = TRUE)
max_profit <- max(movie$revenue,na.rm = TRUE)

#UI
metric_choices <- c(
  "Profit (Profit)" = "profit",
  "Investment Return (ROI)" = "roi",
  "Total Revenue (Revenue)" = "revenue"
)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tabsetPanel(
    tabPanel("Discover",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year_d","Year Range",
                             min = min_year, max = max_year,
                             value = c(min_year, max_year)),
                 selectInput("genre_d", "Genre",
                             choices = setNames(genres_col, genres_name),
                             multiple = TRUE),
                 selectInput("country_d", "Country",
                             choices = setNames(country_col, country_name),
                             multiple = TRUE),
                 sliderInput("runtime_d", "Runtime (min)",
                             min = min_runtime, max = max_runtime,
                             value = c(min_runtime, max_runtime)),
                 selectInput("lang_d", "Original Language",
                             choices = original_language,
                             multiple = TRUE)
               ),
               mainPanel(
                 plotOutput("mainPlot"),
                 hr(),
                 tableOutput('subTable')
               )
             )),
    tabPanel("Invest",
             sidebarLayout(
               sidebarPanel(
                  selectInput("chart_i","Chart type",
                              choices = plot_type_col,
                              selected = "heatmap"),
                  selectInput("metric_i", "Select Metric to Analyze:",
                              choices = metric_choices,
                              selected = "profit"),
                  selectInput("genre_i", "Genre",
                              choices = setNames(genres_col, genres_name),
                              multiple = TRUE),
                  selectInput("country_i", "Country",
                              choices = setNames(country_col, country_name),
                              multiple = TRUE),
                  sliderInput("budget_i","Budget bands",
                              min = buget_min, max = buget_max,
                              value = c(buget_min, buget_max)),
                  sliderInput("runtime_i","Runtime(min)",
                              min = min_runtime, max = max_runtime,
                              value = c(min_runtime, max_runtime)),
                  sliderInput("year_i","Year Range",
                              min = min_year, max = max_year,
                              value = c(min_year, max_year)),
                  sliderInput("profit_i","Profit threshold",
                              min = min_profit, max = max_profit,
                              value = c(min_profit, max_profit))
                ),
              mainPanel(
                 h4("Investment Analysis Visualization"),
                 helpText("数据将根据侧边栏的筛选条件进行过滤。使用侧边栏的 'Chart type' 和 'Metric' 下拉菜单切换分析视图。"),
                 uiOutput("invest_dynamic_output")
             )))
  )
)

server <- function(input, output, session) {
  
  filter_movie_data <- function(df, year_range, genres, countries,
                                runtime_range, languages,
                                budget_range = NULL, profit_range = NULL) {
    df <- df %>% 
      filter(year >= as.numeric(year_range[1]),
           year <= as.numeric(year_range[2]))
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
    df <- df %>% filter(runtime >= runtime_range[1],
                        runtime <= runtime_range[2])
    if (length(languages) > 0) {
      df <- df %>% filter(original_language %in% languages)
    }
    if (!is.null(budget_range)) {
      df <- df %>% filter(budget >= budget_range[1],
                          budget <= budget_range[2])
    }
    if (!is.null(profit_range)) {
      df <- df %>% filter(revenue >= profit_range[1],
                          revenue <= profit_range[2])
    }
    df
  }
  
  filtered_discover <- reactive({
    filter_movie_data(
      df          = movie,
      year_range  = input$year_d,
      genres      = input$genre_d,
      countries   = input$country_d,
      runtime_range = input$runtime_d,
      languages   = input$lang_d
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
      select(title, budget,revenue) %>%
      pivot_longer(
        cols = c(budget, revenue),
        names_to = "metric",
        values_to = "value"
      )
    
    ggplot(df_top10_long, aes(x = reorder(title, value), y = value, fill = metric)) +
      geom_col(position = "dodge") +
      coord_flip() +
      labs(title = "Top 10 Movies with Multiple Metrics",
           x = "Movie Title", y = "Value") +
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
  # --- <b>新增: "Invest" 标签页的后端逻辑</b> ---
  
  # 1. "Invest" 的 reactive 筛选器 (这部分保持不变)
  filtered_invest <- reactive({
    req(movie)
    
    df_invest <- movie %>%
      filter(year >= input$year_i[1],
             year <= input$year_i[2],
             budget >= input$budget_i[1],
             budget <= input$budget_i[2],
             runtime >= input$runtime_i[1],
             runtime <= input$runtime_i[2],
             revenue >= input$profit_i[1],
             revenue <= input$profit_i[2])
    
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
        is_hit = ifelse(profit > 0, 1, 0) # (逻辑不变, 只是用了新列)
      )
    
    df_invest
  })
  
# <b>(你的 filtered_invest reactive 保持不变，它已经创建了 'roi' 和 'profit' 列)</b>

  # 2. <b>"Invest" 标签页的【单一】主图表输出 (已更新为完全动态)</b>
  output$invest_main_plot <- renderPlot({
    
    df_filtered_movies <- filtered_invest()
    if (nrow(df_filtered_movies) == 0) {
      plot.new()
      text(0.5, 0.5, "没有找到满足侧边栏筛选条件的电影。")
      return()
    }
    
    # <b>--- 1. 获取动态输入 ---</b>
    selected_genres <- input$genre_i
    plot_type <- input$chart_i
    
    # <b>(获取所选指标的值，例如 "roi")</b>
    metric_to_plot <- input$metric_i
    # <b>(获取所选指标的标签，例如 "Investment Return (ROI)")</b>
    metric_label <- names(metric_choices)[metric_choices == metric_to_plot]
    
    # (步骤 2: 动态准备绘图数据... 这部分逻辑保持不变)
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
        plot.new()
        text(0.5, 0.5, "数据重塑后无结果。")
        return()
    }

    # <b>步骤 3: 绘制图表 (已更新)</b>
    
    # <b>(为 Y 轴和图例动态设置格式)</b>
    dynamic_scale_y <- if (metric_to_plot == "roi") {
      scale_y_continuous(labels = scales::percent)
    } else {
      scale_y_log10(labels = scales::dollar) # Profit 和 Revenue 范围太大，用 log scale
    }
    
    dynamic_fill_scale <- if (metric_to_plot == "roi") {
      # ROI (有正有负)
      scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", labels = scales::percent)
    } else {
      # Profit / Revenue (通常为正)
      scale_fill_gradient(low = "lightblue", high = "darkblue", labels = scales::dollar)
    }
    
    
    switch(plot_type,
           "heatmap" = {
             # <b>(动态汇总所选指标)</b>
             summary_data <- plot_data %>%
               group_by(!!sym(x_axis_variable), budget_band) %>%
               summarise(median_metric = median(!!sym(metric_to_plot), na.rm = TRUE), .groups = 'drop')
             
             ggplot(summary_data, aes(x = !!sym(x_axis_variable), y = budget_band, fill = median_metric)) +
               geom_tile(color = "white") +
               dynamic_fill_scale + # <b>(使用动态图例)</b>
               labs(title = paste("Heatmap: Median", metric_label),
                    x = x_axis_label, y = "Budget Band", fill = paste("Median", metric_label)) +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           
           "bar_chart" = {
             # (Hit Rate 不受新指标影响，保持不变)
             summary_data <- plot_data %>%
               group_by(!!sym(x_axis_variable), budget_band) %>%
               summarise(hit_rate = mean(is_hit, na.rm = TRUE), .groups = 'drop')
             ggplot(summary_data, aes(x = !!sym(x_axis_variable), y = hit_rate, fill = budget_band)) +
               geom_col(position = "dodge") +
               scale_y_continuous(labels = scales::percent) +
               labs(title = "Bar Chart: Hit Rate (Revenue > Budget)",
                    x = x_axis_label, y = "Hit Rate (%)", fill = "Budget Band") +
               theme_minimal() +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           },
           
           "boxplot" = {
             # <b>(Y 轴变为动态)</b>
             ggplot(plot_data, aes(x = !!sym(x_axis_variable), y = !!sym(metric_to_plot), fill = !!sym(x_axis_variable))) +
               geom_boxplot() +
               dynamic_scale_y + # <b>(使用动态 Y 轴)</b>
               theme_minimal() +
               theme(legend.position = "none",
                     axis.text.x = element_text(angle = 45, hjust = 1)) +
               labs(title = paste("Boxplot:", metric_label, "Distribution"),
                    x = x_axis_label, y = metric_label)
           },
           
           "line_graph" = {
             # <b>(动态汇总所选指标)</b>
             line_data <- df_filtered_movies %>%
               group_by(year) %>%
               summarise(mean_metric = mean(!!sym(metric_to_plot), na.rm = TRUE), .groups = 'drop')
             
             ggplot(line_data, aes(x = year, y = mean_metric)) +
               geom_line(color = "steelblue") + geom_point(color = "steelblue") +
               dynamic_scale_y + # <b>(使用动态 Y 轴)</b>
               labs(title = paste("Line Graph: Average", metric_label, "Over Time"),
                    x = "Year", y = paste("Average", metric_label)) +
               theme_minimal()
           },
           
           "scatter_chart" = {
             # <b>(Y 轴变为动态)</b>
             ggplot(df_filtered_movies, aes(x = budget, y = !!sym(metric_to_plot))) +
               geom_point(alpha = 0.5, color = "blue") +
               scale_x_log10(labels = scales::dollar) + # X 轴始终是 budget
               dynamic_scale_y + # <b>(使用动态 Y 轴)</b>
               geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # (0 基准线)
               labs(title = paste("Scatter Plot: Budget vs.", metric_label),
                    x = "Budget (Log Scale)", y = metric_label) +
               theme_minimal()
           }
    ) # switch 结束
  }) # renderPlot 结束

# <b>↓ ↓ ↓ 粘贴下面所有代码到 server 中 ↓ ↓ ↓</b>
  
  # --- 3. 动态 UI 切换器 ---
  # (此函数将决定显示图表还是表格)
  output$invest_dynamic_output <- renderUI({
    
    # 检查用户选择的是图表还是表格
    if (input$chart_i == "top_10_table") {
      # <b>情况 A: 用户选择了 "Top 10 Table"</b>
      # 我们显示表格和文本
      tagList(
        h4("Top 10 Movies (Sorted by Selected Metric)"),
        tableOutput("invest_top_movies_table"), # <b>(我们将重新创建这个)</b>
        hr(),
        h5("Genre Details:"),
        verbatimTextOutput("invest_movie_genres_text") # <b>(这是一个纯文本输出)</b>
      )
    } else {
      # <b>情况 B: 用户选择了其他图表 (heatmap 等)</b>
      # 我们显示已有的图表
      plotOutput("invest_main_plot")
    }
  })
  
  
  # --- 4. 为表格和文本创建 Reactive 数据 ---
  # (这可以避免在表格和文本中重复计算)
  reactive_top_10_data <- reactive({
    
    # <b>(这部分逻辑与你之前的 DT 版本相同)</b>
    metric_to_sort <- input$metric_i
    
    filtered_invest() %>%
      arrange(desc(!!sym(metric_to_sort))) %>% # <b>(动态排序)</b>
      slice_head(n = 10) %>%
      select(
        title, 
        genres_list, # <b>(获取 genres_list)</b>
        profit,
        roi,
        revenue
      )
  })

  # --- 5. 重新创建 Top 10 表格 (使用 renderTable) ---
  output$invest_top_movies_table <- renderTable({
    
    table_data <- reactive_top_10_data() %>%
      select(-genres_list) # <b>(在最终表格中隐藏原始 genres_list)</b>
    
    # (格式化输出)
    table_data %>%
      mutate(
        Profit = scales::dollar(profit, accuracy = 1),
        Revenue = scales::dollar(revenue, accuracy = 1),
        ROI = scales::percent(roi, accuracy = 0.1)
      ) %>%
      # <b>(重命名列以使其更美观)</b>
      select(Title = title, Profit, ROI, Revenue)
      
  }, striped = TRUE)

  # --- 6. 创建新的 Genre 文本输出 ---
  output$invest_movie_genres_text <- renderText({
    
    text_data <- reactive_top_10_data()
    
    # <b>(格式化文本)</b>
    movie_genre_strings <- text_data %>%
      mutate(
        # <b>(清理 genres_list 字符串)</b>
        genres_clean = gsub("\\[|\\]|'", "", genres_list)
      ) %>%
      # <b>(创建 "Title: Genres" 格式的字符串)</b>
      rowwise() %>%
      mutate(
        display_text = paste0(title, ": ", genres_clean)
      ) %>%
      pull(display_text) # <b>(提取为向量)</b>
    
    # <b>(将所有电影的字符串用换行符 \n 连接起来)</b>
    paste(movie_genre_strings, collapse = "\n")
  })
}

shinyApp(ui = ui, server)