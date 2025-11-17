# 正确的做法
library(shiny)
# (注意：所有其他 library() 调用应在 global.R 中)

# 1. 加载模块化的 UI (在外部)
source("R/ui_discover.R", local = TRUE)
source("R/ui_invest.R", local = TRUE)

# 2. 定义主 UI
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  tabsetPanel(
    ui_discover,
    ui_invest
  )
)

# 3. 定义主 Server
server <- function(input, output, session) {
  source("R/server_discover.R", local = TRUE)
  source("R/server_invest.R", local = TRUE)
}

# 5. 运行 App
shinyApp(ui, server)