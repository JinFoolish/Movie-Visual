启动网页 在 `R console` 运行 `shiny::runApp()`  

如果想添加一个新的标签页（例如 "Compare"），你只需：

1. 创建 R/ui_compare.R 和 R/server_compare.R。
2. 在 app.R 中添加两行 source()。
3. 将 ui_compare 变量添加到 tabsetPanel 中