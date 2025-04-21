library(shiny)
library(shinythemes)
library(tidyquant)
library(gt)
library(tidyverse)
library(scales)
library(plotly)
library(ggrepel)
library(RColorBrewer)
library(glue)

# Used for the company table in introduction page
source("ticker_info.R")  

## ------------------------------ UI --------------------------------------- ##
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("US Tech Stocks since COVID-19"),
  tags$h5("Amy Hwang"),
  
  sidebarLayout(
    sidebarPanel(selectizeInput("selected_tickers", "Choose 1 to 10 tickers:",
                                choices = tickers,
                                selected = c("AAPL", "ADBE", "NVDA"),
                                multiple = TRUE,
                                options = list(maxItems = 10,
                                               plugins = list("remove_button"))),
                 dateRangeInput("daterange", "Date Range:",
                                start = "2020-03-01",
                                end = Sys.Date(),
                                min = "2020-03-01",
                                max = Sys.Date()),
                 checkboxInput("facet_toggle", "Facet? (Only applicable for 2️⃣)", value = FALSE)),
    
    
    mainPanel(
      tabsetPanel(
        ## Intro page
        tabPanel("📘 Project Overview",
                 tagList(
                   h4("Tech Stocks since COVID-19"),
                   plotOutput("introPlot"),
                   p("This dashboard explores the post-COVID performance of major
                     U.S. technology companies using daily stock price data from 2020 to today's date."),
                   p("The analysis focuses on trends in return and volatility,
                     company-level risk profiles, and key performance metrics."),
                   p(HTML("The dataset consists of the top 20 tech stocks by market cap, selected from 
                          <a href='https://finance.yahoo.com/research-hub/screener/sec-ind_sec-largest-equities_technology/' target='_blank'>Yahoo Finance</a>.")),
                   br(),
                   h5("Companies Analyzed:"),
                   gt_output("tickerTable"),
                   br(),
                   p("Use the tabs above to explore each visualization and gain insights into how tech companies have navigated the post-COVID market."))),
        
        ## Graph 1
        tabPanel("1️⃣ Return vs. Volatility",
                 h4("Scatterplot of Cumulative Return vs. Volatility"),
                 plotlyOutput("scatterPlot"),
                 
                 p("This scatterplot compares each stock’s cumulative return
                 against its volatility over the selected date range.",
                   strong("Hover over the circle points to see the details.")),
                 p("Stocks that appear farther to the right have higher daily variability, 
                   while those higher on the plot have larger price gains.
                   The size of each circle reflects the", strong("magnitude of the return.")),
                 p("This plot can easily highlight some high-growth stocks, such as NVIDIA and Palantir, which were also the most volatile.
                   On the other hand, companies like Microsoft or SAP offered steadier returns with lower risk.")),
                 
        ## Graph 2
        tabPanel("2️⃣ Cumulative Return Over Time",
                 h4("Time Series line chart of Cumulative Return"),
                 plotOutput("linePlot"),
                 
                 p("This time series plot displays the normalized cumulative return of each selected stock since the start date.
                   It shows how each company grew (or shrank) in value."),
                 p(strong("The option to facet the graph by ticker"), "gives a clearer view of each company's performance independently,
                   scaled to its own y-axis.
                   This is especially useful when some stocks (like NVIDIA) dramatically outperform others, because that
                   will compress smaller-scale lines when plotted together."),
                 p("Overall, this graph is helpful to explore timing differences in rebounds, volatility waves, and general trends.")),
        
        ## Graph 3
        tabPanel("3️⃣ Radar Chart",
                 h4("Radar Chart of Risk & Return using R graph fundamentals"),
                 plotOutput("radarPlot"),
                 p("This radar chart visualizes four key performance metrics for each selected company:"),
                 tags$ul(
                   tags$li("Total Return"),
                   tags$li("Volatility (standard deviation of daily returns)"),
                   tags$li("Drawdown (worst daily return)"),
                   tags$li("Recovery (best daily return)")
                 ),
                 p("Each axis is normalized with the 0-1 scale to compare across different metrics.
                   The polygons shows which companies are well-balanced and which may be high-risk, high-reward.")),
        
        ## Graph 4
        tabPanel("4️⃣ Performance Summary Table",
                 h4("Formatted Performance Table"),
                 gt_output("summaryTable"),
                 p("This table summarizes each selected company’s performance in a clean tabular format. It includes:"),
                 tags$ul(
                   tags$li("Start and End prices"),
                   tags$li("Total Return"),
                   tags$li("Volatility"),
                   tags$li("Worst (Drawdown) and Best (Recovery) daily returns")
                   ),
                 p("The table also includes company logos and uses color gradients for quick visual insights of outperformers or risky profiles.")),
    
        
        ## Conclusion
        tabPanel("✅ Conclusion",
                 h4("Main Takeaways"),
                 p("This dashboard provides a comprehensive overview of how the U.S. technology sector changed in the aftermath of the COVID-19 market shock, 
                   using data from March 2020 through today's date. The selected companies are some of the largest and most influential tech firms, which allows 
                   for a meaningful comparison of performance patterns during a period using volatility, recovery etc."),
                 p("Key insights include:"),
                 tags$ul(
                   tags$li("High performers like NVIDIA and Palantir saw >800% growth, but carried much higher volatility."),
                   tags$li("Steady players like Microsoft and Oracle delivered consistent gains with less risk, often favored by long-term investors."),
                   tags$li("Radar profiles reveal companies that shine across all metrics, while others show risk asymmetries."),
                   tags$li("Table summaries validate which stocks combined strong growth with favorable risk metrics.")
                 ),
                 p("Overall, this dashboard shows how statistical visualization plays a big role in understanding financial trends. 
                 It highlights that investment decisions need evaluating multiple facets of performance.
                 Although all the companies analyzed belong to the U.S. tech sector, their post-COVID trend 
                 have varied dramatically. While some experienced exponential growth, others experienced more mild gains or even losses. 
                 The radar charts, scatterplots, and time series lines highlights these differences."),
                 p("In conclusion, it has clearly be seen that", strong("the post-COVID era has reshaped tech stock dynamics.")))
      )
    )
  )
)

## ---------------------------- SERVER ------------------------------------- ##
server <- function(input, output) {
  ## Data setup: stock data for selected tickers within the selected data range
  stock_data <- reactive({
    req(input$selected_tickers)
    tq_get(input$selected_tickers, 
           from = input$daterange[1],
           to   = input$daterange[2]) %>%
      group_by(symbol)
  })
  
  # Using the separate function, process the stock data to a desired format
  source("processed_data.R")
  summary_df <- reactive({ processed_data(stock_data()) })
  #############################################################################
  
  ## Option 1: Scatter plot: Return vs. Volatility ############################
  output$scatterPlot <- renderPlotly({
    df <- summary_df() 
    
    p <- ggplot(df, aes(
      x = Volatility,
      y = Return,
      label = symbol,
      color = Volatility,
      size = abs(Return),
      text = paste0("Symbol: ", symbol, "<br>",
                    "Return: ", round(Return * 100, 2), "%<br>",
                    "Volatility: ", round(Volatility * 100, 2), "%")
    )) +
      geom_point(alpha = 0.8) + # scatter plot
      scale_color_gradient(low = "skyblue", high = "forestgreen") + # color by volatility
      scale_x_continuous(labels = percent) +
      scale_y_continuous(labels = percent) +
      labs(
        title = "Volatility vs. Cumulative Return",
        x = "Volatility",
        y = "Cumulative Return",
        color = "Volatility") +
      theme_minimal()
    
    # Use plotly for more interaction
    ggplotly(p, tooltip = "text")
  })

  #############################################################################
  
  ## Option 3: time series line plot ##########################################
  # process data with timeseries option
  timeseries_df <- reactive({ processed_data(stock_data(), 
                                             include_timeseries = TRUE) })
  
  output$linePlot <- renderPlot({
    df <- timeseries_df()
    
    # if facet true, draw ggplot independently
    if (input$facet_toggle) {
      ggplot(df, aes(x = date, y = cum_return, color = symbol)) +
        geom_line() +
        scale_color_brewer(palette = "Set2") +
        facet_wrap(~ symbol, ncol = 1, scales = "free_y") + # use dynamic y-scaling
        scale_y_continuous(labels = percent) +
        labs(
          title = "Cumulative Return by Ticker",
          x = NULL,
          y = "Cumulative Return") +
        theme_minimal()
    } else {
      # if facet false, draw all companies in one graph
      ggplot(df, aes(x = date, y = cum_return, color = symbol)) +
        geom_line(linewidth = 1) +
        scale_y_continuous(labels = percent) +
        labs(
          title = "Cumulative Return of Selected Stocks",
          x = NULL,
          y = "Cumulative Return") +
        theme_minimal()
    }
  })
  #############################################################################

  ## Option 4: spider chart using graph fundamentals ##########################
  output$radarPlot <- renderPlot({
    # get relevant performance metrics
    radar_data <- summary_df() %>%
      filter(symbol %in% input$selected_tickers) %>%
      select(symbol, Return, Volatility, Drawdown, Recovery) %>%
      column_to_rownames("symbol") %>%
      as.matrix()
    
    if (nrow(radar_data) < 1) return()
    
    # normalize each metric to [0, 1] scale
    scaled <- radar_data %>% 
      as.data.frame() %>%
      mutate(across(everything(), scales::rescale))
    
    # prepare variables for radar chart structure
    n_vars <- ncol(scaled)
    n_companies <- nrow(scaled)
    angles <- seq(0, 2 * pi, length.out = n_vars + 1)
    
    # set colors for each company
    colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))(n_companies)
    
    # square plot area
    par(mar = c(1, 1, 3, 1), xpd = TRUE)
    plot(0, 0, type = "n", xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
         xlab = "", ylab = "", axes = FALSE, asp = 1)
    
    # draw center cricles + percent labels
    for (r in seq(0.2, 1, by = 0.2)) {
      x_circle <- r * cos(angles)
      y_circle <- r * sin(angles)
      lines(x_circle, y_circle, col = "grey85")
      
      # Place label at top
      text(0, r, labels = paste0(r * 100, "%"),
           cex = 0.7, col = "grey10", pos = 3, offset = 0.2)}
    
    # metric axes and labels
    for (i in 1:n_vars) {
      x_end <- cos(angles[i])
      y_end <- sin(angles[i])
      lines(c(0, x_end), c(0, y_end), col = "grey60")
      text(1.2 * x_end, 1.1 * y_end, labels = colnames(scaled)[i], cex = 0.9)}
    
    # radar polygons
    for (i in 1:n_companies) {
      values <- scaled[i, ]
      x_vals <- values * cos(angles[1:n_vars])
      y_vals <- values * sin(angles[1:n_vars])
      polygon(x_vals, y_vals, border = colors[i], 
              col = adjustcolor(colors[i], alpha.f = 0.3), lwd = 2)
      points(x_vals, y_vals, pch = 19, col = colors[i])
    }

    # legend at bottom
    legend("bottom", legend = rownames(scaled), 
           col = colors, pch = 19, horiz = TRUE, bty = "n")
    
    title("Radar Chart (Base R)")
  })
  #############################################################################

  ## Option 5: gt table #######################################################
  output$summaryTable <- render_gt({
    # data prep: format metrics, join logo/company info
    df <- summary_df() %>%
      mutate(
        Start = round(Start, 2),
        End = round(End, 2),
        Return = round(Return * 100, 2),
        Volatility = round(Volatility * 100, 2),
        Drawdown = round(Drawdown * 100, 2),
        Recovery = round(Recovery * 100, 2),
        Label = factor(ifelse(Return >= 0, "Positive", "Negative"),
                       levels = c("Negative", "Positive"))) %>%
      left_join(ticker_info, by = c("symbol" = "ticker")) %>%
      select(logo, symbol, Start, End, Return, Volatility, Drawdown, Recovery, Label)
    
    gt(df) %>%
      # Combine logo + ticker in one cell
      text_transform(
        locations = cells_body(symbol),
        fn = function(x) {
          mapply(function(ticker, logo) {
            glue(
              "<div style='display: flex; align-items: center;'>
               <img src='{logo}' style='height: 25px; margin-right: 8px;'>
               <span>{ticker}</span>
             </div>")}, x, df$logo, SIMPLIFY = FALSE)}) %>%
      
      # Formatting
      # numeric columns to 2 decimals
      fmt_number(columns = c(Start, End), decimals = 2) %>%
      # add % sign to percentage values
      fmt_percent(columns = c(Return, Volatility, Drawdown, Recovery), scale_values = FALSE) %>%

      data_color(columns = Return,
                 colors = col_numeric(c("darkred", "orange", "lightgreen", "forestgreen"), NULL)) %>%
      data_color(columns = Label,
                 colors = col_factor(palette = c("darkred", "forestgreen"),
                                     domain = c("Negative", "Positive"))) %>%
      
      # grouping using spanner
      tab_spanner("Prices", c(Start, End)) %>%
      tab_spanner("Performance Metrics", c(Return, Volatility, Drawdown, Recovery)) %>%
      
      cols_label(
        symbol = "Ticker",
        Start = "Start Price", End = "End Price",
        Return = "Total Return", Volatility = "Volatility",
        Drawdown = "Worst Daily Return", Recovery = "Best Daily Return",
        Label = "Return") %>%
      tab_header(title = "Stock Performance Table") %>%
      cols_hide(logo)
  })
  #############################################################################
  
  ##### Introduction plot (just for the visual) ###############################
  output$introPlot <- renderPlot({
    df <- tq_get("AAPL", from = "2015-01-01", to = Sys.Date())
    
    df %>%
      ggplot(aes(x = date, y = close)) +
      geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
      labs(title = "Example Graph:Apple Stock Price 2015~",
           y = "Closing Price", x = "Years") +
      theme_tq()
  })
  #############################################################################
  
  ##### Ticker table (just for the visual) ####################################
  output$tickerTable <- render_gt({
    ticker_info %>%
      gt() %>%
      text_transform(
        locations = cells_body(columns = logo),
        fn = function(x) {web_image(url = x)}) %>%
      cols_label(logo = "",
                 ticker = "Ticker",
                 company = "Name") %>%
      tab_source_note("Logos sourced from Clearbit")
    })
  #############################################################################
}

## load the app
shinyApp(ui, server)