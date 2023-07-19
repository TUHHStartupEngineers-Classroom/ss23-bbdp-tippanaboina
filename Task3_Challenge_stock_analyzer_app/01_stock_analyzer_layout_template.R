# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(zoo)
library(plotly)
library(tidyverse)

library(rvest)
library(glue)

source(file = "stock_analysis_functions.R")


# UI -----

# 1.0 HEADER ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 0.0 Test ----

  
  div(
    h1("Stock Analzer"),
    p("Tippanaboina data analysis challenge 3"), 
    
    
    
    # 2.0 APPLICATION UI -----    
    
    #First column
    column(width = 4, "Selection",
           wellPanel(
             
             pickerInput(
               inputId = "index_picker",
               label = "Select an index:",
               choices = c("SP500", "DOW", "NASDAQ", "DAX"),
               selected = NULL,
               
               
             ),
             # Second pickerInput (dynamic)
             uiOutput("stock_picker"),
             
             
             
             
             br(),
             #Button for frist column
             actionButton(inputId = "analyze", label = "Analyze", icon = icon("download")),
             
             # For Testing observeEvent()
             verbatimTextOutput("eventOutput"),
             
             
             #Moving average sliders
             hr(),
             
             sliderInput(
               inputId = "short_ma_slider",
               label = "Short Moving Average",
               min = 5, max = 40, value = 20
             ),
             sliderInput(
               inputId = "long_ma_slider",
               label = "Long Moving Average",
               min = 50, max = 120, value = 50
             ),
             
             dateRangeInput(inputId = "date_range", label = "Select Date Range:",
                            start = today() - days(180), end = today())
             
           )
           
           
    ),
    
    
    #2nd column
    column(width = 8, "Plot",
           # For stock name
           verbatimTextOutput("selected_symbol_output"),
           # For stock data table
           div(
             style = "height: 200px; overflow: scroll;",
             verbatimTextOutput("tableOutput"),
           ),
           
           
           div(
             h4("Stock Analysis"),
             div(id = "stock_plot",
                 plotlyOutput(outputId = "plotly_plot")
             )
             
           )
    )
  ),
  
  # 3.0 ANALYST COMMENTARY ----
  fluidRow(
    column(width = 12,
           div(
             h4("Analyst Commentary"),
             textOutput("analyst_commentary"),
           )
    )
  )
)



# SERVER ----
server <- function(input, output, session) {
  
  # Test using observeEvent(), printing to console
  observeEvent(input$analyze, {
    selected_stock_name <- input$stock_selection
    print(selected_stock_name)
  })
  
  observeEvent(input$analyze, {
    output$eventOutput <- renderText({
      "Testing observeEvent Triggered"
    })
  })
  
  #Plotting-----------------------
  # Store the stock data
  stock_data <- reactive({
    stock_symbol <- selected_symbol()
    stock_symbol() %>% get_symbol_from_user_input() %>% get_stock_data()
  })
  
  # Render the time series plot
  output$time_series_plot <- renderPlotly({
    plot_stock_data(stock_data())
  })
  
  # Analyst commentary -----------------------------
  # Render the analyst commentary
  output$analyst_commentary <- renderText({
    generate_commentary(stock_data(), user_input = selected_symbol())
  })
  
  # Reactive symbol extraction
  selected_symbol <- eventReactive(input$analyze, {
    input$stock_selection
  })
  
  # Render the selected symbol
  output$selected_symbol_text <- renderText({
    paste("Selected symbol:", selected_symbol())
  })
  
  
  # Stock Symbol name ------------------------
  stock_symbol <- eventReactive(input$analyze, ignoreNULL = FALSE, {
    input$stock_selection
  })
  
  output$selected_symbol_output <- renderText({stock_symbol()
  })
  
  
  # extract / get stock data / Also slider input------------------------------------
  output$tableOutput <- renderPrint({
    if (input$analyze > 0) {
      stock_symbol() %>% get_symbol_from_user_input() %>%
        get_stock_data(#from = today() - days(180), 
          #to   = today(),
          from = input$date_range[1], 
          to   = input$date_range[2],
          #mavg_short = 20,
          #mavg_long  = 50)
          mavg_short = input$short_ma_slider,
          mavg_long  = input$long_ma_slider)
    }
  })
  
  # Plotting / Also slider input ----------------------------------------------
  
  
  output$plotly_plot <- renderPlotly({
    #short_ma <- input$short_ma_slider
    #long_ma <- input$long_ma_slider
    stock_data_tbl <- stock_symbol() %>% get_symbol_from_user_input() %>%
      get_stock_data(#from = today() - days(180), 
        #to   = today(),
        from = input$date_range[1], 
        to   = input$date_range[2],
        #mavg_short = 20,
        #mavg_long  = 50)
        mavg_short = input$short_ma_slider,
        mavg_long  = input$long_ma_slider)
    
    print(input$short_ma_slider)
    print(input$long_ma_slider)
    print(input$date_range[1])
    print(input$date_range[2])
    
    plot_stock_data(stock_data_tbl)#, input$short_ma_slider, input$long_ma_slider)
  })
  
  
  # Create stock list ----------------------------------------------------
  output$stock_picker <- renderUI({
    selected_index <- input$index_picker
    
    if (!is.null(selected_index)) {
      
      
      # Create the second pickerInput
      pickerInput(
        inputId = "stock_selection",
        label = "Select a stock:",
        choices = get_stock_list(selected_index)
      )
    }
  })
  
  
  
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)