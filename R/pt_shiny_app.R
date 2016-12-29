library(shiny)
library(DT)
library(shinyDND)
library(miniUI)
source("R/module_create_metric.R")
source("R/module_create_filter.R")
source("R/testmodule.R")
source("R/pt_helper_functions.R")

dat <- mtcars


ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::includeCSS("R/styles.css"),
  # shiny::sidebarLayout(
  #   shiny::sidebarPanel(
  #     shiny::selectInput("input_metrics", "Metric", multiple = TRUE, choices = ""),
  #     shiny::selectInput("select_order", "Order", multiple = TRUE, choices = "")
  #   ),
  #   shiny::mainPanel(
  #     # tags$hr(),  
  #     # fluidRow(
  #     #   column(2, 
  #     #          tags$div("Fields", class = "divfieldheader")
  #     #          ),
  #     #   column(10,
  #     #          tags$div(shiny::uiOutput("col_names"),
  #     #                   id = "drop_fields", class = "dropelement")
  #     #          )
  #     # ),
      tags$hr(),
      fluidRow(
        column(2,
               tags$div("Options", class = "divfieldheader")),
        column(10,
               miniUI::miniButtonBlock(
                 shiny::actionButton("button_create_filter", "New Filter"),
                 shiny::actionButton("button_create_metric", "New Metric"),
                 shiny::actionButton("button_row_subtotal", "Row Subtotals"),
                 shiny::actionButton("button_col_subtotal", "Col Subtotals")
               )
               )
      ),
      fluidRow(
        column(2, 
               tags$div("Column", class = "divfieldheader")
        ),
        column(10,
               shiny::selectizeInput("select_col", NULL, multiple = TRUE, choices = "", width = '100%', options = list(plugins = list('remove_button')))
      )),
      fluidRow(
        column(2, 
               tags$div("Row", class = "divfieldheader")
        ),
        column(10,
               shiny::selectizeInput("select_row", NULL, multiple = TRUE, choices = "", width = '100%', options = list(plugins = list('remove_button')))
        )
      ),
      fluidRow(
        column(2, 
               tags$div("Metrics", class = "divfieldheader")
        ),
        column(10,
               shiny::selectInput("input_metrics", NULL, multiple = TRUE, choices = "", width = '100%')
        )
      ),  
      tags$hr(),
      shiny::textOutput("test_metric_name"),
      shiny::textOutput("test_metric_text"),
      shiny::textOutput("test_filter_text"),
      shiny::textOutput("test_row"),
      shiny::textOutput("test_col"),
      shiny::textOutput("test_sub")
    )
)


server <- shiny::shinyServer(function(input, output, session) {
  
  #current new metric created by create_metric module
  new_metric <- reactive(callModule(create_metric, "create_metric"))
  
  #filter
  new_filter <- reactive(callModule(create_filter, "create_filter"))
  
  #complete list of metrics with metric text
  metric_list <- reactiveValues(m_name = c("sum1", "sum2"), m_text = c("sum(col1)","sum(col2)"))
  
  vals <- reactiveValues(col_sub_val = FALSE, row_sub_val = FALSE)
  
  observeEvent(input$button_row_subtotal, {
    vals$row_sub_val <- !(isolate(vals$row_sub_val))
  })
  
  observeEvent(input$button_col_subtotal, {
    vals$col_sub_val <- !(isolate(vals$col_sub_val))
  })
  
  
  
  pt_options <- reactive({
    
    m_name <- metric_list$m_name[metric_list$m_name %in% input$input_metrics] 
    m_text <- metric_list$m_text[metric_list$m_name %in% input$input_metrics]
      
    new_pt_options(
      pt_row = input$select_row,
      pt_row_subtotals = vals$row_sub_val,
      pt_col = input$select_col,
      pt_col_subtotals = vals$col_sub_val,
      pt_metrics = text_to_pt_metrics(m_name, m_text),
      pt_filter = new_filter()$f_text
    )
  })
  
  updateSelectInput(session, "select_col", label = NULL, choices = names(dat))
  updateSelectInput(session, "select_row", label = NULL, choices = names(dat))
  
  observe({
    metric_list$m_name <- c(isolate(metric_list$m_name), new_metric()$m_name)
    metric_list$m_text <- c(isolate(metric_list$m_text), new_metric()$m_text)
    updateSelectInput(session, "input_metrics", label = "metrics", choices = isolate(metric_list$m_name), selected = isolate(input$input_metrics))
  })
  
  shiny::observeEvent(input$button_create_metric, {
    showModal(create_metricOutput("create_metric",
                                  col_names = names(dat)))
  })
  
  shiny::observeEvent(input$button_create_filter, {
    showModal(create_filterOutput("create_filter",
                                  filter_text = isolate(new_filter()$f_text), 
                                  col_names = names(dat)))
  })
  
  output$test_metric_name <- renderText({
    paste0("metric name: ", paste0(metric_list$m_name, collapse = ", "))
  })
  
  output$test_metric_text <- renderText({
    paste0("metric text: ", pt_options()$pt_metrics)
  })
  
  output$test_filter_text <- renderText({
    paste0("filter text: ", pt_options()$pt_filter)
  })
  
  output$test_row <- renderText({
      paste0("row text: ", paste0(pt_options()$pt_row, collapse = ", "))
  })
  
  
  output$test_col <- renderText({
    paste0("col text: ", paste0(pt_options()$pt_col, collapse = ", "))
  })
  
  output$test_sub <- renderText({
    paste0("row subtotal: ", pt_options()$pt_row_subtotal, " | col subtotal: ", pt_options()$pt_col_subtotal)
  })
  
})

shinyApp(ui, server)