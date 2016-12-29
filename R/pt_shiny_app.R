library(shiny)
library(DT)
library(shinyDND)
source("R/module_create_metric.R")
source("R/pt_helper_functions.R")

dat <- mtcars


ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::actionButton("button_create_metric", "New Metric"),
      shiny::selectInput("input_metrics", "Metric", multiple = TRUE, choices = "")
    ),
    shiny::mainPanel(
      shiny::textOutput("test_metric_name"),
      shiny::textOutput("test_metric_text")
    )
  )
))


server <- shiny::shinyServer(function(input, output, session) {
  
  #current new metric created by create_metric module
  new_metric <- reactive(callModule(create_metric, "create_metric"))
  
  #complete list of possible metric choices - used for select input
#  r_val <- reactiveValues(metric_choices = c("sum1", "sum2"))
  
  #complete list of metrics with metric text
  metric_list <- reactiveValues(m_name = c("sum1", "sum2"), m_text = c("sum(col1)","sum(col2)"))
  
  pt_options <- reactive({
    
    m_name <- metric_list$m_name[metric_list$m_name %in% input$input_metrics] 
    m_text <- metric_list$m_text[metric_list$m_name %in% input$input_metrics]
      
    new_pt_options(
      pt_metrics = text_to_pt_metrics(m_name, m_text)
    )
  })
  
#  observe({
#    r_val$metric_choices <- c(isolate(r_val$metric_choices), new_metric()$m_name)
#    updateSelectInput(session, "input_metrics", label = "metrics", choices = isolate(r_val$metric_choices), selected = isolate(input$input_metrics))
#  })
  
  observe({
    metric_list$m_name <- c(isolate(metric_list$m_name), new_metric()$m_name)
    metric_list$m_text <- c(isolate(metric_list$m_text), new_metric()$m_text)
    updateSelectInput(session, "input_metrics", label = "metrics", choices = isolate(metric_list$m_name), selected = isolate(input$input_metrics))
  })
  
  shiny::observeEvent(input$button_create_metric, {
    showModal(create_metricOutput("create_metric"))
  })
  
  output$test_metric_name <- renderText({
    paste0("metric name: ", paste0(metric_list$m_name, collapse = ", "))
  })
  
  output$test_metric_text <- renderText({
    paste0("metric text: ", pt_options()$pt_metrics)
  })  
  
})

shinyApp(ui, server)