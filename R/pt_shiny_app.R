library(shiny)
library(DT)
library(shinyDND)
library(miniUI)
library(data.table)
source("R/module_create_metric.R")
source("R/module_create_filter.R")
source("R/module_format_table.R")
source("R/testmodule.R")
source("R/pt_helper_functions.R")

dat <- mtcars


ui <- shiny::shinyUI(shiny::fluidPage(
  shiny::includeCSS("R/styles.css"),
      tags$hr(),
      fluidRow(
        column(2,
               tags$div("Options", class = "divfieldheader")),
        column(10,
               miniUI::miniButtonBlock(
                 shiny::actionButton("button_create_filter", "Filter Data"),
                 shiny::actionButton("button_create_metric", "New Metric"),
                 shiny::actionButton("button_row_subtotal", "Row Subtotals"),
                 shiny::actionButton("button_col_subtotal", "Col Subtotals"),
                 shiny::actionButton("button_col_filter", "Col Filters"),
                 shiny::actionButton("button_format", "Format")
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
      shiny::textOutput("test_sub"),
      shiny::textOutput("test_format"),
      DT::dataTableOutput("dt_pivot_data", height = "600px")
    )
)


server <- shiny::shinyServer(function(input, output, session) {
  
  # ------------------------------------------
  # UI Menu ----------------------------------
  # ------------------------------------------
  
  updateSelectInput(session, "select_col", label = NULL, choices = names(dat))
  updateSelectInput(session, "select_row", label = NULL, choices = names(dat))
  
  vals <- reactiveValues(col_sub_val = FALSE, 
                         row_sub_val = FALSE,
                         col_filter_val = FALSE)
  
  # CREATE METRIC ----------------------------
  new_metric <- reactive(callModule(create_metric, "create_metric"))
  
  default_metric_list <- 
    default_metrics(col_names = names(dat), col_types = unlist(lapply(dat, class)))
    
  metric_list <- reactiveValues(m_name = default_metric_list$m_name, m_text = default_metric_list$m_text)
  
  observe({
    metric_list$m_name <- c(isolate(metric_list$m_name), new_metric()$m_name)
    metric_list$m_text <- c(isolate(metric_list$m_text), new_metric()$m_text)
    updateSelectInput(session, "input_metrics", label = "metrics", choices = isolate(metric_list$m_name), selected = isolate(input$input_metrics))
  })
  
  shiny::observeEvent(input$button_create_metric, {
    showModal(create_metricOutput("create_metric",
                                  col_names = names(dat)))
  })
  
  
  # CREATE FILTER
  new_filter <- reactive(callModule(create_filter, "create_filter"))
  
  observeEvent(input$button_col_filter, {
    vals$col_filter_val <- !(isolate(vals$col_filter_val))
  })

  shiny::observeEvent(input$button_create_filter, {
    showModal(create_filterOutput("create_filter",
                                  filter_text = isolate(new_filter()$f_text), 
                                  col_names = names(dat)))
  })
  
  
  # EDIT FORMATS
  updated_pt_format <- reactive(callModule(format_table, "format_table"))
  
  pt_format <- reactive({
    current <- updated_pt_format()
    new_pt_format(pt_class = current$pt_class,
                  pt_style = current$pt_style)
  })
  
  shiny::observeEvent(input$button_format, {
    showModal(format_tableOutput("format_table",
                                 pt_format = pt_format()))
  })
  
  
  # Subtotals
  
  observeEvent(input$button_row_subtotal, {
    vals$row_sub_val <- !(isolate(vals$row_sub_val))
  })
  
  observeEvent(input$button_col_subtotal, {
    vals$col_sub_val <- !(isolate(vals$col_sub_val))
  })
  

  #---------------------------------------------------
  # Pivot Table Data ---------------------------------
  #---------------------------------------------------
  
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
  
  
  data_set <- reactive({
    as.data.table(dat)
  })
  
  pt_data <- reactive({
    pt_data <- pt_calc.data.table(data_set(), pt_options())
  })

  
  #---------------------------------------------------
  # Pivot Table UI ---------------------------------
  #---------------------------------------------------
  
  
  output$dt_pivot_data <- DT::renderDataTable({
    pt_f <- pt_format()
    pt_class <- paste0(pt_f$pt_class, collapse = " ")
    
    DT::datatable(pt_data(), 
                  rownames = FALSE, 
                  filter = ifelse(vals$col_filter_val, "top", "none"), 
                  fillContainer = TRUE, 
                  extensions = c("ColReorder", "Buttons", "FixedColumns", "FixedHeader"), 
                  class = pt_class,
                  style = pt_f$pt_style,
                  options = list(
                    sDom  = '<"top">Blrt<"bottom">ip', 
                    bLengthChange = FALSE,
                    pageLength = 25,
                    fixedHeader = TRUE,
                    colReorder = TRUE,
                    dom = 'Bfrtip', 
                    fixedColumns = TRUE,
                    buttons = list('colvis', 'copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                    ))
                  ))
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
  
  output$test_format <- renderText({
    paste0("table_style ", pt_format()$pt_class)
  })
  
})

shinyApp(ui, server)