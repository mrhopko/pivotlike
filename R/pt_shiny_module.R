library(dtplyr)
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(shiny)
library(miniUI)
library(formattable)
library(DT)
library(shinyjs)

source('R/pt_helper_functions.R')

pivot_table_moduleOutput <- function(id) {
  
  ns <- NS(id)
  
  fluidRow(
    fluidRow(
      actionButton(ns("controls"), "Controls")
    ),
    conditionalPanel(condition = paste0("input.[",ns("controls"),"]%2==0"),
      fluidRow(
        column(width = 2,
               uiOutput(ns("rows_o"))
               ),
        column(width = 2,
               uiOutput(ns("cols_o"))
               ),
        column(width = 2,
               uiOutput(ns("sort_o"))
               ),
        column(width = 2,
               textInput(ns("filter", label = "filter"))
               ),
        column(width = 2,
               textInput(ns("metric", label = "metric"))
               )
      )
    ),
    fluidRow(
      column(width = 12,
             DT::dataTableOutput(ns("dataTable"), width = "100%", height = 800)
             )
    )
  )

}

pivot_table_module <- function(input, output, session, rf_data_table) {
  
  ns <- session$ns
  
  data_names <- reactive({
    names(rf_data_table())
  })
  
  output$rows_o <- renderUI({
    selectizeInput(ns("rows"), choices = data_names(), multiple = TRUE)
  })
  
  output$cols_o <- renderUI({
    selectizeInput(ns("cols"), choices = data_names(), multiple = TRUE)
  })
  
  output$sort_o <- renderUI({
    selectizeInput(ns("sort"), choices = data_names(), multiple = TRUE)
  })
  
  pt_selection <- reactive({
    new_pt_options(
      pt_filter = input$filter,
      pt_sort = input$sort,
      pt_metric = input$metric,
      pt_row = input$rows,
      pt_col = input$cols
    )
  })
  
  pt_data <- reactive({
    pt_calc(rf_data_table(), pt_selection())
  })
  
  output$pt <- DT::renderDataTable({
    
    d <- pt_data()
    names(d) <- gsub("__","<br>",names(d))
    
    datatable(d, rownames = FALSE, filter = "top", escape = FALSE, options = list(
      scrollY = TRUE, scrollX = TRUE, searching = TRUE
    ))
  })
  
  return(pt_data)

}
