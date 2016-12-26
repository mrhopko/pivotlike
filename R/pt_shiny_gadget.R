library(dtplyr)
library(dplyr)
library(data.table)
library(purrr)
library(shiny)
library(miniUI)
library(DT)

source('R/pt_helper_functions.R')

pivot_table_gadget <- function(x) {
  
  ui <- miniPage(
    
    gadgetTitleBar("Pivot Table", left = miniTitleBarButton("controls","controls", primary = FALSE), right = miniTitleBarButton("done","Done", primary = TRUE)),
    
    shinyjs::useShinyjs(),
    
    miniContentPanel(
      conditionalPanel(
        condition = "input.controls%2==0",
        fillRow(
          selectizeInput("rows", label = "rows", choices = names(x), multiple = TRUE),
          selectizeInput("cols", label = "cols", choices = names(x), multiple = TRUE),
          selectizeInput("sort", label = "sort", choices = names(x), multiple = TRUE),
          textInput("filter", label = "filter"),
          textInput("metric", label = "metric")
        )
      ), 
      
      div(
        conditionalPanel(
          condition = "input.controls%2==0",
          br(),
          br(),
          br(),
          br()
        ),
      
        DT::dataTableOutput("pt", width = "100%", height = "100%")
      )
    )


  
  )

  server <- function(input, output, session) {
    
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
      pt_calc(x, pt_selection())
    })
    
    
    output$pt <- DT::renderDataTable({
      
      d <- pt_data()
      names(d) <- gsub("__","<br>",names(d))
      
      datatable(d, rownames = FALSE, filter = "none", escape = FALSE, options = list(
        scrollY = TRUE, scrollX = TRUE, searching = FALSE, lengthChange = FALSE
      ))
    })
    
    observeEvent(input$done, {
      stopApp(pt_data())
    })
    
  }
  
  runGadget(ui, server,viewer = dialogViewer("Pivot Table", width = 1000, height = 600))
}


