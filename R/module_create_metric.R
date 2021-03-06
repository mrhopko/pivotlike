create_metricOutput <- function(id, col_names) {
  
  ns <- shiny::NS(id)
  
  return(
    shiny::modalDialog(
      shinyDND::dragSetUI(id = ns("col_names"), textval = col_names, dragdata = col_names),
      shiny::textInput(ns("text_metric_name"), "Metric Name"),
      shinyAce::aceEditor(ns("ace_metric_code"), mode = "r"),
      size = "m",
      footer = shiny::tagList(
        shiny::modalButton("cancel"),
        shiny::actionButton(ns("button_save"), "", icon = icon("floppy-o"))
      ),
      easyClose = TRUE
    )
  )
  
}

create_metric <- function(input, output, session) {

  metric <- shiny::reactiveValues()
    
  shiny::observeEvent(input$button_save, {
    metric$m_text <- input$ace_metric_code
    metric$m_name <- input$text_metric_name
    shiny::removeModal()
  })
  
  return(metric)
  
}