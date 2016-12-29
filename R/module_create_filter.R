create_filterOutput <- function(id, filter_text, col_names) {
  
  ns <- NS(id)
  
  return(
    shiny::modalDialog(
      shinyDND::dragSetUI(id = ns("col_names"), textval = col_names, dragdata = col_names),
      shinyAce::aceEditor(ns("ace_filter_code"),
                         mode = "r",
                         value = filter_text,
                         autoComplete = "enabled",
                         autoCompleteList = list(pt = col_names)),
      size = "m",
      footer = shiny::tagList(
        shiny::modalButton("cancel"),
        shiny::actionButton(ns("button_save"), "", icon = icon("floppy-o"))
      ),
      easyClose = TRUE
    )
  )
}

create_filter <- function(input, output ,session) {
  
  f <- shiny::reactiveValues()
  
  shiny::observeEvent(input$button_save, {
    f$f_text <- input$ace_filter_code
    shiny::removeModal()
  })
  
  return(f)
  
}