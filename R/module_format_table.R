format_tableOutput <- function(id, col_names, pt_format = list()) {
  
  ns <- shiny::NS(id)
  
  pt_class <- if(is_null_empty_na_blank(pt_format$pt_class)) {
    c("row-border","stripe")
  } else {
    pt_format$pt_class
  }
  
  pt_style <- if(is_null_empty_na_blank(pt_format$pt_style)) {
    "default"
  } else {
    pt_format$pt_style
  }
  
  return(
    shiny::modalDialog(
      shiny::selectInput(ns("select_class"), "Table Class", multiple = TRUE, selected = pt_class,
                         choices = c("cell-border", "compact", "hover", "order-column","row-border", "stripe")),
      shiny::selectInput(ns("select_style"), "Table Style", multiple = FALSE, selected = pt_style,
                         choices = c("default", "bootstrap", "bootstrap4", "foundation", "jqueryui", "material", "semanticui", "uikit")),
      size = "m",
      footer = shiny::tagList(
        shiny::modalButton("cancel"),
        shiny::actionButton(ns("button_save"), "", icon = icon("floppy-o"))
      ),
      easyClose = TRUE
    )
  )
  
}

format_table <- function(input, output, session) {
  
  pt_format <- shiny::reactiveValues()
  
  shiny::observeEvent(input$button_save, {
    pt_format$pt_class <- input$select_class
    pt_format$pt_style <- input$select_style
    shiny::removeModal()
  })
  
  return(pt_format)
  
}