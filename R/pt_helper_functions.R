#'pt_options hold options passed to pivot table functions
#'
#'@param pt_filter character that is parsed in context of dt i
#'@param pt_row character vector - rows to group by
#'@param pt_col character vector - columns to pivot and group by
#'@param pt_metrics character that is parsed in context of dt j 
#'@param pt_sort character vecotr used to sort dt
#'@param pt_sort_order 1 decreasing (default) or -1 increasing. can be vector.
#'@param pt_subtotals to be implemented
#'@param pt_grand_totals to be implemented
#'@param pt_other to be implemented
#'@export
#'@import data.table
new_pt_options <- function(pt_filter = NA_character_, 
                           pt_row = NA_character_, 
                           pt_col = NA_character_, 
                           pt_metrics = NA_character_,
                           pt_sort = NA_character_,
                           pt_sort_order = 1,
                           pt_row_subtotals = FALSE, 
                           pt_col_subtotals = FALSE,
                           pt_grand_totals = NA_character_,
                           pt_other = NULL) {
  
  me <- list(pt_filter = pt_filter,
             pt_row = as.character(pt_row),
             pt_col = as.character(pt_col),
             pt_metrics = pt_metrics,
             pt_sort = pt_sort,
             pt_sort_order = pt_sort_order,
             pt_row_subtotals = as.logical(pt_row_subtotals),
             pt_col_subtotals = as.logical(pt_col_subtotals),
             pt_grand_totals = as.character(pt_grand_totals),
             pt_other = as.list(pt_other))
  
  class(me) <- c(class(me),"pt_options")
  
  return(me)
}

pt_calc <- function(x, pt_options) UseMethod("pt_calc")

pt_calc.default <- function(x, pt_options) {
  stop("x must be a data.table")
}

pt_calc.data.table <- function(x, pt_options) {
  
  result <- filter_dt(x, pt_options)
  
  result <- sort_dt(result, pt_options)
  
  result_group <- group_row_col_dt(result, pt_options)

  result_subtotal <- subtotal_dt(result, pt_options)
  
  result <- rbind(result_group, result_subtotal, use.names = TRUE)
  
  result <- pivot_dt(result, pt_options)

  result
}

subtotal_dt <- function(x, pt_options) {
  
  if(is_empty_data(x)) return(x)
  if(is_null_empty_na_blank(pt_options)) return(x)
  if(!is.logical(pt_options$pt_row_subtotals) & !is.logical(pt_options$pt_col_subtotals)) return(x)
  if(!pt_options$pt_row_subtotals & !pt_options$pt_col_subtotals) return(x)
  
  pt_col <- pt_options$pt_col
  pt_col <- pt_col[pt_col %in% names(x)]
  if(is_null_empty_na_blank(pt_col)) pt_col <- NULL
  
  pt_row <- pt_options$pt_row
  pt_row <- pt_row[pt_row %in% names(x)]
  if(is_null_empty_na_blank(pt_row)) pt_row <- NULL
  
  pt_row_col <- c(pt_col, pt_row)
  
  row_groups <-
    if(pt_options$pt_row_subtotals & !is_null_empty_na_blank(pt_row)) {
      lapply(seq(0, length(pt_row)), function(x) c(pt_col, pt_row[seq(0, x)]))
    }
  
  col_groups <-
    if(pt_options$pt_col_subtotals & !is_null_empty_na_blank(pt_col)) {
      lapply(seq(0, length(pt_col)), function(x) c(pt_row, pt_col[seq(0, x)]))
    }
  
  row_col_groups <- append(row_groups, col_groups)
  
  if(is_null_empty_na_blank(row_col_groups)) return(x)
  
  sub_group <- function(g) {
    current_data_group <- group_dt(x, pt_options, g)
    current_total_groups <- pt_row_col[!(pt_row_col %in% g)]
    if(length(current_total_groups) > 0) {
      for(i in 1:length(current_total_groups)) {
        current_data_group[[current_total_groups[i]]] <- paste0("total__", current_total_groups[1])
      }
      current_data_group
    } else {
      NULL
    }
  }
  
  result_list <- lapply(row_col_groups, sub_group)
  
  result_list <- purrr::compact(result_list)
  
  rbindlist(result_list, use.names = TRUE)
  
}


group_row_col_dt <- function(x, pt_options) {
  
  if(is_null_empty_na_blank(pt_options)) return(x)
  if(is_null_empty_na_blank(pt_options$pt_col) & is_null_empty_na_blank(pt_options$pt_row)) return(x)
  
  pt_group = c(pt_options$pt_row, pt_options$pt_col)

  result <- group_dt(x, pt_options, pt_group)
  
  result
  
}


group_dt <- function(x, pt_options, pt_group) {
  
  if(is_null_empty_na_blank(pt_group)) return(x)
  
  #identify group by variables
  pt_group = pt_group[!is.na(pt_group) & pt_group != ""]
  pt_group = pt_group[pt_group %in% names(x)]
  
  if(is_null_empty_na_blank(pt_group)) return(x)
  if(is_empty_data(x)) return(x)
  if(is_null_empty_na_blank(pt_options$pt_metrics)) return(x)
  
  pt_metrics <- if( substring( pt_options$pt_metrics, 1, 5) != "list(") {
    paste0("list(", pt_options$pt_metrics, ")")
  } else {
    pt_options$pt_metrics
  }
  
  pt_metrics_parse = parse(text = pt_metrics)
  
  result <- 
    tryCatch({
      x[,eval(pt_metrics_parse), by = pt_group]
    },
    error = function(cond) {
      message(paste("group and metrics have failed","col:" ,pt_options$pt_col, "row:", pt_options$pt_row, "metrics:", pt_options$pt_metrics))
      message(cond)
      return(x)
    })
    
  result
  
}


pivot_dt <- function(x, pt_options) {
  
  if(is_null_empty_na_blank(pt_options$pt_col)) return(x)
  if(is_empty_data(x)) return(x)

  #identify group by variables
  pt_group = c(pt_options$pt_row, pt_options$pt_col)
  pt_group = pt_group[!is.na(pt_group) & pt_group != ""]
  pt_group = pt_group[pt_group %in% names(x)]
  
  if(is_null_empty_na_blank(pt_group)) return(x)
  
  pt_col <- pt_options$pt_col[pt_options$pt_col %in% names(x)]
  if(is_null_empty_na_blank(pt_col)) return(x)
  
  pt_row <- pt_options$pt_row[pt_options$pt_row %in% names(x)]
  if(is_null_empty_na_blank(pt_row)) pt_row <- "."

  value.var <- setdiff(names(x),pt_group)

  formula <- as.formula(paste(paste(pt_row, collapse = " + "), 
                              paste(pt_options$pt_col, collapse = " + "),
                              sep = " ~ "))
  
  result <- dcast.data.table(x, formula, value.var = value.var, sep = "__")
  
  result
  
}


group_pivot_dt <- function(x, pt_options) {
  
  result <- group_row_col_dt(x, pt_options)
  
  result <- pivot_dt(result, pt_options)
  
  result
}


filter_dt <- function(x, pt_options) {
  
  if(is_null_empty_na_blank(pt_options$pt_filter)) return(x)
  
  pt_filter_parse = parse(text = pt_options$pt_filter)
  
  result <- 
      tryCatch({
        x[eval(pt_filter_parse)]
      },
      error = function(cond) {
        message(paste("filter has failed",pt_options$pt_filter))
        message(cond)
        return(x)
      })
  
  result
}
    

sort_dt <- function(x, pt_options) {
  
  if(is_null_empty_na_blank(pt_options$pt_sort)) return(x)
  if(is_empty_data(x)) return(x)
  
  sort_var <- pt_options$pt_sort[!is.na(pt_options$pt_sort) & pt_options$pt_sort != ""]
  sort_var <- sort_var[sort_var %in% names(x)]
  
  if(is_null_empty_na_blank(sort_var)) return(x)
  
  result <- setorderv(x, sort_var, order = pt_options$pt_sort_order)

  result
}


