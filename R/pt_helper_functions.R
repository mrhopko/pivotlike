#'pt_options hold options passed to pivot table functions
#'
#'@param pt_filter character that is parsed in context of dt i
#'@param pt_row character vector - rows to group by
#'@param pt_col character vector - columns to pivot and group by
#'@param pt_metrics character that is parsed in context of dt j 
#'@param pt_sort character vecotr used to sort dt
#'@param pt_sort_order 1 decreasing (default) or -1 increasing. can be vector.
#'@param pt_row_subtotals logical - should row subtotals be included?
#'@param pt_col_subtotals logical - should column subtotals be included?
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

#'New pivot table format list that controls pt UI formatting
#' 
#'@param pt_class character passed to datatable class
#'@parma pt_style character passed to datatable style
#'@returns pt_format list
#'@export
new_pt_format <- function(pt_class = c("row-border","stripe"), pt_style = "default") {
  
  pt_class <- 
    if(is_null_empty_na_blank(pt_class)) {
      c("row-border", "stripe")
    } else {
      pt_class
    }
    
  pt_style <- 
    if(is_null_empty_na_blank(pt_style)) {
      "default"
    } else {
      pt_style
    }
  
  me <- list(pt_class = pt_class,
             pt_style = pt_style)
  
  class(me) <- c(class(me), "pt_format")
  
  return(me)
  
}




#'Calculate pivottable data given pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table altered according to pt_options
#'@export
pt_calc <- function(x, pt_options) UseMethod("pt_calc")


#'Calculate pivottable data given pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table altered according to pt_options
#'@export
pt_calc.default <- function(x, pt_options) {
  stop("x must be a data.table")
}


#'Calculate pivottable data given pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table altered according to pt_options
#'@export
pt_calc.data.table <- function(x, pt_options) {
  
  result <- filter_dt(x, pt_options)
  
  result <- sort_dt(result, pt_options)

  result_group <- group_row_col_dt(result, pt_options)
  
  if(pt_options$pt_row_subtotals | pt_options$pt_col_subtotals) {
    result_subtotal <- subtotal_dt(result, pt_options)
    result <- rbind(result_group, result_subtotal, use.names = TRUE)  
  } else {
    result <- result_group
  }

  result <- pivot_dt(result, pt_options)

  result
}


#'add subtotals to a dt using pt_options rows/cols/subtotals
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table containing subtotals based on row/cols in pt_options
#'@export
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

#'group a data.table according to rows/columns of pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table containing subtotals based on row/cols in pt_options
#'@export
group_row_col_dt <- function(x, pt_options) {
  
  if(is_null_empty_na_blank(pt_options)) return(x)
  if(is_null_empty_na_blank(pt_options$pt_col) & is_null_empty_na_blank(pt_options$pt_row)) return(x)
  
  pt_group = c(pt_options$pt_row, pt_options$pt_col)

  result <- group_dt(x, pt_options, pt_group)
  
  result
  
}

#'group a data.table according to rows/columns of pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@param pt_group vector of field names of x to be used in by statement of data.table
#'@return data.table containing grouped data
#'@export
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

#'pivot a data.table according to rows/columns of pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table containing pivoted data
#'@export
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

#'group then pivot dt according to row/cols of pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table containing grouped/pivoted data
#'@export
group_pivot_dt <- function(x, pt_options) {
  
  result <- group_row_col_dt(x, pt_options)
  
  result <- pivot_dt(result, pt_options)
  
  result
}


#'filter dt according filter in pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table containing grouped/pivoted data
#'@export
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
    

#'sort dt according sort in pt_options
#'
#'@param x data.table containing data
#'@param pt_options pt_options list
#'@return data.table containing grouped/pivoted data
#'@export
sort_dt <- function(x, pt_options) {
  
  if(is_null_empty_na_blank(pt_options$pt_sort)) return(x)
  if(is_empty_data(x)) return(x)
  
  sort_var <- pt_options$pt_sort[!is.na(pt_options$pt_sort) & pt_options$pt_sort != ""]
  sort_var <- sort_var[sort_var %in% names(x)]
  
  if(is_null_empty_na_blank(sort_var)) return(x)
  
  result <- setorderv(x, sort_var, order = pt_options$pt_sort_order)

  result
}


#'convert name and text to pt_metrics
#'
#'@param m_name character metric name(s)
#'@param m_text character metric text
#'@return character string to slot into pt_options$pt_metrics
text_to_pt_metrics <- function(m_name, m_text) {
  paste0(
    "list(",
    paste0(unlist(Map(function(x,y) paste0(x, "=", y), x = m_name, y = m_text)), collapse = ","),
    ")")
}


default_metrics <- function(col_names, col_types) {
  
  numeric_cols <- col_names[col_types %in% c("numeric", "integer")]
  
  sum_metrics <- 
    list(m_name = paste0("sum_", numeric_cols),
         m_text = paste0("sum(", numeric_cols, ", na.rm = TRUE)"))
  
  count_metrics <- 
    list(m_name = paste0("count_", col_names),
         m_text = rep(".N", length(col_names)))
  
  mean_metrics <- 
    list(m_name = paste0("mean_", numeric_cols),
         m_text = paste0("mean(", numeric_cols, ", na.rm = TRUE)"))
  
  list(
    m_name = c(sum_metrics$m_name, count_metrics$m_name, mean_metrics$m_name),
    m_text = c(sum_metrics$m_text, count_metrics$m_text, mean_metrics$m_text)
  )
}
