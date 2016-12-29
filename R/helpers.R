is_null_empty_na_blank <- function(x) {
  if(is.null(x)) return(TRUE)
  if(length(x) == 0) return(TRUE)
  if(all(is.na(x))) return(TRUE)
  if(all(as.character(x) == "")) return(TRUE)
  FALSE
}

is_empty_data <- function(x) {
  if(is.null(x)) return(TRUE)
  if(nrow(x) <= 0) return(TRUE)
  FALSE
}

