#' @title Categorical Descriptives Base Function
#' 
#' @description
#' This function generates a short summary of descriptive statistics for a vector with data from a categorical variable
#' 
#' @param x The vector with data on the categorical variable to be used in calculating the descriptives.
#' @param dig How many digits should values round to? Defaults to 1 (e.g., 99.1%).
#' @param which_in_paren Which should be in parentheses? Arguments are "n" and "p". Defaults to "p"
#' @param include_perc_sign Should the percent sign be included? Defaults to FALSE.
#' @param ... Additional arguments for `table` (e.g., `exclude=NULL` to include NA's)
#' 
#' @details
#' This really doesn't do much, essentially just adding percentages to a basic table command
#' 
#' @examples
#' catf(ChickWeight$Diet)
#' catf(ChickWeight$Diet, which_in_paren = "n")
#' 
#' @author Michael Floren
#' 
#' @seealso [table()]
#' @family categorical data functions
#' 
#' 
#' @export

catf <- function(x, dig=1, which_in_paren="p", include_perc_sign=TRUE, ...) {
  out <- table(x, ...)
  table_names <- names(out)
  
  if(include_perc_sign){
    perc_sign_print <- "%"
  } else {
    perc_sign_print <- ""
  }
  
  if(which_in_paren=="p"){
    out_str <- paste0(out, " (", myStuff::fr(prop.table(out)*100, dig), perc_sign_print, ")")
  } else if (which_in_paren=="n") {
    out_str <- paste0(myStuff::fr(prop.table(out)*100, dig), perc_sign_print, " (", out, ")")
  } else {
    warning("'which_in_paren' not valid. Must be 'p' or 'n'. Printing value with which_in_paren='p'.")
    out_str <- paste0(out, " (", myStuff::fr(prop.table(out)*100, dig), perc_sign_print, ")")
  }
  
  names(out_str) <- table_names
  out_str
}