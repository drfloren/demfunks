#' @title Categorical Descriptives Individual Base Function
#' 
#' @description
#' This function generates a short summary of descriptive statistics for binary vector (representing data from a single category)
#' 
#' @param x The binary vector (0's and 1's) with data on the categorical variable to be used in calculating the descriptives.
#' @param dig How many digits should values round to? Defaults to 1 (e.g., 99.1%).
#' @param na_in_denom Should NA's be included in the denominator when calculating the percentage? Note that NA's will not be included in the sum. Default is FALSE.
#' @param which_in_paren Which should be in parentheses? Arguments are "n" and "p". Defaults to "p"
#' @param include_perc_sign Should the percent sign be included? Defaults to FALSE.
#' @param ... Additional arguments for `table` (e.g., `exclude=NULL` to include NA's)
#' 
#' @details
#' This function differs from `catf` in that `catf` expects a categorical variable, while this expects a binary one (or one that can be treated as binary). Note that this function will *not* work with factors or string vectors, while `catf` *can* work with binary variables (simply producing an undesired entry for *both* 0 and 1).
#' 
#' Also note that the default behavior is to *not* include NA's in the denominator when calculating percentages. I think this is the right answer for most applications (and this matches the behavior of `catf`), but the option is available to change as needed.
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
#' @export 

ind_catf <- function(x, dig=1, na_in_denom=FALSE, which_in_paren="p", include_perc_sign=TRUE, ...) {
  num = sum(x, na.rm=na.rm)
  
  if(!na_in_denom){
    den =length(x[!is.na(x)])
  } else {
    den =length(x)
  }
  
  if(include_perc_sign){
    perc_sign_print <- "%"
  } else {
    perc_sign_print <- ""
  }
  
  paste0(num, " (", myStuff::fr((num/den)*100, dig), perc_sign_print, ")")
  
  if(which_in_paren=="p"){
    out_str <- paste0(num, " (", myStuff::fr((num/den)*100, dig), perc_sign_print, ")")
  } else if (which_in_paren=="n") {
    out_str <- paste0(myStuff::fr((num/den)*100, dig), perc_sign_print, " (", num, ")")
  } else {
    warning("'which_in_paren' not valid. Must be 'p' or 'n'. Printing value with which_in_paren='p'.")
    out_str <- paste0(num, " (", myStuff::fr((num/den)*100, dig), perc_sign_print, ")")
  }
  
  out_str
}


