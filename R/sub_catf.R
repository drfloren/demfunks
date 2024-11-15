#' @title Categorical Descriptives By Subgroup
#' 
#' @description
#' This function calculates categorical descriptives of a variable broken out by subgroups
#' 
#' @param x The vector with data on the categorical variable to be used in calculating the descriptives.
#' @param subgroup The vector listing out the categorical variable to use in making subgroups. Defaults to `NULL`, which will simply use a single group
#' @param dig How many digits should values round to? Defaults to 1 (e.g., 99.1%).
#' @param names Would you like the results to be named?
#' @param out_df Should the output be formatted as a data.frame? Default is TRUE.
#' @param margin Passed to `prop.table`: which margin should the percentages be calculated for?
#' @param header_name A header name to be included (for inclusion in certain strutures of demographic tables)
#' @param ... Additional arguments to pass to `catf`
#' 
#' @details
#' This is essentially an `aggregate` wrapper for `catf`.
#'
#' @examples
#' sub_catf(ChickWeight$Time, ChickWeight$Diet)
#' sub_catf(ChickWeight$Time, ChickWeight$Diet, margin=2)
#' 
#' @author Michael Floren
#' 
#' @seealso [aggregate()]
#' @family categorical data functions
#' 
#' @export
#' 


sub_catf <- function (x, subgroup=NULL, dig=1, names=TRUE, out_df=TRUE, margin=2, header_name=NA, ...){
  if(length(subgroup)==1 | is.null(subgroup)){
    warning("Subgroups should be the same length as x. Moving forward with no subgroups.")
    subgroup <- rep(1, length(x))
  }
  
  in_tab <- table(x, subgroup)
  col_table_names <- colnames(in_tab)
  row_table_names <- rownames(in_tab)
  
  formatted_vals <- paste0(in_tab, " (", myStuff::fr(prop.table(in_tab, margin=margin)*100, dig), "%", ")") 
  out <- matrix(formatted_vals, ncol=length(col_table_names))
  if(names){
    colnames(out) <- col_table_names
    rownames(out) <- row_table_names
  }
  if(!is.na(header_name)){
    out <- rbind(c(NA),
                 out)
    rownames(out)[1] <- header_name
  }
  if(out_df)
    out <- data.frame(out)
  out
}