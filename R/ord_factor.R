#' @title Order Factor
#' 
#' @description Somewhat depreciated in place of alternate function options from the `forcats` package. Orders factor levels based on count or alphabetical order
#' 
#' @param x The factor vector to be reordered
#' @param by The method by which to reorder. Currently "count" and "alphabetical" are supported
#' @param decreasing The order to put factors in. NULL by default, with separate defaults for count (TRUE) and alphabetical (FALSE).
#' @param na.rm Should `NA`'s be removed?
#' @param ... Additional arguments passed to factor.
#' 
#' @details Orders factor levels based on count or alphabetical order
#' 
#' @examples
#' #Note that while entries in each of the variables are the same, the order of the levels has changed
#' ord_factor(ToothGrowth$supp, decreasing = FALSE, by="alphabetical")
#' ord_factor(ToothGrowth$supp, decreasing = TRUE, by="alphabetical")
#' 
#' #this is most apparent when looking at tables of these (the order of presentation is different)
#' table(ord_factor(ToothGrowth$supp, decreasing = FALSE, by="alphabetical"))
#' table(ord_factor(ToothGrowth$supp, decreasing = TRUE, by="alphabetical"))
#' 
#' @author Michael Floren
#' 
#' @seealso [factor()], and for other packages consider [forcats::fct_infreq()] from `forcats` (a good additional function to have when working with these). 
#' 
#' @export









ord_factor <- function(x, by="count", decreasing=NULL, na.rm=TRUE, ...){ 
  init_factor_var <- factor(x, ...)
  
  #set default decreasing (yes for count, no for alphabetical)
  
  if(na.rm){
    temp_tab <- table(init_factor_var)
  } else{
    temp_tab <- table(init_factor_var, exclude = NULL)
  }
  
  if(by == "count"){
    if(is.null(decreasing)) decreasing <- TRUE #set default decreasing for count, if not specified
    final_factor <- factor(init_factor_var, levels=names(sort(temp_tab, decreasing=decreasing)))
    
  } else if (by == "alphabetical"){
    if(is.null(decreasing)) decreasing <- FALSE #set default decreasing for alpha, if not specified
    final_factor <- factor(init_factor_var, levels=names(temp_tab)[order(names(temp_tab), decreasing=decreasing)])
  }
  
  final_factor
}
