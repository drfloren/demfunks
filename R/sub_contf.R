#' @title Continuous Descriptives By Subgroup
#' 
#' @description
#' This function calculates continuous descriptives of a variable broken out by subgroups
#' 
#' @param x The vector with data on the continuous variable to be used in calculating the descriptives.
#' @param subgroup The vector listing out the categorical variable to use in making subgroups
#' @param names Would you like the results to be named?
#' @param n An argument to pass to `contf`: would you like the sample size to be included? Included as an argument so that there isn't confusion with the names argument (no doubt due to my sloppy programming)
#' @param header_name A header name to be included (for inclusion in certain strutures of demographic tables).
#' @param ... Additional arguments to pass to `contf`
#' 
#' @details
#' This is essentially an `aggregate` wrapper for `contf`.
#'
#' @examples
#' sub_contf(ChickWeight$weight, ChickWeight$Diet)
#' sub_contf(ChickWeight$weight, ChickWeight$Diet, names=FALSE)
#' 
#' @author Michael Floren
#' 
#' @seealso [aggregate()], [contf()]
#' 
#' @export
#' 


sub_contf <- function (x, subgroup, names=TRUE, n=TRUE, header_name=NA, ...){
  raw_ag <- aggregate(x~subgroup, FUN=function(vals) contf(vals, n=n, ...))
  out <- raw_ag[,2]
  if(names)
    names(out) <- raw_ag[,1]
  if(!is.na(header_name)){
    out <- rbind(out)
    rownames(out)[1] <- header_name
  }
  out
}