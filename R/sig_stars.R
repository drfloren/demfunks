#' @title Significance Stars
#' 
#' @description Based on p-values, output a specific number of stars
#' 
#' @param p The p-value to calculate stars for
#' @param breaks The intervals that you're interested in doing stars for (defaults to 0, 0.001, 0.01, 0.05, 0.10, 1)
#' @param labels The labels for each of the interval spaces
#' @param ... Other info passed to cut
#' 
#' @details Uses cut to correctly specify p-values. Can handle multiple entries.
#' 
#' @examples
#' sig_stars(.023)
#' sig_starts(.0023)
#' 
#' @author Michael Floren
#' 
#' @export

sig_stars <- function(p, breaks=NULL, labels=NULL, ...) {
  if(is.null(breaks))
    breaks = c(0, 0.001, 0.01, 0.05, 0.10, 1)
  if(is.null(labels))
    labels = c("***","**","*","'", "")
  
  cut(p, breaks = breaks, labels = labels, ...)
}