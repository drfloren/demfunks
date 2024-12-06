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
  
  out <- cut(p, breaks = breaks, labels = labels, ...)
  
  if(any(p==0)){
    out[which(p==0)] <- labels[1]
    warning("There is a p-value that is zero. Assuming it was just very small and marking it with the highest significance label.")
  }
  
  if(any(p>=1)){
    
    out[which(p>=1)] <- labels[length(labels)]
    warning("There is a p-value that is 1 or more. Assuming it was just very large and marking it nonsignificant.")
  }
  
  out
}