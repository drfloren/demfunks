#' @title Catgorical Descriptives as Row
#' 
#' @description
#' This function is a wrapper for `catf` that presents the categorical entries as rows (with a potential header). This is useful when creating categorical tables.
#' 
#' @param x The vector with categorical data
#' @param header_name Character. The name to include at the top of the listing.
#' @param ... Other arguments to pass to `catf`.
#' 
#' @details
#' This function was created to ease the inclusion of `catf` results into report table formats. 
#' 
#' @examples
#' library(flextable)
#' library(dplyr)
#' 
#' temp <- rbind(cat_row(ChickWeight$Diet, header_name = "Diets"),
#'               cat_row(ChickWeight$Time, header_name = "Times"))
#'               
#' cat_tab <- data.frame(Characteristics=rownames(temp), "Values"=temp)
#' 
#' rows_to_indent <- which(!is.na(temp))
#' 
#' flextable(cat_tab) %>%
#'   set_header_labels(Values="Values") %>%
#'   align(j=1, align="left", part="all") %>%
#'   align(j=2, align="center", part="all") %>% 
#'   italic(i=1, part="head") %>%
#'   padding(i=rows_to_indent, j=1, padding.left = 25) %>%
#'   font(fontname="Times New Roman", part="all") %>%
#'   fontsize(size=12, part="all") %>%
#'   autofit()
#' 
#' @author Michael Floren
#' 
#' @seealso [catf()]
#' 
#' @export
#' 



cat_row <- function(x, header_name = NA, ...){
  out <- cbind(catf(x, ...))
  if(!is.na(header_name)){
    out <- rbind(c(NA),
                 out)
    rownames(out)[1] <- header_name
  }
  out
}