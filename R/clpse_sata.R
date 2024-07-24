#' @title Collapse Select All That Apply
#' 
#' @description
#' This function is used to collapse select all that apply questions into a single column (with options for how to address when multiple options are selected). Useful when some demographic information (e.g., race) is listed as a select-all-that-apply, but is analyzed as a single categorical variable.
#' 
#' @param x A data.frame including the select-all-that-apply group to be condensed
#' @param multiple_text What text should be used when multiple categories have been selected? Default is "Multiple".
#' @param mark_sceme Currently not used. What marking scheme should be used to assess which entries are "selected" or "marked"?
#' @param none_text What text should be used when none are selected? Defaults to `NA`.
#' 
#' @details
#' For each entry, assessments are made on each row if multiple selections have been made, or if no selections have been made. Current implementation checks vs NA to see (i.e., assumes any text is a "selection", and all unselected entries are marked "NA" (*not* 0, or something else)). For rows with multiple, use the multiple text. For rows with none, marks as `NA` (default) or use `none_text`. For rows with a single entry, use the column name of the row that has that entry (i.e., column names should be pretty).
#' 
#' @examples
#' set.seed(1)
#' df <- data.frame(white = sample(c(NA,1), 100, replace=TRUE),
#'                  black = sample(c(NA,1), 100, replace=TRUE))
#' clpse_sata(df)
#' 
#' @author Michael Floren
#' 
#' @export
#' 

clpse_sata <- function(x, multiple_text="Multiple", mark_scheme="anyvsNA", none_text=NA){ #collapsing select all that apply into a single row
  # if(mark_scheme == "anyvsNA"){ #would need to included selected with this, too.
    tot_sel <- apply(x, 1, function(t) sum(!is.na(t)))
  # }
  
  out <- character(length=nrow(x))
  out[tot_sel>1] <- multiple_text
  out[tot_sel==0] <- none_text
  for(i in which(tot_sel==1)){
    out[i] <- colnames(x)[which(!is.na(x[i,]))]
  }
  
  out
}
