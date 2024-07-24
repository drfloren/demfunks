#' @title XTabs for Categorical Data
#' 
#' @description This function is comparable to xtabs, but where xtabs only functions for numeric data, this functions for categorical data. 
#' 
#' @param voi Character. The variable of interest (e.g., the one that contains the summary statistics)
#' @param dat The dataset, formatted with 3 columns: two contain information that will become row and column names, and the third contains infromation that will go into the table. For the two that will become row and column names, their combination should be unique in each row.
#' 
#' @details This function is handy for placing information in a matrix. The starting dataset should have 3 columns: a column with the row labels, a column with the column labels, and a column with the entries in each cell of the matrix. For the input dataset, there should be only a single row entry for the combination of column entries that will be row and column (as this is purely for formatting, there will be no row aggregation).
#' 
#' @examples
#' xtabs_cat("len", aggregate(len~ supp + dose, data=ToothGrowth, contf))
#' 
#' @author Michael Floren
#' 
#' @seealso [xtabs()]
#' 
#' @export


xtabs_cat <- function(voi=NULL, dat){
  if(is.null(voi))
    voi <- names(dat)[ncol(dat)]
  
  dnv <- dat[,-which(names(dat)==voi)] #dat no voi
  
  rows <- unique(dnv[,1])
  cols <- unique(dnv[,2])
  
  out <- data.frame(matrix(ncol=length(cols), nrow=length(rows)))
  colnames(out) <- cols
  rownames(out) <- rows
  
  for(i in 1:length(rows)){
    for(j in 1:length(cols)){
      out[i,j] <- dat[(dnv[,1] == rows[i]) & (dnv[,2] == cols[j]), voi]
      # out[as.character(rows[i]),as.character(cols[j])] <- 
    }
  }
  out
}