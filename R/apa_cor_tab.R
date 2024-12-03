#' @title APA Correlation Table
#' 
#' @description This is a general function for building typical tables that I see in reports. Similar to `apa.cor.table` from the `apaTables` package.
#' 
#' @param dat The data.frame of continuous variables you'd like to make a table for
#' @param dig How many digits should values round to? Defaults to 2
#' @param stars Logical. Should significance stars be included? Defaults to TRUE
#' @param msd Logical. Should the mean and standard deviation be included? Defaults to TRUE.
#' @param minmax Logical. Should the minimum and maximum be included? Defaults to FALSE.
#' 
#' @details Calculates a base correlation matrix, then adds leading columns as requested. Rounding is conducted via `myStuff::fr` and stars are calculated via `demfunks::sig_stars`.
#' 
#' @examples
#' apa_cor_table(iris[,-5])
#' 
#' @author Michael Floren
#' 
#' @seealso [apaTables::apa.cor.table()]
#' 
#' @export

apa_cor_table <- function(dat, dig=2, stars=TRUE, msd=TRUE, minmax=FALSE){
  vars <- colnames(dat)
  
  # Making the base correlation table
  base_cor_tab <- cor(dat, use = "complete.obs")
  out_cor_tab <- myStuff::fr(base_cor_tab, dig = dig)
  out_cor_tab[upper.tri(out_cor_tab)] <- NA
  diag(out_cor_tab) <- "-"
  
  # Adding significance stars, if requested, using the sig_stars function from demfunks
  if(stars){
    for (j in 1:(ncol(dat)-1)){
      for (i in (j+1):ncol(dat)){
        temp_cor_test <- cor.test(dat[,i], dat[,j])
        temp_stars <- demfunks::sig_stars(temp_cor_test$p.value)
        out_cor_tab[i,j] <- paste0(out_cor_tab[i,j],
                                   temp_stars)
      }
    }
  }
  
  # Fixing the column names to be nicer
  colnames(out_cor_tab) <- paste0("(", 1:ncol(out_cor_tab), ")")
  rownames(out_cor_tab) <- paste0("(", 1:ncol(out_cor_tab), ") ", rownames(out_cor_tab))
  
  # Starting to make the final table
  fin_tab <- out_cor_tab #start with the correlation table
  
  # Adding minimum and maximum, if requested
  if(minmax){
    dat_min <- apply(dat, 2, function(x) min(x, na.rm = TRUE))
    dat_max <- apply(dat, 2, function(x) max(x, na.rm = TRUE))
    fin_tab <- cbind("Minimum"=myStuff::fr(dat_min, dig = dig), "Maximum"=myStuff::fr(dat_max, dig = dig), fin_tab)
  }
  
  # Adding mean and SD, if requested
  if (msd){
    dat_mean <- apply(dat, 2, function(x) mean(x, na.rm = TRUE))
    dat_sd <- apply(dat, 2, function(x) sd(x, na.rm = TRUE))
    fin_tab <- cbind("Mean"=myStuff::fr(dat_mean, dig = dig), "Standard Deviation"=myStuff::fr(dat_sd, dig = dig), fin_tab)
  }
  
  # if rownames changed due to one of the above functions, change them back to how they were in the correlation table.
  rownames(fin_tab) <- rownames(out_cor_tab) #keep old rownames
  
  fin_tab
}
