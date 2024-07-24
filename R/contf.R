#' @title Continuous Descriptives Base Function
#' 
#' @description
#' This function generates a short summary of descriptive statistics for a single numeric vector.Additional arguments are allowed to specify desired measure of central tendancy, desired additional information (e.g., SD, quartiles), and whether or not sample size is reported.
#' 
#' 
#' @param x The numeric vector to be used in calculating the descriptives.
#' @param na.rm Should missing values be removed? Defaults to TRUE
#' @param dig How many digits should values round to? Defaults to 2
#' @param morm Which measure of central tendency would you like? Options are "mean" or "median"
#' @param supp Which supplementary statistic(s) would yu like to be provided? Options are "none", "match", "sd", "ci", "range", and "quartiles". "match" is default, and will provide "sd" for mean and "quartiles" for median.
#' @param alpha What alpha level would you like to set for the CI? Defaults to 0.05 (only used for confidence intervals)
#' @param n Should sample size be provided? Defaults to TRUE.
#' @param quart_vals Numeric vector of length 2, passed to `quantile` when quartiles are requested. What proportions should be used when calculating quartiles? Defaults to c(0.25, 0.75).
#' @param quart_type What type of quantiles should be used in the quartile calculation (passed to `quantile`).
#' 
#' @details
#' Calculations use R standard functions. Quantiles is used for the calculation of quartiles.
#' 
#' 
#' @examples
#' contf(ChickWeight$weight)
#' 
#' contf(ChickWeight$weight, dig=3, supp="ci", alpha=0.10)
#' 
#' contf(ChickWeight$weight, dig=0, morm="median", quart_vals = c(0.05, 0.95), quart_type=5, n=FALSE)
#'
#' @author Michael Floren
#' 
#' @seealso Base calculation functions of [mean()], [median()], [sd()], and [quantile()]. Formatting and rounding is done using [fr()].
#' 

contf <- function(x, na.rm=TRUE, dig=2, morm = "mean", supp="match", alpha=0.05, n=TRUE, quart_vals = c(0.25, 0.75), quart_type=2){
  # Did you do want mean or median?
  morm_opts <- c("mean", "median")
  if (!(morm %in% morm_opts)){
    warning("Did not request 'mean' or 'median'. Continuing with mean...")
    morm <- "mean"
  }
  
  # For mean...
  if (morm == "mean"){
    morm_string <- myStuff::fr(mean(x, na.rm=na.rm), dig = dig)
    
    # For median...
  } else if (morm == "median"){
    morm_string <- myStuff::fr(median(x, na.rm=na.rm), dig = dig)
  }
  
  
  
  # What supplemental info do you want?
  supp_opts <- c("none", "match", "sd", "ci", "range", "quartiles")
  if (!(supp %in% supp_opts))
    warning(paste0("Did not request option in: ", paste(supp_opts, collapse=", "), ". Continuing with match..."))
  
  # For matching
  # match_list <- data.frame(morm_opts, supp_match = c("sd", "quartiles")) # could do it this way later, for more efficiency and flexibility
  if(supp=="match"){
    if(morm=="mean"){
      supp <- "sd"
    } else if (morm=="median") {
      supp <- "quartiles"
    } else {
      warning("Cannot match when mean/median not specified. Continuing with `sd`...")
      supp <- "sd"
    }
  }
  
  # For no supplement
  if (supp=="none"){
    supp_string <- ""
    
    # For SD supplement
  } else if (supp=="sd") {
    supp_string <- paste0(" (", myStuff::fr(sd(x, na.rm=na.rm), dig = dig), ")")
    
    # For CI
  } else if (supp=="ci") {
    supp_string <- 
      paste0(" (",
             myStuff::fr(mean(x, na.rm=na.rm)-qt(alpha/2, df=sum(!is.na(x)), lower.tail=FALSE)*sqrt(var(x, na.rm=na.rm)/sum(!is.na(x))), dig = dig),
             "-",
             myStuff::fr(mean(x, na.rm=na.rm)+qt(alpha/2, df=sum(!is.na(x)), lower.tail=FALSE)*sqrt(var(x, na.rm=na.rm)/sum(!is.na(x))), dig = dig)
             ,")")
    
    # For Quartiles
  } else if (supp=="quartiles") {
    if(length(quart_vals) ==2){ #need 2 values
      supp_string <- 
        paste0(" (",
               myStuff::fr(quantile(x, probs = quart_vals[1], type = quart_type, na.rm=na.rm), dig = dig),
               "-",
               myStuff::fr(quantile(x, probs = quart_vals[2], type = quart_type, na.rm=na.rm), dig = dig),
               ")"
        )
    } else {
      warning("Need 2 values in quart_vals. Continuing with none.")
      supp_string <- ""
    }
    
    # For Range
  } else if (supp=="range") {
    supp_string <- 
      paste0(" (",
             myStuff::fr(min(x, na.rm=na.rm), dig = dig),
             "-",
             myStuff::fr(max(x, na.rm=na.rm), dig = dig),
             ")"
      )
  }
  
  
  
  # add on the sample at the end (or not)
  if (na.rm & n) {
    samp_string <- paste0(" (n=", sum(!is.na(x)),")")
  } else if (!na.rm & n){
    samp_string <- paste0(" (n=", length(x),")")
  } else {
    samp_string <- ""
  }
  
  
  
  # Putting it all together
  paste0(morm_string, supp_string, samp_string)
}