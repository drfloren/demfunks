% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xtabs_cat.R
\name{xtabs_cat}
\alias{xtabs_cat}
\title{XTabs for Categorical Data}
\usage{
xtabs_cat(voi = NULL, dat)
}
\arguments{
\item{voi}{Character. The variable of interest (e.g., the one that contains the summary statistics)}

\item{dat}{The dataset, formatted with 3 columns: two contain information that will become row and column names, and the third contains infromation that will go into the table. For the two that will become row and column names, their combination should be unique in each row.}
}
\description{
This function is comparable to xtabs, but where xtabs only functions for numeric data, this functions for categorical data.
}
\details{
This function is handy for placing information in a matrix. The starting dataset should have 3 columns: a column with the row labels, a column with the column labels, and a column with the entries in each cell of the matrix. For the input dataset, there should be only a single row entry for the combination of column entries that will be row and column (as this is purely for formatting, there will be no row aggregation).
}
\examples{
#this works great
xtabs(len~supp+dose, data=aggregate(len~ supp + dose, data=ToothGrowth, mean))

#but this does not
xtabs(len~supp+dose, data=aggregate(len~ supp + dose, data=ToothGrowth, contf))

#enter this function!
xtabs_cat("len", aggregate(len~ supp + dose, data=ToothGrowth, contf))

}
\seealso{
\code{\link[=xtabs]{xtabs()}}
}
\author{
Michael Floren
}
