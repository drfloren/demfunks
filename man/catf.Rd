% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catf.R
\name{catf}
\alias{catf}
\title{Categorical Descriptives Base Function}
\usage{
catf(x, dig = 1, which_in_paren = "p", include_perc_sign = TRUE, ...)
}
\arguments{
\item{x}{The vector with data on the categorical variable to be used in calculating the descriptives.}

\item{dig}{How many digits should values round to? Defaults to 1 (e.g., 99.1\%).}

\item{which_in_paren}{Which should be in parentheses? Arguments are "n" and "p". Defaults to "p"}

\item{include_perc_sign}{Should the percent sign be included? Defaults to FALSE.}

\item{...}{Additional arguments for \code{table} (e.g., \code{exclude=NULL} to include NA's)}
}
\description{
This function generates a short summary of descriptive statistics for a vector with data from a categorical variable
}
\details{
This really doesn't do much, essentially just adding percentages to a basic table command
}
\examples{
catf(ChickWeight$Diet)
catf(ChickWeight$Diet, which_in_paren = "n")

}
\seealso{
\code{\link[=table]{table()}}

Other categorical data functions: 
\code{\link{cat_row}()},
\code{\link{ind_catf}()},
\code{\link{sub_catf}()}
}
\author{
Michael Floren
}
\concept{categorical data functions}
