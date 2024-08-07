% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sub_catf.R
\name{sub_catf}
\alias{sub_catf}
\title{Categorical Descriptives By Subgroup}
\usage{
sub_catf(
  x,
  subgroup = NULL,
  dig = 1,
  names = TRUE,
  out_df = TRUE,
  margin = 2,
  ...
)
}
\arguments{
\item{x}{The vector with data on the categorical variable to be used in calculating the descriptives.}

\item{subgroup}{The vector listing out the categorical variable to use in making subgroups. Defaults to \code{NULL}, which will simply use a single group}

\item{dig}{How many digits should values round to? Defaults to 1 (e.g., 99.1\%).}

\item{names}{Would you like the results to be named?}

\item{out_df}{Should the output be formatted as a data.frame? Default is TRUE.}

\item{margin}{Passed to \code{prop.table}: which margin should the percentages be calculated for?}

\item{...}{Additional arguments to pass to \code{catf}}
}
\description{
This function calculates categorical descriptives of a variable broken out by subgroups
}
\details{
This is essentially an \code{aggregate} wrapper for \code{catf}.
}
\examples{
sub_catf(ChickWeight$Time, ChickWeight$Diet)
sub_catf(ChickWeight$Time, ChickWeight$Diet, margin=2)

}
\seealso{
\code{\link[=aggregate]{aggregate()}}

Other categorical data functions: 
\code{\link{cat_row}()},
\code{\link{catf}()},
\code{\link{ind_catf}()}
}
\author{
Michael Floren
}
\concept{categorical data functions}
