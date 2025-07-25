% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadAndPivot.R
\name{loadAndPivot}
\alias{loadAndPivot}
\title{Load and pivot student data files
Loads absorbance data from all student \code{.xlsx} files and pivots into long format}
\usage{
loadAndPivot()
}
\value{
Return value is irrelevant to user, but is a named list of dataframes
}
\description{
Load and pivot student data files
Loads absorbance data from all student \code{.xlsx} files and pivots into long format
}
\details{
Loads student \code{.xlsx} files from most recent \verb{\\output} directory, outputs into long format with \verb{substrate concentration} as a column.
List is then saved as \code{pivoted_data_list.rda} into \verb{\\variables}
}
\seealso{
\code{\link{runMaudr}}
}
