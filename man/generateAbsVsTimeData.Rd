% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateAbsVsTimeData.R
\name{generateAbsVsTimeData}
\alias{generateAbsVsTimeData}
\title{Generate simulated absorbance vs time data for all students
Generates simulated absorbance values over time for different substrate concentrations and inhibition conditions
Outputs a single-sheet \code{.xlsx} file per student with both inhibition types stacked}
\usage{
generateAbsVsTimeData()
}
\value{
No return value. Function is called for its side effects (writes Excel files for each student). File is then passed onto \code{loadAndPivot}
}
\description{
Generate simulated absorbance vs time data for all students
Generates simulated absorbance values over time for different substrate concentrations and inhibition conditions
Outputs a single-sheet \code{.xlsx} file per student with both inhibition types stacked
}
\details{
Reads the latest set of assigned reaction conditions from the most recent \code{students_rxn_params.rda} in the output directory
Simulates absorbance values at a series of fixed time points and substrate concentrations using pre-defined constants for enzyme and cuvette volume and extinction coefficient
Generates and saves an Excel file for each student in the appropriate output subdirectory, combining data for both \code{with_inhibitor} and \code{no_inhibitor}
}
\seealso{
\code{\link{runMaudr}} \code{\link{assignReactionConditions}} \code{\link{runMaudr}} \code{\link{loadAndPivot}}
}
