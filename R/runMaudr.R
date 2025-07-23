#' Run Full MAUDR Pipeline
#'
#' This function runs the full maudr workflow, including:
#' - Input validation and setup
#' - Kinetic parameter estimation
#' - Absorbance vs. time simulation
#' - Lineweaver-Burk and Michaelis-Menten plotting
#' - Student summary file generation
#'
#' All output is saved to the latest timestamped folder inside `output/`.
#'
#' @param individual Logical; if TRUE (default), generate individual student PDFs.
#' @param combined Logical; if TRUE (default), generate a single combined PDF of all students.
#' @export
runMaudr <- function(individual = TRUE, combined = TRUE) {
# ---- Load required packages ----
  if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
  pacman::p_load(
    here, fs, readxl, writexl, tidyverse, magrittr, broom,
    gridExtra, ggpmisc, rlang, xfun, usethis, devtools, lubridate
  )

  if (!file.exists(here::here("data"))) {
    stop("It looks like you're not in the right project directory.
         Please open the RStudio Project (.Rproj) file before running runMaudr().")
  }

# ---- `maudr` pipeline ----
  suppressMessages(initialiseMaudrData())
  suppressMessages(assignReactionConditions()) # create folders + load variables
  suppressMessages(processReactionParameters()) # fit kinetic parameters
  suppressMessages(generateAbsVsTimeData()) # simulate absorbance-time data
  suppressMessages(loadAndPivot()) # pivot data for plotting
  suppressMessages(generateStudentSummaryPDF(individual = individual, combined = combined)) # generate plots and PDF reports
  print("maudr pipeline complete! All outputs are in the latest timestamped 'output/' subfolder.")
}

