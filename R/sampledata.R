#' Example student names
#'
#' Small dataset containing sample student IDs and names for use in `maudr`
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{student_no}{Student number, formatted as a string (e.g. "u123444")}
#'   \item{first_name}{First name of student}
#'   \item{surname}{Surname of student}
#' }
#' @source Generated for testing
"student_names"

#' Example enzyme properties
#'
#' A dataset of enzyme kinetic and inhibition parameters used in simulations
#' Substrate set is alcohol by default
#'
#' @format A data frame with variables:
#' \describe{
#'   \item{rxn_substrate}{Name of alcohol substrate (e.g. "ethanol", "propanol")}
#'   \item{Kcat}{Turnover number (numeric)}
#'   \item{Km}{Michaelis constant (numeric, mM)}
#'   \item{Vmax}{Maximum reaction rate (numeric)}
#'   \item{enzyme_conc}{Enzyme concentration (numeric, mM)}
#'   \item{inhibition_actual}{Inhibitor condition (e.g. "no_inhibition", "competitive", "noncompetitive", "uncompetitive")}
#' }
#' @source Generated for testing
"enzyme_properties"

#' Example variable storage
#'
#' A dataset of experimental constants for calculating reaction conditions
#' The extinction coefficient provided is for alcohol dehydrogenase (ADH), same with the enzyme substrate set
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{time_sec}{Reaction time (in seconds)}
#'   \item{substrate_conc_mM}{Substrate concentration (in mM)}
#'   \item{extinction_coeff}{Extinction coefficient (numeric, 6220 M\eqn{^{-1}} cm\eqn{^{-1}})}
#'   \item{cuvette_volume_l}{Cuvette volume (in liters, numeric)}
#'   \item{enzyme_volume_ml}{Enzyme volume (in milliliters, numeric)}
#' }
#' @source Generated for testing
"variable_storage"
