#' Calculate reaction rate with Michaelis-Menten equation
#' Calculates the reaction rate for given parameters and substrate concentration
#' @details
#' Uses Michaelis-Menten \eqn{v = (Vmax * [S]) / (Km + [S])} to calculate reaction rate.
#' Adds jitter to concentration levels greater than 0 to simulate wet lab variability.
#' @return
#' Calculated reaction rate when called in \code{processReactionParameters} and \code{generateAbsVsTimeData}. User does not need to interact.
#' @seealso \code{\link{processReactionParameters}} \code{\link{generateAbsVsTime}}
#' @export
calculateReactionRate <- function(Vmax, Km, substrate_conc) {
  V <- Vmax * ifelse(substrate_conc > 0, jitter(substrate_conc), substrate_conc) /
    (Km + ifelse(substrate_conc > 0, jitter(substrate_conc), substrate_conc))
  return(V)
}

#' Calculate absorbance-time gradient
#' Calculates absorbance-time gradient for given parameters
#' @details
#' Internally calls \code{calculateReactionRate} to simulate absorbance changes and spectrophotometer assays
#' @return
#' Calculated absorbance-time gradient when called in \code{processReactionParameters} and \code{generateAbsVsTimeData}. User does not need to interact.
#' @seealso \code{\link{processReactionParameters}} \code{\link{generateAbsVsTime}} \code{\link{calculateReactionRate}}
#' @export
calculateGradient <- function(Vmax, Km, substrate_conc, enz_vol, cuv_vol, eps) {
  V <- calculateReactionRate(Vmax, Km, substrate_conc)
  gradient <- V * jitter(enz_vol) / 1e6 / cuv_vol * eps
  return(gradient)
}

#' Calculate estimated `Km` and `Vmax` values
#' Fits Michaelis-Menten equation to substrate concentrations and reaction rates using nonlinear least squares regression
#' @details
#' Uses \code{\link[stats]{nls}} to estimate Km and Vmax.
#' @return
#' Estimated values for gradient when called in \code{processReactionParameters}. User does not need to interact.
#' @seealso \code{\link{processReactionParameters}} \code{\link[stats]{nls}}
#' @export
estimateKmVmax <- function(df) {
  nls(rxn_rate ~ estimated_Vmax * substrate_conc / (estimated_Km + substrate_conc),
      data = df,
      start = list(estimated_Km = 5, estimated_Vmax = 0.05)
  ) |>
    tidy() |>
    select(parameter = term, estimate) |>
    mutate(estimate = round(estimate, 2)) |>
    pivot_wider(names_from = parameter, values_from = estimate)
}
