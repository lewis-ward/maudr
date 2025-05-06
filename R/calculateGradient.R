#' @export
calculateGradient <- function(Vmax, Km, substrate_conc, enz_vol, cuv_vol, eps) {
  V <- calculateReactionRate(Vmax, Km, substrate_conc)
  gradient <- V * jitter(enz_vol) / 1e6 / cuv_vol * eps
  return(gradient)
}
