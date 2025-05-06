#' @export
calculateReactionRate <- function(Vmax, Km, substrate_conc) {
  V <- Vmax * ifelse(substrate_conc > 0, jitter(substrate_conc), substrate_conc) /
    (Km + ifelse(substrate_conc > 0, jitter(substrate_conc), substrate_conc))
  return(V)
}
