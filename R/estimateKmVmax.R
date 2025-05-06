#' @export
estimateKmVmax <- function(df) {
  nls(rxn_rate ~ estimated_Vmax * substrate_conc / (estimated_Km + substrate_conc),
      data = df,
      start = list(estimated_Km = 5, estimated_Vmax = 0.05)
  ) %>%
    tidy() %>%
    select(parameter = term, estimate) %>%
    mutate(estimate = round(estimate, 2)) %>%
    pivot_wider(names_from = parameter, values_from = estimate)
}
