#' Process and estimates kinetic parameters for students
#' Locates latest timestamped directory in `\output`, calculate reaction rates for all substrate concentrations, fits Michaelis-Menten
#' parameters per student and inhibition type
#' @details
#' Loads student parameters and calculates reaction rate with \code{calculateReactionRate}, fits to Michaelis-Menten curves using \code{estimateKmVmax},
#' stores the results as `processed_data.rda` in the output directory for downstream use.
#'@return
#' Processed data frame with estimated kinetic parameters for each student and substrate. Saves to `\variables` directory but is irrelevant to average user.
#' @seealso \code{\link{runMaudr}} \code{\link{calculateReactionRate}}, \code{\link{estimateKmVmax}}, \code{\link{generateAbsVsTimeData}}, \code{\link{generateStudentSummaryPDF}}
#' @export
processReactionParameters <- function() {
# ---- Locate latest timestamped output directory ----
  output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)

  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  if (length(latest_dir) == 0 || is.na(latest_dir)) {
    stop("Error: Could not find the latest output directory.")
  }

  variables_dir <- file.path(latest_dir, "Variables")
  if (!dir.exists(variables_dir)) {
    stop("Error: No 'Variables' folder found in the latest timestamped directory.")
  }

  students_rxn_params_file <- file.path(variables_dir, "students_rxn_params.rda")

  if (file.exists(students_rxn_params_file)) {
    load(students_rxn_params_file)
    message("Successfully loaded students_rxn_params.rda from ", students_rxn_params_file)
  } else {
    stop("Error: students_rxn_params.rda not found in the Variables folder.")
  }
# ---- Process data and save to intermediate `variables` storage ----
  processed_data <- students_rxn_params |>
    mutate(substrate_conc = map(substr_conc, ~ as.numeric(.x))) |>
    unnest(substrate_conc) |>
    rowwise() |>
    mutate(rxn_rate = calculateReactionRate(Vmax, Km, substrate_conc)) |>
    ungroup() |>
    mutate(rxn_rate = round(rxn_rate, 2)) |>
    group_by(student_id, rxn_substrate, inhibition_actual) |>
    nest() |>
    mutate(estimated_params = map(data, estimateKmVmax)) |>
    select(-data) |>
    mutate(estimated_params = map(estimated_params, ~ bind_cols(inhibition_type = inhibition_actual, .x))) |>
    unnest(cols = estimated_params, keep_empty = TRUE) |>
    ungroup() |>
    select(-inhibition_actual) |>
    group_by(student_id, rxn_substrate) |>
    nest() |>
    summarise(estimated_params = list(data), .groups = "drop") |>
    unnest(cols = estimated_params, keep_empty = TRUE)

  save(processed_data, file = file.path(variables_dir, "processed_data.rda"))

  return(processed_data)
}
