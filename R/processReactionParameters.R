#' @export
processReactionParameters <- function() {
  # Find the most recent timestamped folder
  output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)

  # Exclude the root "output" folder and sort the remaining directories by name (timestamp)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  # Ensure a directory was found
  if (length(latest_dir) == 0 || is.na(latest_dir)) {
    stop("Error: Could not find the latest output directory.")
  }

  # Check if the "Variables" folder exists within the latest timestamped directory
  variables_dir <- file.path(latest_dir, "Variables")
  if (!dir.exists(variables_dir)) {
    stop("Error: No 'Variables' folder found in the latest timestamped directory.")
  }

  # Load students_rxn_params.rda from the "Variables" folder
  students_rxn_params_file <- file.path(variables_dir, "students_rxn_params.rda")

  if (file.exists(students_rxn_params_file)) {
    load(students_rxn_params_file)  # Load the data
    message("Successfully loaded students_rxn_params.rda from ", students_rxn_params_file)
  } else {
    stop("Error: students_rxn_params.rda not found in the Variables folder.")
  }

  # Process the reaction parameters (assuming students_rxn_params is loaded correctly)
  processed_data <- students_rxn_params %>%
    mutate(substrate_conc = map(substr_conc, ~ as.numeric(.x))) %>%
    mutate(rxn_rate = pmap(select(., Vmax, Km, substrate_conc), ~ calculateReactionRate(..1, ..2, ..3))) %>%
    unnest(c(rxn_rate, substrate_conc)) %>%
    mutate(rxn_rate = round(rxn_rate, 2)) %>%
    group_by(student_id, rxn_substrate, inhibition_actual) %>%
    nest() %>%
    mutate(estimated_params = map(data, estimateKmVmax)) %>%
    select(-data) %>%
    mutate(estimated_params = map(estimated_params, ~ bind_cols(inhibition_type = inhibition_actual, .x))) %>%
    unnest(cols = estimated_params, keep_empty = TRUE) %>%
    ungroup() %>%
    select(-inhibition_actual) %>%
    group_by(student_id, rxn_substrate) %>%
    nest() %>%
    summarise(estimated_params = list(data), .groups = "drop") %>%
    unnest(cols = estimated_params, keep_empty = TRUE)

  # Save processed_data to the correct timestamped Variables folder
  save(processed_data, file = file.path(variables_dir, "processed_data.rda"))

  return(processed_data)
}
