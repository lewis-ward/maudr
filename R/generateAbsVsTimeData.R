#' @export
generateAbsVsTimeData <- function() {
  # Define constants
  rxn_time_values <- c(10, 20, 30, 40, 50, 60)  # in minutes
  enz_vol <- 0.1   # mL
  cuv_vol <- 0.003 # L
  eps <- 6220

  # Locate the latest timestamped directory in output
  output_dir <- here::here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]

  if (length(dirs) == 0) {
    stop("Error: No timestamped directories found in 'output'.")
  }

  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]
  message(paste("Latest timestamped directory found:", latest_dir))

  # Build the full path to students_rxn_params.rda
  rda_file <- file.path(latest_dir, "Variables", "students_rxn_params.rda")
  if (!file.exists(rda_file)) {
    stop("Error: students_rxn_params.rda not found in 'Variables' folder of latest directory.")
  }

  message(paste("Successfully loaded students_rxn_params.rda from", dirname(rda_file)))
  load(rda_file)  # loads `students_rxn_params`

  # Process the data
  students_rxn_params_processed <- students_rxn_params %>%
    mutate(
      substr_conc = map(substr_conc, ~as.numeric(trimws(.))),
      rxn_time = list(rxn_time_values)
    ) %>%
    unnest(cols = c(substr_conc)) %>%
    uncount(length(rxn_time_values)) %>%
    group_by(student_id, inhibition_actual) %>%
    mutate(rxn_time = rep(rxn_time_values, length.out = n())) %>%
    ungroup() %>%
    mutate(
      gradient = pmap_dbl(
        list(Vmax, Km, substr_conc),
        ~calculateGradient(..1, ..2, ..3, enz_vol = enz_vol, cuv_vol = cuv_vol, eps = eps)
      ),
      absorbance = round(gradient * rxn_time, 3),
      rxn_condition = case_when(
        inhibition_actual == "no_inhibition" ~ "without_inhibitor",
        TRUE ~ "with_inhibitor"
      )
    ) %>%
    relocate(rxn_condition, .after = inhibition_actual)

  # Create output directory for student files
  output_student_dir <- file.path(latest_dir, "Student_data")
  dir.create(output_student_dir, showWarnings = FALSE)

  # Pivot and write single-sheet Excel per student (stacking both inhibition types)
  students_rxn_params_processed %>%
    select(student_id, rxn_substrate, inhibition_actual, rxn_condition,
           rxn_time, substr_conc, absorbance) %>%
    pivot_wider(
      names_from = substr_conc,
      values_from = absorbance,
      names_sort = TRUE
    ) %>%
    group_by(student_id) %>%
    group_split() %>%
    walk(function(student_df) {
      student_id <- unique(student_df$student_id)
      filename <- file.path(output_student_dir, paste0(student_id, "_data.xlsx"))
      writexl::write_xlsx(list(Data = student_df), path = filename)
      message(paste("File written for student:", student_id))
    })

  message("xcel files written with stacked inhibition data per student.")
}
