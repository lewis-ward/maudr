#' Generate simulated absorbance vs time data for all students
#' Generates simulated absorbance values over time for different substrate concentrations and inhibition conditions
#' Outputs a single-sheet `.xlsx` file per student with both inhibition types stacked
#' @details
#' Reads the latest set of assigned reaction conditions from the most recent `students_rxn_params.rda` in the output directory
#' Simulates absorbance values at a series of fixed time points and substrate concentrations using pre-defined constants for enzyme and cuvette volume and extinction coefficient
#' Generates and saves an Excel file for each student in the appropriate output subdirectory, combining data for both `with_inhibitor` and `no_inhibitor`
#' @return
#' No return value. Function is called for its side effects (writes Excel files for each student). File is then passed onto \code{loadAndPivot}
#' @seealso \code{\link{runMaudr}} \code{\link{assignReactionConditions}} \code{\link{runMaudr}} \code{\link{loadAndPivot}}
#' @export
generateAbsVsTimeData <- function() {
# ---- Define constants ----
  rxn_time_values <- c(10, 20, 30, 40, 50, 60) # seconds
  enz_vol <- 0.1 # mL
  cuv_vol <- 0.003 # L
  eps <- 6220 # extinction coeff

# ---- Locate most recent generate timestamp directory ----
  output_dir <- here::here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]

  if (length(dirs) == 0) {
    stop("Error: No timestamped directories found in 'output'.")
  }

  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]
  message(paste("Latest timestamped directory found:", latest_dir))

# ---- Building path to shorten ----
  rda_file <- file.path(latest_dir, "Variables", "students_rxn_params.rda")
  if (!file.exists(rda_file)) {
    stop("Error: students_rxn_params.rda not found in 'Variables' folder of latest directory.")
  }

  message(paste("Successfully loaded students_rxn_params.rda from", dirname(rda_file)))
  load(rda_file) # loads `students_rxn_params`

# ---- Processing data to generate each student's absorbance-time values ----
  students_rxn_params_processed <- students_rxn_params |>
    mutate(
      substr_conc = map(substr_conc, ~ as.numeric(trimws(.))),
      rxn_time = list(rxn_time_values)
    ) |>
    unnest(cols = c(substr_conc)) |>
    uncount(length(rxn_time_values)) |>
    group_by(student_id, inhibition_actual) |>
    mutate(rxn_time = rep(rxn_time_values, length.out = n())) |>
    ungroup() |>
    mutate(
      gradient = pmap_dbl(
        list(Vmax, Km, substr_conc),
        ~ calculateGradient(..1, ..2, ..3, enz_vol = enz_vol, cuv_vol = cuv_vol, eps = eps)
      ),
      absorbance = round(gradient * rxn_time, 3),
      rxn_condition = case_when(
        inhibition_actual == "no_inhibition" ~ "without_inhibitor",
        TRUE ~ "with_inhibitor"
      )
    ) |>
    relocate(rxn_condition, .after = inhibition_actual)

# ---- Creates directory within latest_dir (see above) to store files ----
  output_student_dir <- file.path(latest_dir, "Student_data")
  dir.create(output_student_dir, showWarnings = FALSE)

# ---- Pivot and write single-sheet .xlsx per student (stacking inhib types) ----
  students_rxn_params_processed |>
    select(
      student_id, rxn_substrate, inhibition_actual, rxn_condition,
      rxn_time, substr_conc, absorbance
    ) |>
    pivot_wider(
      names_from = substr_conc,
      values_from = absorbance,
      names_sort = TRUE
    ) |>
    group_by(student_id) |>
    group_split() |>
    walk(function(student_df) {
      student_id <- unique(student_df$student_id)
      filename <- file.path(output_student_dir, paste0(student_id, "_data.xlsx"))
      writexl::write_xlsx(list(Data = student_df), path = filename)
      message(paste("File written for student:", student_id))
    })

  message("Excel files written with stacked inhibition data per student.")
}
