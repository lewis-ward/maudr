#' Assign reaction conditions to students
#' @description
#' Loads variables, student names, and enzyme properties, then assigns reaction conditions and saves the results.
#' @details
#' Creates timestamped subfolders within `output/` directory for variables, student data, and answers.
#' Assigns enzyme substrate and inhibition types to each student (student always receives `no_inhibition` and then one of: `competitive`, `non-competitive`, and `uncompetitive`).
#' Prepares `student_rxn_params.rda` within `Variables` subfolder for downstream analysis. `Variables` subfolder is of little importance to the user.
#' @return
#' No return value. Called to generate `students_rxn_params.rda`
#' @seealso \code{\link{runMaudr}}
#' @export
assignReactionConditions <- function() {
# ---- Establishing output directories ----
  base_dir <- here::here("output", format(lubridate::now(), "%Y-%m-%d-%H-%M-%S"))
  dir.create(file.path(base_dir, "Student_data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "Answer_files"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(base_dir, "Variables"), recursive = TRUE, showWarnings = FALSE)

# ---- Variable storage: supports .xlsx or .rda, defaults to .xlsx if both present ----
  if (file.exists(here::here("data", "variable_storage.xlsx"))) {
    message("Loading variable_storage.xlsx")
    variables_df <- readxl::read_xlsx(here::here("data", "variable_storage.xlsx"))
    required_vars_cols <- c(
      "time_sec", "substrate_conc_mM", "extinction_coeff",
      "cuvette_volume_l", "enzyme_volume_ml"
    )
    if (!all(required_vars_cols %in% names(variables_df))) {
      stop(
        "Error: Missing one or more required columns in 'variable_storage.xlsx'. Expected: ",
        paste(required_vars_cols, collapse = ", ")
      )
    }
    file.copy(
      from = here::here("data", "variable_storage.xlsx"),
      to = file.path(base_dir, "Variables")
    )
    message("Copied variable_storage.xlsx to Variables folder.")
  } else if (file.exists(here::here("data", "variable_storage.rda"))) {
    message("Loading variable_storage.rda as fallback")
    load(here::here("data", "variable_storage.rda"))
    # variable_storage object is created by default from initializeMaudrData() if it doesnt exist
    variables_df <- variable_storage
    file.copy(
      from = here::here("data", "variable_storage.rda"),
      to = file.path(base_dir, "Variables")
    )
    message("Copied variable_storage.rda to Variables folder.")
  } else {
    stop("Error: No variable storage files found.")
  }
    # renaming for the sake of sanity
  substr_conc <- variables_df$substrate_conc_mM

# ---- Student names: supports .xlsx or .rda, defaults to .xlsx if both present ----
  if (file.exists(here::here("data", "student_names.xlsx"))) {
    message("Loading student_names.xlsx")
    student_names <- readxl::read_xlsx(here::here("data", "student_names.xlsx"))
    required_student_names_cols <- c("student_no", "first_name", "surname")
    if (!all(required_student_names_cols %in% names(student_names))) {
      stop(
        "Error: Missing one or more required columns in 'student_names.xlsx'. Expected: ",
        paste(required_student_names_cols, collapse = ", ")
      )
    }
    file.copy(
      from = here::here("data", "student_names.xlsx"),
      to = file.path(base_dir, "Variables")
    )
    message("Copied student_names.xlsx to Variables folder.")
  } else if (file.exists(here::here("data", "student_names.rda"))) {
    message("Loading student_names.rda as fallback")
    load(here::here("data", "student_names.rda"))
    # student_names object is created by default from initializeMaudrData() if it doesnt exist
    file.copy(
      from = here::here("data", "student_names.rda"),
      to = file.path(base_dir, "Variables")
    )
    message("Copied student_names.rda to Variables folder.")
  } else {
    stop("Error: No student names files found.")
  }

# ---- Enzyme properties: supports .xlsx or .rda, defaults to .xlsx if both present ----
  if (file.exists(here::here("data", "enzyme_properties.xlsx"))) {
    message("Loading enzyme_properties.xlsx")
    enzyme_properties <- readxl::read_xlsx(here::here("data", "enzyme_properties.xlsx"))
    required_enzyme_cols <- c(
      "rxn_substrate", "Kcat", "Km", "Vmax",
      "enzyme_conc", "inhibition_actual"
    )
    if (!all(required_enzyme_cols %in% names(enzyme_properties))) {
      stop(
        "Error: Missing one or more required columns in 'enzyme_properties.xlsx'. Expected: ",
        paste(required_enzyme_cols, collapse = ", ")
      )
    }
    file.copy(
      from = here::here("data", "enzyme_properties.xlsx"),
      to = file.path(base_dir, "Variables")
    )
    message("Copied enzyme_properties.xlsx to Variables folder.")
  } else if (file.exists(here::here("data", "enzyme_properties.rda"))) {
    message("Loading enzyme_properties.rda as fallback")
    load(here::here("data", "enzyme_properties.rda"))
    # enzyme_properties object is created by default from initializeMaudrData() if it doesnt exist
    file.copy(
      from = here::here("data", "enzyme_properties.rda"),
      to = file.path(base_dir, "Variables")
    )
    message("Copied enzyme_properties.rda to Variables folder.")
  } else {
    stop("Error: No enzyme properties files found.")
  }

# ---- Random seed for reproducability ----
  seed <- sample(100:999, 1)
  set.seed(seed)
  message(paste0("Seed value for this round: ", seed))

# ---- Assign reaction conditions to students randomly and save output ----
  students_rxn_params <- student_names |>
    dplyr::mutate(
      student_id = paste(toupper(student_no), toupper(first_name), sep = "_"),
      rxn_substrate = sample(unique(enzyme_properties$rxn_substrate),
        nrow(student_names),
        replace = TRUE
      ),
      inhibition_actual = sample(unique(enzyme_properties$inhibition_actual)[-1],
        nrow(student_names),
        replace = TRUE
      )
    ) |>
    tidyr::uncount(2, .id = "id") |>
    dplyr::mutate(
      inhibition_actual = ifelse(id == 2, "no_inhibition", inhibition_actual)
    ) |>
    dplyr::select(-student_no, -first_name, -surname, -id) |>
    dplyr::left_join(enzyme_properties, by = c("rxn_substrate", "inhibition_actual")) |>
    dplyr::mutate(substr_conc = list(substr_conc))

  save(students_rxn_params, file = file.path(base_dir, "Variables", "students_rxn_params.rda"))
}
