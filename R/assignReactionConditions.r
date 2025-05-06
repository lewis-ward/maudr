#' @export
assignReactionConditions <- function() {

# creating output folders and getting and formatting current date in ISO 8601 (slight deviation) without timezone
    base_dir <- here("output", format(now(), "%Y-%m-%d-%H-%M-%S"))
    dir.create(file.path(base_dir, "Student_data"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_dir, "Answer_files"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(base_dir, "Variables"), recursive = TRUE, showWarnings = FALSE)


# 05/04 ensure that the variable_storage.xlsx file actually has proper syntax/isnt missing a column. This also avoids duplicating data. This also adds in a way to check if the other two files (students and enzyme params) have the proper columns/syntax and prevent the script from crashing later on. Although it seems repetitive assigning filepath means no rewriting for later approaches, just edit the filepath of the object "X_file"


  # Variable Storage
  if (file.exists(here("data", "variable_storage.xlsx"))) {
    print("Loading variable_storage.xlsx")
    variables_df <- read_xlsx(here("data", "variable_storage.xlsx"))
    required_vars_cols <- c("time_sec", "substrate_conc_mM", "extinction_coeff", "cuvette_volume_L", "enzyme_volume_ml") # Check for required columns
    if (!all(required_vars_cols %in% colnames(variables_df))) {
      stop("Error: Missing one or more required columns in 'variable_storage.xlsx'. Expected: ",
           paste(required_vars_cols, collapse = ", "))
    }
    # Copy file to the timestamped Variables folder
    file.copy(from = here("data", "variable_storage.xlsx"), to = file.path(base_dir, "Variables"))
    message("Copied variable_storage.xlsx to Variables folder.")

    time <- variables_df$time_sec # Extract values (renaming to match .rda variables)
    substr_conc <- variables_df$substrate_conc_mM
    eps <- unique(variables_df$extinction_coeff)
    cuv_vol <- unique(variables_df$cuvette_volume_L)
    enz_vol <- unique(variables_df$enzyme_volume_ml)
  } else if (file.exists(here("data", "variable_storage.rda"))) {
    print("Loading variable_storage.rda as fallback")
    load(here("data", "variable_storage.rda"))
    # Copy .rda file to the timestamped Variables folder
    file.copy(from = here("data", "variable_storage.rda"), to = file.path(base_dir, "Variables"))
    message("Copied variable_storage.rda to Variables folder.")
  } else {
    stop("Error: No variable storage files found.")
  }

  # Students
  if (file.exists(here("data", "student_names.xlsx"))) {
    print("Loading student_names.xlsx")
    students <- read_xlsx(here("data", "student_names.xlsx"))
    required_students_cols <- c("student_no", "first_name", "surname") # Check for required columns
    if (!all(required_students_cols %in% colnames(students))) {
      stop("Error: Missing one or more required columns in 'student_names.xlsx'. Expected: ",
           paste(required_students_cols, collapse = ", "))
    }
    # Copy file to the timestamped Variables folder
    file.copy(from = here("data", "student_names.xlsx"), to = file.path(base_dir, "Variables"))
    message("Copied student_names.xlsx to Variables folder.")
  } else if (file.exists(here("data", "student_names.rda"))) {
    print("Loading student_names.rda as fallback")
    load(here("data", "student_names.rda"))
    # Copy .rda file to the timestamped Variables folder
    file.copy(from = here("data", "student_names.rda"), to = file.path(base_dir, "Variables"))
    message("Copied student_names.rda to Variables folder.")
  } else {
    stop("Error: No student names files found.")
  }

  # Enzymes
  if (file.exists(here("data", "enzyme_properties.xlsx"))) {
    print("Loading enzyme_properties.xlsx")
    enzyme_properties <- read_xlsx(here("data", "enzyme_properties.xlsx"))
    required_enzyme_cols <- c("rxn_substrate", "Kcat", "Km", "Vmax", "enzyme_conc", "inhibition_actual") # Check for required columns (UPDATED for enzyme properties)
    if (!all(required_enzyme_cols %in% colnames(enzyme_properties))) {
      stop("Error: Missing one or more required columns in 'enzyme_properties.xlsx'. Expected: ",
           paste(required_enzyme_cols, collapse = ", "))
    }
    # Copy file to the timestamped Variables folder
    file.copy(from = here("data", "enzyme_properties.xlsx"), to = file.path(base_dir, "Variables"))
    message("Copied enzyme_properties.xlsx to Variables folder.")
  } else if (file.exists(here("data", "enzyme_properties.rda"))) {
    print("Loading enzyme_properties.rda as fallback")
    load(here("data", "enzyme_properties.rda"))
    # Copy .rda file to the timestamped Variables folder
    file.copy(from = here("data", "enzyme_properties.rda"), to = file.path(base_dir, "Variables"))
    message("Copied enzyme_properties.rda to Variables folder.")
  } else {
    stop("Error: No enzyme properties files found.")
  }

seed <- sample(100:999, 1) # Record the seed to recreate the data later if needed
  set.seed(seed)
  print(paste0("Seed value for this round: ", seed))

  # Generate student-specific reaction parameters
  students_rxn_params <- students %>%
    mutate(
      student_id = paste(toupper(student_no), toupper(first_name), sep = "_"),
      rxn_substrate = sample(unique(enzyme_properties$rxn_substrate), nrow(students), replace = TRUE),
      inhibition_actual = sample(unique(enzyme_properties$inhibition_actual)[-1], nrow(students), replace = TRUE)
    ) %>%
    uncount(., 2, .id = "id") %>%
    mutate(inhibition_actual = ifelse(id == 2, "no_inhibition", inhibition_actual)) %>%
    select(-student_no, -first_name, -surname, -id) %>%
    left_join(., enzyme_properties, by = c("rxn_substrate", "inhibition_actual")) %>%
    mutate(substr_conc = list(substr_conc))

save(students_rxn_params, file = file.path(base_dir, "Variables", "students_rxn_params.rda"))
# Copy relevant input files into the Variables folder for record-keeping
}
