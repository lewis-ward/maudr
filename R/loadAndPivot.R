#' Load and pivot student data files
#' Loads absorbance data from all student `.xlsx` files and pivots into long format
#' @details
#' Loads student `.xlsx` files from most recent `\output` directory, outputs into long format with `substrate concentration` as a column.
#' List is then saved as `pivoted_data_list.rda` into `\variables`
#' @return
#' Return value is irrelevant to user, but is a named list of dataframes
#' @seealso \code{\link{runMaudr}}
#' @export
loadAndPivot <- function() {
# ----  Locate the latest timestamped output directory ----
  output_dir <- here::here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  if (length(latest_dir) == 0 || is.na(latest_dir)) {
    stop("Error: Could not find latest output directory.")
  }

  message(paste("Latest timestamped directory found:", latest_dir))

  # ---- Variable Storage: supports .xlsx or .rda, ----
  variable_storage_xlsx <- file.path(latest_dir, "Variables", "variable_storage.xlsx")
  variable_storage_rda <- file.path(latest_dir, "Variables", "variable_storage.rda")

  if (file.exists(variable_storage_xlsx)) {
    message("Loading variable_storage.xlsx")
    variable_storage <- readxl::read_xlsx(variable_storage_xlsx)
    message("Loaded variable_storage.xlsx from latest Variables folder.")
  } else if (file.exists(variable_storage_rda)) {
    message("Loading variable_storage.rda as fallback")
    load(variable_storage_rda) # loads variable_storage object
    message("Loaded variable_storage.rda from latest Variables folder.")
    if (!exists("variable_storage")) stop("variable_storage object not found in RDA file!")
  } else {
    stop("Error: No variable_storage.xlsx or variable_storage.rda found in latest Variables folder.")
  }

  substrate_columns <- as.character(variable_storage$substrate_conc_mM)

# ---- Get full paths to student .xlsx files and assign names ----
  student_files <- list.files(
    path = file.path(latest_dir, "Student_data"),
    full.names = TRUE
  )
  names(student_files) <- basename(student_files)

# ----  Read and pivot each student file into long format ----
  pivoted_data_list <- student_files |>
    purrr::map(\(x) readxl::read_xlsx(x) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(substrate_columns),
        names_to = "substrate_conc",
        values_to = "absorbance"
      ) |>
      dplyr::mutate(substrate_conc = as.numeric(substrate_conc)))

# ---- Save the list of pivoted data to .rda file ----
  save(
    pivoted_data_list,
    file = file.path(latest_dir, "Variables", "pivoted_data_list.rda")
  )

  return(pivoted_data_list)
}
