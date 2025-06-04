#' @export
load_required_files <- function() {
  message("Starting to load required files...")

  output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)

  # exclude out non-timestamped directories (e.g., 'output' itself)
  dirs <- dirs[basename(dirs) != "output"]

  # sort the directories in lexicographical order (will sort by timestamp)
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  # checking that there are directories (just stops R from hanging :'))
  if (length(latest_dir) == 0) {
    stop("Error: No timestamped directories found in 'output'.")
  }

  message(paste("Latest timestamped directory found:", latest_dir))

  # check and load required files directly without using variables for paths
  for (file_desc in c("variable_storage", "processed_data", "enzyme_properties")) {
    # file path for both .xlsx and .rda files
    file_path_rda <- file.path(latest_dir, "Variables", paste0(file_desc, ".rda"))
    file_path_xlsx <- file.path(latest_dir, "Variables", paste0(file_desc, ".xlsx"))

    # Check for the .xlsx file first (priority), if not found, check for .rda file
    if (file.exists(file_path_xlsx)) {
      file_path <- file_path_xlsx
    } else if (file.exists(file_path_rda)) {
      file_path <- file_path_rda
    } else {
      message(paste("Warning: The file", file_desc, "does not exist at", latest_dir))
      next
    }

    # Try loading the file with proper error handling
    tryCatch({
      if (grepl(".rda$", file_path)) {
        load(file_path)  # For .rda files
      } else if (grepl(".xlsx$", file_path)) {
        if (file_desc == "variable_storage") {
          assign("variable_storage", read_xlsx(file_path), envir = .GlobalEnv)
        } else if (file_desc == "processed_data") {
          assign("students_rxn_params", read_xlsx(file_path), envir = .GlobalEnv)
        } else if (file_desc == "enzyme_properties") {
          assign("enzyme_properties", read_xlsx(file_path), envir = .GlobalEnv)
        }
      }
      message(paste(file_desc, "loaded successfully from", file_path))
    }, error = function(e) {
      message("Error occurred while loading ", file_desc, ": ", e$message)
    })
  }
}
