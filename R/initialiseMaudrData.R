#' Initialise the maudr data directory with sample data
#' Creates a `data/` folder in the working directory (if it doesn't exist)
#' and copies in default `.rda` files from the maudr package if the user hasn't already
#' added their own .xlsx or .rda files.
#' @details
#' Checks for three core data files: `enzyme_properties.rda`, `student_names.rda`, and `variable_storage.rda`.
#' Looks for either user provided `.xlsx` or `.rda` files within `data/` directory. If neither are found, built-in samples
#' are written to `data/`.
#' Function is called as a part of the `runMaudr()` pipeline but can be run manually to reinitialise data.
#' @return
#' No return value. Called to ensure `data/` folder exists, and loads in provided samples if `data/` is empty.
#' @seealso \code{\link{runMaudr}}
#' @export
initialiseMaudrData <- function() {
# ---- Create `data/` directory if it doesn't exist ----
  data_dir <- here::here("data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
    message("Created 'data/' directory.")
  }

# ---- Helper to copy `.rda` if neither `.xlsx` nor `.rda` exists ----
  copy_sample <- function(name, object) {
    xlsx_path <- file.path(data_dir, paste0(name, ".xlsx"))
    rda_path <- file.path(data_dir, paste0(name, ".rda"))

    if (!file.exists(xlsx_path) && !file.exists(rda_path)) {
      save(list = name, file = rda_path, envir = list2env(setNames(list(object), name)))
      message(paste0("Sample data for '", name, "' written to 'data/", name, ".rda'"))
    }
  }

# ---- Copy built-in data from the package if needed ----
  copy_sample("enzyme_properties", maudr::enzyme_properties)
  copy_sample("student_names", maudr::student_names)
  copy_sample("variable_storage", maudr::variable_storage)
}
