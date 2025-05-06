#' @export
loadandpivot <- function() {
  #  the latest timestamped output directory
  output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  if (length(latest_dir) == 0 || is.na(latest_dir)) {
    stop("Error: Could not find latest output directory.")
  }

  # load substrate concentrations from variable storage
  load_required_files()  # loads `variables_df`
  substrate_columns <- as.character(variables_df$substrate_conc_mM)

  # read and pivot each file
  pivoted_data_list <- lapply(
    list.files(path = file.path(latest_dir, "Student_data"), full.names = TRUE),
    function(file) {
      read_xlsx(file) %>%
        pivot_longer(
          cols = all_of(substrate_columns),
          names_to = "substrate_conc",
          values_to = "absorbance"
        ) %>%
        mutate(substrate_conc = as.numeric(substrate_conc))
    }
  )

  # filenames for id
  names(pivoted_data_list) <- basename(list.files(path = file.path(latest_dir, "Student_data"), full.names = TRUE))


  save(pivoted_data_list, file = file.path(latest_dir, "Variables", "pivoted_data_list.rda"))

  return(pivoted_data_list)
}
