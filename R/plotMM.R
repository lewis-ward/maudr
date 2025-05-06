#' @export
plotMM <- function() {
  # Find the latest timestamped directory
  output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  # Load processed_data from Variables folder
  load(here(latest_dir, "Variables", "processed_data.rda"))

  # Unnest substrate concentrations and compute reaction rate
  students_rxn_params %>%
    mutate(substr_conc = map(substr_conc, ~ as.numeric(.x))) %>%
    unnest(cols = c(substr_conc)) %>%
    mutate(
      rxn_rate = calculateReactionRate(Vmax, Km, substr_conc)
    ) %>%
    group_by(student_id) %>%
    group_split() %>%
    walk(function(student_df) {
      student_name <- unique(student_df$student_id)

      student_df %>%
        ggplot(aes(x = substr_conc, y = rxn_rate, colour = inhibition_actual)) +
        geom_point() +
        geom_line() +
        labs(
          title = paste("Michaelis-Menten Plot for", student_name),
          x = "Substrate Concentration (mM)",
          y = "Reaction Rate (∆abs/min)",
          colour = "Inhibition Type"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(size = 10, hjust = 0.5)
        ) -> p

      # Save plot per student
      ggsave(
        filename = here(latest_dir, "Answer_files", paste0(student_name, "_MichaelisMenten.pdf")),
        plot = p,
        width = 6, height = 4
      )
    })
}
