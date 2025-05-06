#' @export
plotAbsVsTime <- function() {

  # find the most recent timestamped directory in "output"
 output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]  # Remove the root "output" directory
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

  # loading pivtoed data list from previous function
  load(here(latest_dir, "Variables", "pivoted_data_list.rda"))


  lapply(names(pivoted_data_list), function(student_name) {
    df <- pivoted_data_list[[student_name]]
    inhibition_types <- unique(df$inhibition_actual)

    # filter data for both inhibition types for the student: "no_inhibition" and other inhibition types
    df_filtered <- df %>%
      filter(inhibition_actual %in% c("no_inhibition", inhibition_types[inhibition_types != "no_inhibition"])) %>%
      mutate(student_name = str_remove(student_name, "_data\\.xlsx$"))  # Add student name as a new column

    # plotting
    absvstimeplot <- ggplot(df_filtered) +
      aes(x = rxn_time, y = absorbance, colour = as.factor(substrate_conc)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      ggtitle(paste("Absorbance vs Reaction Time for", str_remove(student_name, "_data\\.xlsx$"))) +
      labs(
        x = "Reaction time (min)",
        y = "Absorbance (AU)",
        colour = "Substrate concentration (mM)"
      ) +
      scale_x_continuous(
        breaks = seq(min(df_filtered$rxn_time), max(df_filtered$rxn_time), by = 10)
      ) +
      facet_grid(. ~ inhibition_actual, scales = "free", switch = "y") +  # Facet by inhibition_actual (2 facets per student)
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 1),
        strip.text = element_text(size = 8),  # Adjust facet label size
        strip.background = element_blank()   # Remove background of facet labels for clarity
      )

    # Save each student's plot to a separate PDF file
    ggsave(filename = here(latest_dir, "Answer_files", paste0(str_remove(student_name, "_data\\.xlsx$"), "_AbsVsTime.pdf")), plot = absvstimeplot)
    message(paste("Saved plot for student:", student_name))
  })
}
