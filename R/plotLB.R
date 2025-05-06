#' @export
plotLB= function() {

  # Get latest timestamped directory
  output_dir = here("output")
  dirs = list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs = dirs[basename(dirs) != "output"]
  latest_dir = dirs[order(dirs, decreasing = TRUE)][1]

  # Load processed data
  load(here(latest_dir, "Variables", "processed_data.rda"))

  # Expand nested substrate concentration list
  processed_data %>%
    mutate(substrate_conc = map(substrate_conc, ~ as.numeric(.x))) %>%
    unnest(cols = substrate_conc) %>%
    group_by(student_id) %>%
    group_split() %>%
    walk(function(df) {

      student_name = unique(df$student_id)

      # Compute reaction rate
      df = df %>%
        mutate(rxn_rate = map2_dbl(Vmax, Km, ~ calculateReactionRate(.x, .y, substrate_conc))) %>%
        filter(substrate_conc != 0) %>%
        mutate(
          substrate_conc_reciprocal = 1 / substrate_conc,
          rxn_rate_reciprocal = 1 / rxn_rate
        )

      # Estimate x-intercept from no_inhibition condition
      x_intercept = df %>%
        filter(inhibition_actual == "no_inhibition") %>%
        lm(rxn_rate_reciprocal ~ substrate_conc_reciprocal, data = .) %>%
        coef() %>%
        { -.[1] / .[2] }

      # Generate plot
      lb_plot = ggplot(df) +
        aes(x = substrate_conc_reciprocal, y = rxn_rate_reciprocal, colour = inhibition_actual) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
        expand_limits(x = x_intercept) +
        facet_grid(. ~ inhibition_actual) +
        labs(
          title = paste("Lineweaver-Burke Plot -", student_name),
          x = "1 / [S] (mM⁻¹)",
          y = "1 / v (min / ∆abs)",
          colour = "Inhibition type"
        ) +
        theme_minimal() +
        theme(legend.position = "bottom")

      # Save each plot
      ggsave(
        filename = here(latest_dir, "Answer_files", paste0(student_name, "_LB_Plot.pdf")),
        plot = lb_plot
      )

      message("Saved LB plot for student: ", student_name)
    })
}
