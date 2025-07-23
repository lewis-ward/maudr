#' Plot absorbance vs time for each student
#' @details
#' Generates scatter plot of absorbance versus reaction time for each student from the `pivoted_data_list.rda` output of \code{loadAndPivot}
#' Inhibition type on two different plots and colour-coded concentration points.
#' @return
#' Used internally by \code{generateStudentSummaryPDF}
#' @seealso \code{\link{plotLB}}, \code{\link{plotMM}}, \code{\link{generateStudentSummaryPDF}}
#' @export
plotAbsVsTime <- function() {
# ---- Locate latest timestamped output directory ----
  output_dir <- here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]
# ---- Load pivoted data list ----
  load(here(latest_dir, "Variables", "pivoted_data_list.rda"))

  absvstime_plots <- map(names(pivoted_data_list), function(student_name) {
    df <- pivoted_data_list[[student_name]]
    inhibition_types <- unique(df$inhibition_actual)

    df_filtered <- df |>
      filter(
        inhibition_actual %in% c(
          "no_inhibition",
          inhibition_types[inhibition_types != "no_inhibition"]
        )
      ) |>
      mutate(
        student_name = str_remove(student_name, "_data\\.xlsx$"),
        inhibition_actual = factor(
          inhibition_actual,
          levels = c("no_inhibition", setdiff(unique(inhibition_actual), "no_inhibition"))
        )
      )

    plot_obj <- ggplot(df_filtered) +
      aes(x = rxn_time, y = absorbance, colour = as.factor(substrate_conc)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      ggtitle(paste("Absorbance vs Reaction Time for", df_filtered$student_name[1])) +
      labs(
        x = "Reaction time (min)",
        y = expression("Absorbance (" * Delta * "abs)"),
        colour = "Substrate concentration (mM)"
      ) +
      scale_x_continuous(
        breaks = seq(min(df_filtered$rxn_time), max(df_filtered$rxn_time), by = 10)
      ) +
      facet_grid(. ~ inhibition_actual, scales = "free", switch = "y") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 10, hjust = 0.5, vjust = 1),
        strip.text = element_text(size = 8),
        strip.background = element_blank()
      )

    list(student_id = df_filtered$student_name[1], plot = plot_obj)
  })

  return(absvstime_plots)
}

#' Plot Lineweaver-Burk for each student
#' @details
#' Generates a Lineweaver-Burk (double recip.) plot for each student using the `students_rxn_params.rda` output of \code{assignReactionConditions}
#' Both inhibition types are stacked and differentiated by colour (colour is static)
#' @return
#' Used internally by \code{generateStudentSummaryPDF}, list of plot objects
#' @seealso \code{\link{plotAbsVsTime}}, \code{\link{plotMM}}, \code{\link{generateStudentSummaryPDF}}
#' @export
plotLB <- function() {
# ---- Locate latest timestamped output directory ----
  output_dir <- here::here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

# ---- Load students_rxn_params ----
  load(file.path(latest_dir, "Variables", "students_rxn_params.rda"))

  lb_plots <- students_rxn_params |>
    mutate(substr_conc = purrr::map(substr_conc, as.numeric)) |>
    tidyr::unnest(cols = c(substr_conc)) |>
    mutate(
      rxn_rate = calculateReactionRate(Vmax, Km, substr_conc),
      substrate_conc_reciprocal = 1 / substr_conc,
      rxn_rate_reciprocal = 1 / rxn_rate
    ) |>
    filter(substr_conc != 0) |>
    group_by(student_id) |>
    group_split() |>
    purrr::map(function(df) {
      student_name <- unique(df$student_id)

      df$inhibition_actual <- factor(
        df$inhibition_actual,
        levels = c("no_inhibition", setdiff(unique(df$inhibition_actual), "no_inhibition"))
      )

      plot_obj <- ggplot(df, aes(x = substrate_conc_reciprocal, y = rxn_rate_reciprocal, color = inhibition_actual)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
        labs(
          title = paste("Lineweaver-Burk Plot -", student_name),
          x = expression("1 / [S] (mM"^
                           {
                             -1
                           } * ")"),
          y = expression("1 / v (min / " * Delta * "abs)"),
          color = "Inhibition Type"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          plot.title = element_text(size = 10, hjust = 0.5)
        )

      list(student_id = student_name, plot = plot_obj)
    })

  return(lb_plots)
}

#' Plot Michaelis-Menten curves for each student
#' @details
#' Generates a Michaelis-Menten plot for each student using the `students_rxn_params.rda` output of \code{assignReactionConditions}
#' Both inhibition types are stacked and differentiated by colour (colour is static)
#' @return
#' Used internally by \code{generateStudentSummaryPDF}
#' @seealso \code{\link{plotAbsVsTime}}, \code{\link{plotLB}}, \code{\link{generateStudentSummaryPDF}}
#' @export
plotMM <- function() {
# ---- Locate latest timestamped output directory ----
  output_dir <- here::here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]
# ---- Load students_rxn_params ----
  load(here::here(latest_dir, "Variables", "students_rxn_params.rda"))

  mm_plots <- students_rxn_params |>
    dplyr::mutate(substr_conc = purrr::map(substr_conc, as.numeric)) |>
    tidyr::unnest(cols = substr_conc) |>
    dplyr::mutate(
      rxn_rate = calculateReactionRate(Vmax, Km, substr_conc)
    ) |>
    dplyr::group_by(student_id) |>
    dplyr::group_split() |>
    purrr::map(function(df) {
      student_name <- unique(df$student_id)

      all_types <- unique(df$inhibition_actual)
      df$inhibition_actual <- factor(
        df$inhibition_actual,
        levels = c("no_inhibition", setdiff(all_types, "no_inhibition"))
      )

      plot_obj <- ggplot2::ggplot(df, ggplot2::aes(
        x = substr_conc,
        y = rxn_rate,
        colour = inhibition_actual,
        group = inhibition_actual
      )) +
        ggplot2::geom_point() +
        ggplot2::stat_smooth(
          method = "nls",
          formula = y ~ Vmax * x / (Km + x),
          method.args = list(start = list(Vmax = 1, Km = 1)),
          se = FALSE
        ) +
        ggplot2::labs(
          title = paste("Michaelis-Menten Plot for", student_name),
          x = "Substrate Concentration (mM)",
          y = expression("Reaction Rate (" * Delta * "abs/min)"),
          colour = "Inhibition Type"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggplot2::element_text(size = 10, hjust = 0.5)
        )

      list(student_id = student_name, plot = plot_obj)
    })

  return(mm_plots)
}

