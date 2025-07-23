#' Generate student summary PDFs
#' Generates individual and/or combined summary PDF files for each student
#' @param individual Logical. if TRUE (default), then maudr will generate individual student PDFs
#' @param combined Logical. if TRUE (default), then maudr will generate a single combined PDF of all students
#' @details
#' Internally calls logic from \code{plotAbsVsTime}, \code{plotLB}, and \code{plotMM} to generate summary PDFs for each student,
#' combines these plots and the output from \code{processReactionParameters}, dependent on what parameters the user inputs.
#' @return
#' `.PDF` files in the `/Answer_files` subdir of the latest dir under `output/`. Is called as a part of the \code{runMaudr} pipeline.
#' @seealso \code{\link{runMaudr}} \code{\link{plotAbsVsTime}} \code{\link{plotLB}} \code{\link{plotMM}}
generateStudentSummaryPDF <- function(individual = individual, combined = combined) {
# ---- Locate the latest timestamped output directory ----
  output_dir <- here::here("output")
  dirs <- list.dirs(output_dir, full.names = TRUE, recursive = FALSE)
  dirs <- dirs[basename(dirs) != "output"]
  latest_dir <- dirs[order(dirs, decreasing = TRUE)][1]

# ---- Load processed_data ---
  load(file.path(latest_dir, "Variables", "processed_data.rda"))

# ---- Generate plots ----
  absvstime_plots <- plotAbsVsTime()
  lb_plots <- plotLB()
  mm_plots <- plotMM()

# ----  Map plots by student_id ----
  abs_map <- setNames(absvstime_plots, sapply(absvstime_plots, `[[`, "student_id"))
  lb_map <- setNames(lb_plots, sapply(lb_plots, `[[`, "student_id"))
  mm_map <- setNames(mm_plots, sapply(mm_plots, `[[`, "student_id"))

  student_ids <- sort(intersect(intersect(names(abs_map), names(lb_map)), names(mm_map)))

# ---- Output folder for answers ----
  answer_dir <- file.path(latest_dir, "Answer_files")
  if (!dir.exists(answer_dir)) dir.create(answer_dir, recursive = TRUE)

# ---- All pages for combined PDF ----
  plots_per_student <- list()

  for (student_id in student_ids) {
    abs_plot <- abs_map[[student_id]]$plot
    lb_plot <- lb_map[[student_id]]$plot
    mm_plot <- mm_map[[student_id]]$plot

    data_tbl <- processed_data |>
      dplyr::filter(student_id == !!student_id) |>
      dplyr::select(rxn_substrate, estimated_params) |>
      tidyr::unnest(estimated_params) |>
      as.data.frame()

    table_grob <- gridExtra::tableGrob(data_tbl, rows = NULL)

    full_page <- gridExtra::grid.arrange(
      grid::textGrob(student_id, gp = grid::gpar(fontsize = 14, fontface = "bold")),
      gridExtra::arrangeGrob(abs_plot, lb_plot, ncol = 2),
      gridExtra::arrangeGrob(mm_plot, table_grob, ncol = 2),
      ncol = 1,
      heights = c(0.5, 6, 6)
    )

    if (individual) {
# ---- Save individual PDF ----
      ggsave(
        filename = file.path(answer_dir, paste0(student_id, "_summary.pdf")),
        plot = full_page,
        width = 11, height = 8.5, units = "in"
      )
    }
    plots_per_student[[student_id]] <- full_page
  }

  if (individual) {
    message("Saved individual summaries in: ", answer_dir)
  }

  if (combined) {
    summary_pdf_path <- file.path(answer_dir, "student_summary_all.pdf")
    ggsave(
      filename = summary_pdf_path,
      plot = gridExtra::marrangeGrob(plots_per_student, nrow = 1, ncol = 1),
      width = 11, height = 8.5, units = "in"
    )
    message("Saved combined summary PDF: ", summary_pdf_path)
  }
}
