#' @export
generateAnswerFiles <- function(students_data, output_type = c("combined", "separate", "both")) {
  output_type <- match.arg(output_type)

  if (output_type %in% c("combined", "both")) {
    ggsave(plot = marrangeGrob(pmap(students_data, createAnswerPDF), nrow = 1, ncol = 1),
           here("output", "SIB2004_answers_all_students.pdf"),
           width = 210, height = 297, units = "mm", dpi = "retina")
  }

  if (output_type %in% c("separate", "both")) {
    pmap(students_data, createAnswerPDF2)
  }
}
