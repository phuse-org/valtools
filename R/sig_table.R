#' Generate a signature table for a validation report
#'
#' @param people data frame with variables: role, name, and title
#'
#' @return A data frame with people info formatted for validation report with
#'   the
#'
#' @export
vt_generate_sig_table <- function(people){
  # check that the tables have correct variables
  if (!all(c("role", "name", "title") %in% tolower(names(people)))) {
    stop(paste0("people table must have variables: role, name, and title. ",
                "Contains: ", paste0(tolower(names(people)), collapse = ", ")))
  }

  people$signature <- NA
  people$date <- NA
  names(people) <- tolower(names(people))
  people$name_and_title <- paste(people$name, people$title,  sep = ", " )
  people[, c("role", "name_and_title", "signature", "date")]

}

#' Return the templated code for inclusion in the validation report
#'
#' @param people data frame with variables: role, name, and title
#' @param template Name of the template to render
#' @param output directory passed to `render_template()`
#'
#' @return The text to include in a validation R markdown document
#'
#' @export
vt_render_sig_table <- function(people, template = "sig_table.Rmd", output) {
  render_template(template = template,
                  data = list(
                    people = deparse1(people)
                  ),
                  output = output
  )
}
