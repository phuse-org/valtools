#' Generate a signature table for a validation report
#'
#' @return A dataframe created from the validation config containing a row for
#'   each user with the columns: role, name_and_title, signature, and date.
#'
#' @export
vt_generate_sig_table <- function(){

  # Pull all the usernames into a data.frame
  people <- do.call('rbind', lapply(read_validation_config()$usernames, data.frame))

  people$signature <- NA
  people$date <- NA
  names(people) <- tolower(names(people))
  people$name_and_title <- paste(people$name, people$title,  sep = ", " )
  people[, c("role", "name_and_title", "signature", "date")]

}

#' Kable defaults for rendering validation report
#'
#' @param people A dataframe with the columns role, Name and Title, Signature,
#'   and "Date"
#'
#' @return Output from calls to kable
#'
#' @export
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec column_spec
#' @importFrom grDevices rgb
vt_kable_sig_table <- function(people) {

  # check that the tables have correct variables
  if (!all(c("role", "name_and_title", "signature", "date") %in% tolower(names(people)))) {
    stop(paste0("people table must have variables: role, Name and Title, Signature, and Date ",
                "Contains: ", paste0(tolower(names(people)), collapse = ", ")))
  }

  people %>%
    kable(col.names = c("Role", "Name and Title", "Signature", "Date"),
          escape = FALSE, booktabs = FALSE) %>%
    kable_styling(full_width = FALSE, position = "left") %>%
    row_spec(0, background = rgb(184, 204, 228, maxColorValue = 255)) %>%
    column_spec(1, width = "9em", border_left = TRUE) %>%
    column_spec(2, width = "11em") %>%
    column_spec(3, width = "15em") %>%
    column_spec(4, width = "8em", border_right = TRUE)

}

#' Return the templated code for inclusion in the validation report
#'
#' @param template Name of the template to render
#' @param output directory passed to `render_template()`
#'
#' @return The text to include in a validation R markdown document
#'
#' @export
vt_render_sig_table <- function(template = "sig_table.Rmd", output) {
  output <- vt_set_ext(output, "Rmd")
  render_template(template = template,
                  output = output
  )
}


