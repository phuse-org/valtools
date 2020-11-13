#' @title Generate a signature table
#' @param people data frame with variables: role, name, and title
#' @return a kableExtra table with people info formatted for validation report
#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec column_spec
#' @importFrom grDevices rgb
signature_table <- function(people){
  # check that the tables have correct variables
  if (!all(c("role", "name", "title") %in% tolower(names(people)))) {
    stop(paste0("people table must have variables: role, name, and title. ",
                "Contains: ", paste0(tolower(names(people)), collapse = ", ")))
  }

  people$signature <- NA
  people$date <- NA
  names(people) <- tolower(names(people))
  people$name_and_title <- paste(people$name, people$title,  sep = ", " )
  people[, c("role", "name_and_title", "signature", "date")] %>%
    # mutate_all(linebreak) %>%
    kable(col.names = c("Role", "Name and Title", "Signature", "Date"),
          escape = FALSE, booktabs = FALSE) %>%
    kable_styling(full_width = FALSE, position = "left") %>%
    row_spec(0, background = rgb(184, 204, 228, maxColorValue = 255)) %>%
    column_spec(1, width = "9em", border_left = TRUE) %>%
    column_spec(2, width = "11em") %>%
    column_spec(3, width = "15em") %>%
    column_spec(4, width = "8em", border_right = TRUE)
}
