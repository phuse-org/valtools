#' Generate a signature table for a validation report
#'
#' @param usernames list of vt_names to use when validation.yml does not exist
#' @return A dataframe created from the validation config containing a row for
#'   each user with the columns: role, name_and_title, signature, and date.
#'
#' @export
vt_scrape_sig_table <- function(usernames = NULL){
  if(is.null(usernames)){
    people <- do.call('rbind', lapply(vt_get_all_users(), data.frame, stringsAsFactors = FALSE))
    rownames(people) <- NULL
    people
  } else {
    if(!all(unlist(lapply(as.list(usernames), is_vt_user)))){
      abort("Usernames must be list of vt_users. Run `list(valtools::vt_user(...))`.",
            class = "vt.not_user")
    }
    people <- convert_vtname_table(usernames)
  }

  people$signature <- NA
  people$date <- NA
  names(people) <- tolower(names(people))
  people$name_and_title <- paste(people$name, people$title,  sep = ", " )
  people[, c("role", "name_and_title", "signature", "date")]

}

convert_vtname_table <- function(usernames){
  people <- lapply(usernames, FUN = function(x){
    this_user <- data.frame(x, stringsAsFactors = FALSE)
    names(this_user) <- c("name", "title", "role")
    this_user
  })

  do.call('rbind', people)
}


#' Kable defaults for rendering validation report
#'
#' @param people A dataframe with the columns role, Name and Title, Signature,
#'   and "Date"
#' @param format passed to knitr::kable
#'
#' @return knitr_kable object
#'
#' @export
#'
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling column_spec
vt_kable_sig_table <- function(people, format = "latex") {

  # check that the tables have correct variables
  if (!all(c("role", "name_and_title", "signature", "date") %in% tolower(names(people)))) {
    stop(paste0("people table must have variables: role, Name and Title, Signature, and Date ",
                "Contains: ", paste0(tolower(names(people)), collapse = ", ")))
  }

  t <- kable(people,
             format = format,
             col.names = c("Role", "Name and Title", "Signature", "Date"),
             booktabs = FALSE)
  t <- column_spec(t, 1, width = "9em", border_left = TRUE)
  t <- column_spec(t, 3, width = "15em")
  t <- column_spec(t, 4, width = "8em", border_right = TRUE)

  t

}


