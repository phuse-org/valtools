#' Create validation report from template
#' @param pkg_name name of package
#' @param template what validation report template from {valtools} to use,
#' one of "validation" (default) or "requirements" (forthcoming)
#' @param dynamic_referencing Should dynamic referencing be enabled by default. Boolean defaults to FALSE.
#' @param open boolean to open the validation report for further editing
#' @importFrom tools file_ext
#' @importFrom rlang with_interactive is_interactive
#' @export
vt_use_report <- function(pkg_name = NULL,
                          template = "validation",
                          dynamic_referencing = FALSE,
                          open = is_interactive()){
  if(is.null(pkg_name)){
    pkg_name <- get_config_package()
  }

  val_leads <- get_val_leads()

  if(length(val_leads) == 0){
    val_leads <-  tryCatch({

      val_leads_username <- username(fallback = "")
      val_leads_info <- vt_get_user_info(username = val_leads_username)

      val_leads_info[['role']] <- ifelse(
        grepl(pattern = "validation lead", x = tolower(val_leads_info[['role']])),
        val_leads_info[['role']],
        paste0(c("Validation Lead", val_leads_info[['role']]), collapse = ", "))

      vt_add_user_to_config(
        username = val_leads_username,
        name = val_leads_info[['name']] ,
        title = val_leads_info[['title']] ,
        role = val_leads_info[['role']]
      )

      get_val_leads()

      }, error = function(e){

        val_leads <- username(fallback = "")
        vt_add_user_to_config(username = val_leads, name = val_leads, title = "",
                              role = "Validation Lead")


        inform(
          message = paste0("User `", val_leads, "` does not exist in the config file.  ",
                           "Assigning to role of `Validation Lead`"),
          class = "vt.validation_config_missing_user_inform"
        )
        return(get_val_leads())
      })
  }

  template_files <- c(validation = "validation_report.Rmd")
  report_filename <- file.path(get_config_working_dir(), template_files[[template]])

  render_template( template = template_files[[template]],
                output = report_filename,
                data = list(pkg_name = pkg_name,
                            enable_dynamic_referencing = dynamic_referencing,
                         author = paste0((sapply(val_leads,
                                                 vt_get_user_info,
                                                 type = "name")), collapse = ', ')))

  if(open){
    edit_file(report_filename) # nocov
  }
  invisible(TRUE)
}

#' @noRd
#' @keywords internal
get_val_leads <- function(){

  usernames <- read_validation_config()$usernames
  unlist(lapply(seq_along(usernames), FUN = function(x){
    if(grepl(pattern = "validation lead", x = tolower(usernames[[x]]$role))){
      return(names(usernames)[x])
    }
  }))
}
