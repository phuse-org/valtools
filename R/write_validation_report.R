#' Create validation report from template
#' @param pkg_name name of package
#' @param template what validation report template from {valtools} to use,
#' one of "validation" (default) or "requirements" (forthcoming)
#' @param open boolean to open the validation report for further editing
#' @importFrom tools file_ext
#' @importFrom rlang with_interactive is_interactive
#' @export
vt_use_report <- function(pkg_name = desc::desc_get_field("Package"),
                          template = "validation",
                          open = is_interactive()){

  val_leads <- get_val_leads()

  if(length(val_leads) == 0){
    val_leads <-  tryCatch({
      val_leads_username <- with_interactive(value = FALSE, vt_username())
      val_leads <- vt_get_user_info(username = val_leads_username,
                                        type = "name")[["name"]]

      config_details <- read_validation_config()
      old_role <- config_details$usernames[[val_leads]]$role
      config_details$usernames[[val_leads]]$role <-
        ifelse(grepl(pattern = "validation lead", x = tolower(old_role)),
               old_role,
               paste0(c("Validation Lead", old_role ), collapse = ", "))
      write_validation_config(path = dir,
                              working_dir = config_details$working_dir,
                              output_dir = config_details$output_dir,
                              report_naming_format = config_details$report_naming_format,
                              username_list = config_details$usernames)
      return(val_leads)
      }, error = function(e){
        val_leads <- username(fallback = "")
        vt_add_user_to_config(username = val_leads, name = val_leads, title = "",
                              role = "Validation Lead")


        inform(
          message = paste0("User `", val_leads, "` does not exist in the config file.  ",
                           "Assigning to role of `Validation Lead`"),
          class = "vt.validation_config_missing_user_inform"
        )
        return(val_leads)
      })
  }

  root <- find_root(has_file(".here") | is_rstudio_project | is_r_package | is_vcs_root)

  template_files <- c(validation = "validation_report.Rmd")
  report_filename <- file.path(get_config_working_dir(), "validation.Rmd")

  render_template( template = template_files[[template]],
                output = report_filename,
                data = list(pkg_name = basename(root),
                         title = "Validation Report",
                         author = paste0((sapply(val_leads,
                                                 vt_get_user_info,
                                                 type = "name")), collapse = ', ')))

  if(open){
    edit_file(report_filename)
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
