#' Create validation report from template
#' @param dir filepath to folder containing config file \code{.validation}
#' @param pkg_name name of package
#' @param template what validation report template from {valtools} to use,
#' passed to \code{usethis::use_template}
#' @param open boolean whether to open the validation report, passed to \code{usetthis::use_template}
#' @importFrom usethis use_template proj_set
#' @importFrom tools file_ext
#' @importFrom rlang with_interactive
#' @export
vt_use_report <- function(dir = ".",
                          pkg_name = desc::desc_get_field("Package"),
                          template = "validation_report.Rmd",
                          open = FALSE){

  if(!file.exists(file.path(dir, "validation.yml"))){
    vt_use_validation_config()
  }

  val_leads <- get_val_leads(dir = dir)

  if(length(val_leads) == 0){
    val_leads <-  tryCatch({
      val_leads_username <- with_interactive(value = FALSE, vt_username())
      val_leads <- vt_get_user_info(username = val_leads_username,
                                        type = "name",
                                        pkg = dir)[["name"]]

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

  output_dir <- file.path(dir, get_config_output_dir())
  if(!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  proj_set(path = dir, force = TRUE)
  use_template( template = template,
                save_as = file.path( output_dir,
                            paste0(evaluate_filename(), ".", file_ext(template))),
                data = list(pkg_name = desc_get_field("Package", file = dir),
                         title = "Validation Report",
                         author = paste0((sapply(val_leads,
                                                 vt_get_user_info,
                                                 type = "name")), collapse = ', ')),
                ignore = FALSE,
                open = open,
                package = "valtools")
  if(file.exists(file.path(dir, "validation.yml"))){
    file.copy(from = file.path(dir, "validation.yml"),
              to = file.path(output_dir, "validation.yml"))
  }

  file_list <- get_config_validation_files()
  file_list
}

#' @noRd
#' @keywords internal
get_val_leads <- function(dir = "."){


  if(!file.exists(file.path(dir,"validation.yml"))){
    abort(
      paste0(
        "A validation config file does not exist.\n",
        "Run `valtools::vt_use_validation_config()` to create a validation config file."
      ),
      class = "vt.validation_config_missing"
    )
  }
  usernames <- read_validation_config()$usernames
  unlist(lapply(seq_along(usernames), FUN = function(x){
    if(grepl(pattern = "validation lead", x = tolower(usernames[[x]]$role))){
      return(names(usernames)[x])
    }
  }))
}
