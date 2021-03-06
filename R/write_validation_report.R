#' Create validation report from template
#' @param dir filepath to folder containing config file \code{.validation}
#' @param pkg_name name of package
#' @param template what validation report template from {valtools} to use,
#' passed to \code{usethis::use_template}
#' @importFrom usethis use_template
#' @importFrom tools file_ext
#' @importFrom rlang with_interactive
#' @export
vt_use_report <- function(dir = ".",
                          pkg_name = desc::desc_get_field("Package"),
                          template = "validation_report.Rmd"){
  if(!file.exists(file.path(pkg, ".validation"))){
    vt_use_validation_config()
  }

  tryCatch({
    with_interactive(value = FALSE,
                     vt_get_user_info(username = vt_username(),
                                      type = "name",
                                      pkg = pkg))
    }, error = function(e){
      this_user <- username(fallback = "")
      vt_add_user_to_config(username = this_user, name = this_user, title = "",
                            role = "Validation Lead")


      inform(
        message = paste0("User `", this_user, "` does not exist in the config file.  ",
                         "Assigning to role of `Validation Lead`"),
        class = "vt.validation_config_missing_user_inform"
      )
    })

  use_template(package = "valtools",
             save_as = paste0(evaluate_filename(), ".", file_ext(template)),
             open = TRUE,
             template = template,
             data = list(pkg_name = desc_get_field("Package", file = dir),
                         title = "Validation Report",
                         author = vt_get_user_info(username = vt_username(),
                                                   type = "name",
                                                   pkg = dir)) )
}
