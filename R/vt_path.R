#' Use to set dynamic file paths in a validation.
#'
#' vt_path() allows access of files relative to the working directory identified
#' in the config file. It is also required to be used in the validation report
#' for cases where validation of installed packages is intended as it will
#' shift access to the correct location for the installed package for access.

#'
#'
#' @param ... `[character]`\cr
#'   Path components below the validation folder, can be empty.
#'   Each argument should be a string containing one or more
#'   path components separated by a forward slash `"/"`.
#' @param pkg path to base directory of package
#'
#' @export
#'
#' @importFrom rprojroot find_root has_file is_r_package is_rstudio_project
#'
#' @examples
#' withr::with_tempdir({callr::r(function(){
#'
#'  valtools::vt_use_validation_config()
#'  valtools::vt_use_validation()
#'
#'  valtools::vt_path()
#'  valtools::vt_path("some", "reqs", "req01.md")
#'  valtools::vt_path("some/reqs/req01.md")
#'
#' })})
#'
vt_path <- function(..., pkg = "."){

  state <- Sys.getenv("vt_validation_state")
  package <- Sys.getenv("vt_validation_package")

  if(state == "installed"){
    ## if testing an installed package via `vt_validate_installed_package()`,
    ## state == "installed",
    package_dir <- system.file(package = package)
    path <- file.path(package_dir,"validation") ## users have no choice if output_dir is set to "inst"

  }else{
    ## default and if building (state == "build") uses the same path

    root <- find_root(has_file(".here") | has_file("validation.yml") | is_rstudio_project | is_r_package | is_vcs_root)

    path <- file.path(root, pkg, get_config_working_dir(pkg = file.path(root,pkg)), "validation")
  }

  normalizePath(file.path(path, ...),winslash = "/",mustWork = FALSE)
}
