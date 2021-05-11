#' Use dynamic file paths in a validation.
#'
#' vt_path() allows access of files relative to the working directory,which is
#' identified by the config file. It is also required to be used in the validation
#' report for cases where validation of installed packages is intended as it will
#' shift access to the correct location for the installed package for access.
#'
#' vt_find_config() locates the config file in the working directory, and
#' returns the full path to it.
#'
#' @rdname validation_paths
#'
#'
#' @param ... `[character]`\cr
#'   Path components below the validation folder, can be empty.
#'   Each argument should be a string containing one or more
#'   path components separated by a forward slash `"/"`.
#'
#' @export
#'
#' @importFrom rprojroot find_root has_file is_r_package is_rstudio_project is_vcs_root
#'
#' @examples
#' withr::with_tempdir({callr::r(function(){
#'
#'  valtools::vt_use_validation()
#'
#'  valtools::vt_path()
#'  valtools::vt_path("some", "reqs", "req01.md")
#'  valtools::vt_path("some/reqs/req01.md")
#'
#'  valtools::vt_find_config()
#'
#' })})
#'
vt_path <- function(...){

  state <- Sys.getenv("vt_validation_state")
  package <- Sys.getenv("vt_validation_package")

  if(state == "installed"){
    ## if testing an installed package via `vt_validate_installed_package()`,
    ## state == "installed",
    package_dir <- system.file(package = package)
    path <- file.path(package_dir,"validation") ## users have no choice if output_dir is set to "inst"

  }else{
    ## default and if building (state == "build") uses the same path
    path <- dirname(vt_find_config())
  }

  normalizePath(file.path(path, ...),winslash = "/",mustWork = FALSE)
}


#' @importFrom rprojroot find_root has_file is_r_package is_rstudio_project is_vcs_root
#' @rdname validation_paths
#'
#' @export
vt_find_config <- function(){

  root <- find_root(has_file(".here") | is_rstudio_project | is_r_package | is_vcs_root)

  tryCatch({

    config <- find_file("validation.yml", root, full_names = TRUE)

  },error = function(e){
      if(inherits(e,"vt.file_not_found")){
        abort(
          paste0(
            "A validation structure does not exist.\n",
            "Run `valtools::vt_use_validation()`."
          ),
          class = "vt.validation_config_missing"
        )
      }else{
        abort(e)
      }
    })

  if(length(config) > 1){
    config <- config_selector(config)
  }

  config


}



#' Find file in reference and return path
#'
#' @param filename name of file to find or pattern
#' @param ref where to start searching for file from
#' @param full_names boolean whether to return full or relative path
#' @param preference if there are multiple files found,
#'     identify if there is a preferred folder location from ref
#'
#'
#' @importFrom rlang abort
#' @importFrom withr with_dir
#' @noRd
#'
find_file <- function(filename, ref = ".", full_names = FALSE){

  with_dir(new = normalizePath(ref,winslash = "/"), {
    file_list <- list.files(path = ".", recursive = TRUE, full.names = TRUE)
  })

  file_path <- file_list[basename(file_list) %in% filename]

  if(length(file_path) == 0){
    abort(paste0("File `",filename,"` not found."),
          class = "vt.file_not_found")
  }

  do.call('c',lapply(file_path, function(fp) {
    fp <- as.list(split_path(fp))


    if (full_names) {
      fp <- c(ref, fp)
    }

    fp <- do.call('file.path', fp)

  }))

}



## if interactive, read both config files, determine if one is the "source" -
## it is source by whether it lives in the "Working" or "output" dir
## if not interactive, select file that is within the wd
config_selector <- function(files, is_live = interactive()){
  if (is_live) {

    files[sapply(files,
                 function(config_file) {
                   dirs <- read_yaml(config_file)
                   wd <- do.call('file.path', as.list(split_path(dirs$working_dir)))
                   if (grepl(wd, config_file, fixed = TRUE)) {
                     TRUE
                   } else{
                     FALSE
                   }
                 })]
  } else{
    wd <- normalizePath(do.call('file.path', as.list(split_path(getwd()))), winslash = "/")
    files <- files[grepl(wd, normalizePath(files, winslash = "/"), fixed = TRUE)]
    if(length(files) > 1){
      files <- files[which.min(sapply(files, function(p){length(split_path(p))}))]
    }
    files
  }
}






