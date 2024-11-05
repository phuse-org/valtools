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


#' @importFrom rprojroot find_root has_file is_r_package is_rstudio_project
#' @rdname validation_paths
#'
#' @export
vt_find_config <- function(){
  
  tryCatch({
    root <- find_root(has_file(".here") | is_rstudio_project | is_r_package )
  }, error = function(e){
    abort(
      paste0(
      "Could not find root directory. ",
      "Is your working directory inside a package, validation packet, or project?\n"
      ),
      class = "vt.validation_root_missing"
    )
  })
  
  
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
  
  check_for_child_projects_with_configs(root, config)
  
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
find_file <- function(filename, ref = ".", full_names = FALSE, regex = FALSE, include_hidden_files = FALSE){

  with_dir(new = normalizePath(ref,winslash = "/"), {
    file_list <- list.files(path = ".", recursive = TRUE, full.names = TRUE, all.files = include_hidden_files)
  })
  
  if(!regex){
    file_path <- file_list[basename(file_list) %in% filename]
  }else{
    if(length(filename)>1){
      abort("If `regex` is set to `TRUE`, filename is used as a pattern",
            class = "vt.file_multiple_regex")
    }
    file_path <- file_list[grepl(pattern = filename,x = basename(file_list))]
  }

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
  if (is_live) {  # nocov start

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
  } else{ # nocov end
    wd <- normalizePath(do.call('file.path', as.list(split_path(getwd()))), winslash = "/")
    files <- files[grepl(wd, normalizePath(files, winslash = "/"), fixed = TRUE)]
    if(length(files) > 1){
      files <- files[which.min(sapply(files, function(p){length(split_path(p))}))]
    }
    files
  }
}


check_for_child_projects_with_configs <- function(root, configs){
  
  if(length(configs) > 1){
    
    root_files <- c(
      tryCatch(find_file("[.]Rproj$", root, regex = TRUE, full_names = TRUE, include_hidden_files = TRUE),error = function(e){c()}),
      tryCatch(find_file("[.]here$", root, regex = TRUE, full_names = TRUE, include_hidden_files = TRUE),error = function(e){c()}),
      find_r_pkg_desc(root)
    )
    
    root_dirs <- unique(dirname(root_files))
    root_child_dirs<- setdiff(root_dirs, root)
    
    if(length(root_child_dirs) > 0){
    
      roots_child_with_validation <- c()
      for(root_path in root_child_dirs){
        if(any(grepl(paste0(root_path,"/"),configs,fixed=TRUE))){
          roots_child_with_validation <- c(
            roots_child_with_validation,
            root_path
          )
        }
      }
      
      roots_child_with_validation <- unique(roots_child_with_validation)
      
      if(length(roots_child_with_validation) > 1){
        
        ref_dirs <- gsub(paste0(normalizePath(getwd(),winslash = "/"),"/"),"",roots_child_with_validation,fixed = TRUE)
        
        abort(
          paste0("Nested projects with validation infrastructures exist. Set the working directory to one of:\n",
               paste0("\t- `setwd(\"",ref_dirs,"\")`\n", collapse = "")),
        class = "vt.multiple_validation_roots_found")
      } 
    }
  }

}

find_r_pkg_desc <- function(root){
  
  desc_files <- tryCatch(find_file("DESCRIPTION", root, regex = TRUE, full_names = TRUE, include_hidden_files = TRUE),error = function(e){c()})
  
  if(is.null(desc_files)){
    return(c())
  }else{
    desc_file_out <- c()
    for(desc_file in desc_files){
      contents <- readLines(con = desc_file)
      if(any(grepl("^Package:", contents))){
        desc_file_out <- c(desc_file_out, desc_file)
      }
    }
    return(desc_file_out)
  }
}

