
validation_file_path <- function(file, type, ref = vt_path(), ...){

  path <- find_file(file, ref=ref)

  if(missing(type)){
    type <- tools::file_ext(path)
    if("test_code" %in% split_path(path)){
      type <- "r_test_code"
    }
  }

  structure(
    path,
    type = type,
    class = c(tolower(type),"validation_file_path")
  )
}





split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(split_path(dirname(path)),basename(path)))
}

#' @noRd
#' @importFrom tools file_path_sans_ext
chunk_name <- function(path){
  filename_sans_ex <- file_path_sans_ext(basename(path))
  gsub("_","-",make.names(filename_sans_ex))
}
