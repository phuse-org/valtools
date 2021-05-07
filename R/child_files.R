#' Identify ordering of validation or user-designated child files
#' @param loc location to explore. Either "folder" for naive inclusion of
#' validation folder contents, or "yml" to use validation_folder field of validation.yml
#' @param validation_order optional ordering of validation folders to search
#' @return vector of child file names to include in validation report
#' @export
#' @examples
#' withr::with_tempdir({
#'   vt_use_validation()
#'   vt_use_test_case("testcase1", username = "a user", open = FALSE)
#'   vt_use_req("req1", username = "a user", open = FALSE)
#'   vt_use_test_code("testcode1", username = "another user", open = FALSE)
#'
#'   # as listed in validation.yml validation_files
#'   vt_get_child_files(loc = "yml")
#'
#'   # as ordered in validation subfolders
#'   vt_get_child_files(loc = "folder",
#'     validation_order = c("requirements", "test_cases", "test_code"))
#'
#' })
vt_get_child_files <- function(loc = c("folder", "yml"),
                               validation_order = c("requirements", "test_cases", "test_code")){
  validation_folder <- vt_path()
  all_validation_files <- unname(do.call("c", lapply(validation_order, FUN = function(x){
    file.path(x,
      list.files(path = file.path(validation_folder, x),
                            full.names = FALSE))
    })))


  if(loc[1] == "folder"){
    return(all_validation_files)
  } else if(loc[1] == "yml"){
    return(unname(sapply(get_config_validation_files(), FUN = function(x){
      find_file(x, ref = validation_folder)
    })))
  }
}
