#' @title Dependency versions
#' @description Retrieves the DESCRIPTION Imports field from current project and
#' identifies what versions are installed on current system.
#' @return vector of package versions for the list in  DESCRIPTION imports from current project
#' @importFrom here here
#' @importFrom utils packageVersion
dep_versions <- function(){

  desc <- roxygen2:::read.description(here("DESCRIPTION"))
  imports <- gsub("\n", "", trimws(strsplit(desc$Imports, split = ",")[[1]]))

  package_versions <- do.call(c, lapply(imports, function(i) {
    paste0(i, " (v", packageVersion(i), ")")
  }))

  package_versions
}
