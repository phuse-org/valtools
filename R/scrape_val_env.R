#' @title Retrieve validation environment
#' @description Retrieves dependencies used in validation report.
#' Includes: OS, R version, packages listed in the validation package DESCRIPTION (Depends/Imports/Suggests), packages
#' present in current session.
#' @param pkg path to package
#' @return data.frame with columns:
#'
#' \itemize{
#' \item \code{resource} "OS", "R" or package name
#' \item \code{type} identifier of requirement type:
#'     \itemize{
#'         \item system - OS or R resource
#'         \item package_req - from DESCRIPTION Depends/Imports of the package being validated
#'         \item extended_req - from DESCRIPTION Suggests of the package being validated
#'         \item session - packages in current workspace not captured via package_req/extended_req
#'     }
#' \item \code{detail} OS or version details
#'
#' }
#' @importFrom here here
#' @importFrom utils packageVersion sessionInfo
#' @importFrom desc desc
#' @export
vt_scrape_val_env <- function(pkg = here()){
  # cannot use roxygen2::: due to cran checks (::: calls generate note)
  # cannot use here::here due to cran check for valtools
  #  - here::here() maps to valtools.Rcheck/tests/ which does not include DESCRIPTION
  #  - must switch to valtools.Rcheck/pkgname
  desc <- desc(file.path(pkg, "DESCRIPTION"))

  # use trycatch b/c not all packages have all Depends, Imports & Suggests
  e <- function(cond) {
    character()
  }

  depends <- tryCatch({
    gsub("\n", "", trimws(strsplit(desc$get_field("Depends"), split = ",")[[1]]))
  },
  error = e)

  imports <- tryCatch({
    gsub("\n", "", trimws(strsplit(desc$get_field("Imports"), split = ",")[[1]]))
  },
  error = e)

  suggests <- tryCatch({
    gsub("\n", "", trimws(strsplit(desc$get_field("Suggests"), split = ",")[[1]]))
  },
  error = e)


  val_env <- data.frame(
    resource = gsub(c(depends, imports, suggests), pattern = "\\s\\(.*\\)", replacement = ""),
    type = c(rep("package_req", length(c(depends, imports))),
             rep("extended_req", length(suggests)))
  )
  # Some packages will specify a R version dependency, otherwise add this row if needed
  if("R" %in% val_env$resource){
    val_env[val_env$resource == "R", "type"] <- "system"

  } else {
    val_env <- rbind(val_env, data.frame(resource = "R", type = "system"))
  }

  val_env$detail  <- sapply(val_env$resource, FUN = function(x){
    return(if(x == "R"){
      gsub(R.version.string, pattern = "R\\sversion\\s(.*)\\s.*", replacement = "\\1")
    } else {
      as.character(packageVersion(x))
    })
  })

  val_env <- rbind(val_env, data.frame(resource = "OS", type = "system", detail = sessionInfo()[["running"]]))

  session_pkg <- names(sessionInfo()[["otherPkgs"]])
  session_pkg <- session_pkg[!session_pkg %in%
                               c(desc$get_field("Package"),
                                 val_env[val_env$type %in% c("package_req", "extended_req"), "resource"])]
  if(length(session_pkg) > 0){
    val_env <- rbind(val_env,
                     data.frame(resource = session_pkg,
                                type = "session",
                                detail = unname(sapply(session_pkg,
                                                FUN = function(.x){as.character(packageVersion(.x))}))))
  }
  val_env$type <- factor(val_env$type, levels = c("system", "package_req", "extended_req", "session"))
  val_env[] <- lapply(val_env[order(val_env$type, val_env$resource),], as.character)
  val_env
}

