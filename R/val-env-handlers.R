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
#' @section Last updated by:
#' Marie Vendettuoli
#' @section Last updated date:
#' 2021-02-03
#' @importFrom devtools package_file
#' @importFrom utils packageVersion sessionInfo
#' @importFrom rlang abort
#' @importFrom desc desc
#' @export
vt_scrape_val_env <- function(pkg = "."){
  # cannot use roxygen2::: due to cran checks (::: calls generate note)
  desc <- desc(file.path(package_file(path = pkg), "DESCRIPTION"))
  all_deps <- desc$get_deps()
  depends <- all_deps[all_deps$type == "Depends","package"]
  imports <- all_deps[all_deps$type == "Imports","package"]
  suggests <- all_deps[all_deps$type == "Suggests","package"]


  val_env <- data.frame(
    resource = gsub(c(depends, imports, suggests), pattern = "\\s\\(.*\\)", replacement = ""),
    type = c(rep("package_req", length(c(depends, imports))),
             rep("extended_req", length(suggests))),
    stringsAsFactors = FALSE
  )
  # Some packages will specify a R version dependency, otherwise add this row if needed
  if("R" %in% val_env$resource){
    val_env[val_env$resource == "R", "type"] <- "system"

  } else {
    val_env <- rbind(val_env, data.frame(resource = "R", type = "system", stringsAsFactors = FALSE))
  }

  val_env$detail  <- sapply(val_env$resource, FUN = function(x){
    return(if(x == "R"){
      gsub(R.version.string, pattern = "R\\sversion\\s(.*)\\s.*", replacement = "\\1")
    } else {
      as.character(packageVersion(x))
    })
  })

  val_env <- rbind(val_env, data.frame(resource = "OS", type = "system", detail = sessionInfo()[["running"]], stringsAsFactors = FALSE))

  session_pkg <- names(sessionInfo()[["otherPkgs"]])
  session_pkg <- session_pkg[!session_pkg %in%
                               c(desc$get_field("Package"),
                                 val_env[val_env$type %in% c("package_req", "extended_req"), "resource"])]
  if(length(session_pkg) > 0){
    val_env <- rbind(val_env,
                     data.frame(resource = session_pkg,
                                type = "session",
                                detail = unname(sapply(session_pkg,
                                                FUN = function(.x){
                                                  if(is_installed_package(.x)){
                                                    as.character(packageVersion(.x))
                                                  }else{
                                                    abort(paste0("there is no package called '",.x,"'"),
                                                         class = "vt.missing_package")
                                                  }
                                                    })),
                                stringsAsFactors = FALSE))
  }
  val_env$type <- factor(val_env$type, levels = c("system", "package_req", "extended_req", "session"))
  val_env[] <- lapply(val_env[order(val_env$type, val_env$resource),], as.character)
  val_env
}

#' Generates kable code for validation environment details
#' @param val_env data.frame as output from \code{\link{vt_scrape_val_env}}
#' @param format passed to \code{knitr::kable}
#' @return knitr_kable object
#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra column_spec collapse_rows
vt_kable_val_env <- function(val_env, format = vt_render_to()){
  t <- kable(val_env[,c("type", "resource", "detail")],
             col.names = c("Type", "Resource", "Version Detail"),
             format = format)
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, 2, border_right = TRUE)
  t <- collapse_rows(t, 1)

  t
}

#' @importFrom utils installed.packages
#' @noRd
is_installed_package <- function(pkg){
  pkg %in% rownames(installed.packages())
}
