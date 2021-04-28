#' @title Scrape authorship information
#'
#' @description These functions provide utitilies to scrape the editor and editDate roxygen tags
#' and put them into a nice data.frame for use in the validation reports. In addition,
#' opinionated kable formatting functions are provided as well to facilitate nice
#' printing in the reports.
#'
#' @param tags which tags to keep. defaults to editor and editDate
#' @param src path to package sources. defaults to current directory and passed to \code{\link{vt_scrape_tags_from}}
#' @param ref reference path to whre validation documentation lives. defaults
#' to \code{\link{vt_path}} annd passed to \code{\link{vt_scrape_tags_from}}.
#' @param dynamic_ref dynamic referencer object
#' @return data.frame containing the results of the scraped roxygen tags for each section
#'
#' @examples
#'
#' withr::with_tempdir({
#'
#' captured_output <- capture.output({vt_create_package(open = FALSE)})
#' vt_use_req(
#'      name = "req1",
#'      username = "B user",
#'      title = "Requirement 1",
#'      open = FALSE)
#' writeLines(c(
#'     "#' @title Say Hello",
#'     "#' @editor B User",
#'     "#' @editDate 2021-04-27",
#'     "#' @export",
#'     "hello <- function(){print(\"Hello\")}"
#'     ), con = "R/hello.R")
#' vt_use_test_case(
#'     name = "testcase1",
#'     username = "B user",
#'     title = "TesT Case 1",
#'     open = FALSE)
#' vt_use_test_code(
#'     name = "testcode1",
#'     username = "C user",
#'     open = FALSE)
#'
#' req_editors <- vt_scrape_requirement_editors()
#' vt_kable_requirement_editors(req_editors)
#'
#' fun_editors <- vt_scrape_function_editors()
#' vt_kable_function_editors(fun_editors)
#'
#' t_case_editors <- vt_scrape_test_case_editors()
#' vt_kable_test_case_editors(t_case_editors)
#'
#' t_code_editors <- vt_scrape_test_code_editors()
#' vt_kable_test_code_editors(t_code_editors)
#'
#'
#' })
#'
#' @export
#'
#' @rdname scraping
vt_scrape_requirement_editors <- function(tags = c("editor", "editDate"), src = ".", ref = vt_path(),
                                   dynamic_ref = NULL){
  out <- do.call("rbind", vt_scrape_tags_from(
    type = "requirements",
    tags = tags,
    src = src,
    ref = ref
  ))

  if(!is.null(dynamic_ref)){
    dynamic_ref$scrape_references(out)
    out <- dynamic_ref$reference_insertion(out)
  }

  out
}

#' @export
#' @rdname scraping
vt_scrape_test_case_editors <- function(tags = c("editor", "editDate"), src = ".", ref = vt_path(),
                                   dynamic_ref = NULL){
  out <- do.call("rbind", vt_scrape_tags_from(
    type = "test_cases",
    tags = tags,
    src = src,
    ref = ref
  ))

  if(!is.null(dynamic_ref)){
    dynamic_ref$scrape_references(out)
    out <- dynamic_ref$reference_insertion(out)
  }

  out
}

#' @export
#' @rdname scraping
vt_scrape_test_code_editors <- function(tags = c("editor", "editDate"), src = ".", ref = vt_path(),
                                 dynamic_ref = NULL){
  out <- do.call("rbind", vt_scrape_tags_from(
    type = "test_code",
    tags = tags,
    src = src,
    ref = ref
  ))

  if(!is.null(dynamic_ref)){
    dynamic_ref$scrape_references(out)
    out <- dynamic_ref$reference_insertion(out)
  }

  out
}

#' @note vt_scrape_functions Requires access to raw R/ or function documentation parsed via {valtools} into validation/ folder.
#' Cannot pull information from installed R/ location.
#' @export
#' @rdname scraping
vt_scrape_function_editors  <- function(tags = c("editor", "editDate", "export"), src = ".", ref = vt_path()){
  do.call("rbind", vt_scrape_tags_from(
    type = "functions",
    tags = tags,
    src = src,
    ref = ref
  ))
}

#' @param x data.frame as exported from vt_scrape_*
#' @param format passed to \code{knitr::kable}, NULL by default
#' @return knitr_kable object
#' @export
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#' @rdname scraping
#'
vt_kable_requirement_editors  <- function(x, format = NULL){
  x$requirements = paste0("Requirement ", x$requirements)

  all_colnames <- c(requirements = "Requirement ID",
                    editor = "Editor",
                    editDate = "Edit Date")
  t <- kable(x[, names(all_colnames)],
             format = format, booktabs = FALSE,
             col.names = all_colnames)
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, ncol(x), border_right = TRUE)
  t <- kable_styling(t, latex_options = "hold_position")
  t
}

#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra column_spec
#' @rdname scraping
#'
vt_kable_function_editors  <- function(x, format = "latex"){
  all_colnames <- c(functions = "Function Name",
                    editor = "Editor",
                    editDate = "Edit Date",
                    export = "Exported?")
  this_colnames <- all_colnames[names(x)]

  if("export" %in% names(this_colnames)){
    x$export <- ifelse(is.na(x$export), FALSE, TRUE )
  }

  t <- kable(x[, names(this_colnames)], format = format, booktabs = FALSE,
             col.names = unname(this_colnames))
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, ncol(x), border_right = TRUE)
  t <- kable_styling(t, latex_options = "hold_position")
  t
}

#' @export
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#' @rdname scraping
#'
vt_kable_test_case_editors  <- function(x, format = NULL){
  x$test_case = paste0("Test Case", x$test_case)

  all_colnames <- c(test_case = "Test Case ID",
                    editor = "Editor",
                    editDate = "Edit Date")
  t <- kable(x[, names(all_colnames)],
             format = format, booktabs = FALSE,
             col.names = all_colnames)
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, length(all_colnames), border_right = TRUE)
  t <- kable_styling(t, latex_options = "hold_position")
  t
}

#' @export
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#' @rdname scraping
#'
vt_kable_test_code_editors <- function(x, format = NULL){
  x$test_code = paste0("Test Code", x$test_code)

  all_colnames <- c(test_code = "Test Code ID",
                    editor = "Editor",
                    editDate = "Edit Date")
  t <- kable(x[, names(all_colnames)],
             format = format, booktabs = FALSE,
             col.names = all_colnames)
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, ncol(x), border_right = TRUE)
  t <- kable_styling(t, latex_options = "hold_position")
  t
}

