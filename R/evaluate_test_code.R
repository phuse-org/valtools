#' Evaluate the test code file
#' @param file full path to test code file.
#' @param test_env environment to perform tests in
#' @param ... argument passed to `knitr::kable()`
#' @param ref reference path to use. Defaults to vt_path()
#' @return a kable with variables: \code{Test}, \code{Expected}, \code{Results},
#' \code{Pass/Fail}. Suitable for including in validation report
#' @export
#' @importFrom testthat capture_output Reporter
#' @importFrom knitr kable
#'
#' @rdname eval_test_code
vt_run_test_code_file <- function(file, test_env  = new.env(), ..., ref = vt_path()){

  test_results <- eval_test_code(path = file.path(ref, "test_code",file), test_env = test_env)

  return(test_results)
}

#' Turn test code results data.frame into kable output
#'
#' @param results results data.frame from `vt_run_test_code_file()`
#' @param format  passed to \code{knitr::kable}
#'
#' @returns kableExtra object with formatting
#'
#' @importFrom knitr kable
#' @importFrom kableExtra column_spec kable_styling cell_spec kable_styling
#' @importFrom rlang abort
#'
#' @export
#'
#' @rdname eval_test_code
vt_kable_test_code_results <- function(results, format = vt_render_to()) {
  ## check column names
  if (!all(c("Test", "Results", "Pass_Fail") %in% colnames(results))) {
    abort("Results data must contain the fields `Test`, `Results`, and `Pass_Fail`")
  }

  rownames(results) <- NULL

  Pass_Fail_colorized <- NULL

  if( nrow(results) > 0 & any(results$Pass_Fail %in% c("Pass", "Fail", "Skip"))) {

    Pass_Fail_colorized <- ifelse(
      results$Pass_Fail == "Pass", "#006400",
      ifelse(
        results$Pass_Fail == "Skip", "#FFC800",
        ifelse(
          results$Pass_Fail == "Fail", "#FF0000",
          "black"
        )
      ))
  }

  x <- results[, c("Test", "Results", "Pass_Fail")]
  colnames(x) <- c("Test", "Results", "Pass/Fail")
  if (nrow(x) > 0) {
    x$Results <- gsub("\n","", gsub("\a|\033","", x$Results))
  }

  t <- kable(x, format = format, longtable = TRUE)

  if(nrow(results) > 0){
    t <- column_spec(t, 2:3, width = "10em")
  }

  t <- kable_styling(t, position = "center")
  if(!is.null(Pass_Fail_colorized)){
  t <- column_spec(
    t,
    3,
    color = Pass_Fail_colorized
  )
  }

  t <- kable_styling(t, latex_options = "hold_position")

  return(t)
}


#' @importFrom testthat capture_output Reporter test_file
#' @importFrom rlang warn
#' @noRd
eval_test_code <- function(path, test_env = new.env()) {

  # Get the testthat results (via reporter)
  # Used for obtaining the Pass/Fail variable
  output <- testthat::test_file(path , reporter = testthat::Reporter,env = test_env)
  results <- lapply(output, `[[`, "results")

  if(length(results) == 0){
    warn(paste0("File `",path,"` did not have any tests included."))

    return(data.frame(
        Test = character(),
        Results = character(),
        Pass_Fail = character(),
        stringsAsFactors = FALSE
      ))
  }else{
  # For each instance of test_that in test code file, get the test case name,
  #    expected results, observed results and Pass/Fail designation

  do.call('rbind',
          lapply(seq_along(results),
                 function(i) {
                   test_case_results <-
                     do.call(rbind, lapply(seq_along(results[[i]]), function(x) {
                       outcome <- results[[i]][[x]]

                       expectation_outcome <- ""

                       if(inherits(outcome,"expectation_success")){
                         expectation_outcome <- "Pass"
                       }else if(inherits(outcome,"expectation_skip")){
                         expectation_outcome <- "Skip"
                       }else{
                         expectation_outcome <- "Fail"
                       }

                       data.frame(
                         Test = outcome$test,
                         Results = as.character(format(outcome)),
                         Pass_Fail = expectation_outcome,
                         stringsAsFactors = FALSE
                       )

                     }))

                   if(nrow(test_case_results) > 0){
                     test_case_results$Test <-
                       paste0(test_case_results$Test,
                              ".",
                              seq_len(nrow(test_case_results)))
                   }

                   return(test_case_results)
                 }))
  }

}


