#' @title Evaluate the test code
#' @param one_file full path to test code file.
#' @param test_env environment to perform tests in
#' @return a table with variables: \code{Test}, \code{Expected}, \code{Results},
#' \code{Pass/Fail}. Suitable for including in validation report
#' @export
#' @importFrom testthat capture_output Reporter
#' @importFrom knitr kable
#' @importFrom kableExtra column_spec kable_styling
eval_test_code <- function(one_file, test_env = new.env()) {

  # Get the testthat reults (via reporter)
  # Used for obtaining the Pass/Fail variable
  output <- testthat::test_file(one_file , reporter = Reporter,env = test_env)
  results <- lapply(output, `[[`, "results")

  # For each instance of test_that in test code file, get the test case name,
  #    expected results, observed results and Pass/Fail designation
  out <-
    do.call(rbind, lapply(
      seq_along(results),
      function(i) {
        all_results <-
          do.call(rbind, lapply(seq_along(results[[i]]), function(x) {

            res <- results[[i]][[x]]
            pass_fail <- ifelse(
              inherits(res, "expectation_success"),
              "\\shortstack{\\textcolor{OliveGreen}{Pass}}",
              "\\shortstack{\\textcolor{red}{Fail}}"
            )

            data.frame(
              Test = res$test,
              Results = capture_output(testthat:::print.expectation(res)),
              `Pass/Fail` = pass_fail,
              stringsAsFactors = FALSE
            )
          }))

        if (is.null(all_results)) {
          all_results <- data.frame()
        } else {
          all_results$Test <- paste0(all_results$Test, ".", 1:nrow(all_results))
        }

        return(all_results)
      }
    ))

  # formatting
  rownames(out) <- NULL
  knitr::kable(
    out,
    format = "latex",
    escape = FALSE,
    col.names = c("Test", "Results", "Pass/Fail")
  ) %>%
    kableExtra::column_spec(2:3, width = "10em") %>%
    kableExtra::kable_styling(position = "center")
}
