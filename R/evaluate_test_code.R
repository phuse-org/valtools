#' Evaluate the test code file
#' @param path full path to test code file.
#' @param test_env environment to perform tests in
#' @param ... argument passed to `knitr::kable()`
#' @return a kable with variables: \code{Test}, \code{Expected}, \code{Results},
#' \code{Pass/Fail}. Suitable for including in validation report
#' @export
#' @importFrom testthat capture_output Reporter
#' @importFrom knitr kable
vt_run_test_code_file <- function(file, test_env  = new.env(), ..., ref = vt_path()){
  
  test_results <- eval_test_code(path = file.path(ref, "test_code",file), test_env = test_env)
  
  return(kable(test_results, ...))
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
        `Pass/Fail` = character(),
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
                       
                       outcome <- ""
                       
                       if(inherits(outcome,"expectation_success")){
                         outcome <- "Pass"
                       }else if(inherits(outcome,"expectation_skip")){
                         outcome <- "Skip"
                       }else{
                         outcome <- "Fail"
                       }

                       data.frame(
                         Test = outcome$test,
                         Results = capture_output(testthat:::print.expectation(outcome)),
                         `Pass/Fail` = outcome,
                         stringsAsFactors = FALSE
                       )
                       
                     }))
                   
                   if(nrow(test_case_results) > 1){
                     test_case_results$Test <- 
                       paste0(test_case_results$Test, 
                              ".",
                              seq_len(nrow(test_case_results)))
                   }
                   
                   return(test_case_results)
                 }))
  }
  
}


