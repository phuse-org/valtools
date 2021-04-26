#' Scrape "riskAssessment" tag in requirements to generate table
#' @param reference dynamic reference holder if it already exists
#' @param src,ref passed to \code{\link{vt_scrape_tags_from}}
#' @return a data.frame documenting requirements to risk assessments
#' @export
#'
#' @rdname scrape
vt_scrape_risk_assessment <- function(reference = NULL, src = ".", ref = vt_path()){

  ## end helper functions
  cov_raw_values <-vt_scrape_tags_from(type = "requirements", tags =  c("title", "riskAssessment"),
                                               src = src, ref = vt_path())


  Risk <- do.call("rbind", lapply(cov_raw_values, FUN = function(x){

    assessment_split <- strsplit(x[["riskAssessment"]], split = "\n")[[1]]

    assessment_split_ref <- dynamic_reference_rendering(assessment_split, reference = reference)

    req_assessment_split <- strsplit(assessment_split_ref, split = ":")

    riskAssessment <- do.call('rbind',lapply(req_assessment_split, function(x){
      data.frame(
        Requirement = trimws(x[[1]]),
        Assessment = trimws(x[[2]]),
        check.names = FALSE,
        stringsAsFactors = FALSE)
    }))

    data.frame(Title = x[["title"]],
               riskAssessment, check.names = FALSE,
               stringsAsFactors = FALSE)
  }))

  Risk

}

#' Kable handler for output of \code{\link{vt_scrape_risk_assessment}}
#' @param x data.frame as output from \code{\link{vt_scrape_risk_assessment}}
#' @param format passed to \code{kable}
#' @return knitr_kable object
#' @importFrom knitr kable
#' @importFrom kableExtra collapse_rows column_spec
#' @export
#'
#' @rdname scrape
#'
vt_kable_risk_assessment <- function(x, format = NULL){
  t <- kable(x,
        format = format,
        longtable =  TRUE,
        col.names = c("Requirement Name", "Requirement ID", "Risk Assessment") )
  t <- column_spec(t, 1, border_right = TRUE)
  t <- collapse_rows(t, c(1))
  t
}


