#' @param references dynamic reference holder if it already exists
#' @param src,ref passed to \code{\link{vt_scrape_tags_from}}
#' @export
vt_scrape_coverage_matrix <- function(references = NULL, src = ".", ref = vt_path()){
  #TODO some code to tease out what are the test_case files.
  # only test case have @coverage, maybe trycatch will be enough?

  cov_raw_values <- sapply(vt_scrape_tags_from(type = "test_cases", tags = "coverage"),
                           FUN = function(x){x$coverage})

  if(is.null(references)){
    references <- vt_dynamic_referencer$new()
  }
  references$scrape_references(cov_raw_values)
  numbered_cov_vals <- as.data.frame(do.call("rbind", lapply(
    unlist(lapply(references$reference_insertion(cov_raw_values),
            strsplit, split = "\n")),
    FUN = function(x){
      this_row <- strsplit(x, split = ":")[[1]]
      names(this_row) <- c("req_id", "tc_id")
      this_row["tc_id"] <- trimws(this_row["tc_id"])

      this_row

    })))
  numbered_cov_vals <- numbered_cov_vals[order(numbered_cov_vals$req_id),]
  row.names(numbered_cov_vals) <- 1:nrow(numbered_cov_vals)
  numbered_cov_vals
}
