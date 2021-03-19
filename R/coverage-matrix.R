#' Scrape "coverage" tag in test code to generate mapping
#' @param references dynamic reference holder if it already exists
#' @param src,ref passed to \code{\link{vt_scrape_tags_from}}
#' @param type one of "long" or "wide" which determines shape of output table
#' @return a data.frame
#' @importFrom rlang list2 := !!
#' @export
vt_scrape_coverage_matrix <- function(type = c("long", "wide"), references = NULL, src = ".", ref = vt_path()){

  ## instantiate dynamic referencing
  ## if no dynamic reference used, this won't get populated
  if(is.null(references)){
    references <- vt_dynamic_referencer$new()
  }

  ## helper functions

  split_vals <- function(x){
    this_row <- strsplit(x, split = ":")[[1]]
    names(this_row) <- c("req_id", "tc_id")
    this_row["tc_id"] <- trimws(this_row["tc_id"])

    this_row
  }

  split_tc <- function(vals){
    out <- do.call("rbind", apply(vals, 1, FUN = function(x){
      data.frame(req_id = x[["req_id"]],
                  tc_id = strsplit(x[["tc_id"]], ", ")[[1]],
                  check.rows = FALSE)
    }))
    row.names(out) <- 1:nrow(out)
    out <- out[order(out$req_id),]
    out
  }



  # avoids dependency on tidyr::pivot_wider
  make_wider <- function(long_vals){
    list_x <- apply(long_vals, 1, FUN = function(x){
      as.data.frame(list2(req_id = x[["req_id"]],
                          !!x[["tc_id"]] := "x"),
                    check.names = FALSE)
    })

    all_names <- unique(unlist(lapply(list_x, names)))
    list_all_x <- lapply(list_x, function(x){
      x[setdiff(all_names, names(x))] <- ""
      x
    })
    out <- do.call("rbind", list_all_x)
    row.names(out) <- 1:nrow(out)
    out[, c("req_id", sort(names(out)[-1]))]
  }


  ## end helper functions

  cov_raw_values <- sapply(vt_scrape_tags_from(type = "test_cases", tags = "coverage",
                                               src = src, ref = vt_path()),
                           FUN = function(x){x$coverage})
  references$scrape_references(cov_raw_values)
  indiv_vals <- unlist(lapply(references$reference_insertion(cov_raw_values),
                              strsplit, split = "\n"))

  numbered_cov_vals <- as.data.frame(do.call("rbind", lapply(indiv_vals,
    FUN = split_vals)))

  if(type[1] == "long"){
    numbered_cov_vals <- numbered_cov_vals[order(numbered_cov_vals$req_id),]
    row.names(numbered_cov_vals) <- 1:nrow(numbered_cov_vals)
    out_data <- numbered_cov_vals
  } else if(type[1] == "wide"){
    new_vals <- split_tc(numbered_cov_vals)
    out_data <- make_wider(new_vals)
  }
  out_data
}
