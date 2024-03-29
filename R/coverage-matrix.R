#' Scrape "coverage" tag in test code to generate mapping
#' @param reference dynamic reference holder if it already exists
#' @param src,ref passed to \code{\link{vt_scrape_tags_from}}
#' @param type one of "long" or "wide" which determines shape of output table
#' @return a data.frame mapping requirement ids to test case ids.
#' @importFrom rlang list2 := !!
#' @export
vt_scrape_coverage_matrix <- function(type = c("long", "wide"),
                                      reference = NULL, src = ".", ref = vt_path()){

  ## helper functions
  split_vals <- function(vals){
    do.call("rbind", apply(vals, 1, FUN = function(x){

      this_row <- strsplit(x[["coverage"]], split = ":")[[1]]

      if(length(this_row) != 2){
        rlang::abort(paste0("Coverage details must follow format Test_Case:Requirement.",
                           " See ", x[["tc_title"]], ", Coverage Entry: ",trimws(x[["coverage"]])),
                     class = "vt.coverage_format")
      }
      names(this_row) <- c("tc_id", "req_id")
      this_row["tc_id"] <- trimws(this_row["tc_id"])

      this_row["tc_title"] <- x["tc_title"]
      this_row["deprecate"] <- x["deprecate"]
      as.data.frame(t(this_row), stringsAsFactors = FALSE)
    }))
  }

  split_req <- function(vals){
    out <- do.call("rbind", apply(vals, 1, FUN = function(x){


      req_one_row <- data.frame(tc_title = x[["tc_title"]],
                                tc_id = x[["tc_id"]],
                                req_id = trimws(strsplit(trimws(x[["req_id"]]), split = ", ")[[1]]),
                                deprecate = x[["deprecate"]],
                                stringsAsFactors = FALSE)
      

      
      req_one_row$req_title <- paste0("Requirement ", gsub(req_one_row$req_id,
                                                           pattern = "^(\\d+)\\.*.*",
                                                           replacement = "\\1"))
    
      req_one_row
    }))

    # req_title uses only first numeric position
    out$req_title <- factor(out$req_title,
                            levels = paste0("Requirement ", 
                                            sort(as.numeric(unique(unlist(lapply(strsplit(out$req_title, split = " "), 
                                                        function(x){x[2]})))))))
    out[order(out$req_title),]
    
  }

  # avoids dependency on tidyr::pivot_wider
  make_wider <- function(long_vals){
    list_x <- apply(long_vals, 1, FUN = function(x){

      out <- as.data.frame(list2(req_id = x[["req_id"]],
                          req_title = x[["req_title"]],
                          !!x[["tc_id"]] := "x"),
                    check.names = FALSE,
                    stringsAsFactors = FALSE)
      out
    })

    all_names <- unique(unlist(lapply(list_x, names)))
    list_all_x <- lapply(list_x, function(x){
      x[setdiff(all_names, names(x))] <- ""
      x
    })
    out <- do.call("rbind", list_all_x)

    row.names(out) <- 1:nrow(out)
    out[, c("req_title", "req_id", sort(names(out)[-1:-2]))]
  }


  ## end helper functions

  cov_raw_values <- do.call("rbind", vt_scrape_tags_from(type = "test_cases", tags =  c("title", "coverage", "deprecate"),
                                               src = src, ref = vt_path()))[, c("title", "coverage", "deprecate")]
  indiv_vals <- do.call("rbind", apply(cov_raw_values, 1, FUN = function(x){

    data.frame(tc_title = x[["title"]],
               coverage = strsplit(trimws(x[["coverage"]]), split = "\n")[[1]], check.names = FALSE,
               deprecate = x[["deprecate"]],
               stringsAsFactors = FALSE)

  }))
  indiv_vals$deprecate[is.na(indiv_vals$deprecate)] <- ""
  vals_title <- dynamic_reference_rendering(indiv_vals[!is.na(indiv_vals$coverage),],
                                            reference = reference)

  numbered_cov_vals <- split_vals(vals_title)


  vals_all <- split_req(numbered_cov_vals)

  if(type[1] == "long"){
    out_data <- vals_all[order(vals_all$req_title),]
    row.names(out_data) <- 1:nrow(out_data)
    out_data <- out_data[, c("req_title", "req_id", "tc_title", "tc_id", "deprecate")]
    attr(out_data, "table_type") <- "long"
  } else if(type[1] == "wide"){

    out_data <- make_wider(vals_all)
    attr(out_data, "table_type") <- "wide"
    this_tc_title <- unique(vals_all[,c("tc_id", "tc_title", "deprecate")])
    this_tc_title <- this_tc_title[order(this_tc_title$tc_id),]
    row.names(this_tc_title) <- 1:nrow(this_tc_title)
    attr(out_data, "tc_title") <- this_tc_title


  }
  out_data
}

#' Kable handler for output of \code{\link{vt_scrape_coverage_matrix}}
#' @param x data.frame as output from \code{\link{vt_scrape_coverage_matrix}}
#' @param format passed to \code{kable}
#' @return knitr_kable object
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling collapse_rows add_header_above
#' @export
vt_kable_coverage_matrix <- function(x, format = vt_render_to()){
  switch(attr(x, "table_type"),
         "long" = kable_cov_matrix_long(x, format = format),
         "wide" = kable_cov_matrix_wide(x, format = format))

}


kable_cov_matrix_long <- function(x, format = vt_render_to()){
  this_col_names <- c("Requirement Name", "Requirement ID", "Test Case Name", "Test Cases")
  if(all(x$deprecate == "")){
    x <- x[,-which(names(x) == "deprecate")]
  } else {
    this_col_names <- c(this_col_names, "Comment")
  }
  out_tab <- kable(x,
        format = format,
        longtable =  TRUE,
        col.names = this_col_names )
  out_tab <- kable_styling(out_tab, font_size = 6)
  out_tab <- column_spec(out_tab, 1, border_left = TRUE)
  out_tab <- column_spec(out_tab, ncol(x), border_right = TRUE)
  out_tab <- collapse_rows(out_tab, c(1, 3))
  out_tab
}



kable_cov_matrix_wide<- function(x, format = vt_render_to()){
  this_tc_title <- attr(x, "tc_title")

  lapply(split(this_tc_title, this_tc_title$tc_id),
         FUN = function(x){
           if(nrow(x) > 1){
             abort(paste0("Multiple test cases mapped to single test code identifier in wide coverage table - ",
                          unique(x$tc_id), ". Filter for '",
                          paste(x$tc_title, collapse = "' or '"),
                          "' before running 'vt_kable_coverage_matrix()'."),
                   class = "vt.coverage_matrix_kable")
           }
         })
  # enforce consistent ordering with object
  this_tc_title <- this_tc_title[this_tc_title$tc_id %in% names(x)[!names(x) %in% c("req_title", "req_id", "tc_title")],]

  this_header_info <- data.frame(count = sapply(unique(this_tc_title$tc_title),
                                                 function(y){nrow(this_tc_title[this_tc_title$tc_title == y,])}),
                                 tc_title = unique(this_tc_title$tc_title),
                                 stringsAsFactors = FALSE)
  rownames(this_header_info) <- NULL
  this_final_header <- c(2, this_header_info$count)
  names(this_final_header) <- c(" ", paste(unique(this_header_info$tc_title)))

  out_tab <- kable(x,
                   format = format,
                   booktabs = TRUE,
                   col.names = c("", "", colnames(x)[-1:-2]),
                   align = c("l", "l", rep("c", ncol(x) - 2)))
  out_tab <- kable_styling(out_tab,
                           font_size = 7)
  out_tab <- add_header_above(out_tab,
                              this_final_header)
  if(nrow(x) > 1){
    out_tab <- collapse_rows(out_tab, 1)
  }

  out_tab
}
