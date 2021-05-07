#' Manage dynamic refrerencing
#'
#' The job of the dynamic referencer is to aggregate the references that exist from
#' requirements, test cases, test code and in the validation report and
#' update them accordingly. This helps with the painful cases where a new reference
#' may be added between existing numbering and all subsequent references need to be
#' updated
#'
#' The user should not need to use this object directly, it will be called upon
#' on rendering of the contents in the validation report.
#'
#' By default, the expected indicator for a reference is "##", though this can be
#' changed. A reference starts with the indicator (##) and can then be any
#' contiguous (no white spaces or special characters) alphanumeric sequence, or include
#' an underscore or dash. ie. ##req:THISIS_A-reference1234 is valid for the entire string,
#' but ##req:THISIS_A-reference.12345 is a dynamic reference up to the ".".
#'
#' The method 'scrape_references' takes in a vector of strings, and an indicator whether
#' the input file text is a requirement ('req'), test case or
#' test code ('tc'). This allows numbering to increase independently for each.
#' However, every reference must be unique. ie. ##reference whether it shows up in a
#' test case or requirement will be the same reference.
#'
#' The method 'reference_insertion' takes a vector of strings and replaces references
#' with their numeric values.
#'
#'
#' @keywords internal
#' @export vt_dynamic_referencer
#' @aliases Dynamic Referencing
#' @importFrom R6 R6Class
#' @importFrom rlang abort
#' @examples
#'
#' reference <- vt_dynamic_referencer$new()
#' reference
#'
#'
vt_dynamic_referencer <- R6::R6Class("vt_dynamic_referencer",
        public = list(
          #' @description
          #' collect references from text.
          #' @param text character vector to collect references from.
          #' @param type type of file being converted; a requirement ('req'),
          #' test case  or test code ('tc')
          #' @examples
          #' ref <- vt_dynamic_referencer$new()
          #' ref$list_references()
          #' ref$scrape_references("##req:new_reference")
          #' ref$list_references()

          scrape_references = function(text){

            ## Drop roxygen comment headers from text for scraping references.
            text <- unname(unlist(text[!grepl("^#'", text)]))

            reference_locations <-
              gregexpr(
                paste0(
                  "(?:(",
                  private$reference_indicator_regex,
                  ")",
                  "(([[:alnum:]]|[_]|[-])*:(([[:alnum:]]|[_]|[-])*)))"
                ),
                text,
                perl = TRUE)

            references <- c()

            for( line_idx in seq_along(reference_locations)){

              reference_location <- reference_locations[[line_idx]]

              if(reference_location[[1]] != -1){
                start_match <- as.numeric(reference_location)
                end_match <- start_match + attr(reference_location, "match.length") -1

                for(matching in seq_along(start_match)){

                  ref <- gsub(
                    pattern = private$reference_indicator,
                    replacement = "",
                    substr(text[[line_idx]],
                           start_match[[matching]],
                           end_match[[matching]]),
                    fixed = TRUE)

                  references <- c(references,ref)
                }
              }
            }

            references <- unique(references)

            new_references <- setdiff(references,names(private$references))

            if(length(new_references) > 0){
              for(new_ref in new_references){

                type <- strsplit(new_ref, ":")[[1]][1]

                if(type %in% c("req","tc")){
                  private$add_reference(new_ref, type)
                }else{
                  abort(
                    paste0(
                      "Invalid reference at: ", private$reference_indicator,new_ref,"\n",
                      "Reference type indicator can only be one of: `req` or `tc`!"
                      )
                  )
                }
              }
            }

            invisible()
          },

          #' @description
          #' replace references in text with values
          #' @param text character vector to be inserting references into
          #' @examples
          #' ref <- vt_dynamic_referencer$new()
          #' ref$list_references()
          #' ref$scrape_references("##new_reference")
          #' ref$list_references()
          #' ref$reference_insertion("This is my ##new_reference")
          reference_insertion = function(text){

            ## ensure ordering to try to prevent accdentally replacing strings that are subs of the other
            ## ie repl and repl_value
            references <- private$references[order(nchar(names(private$references)),decreasing = TRUE)]

            for(ref in names(references)){
              ref_value <- references[[ref]]

              if("data.frame" %in% class(text)){
                text <- as.data.frame(lapply(text, gsub,
                                             pattern = paste0(private$reference_indicator, ref),
                                             replacement = ref_value,
                                             fixed = TRUE),
                                      stringsAsFactors = FALSE)
              } else {
                text <- gsub(
                  pattern = paste0(private$reference_indicator,ref),
                  replacement = ref_value,
                  text,
                  fixed = TRUE,
                )
              }


            }

            text

          },

          #' @description
          #' list references available and their value
          #' @examples
          #' ref <- vt_dynamic_referencer$new()
          #' ref$list_references()
          #' ref$scrape_references("##new_reference")
          #' ref$list_references()
          list_references = function(){
            private$references
          },

          #' @description
          #' create a new dynamic reference object
          #' @param reference_indicator character vector that indicates the start of the dynamic references. defaults to "##type:reference"
          #' @param type "number" (arabic) or "letter" (latin uppercase) to use for counters
          #' @return a new `vt_dynamic_reference` object
          initialize = function(reference_indicator = "##", type = c("number","letter")){

            private$references <- list()

            private$ref_iter_number <- list(
              req = 0,
              tc = 0
            )

            private$type <- match.arg(type)

            private$reference_indicator <- reference_indicator
            private$reference_indicator_regex <- paste0("\\",strsplit(reference_indicator,"")[[1]], collapse = "")
          }
          ),

        private = list(
          type = NULL,
          references = list(),
          ref_iter_number = list(
            req = 0,
            test_case = 0,
            test_code = 0
          ),
          reference_indicator = NULL,
          reference_indicator_regex = NULL,
          advance_reference = function( type = c("req","tc")){
            type <- match.arg(type)
            private$ref_iter_number[[type]] <- private$ref_iter_number[[type]] + 1
            private$ref_iter_number[[type]]
          },
          add_reference = function(reference_id, type = c("req","tc")){
            type <- match.arg(type)
            reference_value = private$advance_reference(type)
            if(private$type == "letter"){
              reference_value <- numeric_to_letter_ref(reference_value)
            }
            private$references[[reference_id]] <- reference_value
          }
        ))

##
dynamic_referencer <- vt_dynamic_referencer$new(
  reference_indicator = "##"
)



#' @title Dynamic Reference Rendering
#' @description enable dynamic referencing by reading file and converting any dynamic references
#' into their values for rendering in the validation report.
#' @param input R object or path to the file to convert dynamic referencing to values
#' @param reference which dynamic referencer to use. When NULL, uses internal dynamic referencer.
#' @return text with dynamic referencing evaluated
#' @importFrom rlang inform
dynamic_reference_rendering <- function(input, reference = NULL){
  file_text <- tryCatch({
    if(is_char_scalar(input)){
      if(file.exists(input)){
        file_text <- readLines(input)
        inform(message = paste0("Reading input from file: ", input),
               class = "vt.dynamic_ref_readlines")
        file_text
      }else{
        input
      }
    }else{
      input
    }
  },
  error = function(e){
    input
  })

  if(is.null(reference)){
    reference <- dynamic_referencer
  }

  reference$scrape_references(file_text)
  reference$reference_insertion(file_text)

}


is_char_scalar <- function(x){
  is.character(x) && length(x) == 1
}

## original method from:
## https://stackoverflow.com/questions/181596

numeric_to_letter_ref <- function(x){
  col_name <- c()

  while(x > 0){
    modulo <- (x) %% 26
    col_name <- c(LETTERS[modulo], col_name)
    x <- as.integer( (x - modulo) / 26)
  }

  paste(col_name,collapse = "")

}

