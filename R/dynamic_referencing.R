#' Manage dynamic refrerencing
#'
#' The job of the dynamic referencer is to aggregate the references that exist from
#' specifications, test cases, test code and in the validation report and
#' update them accordingly. This helps with the painful cases where a new reference
#' may be added between existing numbering and all subsequent references need to be
#' updated
#'
#' The user should not need to use this object directly, it will be called upon
#' on rendering of the contents in the validation report.
#'
#' By default, the expected indicator for a reference is "@@", though this can be
#' changed. A reference starts with the indicator (@@) and can then be any
#' contiguous (no white spaces or special characters) alphanumeric sequence, or include
#' an underscore or dash. ie. @@THISIS_A-reference1234 is valid for the entire string,
#' but @@THISIS_A-reference.12345 is a dynamic reference up to the ".".
#'
#' The method 'scrape_references' takes in a vector of strings, and an indicator whether
#' the input file text is a specification ('spec'), test case ('test_case') or
#' test code ('test_code'). This allows numbering to increase independently for each.
#' However, every reference must be unique. ie. @@reference whether it shows up in a
#' test case or spec will be the same reference.
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
          #' @param type type of file being converted; a specification ('spec'), test case ('test_case') or
          #' test code ('test_code')
          #' @examples
          #' ref <- vt_dynamic_referencer$new()
          #' ref$list_references()
          #' ref$scrape_references("@@new_reference")
          #' ref$list_references()

          scrape_references = function(text){

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

                if(type %in% c("spec","test_code","test_case")){
                  private$add_reference(new_ref, type)
                }else{
                  abort(
                    paste0(
                      "Invalid reference at: ", private$reference_indicator,new_ref,"\n",
                      "Reference type indicator can only be one of: `spec`,`test_case`,`test_code`!"
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
          #' ref$scrape_references("@@new_reference")
          #' ref$list_references()
          #' ref$reference_insertion("This is my @@new_reference")
          reference_insertion = function(text){

            ## ensure ordering to try to prevent accdentally replacing strings that are subs of the other
            ## ie repl and repl_value
            references <- private$references[order(nchar(names(private$references)),decreasing = TRUE)]

            for(ref in names(references)){
              ref_value <- references[[ref]]
              text = gsub(
                pattern = paste0(private$reference_indicator,ref),
                replacement = ref_value,
                text,
                fixed = TRUE,
              )
            }

            text

          },

          #' @description
          #' list references available and their value
          #' @examples
          #' ref <- vt_dynamic_referencer$new()
          #' ref$list_references()
          #' ref$scrape_references("@@new_reference")
          #' ref$list_references()
          list_references = function(){
            private$references
          },

          #' @description
          #' create a new dynamic reference object
          #' @param reference_indicator character vector that indicates the start of the dynamic references. defaults to "@@type:reference"
          #' @return a new `vt_dynamic_reference` object
          initialize = function(reference_indicator = "@@"){
            private$references <- list()
            private$ref_iter_number <- list(
              spec = 0,
              test_case = 0,
              test_code = 0
            )

            private$reference_indicator <- reference_indicator
            private$reference_indicator_regex <- paste0("\\",strsplit(reference_indicator,"")[[1]], collapse = "")
          }
          ),

        private = list(
          references = list(),
          ref_iter_number = list(
            spec = 0,
            test_case = 0,
            test_code = 0
          ),
          reference_indicator = NULL,
          reference_indicator_regex = NULL,
          advance_reference = function( type = c("spec","test_case","test_code")){
            type <- match.arg(type)
            private$ref_iter_number[[type]] <- private$ref_iter_number[[type]] + 1
            private$ref_iter_number[[type]]
          },
          add_reference = function(reference_id, type = c("spec","test_case","test_code")){
            type <- match.arg(type)
            reference_value = private$advance_reference(type)
            private$references[[reference_id]] <- reference_value
          }
        ))

##
dynamic_referencer <- vt_dynamic_referencer$new(
  reference_indicator = "@@"
)



#' @title Dynamic Reference Rendering
#' @description enable dynamic referencing by reading file and converting any dynamic references
#' into their values for rendering in the validation report.
#' @param file path to the file to convert dynamic referencing to values
#' @param reference which dynamic referencer to use. When NULL, uses internal dynamic referencer.
#' @return text of file with dynamic referencing evaluated
#' @export
dynamic_reference_rendering <- function(file, reference = NULL){
  file_text <- readLines(file)

  if(is.null(reference)){
    reference <- dynamic_referencer
  }

  reference$scrape_references(file_text)
  reference$reference_insertion(file_text)

}

