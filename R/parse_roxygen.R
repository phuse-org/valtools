#' Scrape Roxygen blocks
#'
#' valtools uses roxygen across multiple file types to provide documentation.
#'     this function provides the tooling necessary to scrape from the
#'     major file types that we use ( R, R test code, markdown, Rmarkdown)
#'     and provides a consistent output type to capture the information
#'     necessary to help high level functions make assumptions.
#'
#' @param file file to scrape roxygen block from
#' @param ... These dots are for future extensions and must be empty.
#' @param type method of parse_roxygen to use if other that file extension
#'
#' @returns a list of roxygen blocks found in the file.

vt_scrape_roxygen <- function(file, ..., type = tools::file_ext(file)){

  text <- readLines(file)

  text <- roxy_text(
    text,
    file = file,
    class = tolower(type)
  )

  parse_roxygen(text)

}

write_roxygen_block <- function(roxy_list, path, append = FALSE, only_with_tags = c()){

  if (length(only_with_tags) > 0) {
    roxy_list <- subset_blocks(roxy_list, tags = only_with_tags)
  }

  if (length(roxy_list) > 0) {
    content <- do.call('c', lapply(roxy_list, function(block) {
      tags <- do.call('c', lapply(block$tags, function(tag) {
        if (!is.na(tag$line)) {
          paste0("#' @", tag$tag, " ", gsub("\n", "\n#'", tag$raw))
        }
      }))

      var <-
        paste0(as.character(as.list(block$call)[[2]]), "<- function(){}")

      c(tags, var, "")
    }))


    file_con <- file(description = path,
                     open = ifelse(append, "at", "wt"))

    on.exit(close(file_con))

    writeLines(content,
               sep = "\n",
               con = file_con)
  }

}

subset_blocks <- function(roxy_list, tags){
  do.call('c',lapply(roxy_list, function(block){
    if(block_has_tags(block, tags = tags)){
      return(list(block))
    }
  }))
}

roxy_text <- function(text, file = "text", class){
  structure(
    text,
    file = file,
    class = class
  )
}

roxy_text_file <- function(x){
  attr(x, "file")
}

roxy_text_class <- function(x){
  class(x)
}

#' @noRd
#' @keywords internal
#' @importFrom utils getFromNamespace
parse_roxygen <- function(text){
  type <- class(text)[[1]]
  func <- getFromNamespace(paste0("parse_roxygen.",type), "valtools")
  func(text)
}

#' @importFrom roxygen2 parse_text block_has_tags block_get_tag_value
#' @importFrom rlang abort
#' @noRd
#'
parse_roxygen.r <- function(text){

  roxyblocks <- roxygen2::parse_text(text, env = NULL)

  roxyblocks <- cleanup_section_last_update(roxyblocks)

  ## set "NULL" functions to their title
  roxyblocks <- lapply(roxyblocks, function(block){

    if(is.null(block$call)){

      if(!block_has_tags(block,"title")){

        abort(
          paste0(
            "NULL/deprecated functions must have a title.\n",
            "Review file `",roxy_text_file(text),"`, line ", block$line),
          class = "vt.deprecated_null_function_missing_title"
        )

      }

      title <- block_get_tag_value(block, tag = "title")

      block$object <- structure(
        list(alias = title,
             topic = title,
             value = "Deprecated",
             methods = NULL
        ),
        class = c("deprecated_function","function","object"))

    }else{

      call_as_list <- as.list(block$call)

      title <- as.character(call_as_list[[2]])

      block$object <- structure(
        list(alias = title,
             topic = title,
             value = call_as_list[[3]],
             methods = NULL
        ),
        class = c("function","object"))
    }

    block
  })

  return(roxyblocks)
}

#' @importFrom roxygen2 parse_text
#' @importFrom rlang abort
#' @importFrom utils capture.output
#' @noRd
parse_roxygen.r_test_code <- function(text){

  roxyblocks <- roxygen2::parse_text(text,env = NULL)
  roxyblocks <- cleanup_section_last_update(roxyblocks)

  roxyblocks <- lapply(roxyblocks,function(block){

    test <- as.list(block$call)[[2]]

    block$object <- structure(
      list(alias = test,
           topic = test,
           value = block$call,
           methods = NULL
           ),
      class = c("test_code","function","object"))

    block

  })

  ## confirm no duplicated test names
  roxy_test_names <- sapply(roxyblocks, function(block) block$object$alias)

  if(any(duplicated(roxy_test_names))){

    test_table <- table(roxy_test_names)

    dup_test_names <- test_table[test_table > 1]

    locs <- sapply(
      names(dup_test_names),
      function(test_name){
        test_record <- which(roxy_test_names == test_name)
        paste(
          sapply(roxyblocks[test_record],function(block) block$line),
        collapse = ",")
      }
    )

    dup_table <- capture.output(
      print(data.frame(
      `Test Name` = names(dup_test_names),
       Lines = locs
      ),
      row.names = FALSE))


    abort(paste(c(
      paste0(
        "Duplicated Test Code names in `",
        attr(text, "file"),
        "`.\n",
        "Each test must have a distinct name:\n"
      ),
      dup_table
    ), collapse = "\n"),
    class = "vt.duplicated_test_code_names")
  }

  return(roxyblocks)
}

#' @importFrom roxygen2 parse_text block_has_tags block_get_tag_value
#' @noRd
#'
parse_roxygen.md <- function(text){

  ## subset to keep only the roxygen comments

  text2 <- roxy_text(
    c(text[grepl("^#'",text)], "NULL"),
    file = roxy_text_file(text),
    class = roxy_text_class(text)
  )

  roxyblocks <- roxygen2::parse_text(text2, env = NULL)

  roxyblocks <- cleanup_section_last_update(roxyblocks)

  ## Clean up to set title to object alias and topic
  roxyblocks <- lapply(roxyblocks, function(block) {
    if (!block_has_tags(block, "title")) {
      abort(
        paste0(
          "All markdown roxygen headers must have a title.\n",
          "Review file `",
          roxy_text_file(text),
          "`."
        ),
        class = "vt.md_missing_title"
      )

    }

    title <- block_get_tag_value(block, tag = "title")

    block$object <- structure(
      list(
        alias = title,
        topic = title,
        value = "Deprecated",
        methods = NULL
      ),
      class = c("md_file", "function", "object")
    )

    block

  })

  return(roxyblocks)
}

parse_roxygen.rmd <- parse_roxygen.md


#' @importFrom rlang warn
#' @importFrom roxygen2 block_has_tags block_get_tags roxy_tag roxy_tag_parse
#' @noRd
#'
cleanup_section_last_update <- function(blocks){

  ## cleanup if using old @section Last Update(d) By/Date
  lapply(blocks, function(block){
    if (block_has_tags(block = block, tags = c("editor", "editDate"))) {
      return(block)

    } else if (block_has_tags(block = block, tags = c("section"))) {

      section_tags <- block_get_tags(block = block, tags = "section")

      content <- do.call('c',lapply(section_tags, function(tags){
        section_split <- strsplit(tags[["val"]],":\n",fixed = TRUE)[[1]]
        selection <- section_split[[2]]
        names(selection) <- section_split[[1]]
        selection
      }))

      last_by <- grepl("last update(d)* by",names(content),ignore.case = TRUE)
      last_date <- grepl("last update(d)* date",names(content),ignore.case = TRUE)
      spec_coverage <- grepl("specification coverage",names(content),ignore.case = TRUE)

      if(any(last_by)){

        which_editor <- which(last_by)
        editor <- content[which_editor]

        block$tags <- c(
          block$tags,
          list(roxy_tag_parse(roxy_tag(
            "editor",
            raw = unname(editor),
            file = block$file,
            line = section_tags[[which_editor]]$line
          )))
        )

        warn(
          paste0(
            "`@section ",names(content[which_editor]),":` ",
            "is superseded.",
            "\nUse `@editor ",trimws(editor),"` instead."
          ),
          class = "vt.superseded_last_updated_by"
        )

      }

      if(any(last_date)){

        which_editDate <- which(last_date)
        editDate <- content[which_editDate]

        block$tags <- c(
          block$tags,
          list(roxy_tag_parse(roxy_tag(
            "editDate",
            raw = unname(editDate),
            file = block$file,
            line = section_tags[[which_editDate]]$line
          )))
        )

        warn(
          paste0(
            "`@section ",names(content[which_editDate]),":` ",
            "is superseded.",
            "\nUse `@editDate ",trimws(editDate),"` instead."
          ),
          class = "vt.superseded_last_update_date"
        )
      }


      if(any(spec_coverage)){

        which_spec_cov <- which(spec_coverage)
        coverage <- unname(content[which_spec_cov])

        block$tags <- c(
          block$tags,
          list(roxy_tag_parse(roxy_tag(
            "coverage",
            raw = coverage,
            file = block$file,
            line = section_tags[[which_spec_cov]]$line
          )))
        )

        warn(
          paste0(
            "`@section ",names(content[which_spec_cov]),":` ",
            "is superseded.",
            "Use the following instead:\n\n```\n",
            paste("#'", c("@coverage",strsplit(coverage,"\n")[[1]]),collapse = "\n"),
            "\n```"
          ),
          class = "vt.superseded_specification_coverage"
        )

      }

      return(block)

    } else{
      return(block)
    }
  })

}

