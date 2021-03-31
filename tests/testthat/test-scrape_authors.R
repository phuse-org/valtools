test_that("Scrape roxygen tags from reqs works", {

  withr::with_tempdir({

    ## test setup
    vt_use_validation_config(
      username_list = list(
        vt_user(
          username = "user1",
          name = "Test User",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user2",
          name = "Test User 2",
          role = "sample",
          title = "Req Writer"
        )
      ))

    vt_use_validation()

    vt_use_req("req001.md",username = "Test User")
    vt_use_req("req002.md",username = "Test User")
    vt_use_req("req003.md",username = "Test User 2")

    ## scrape
    tag_list <- scrape_tags_from(
      type = "requirements",
      tags = c("editor","editDate"),
      ref = "vignettes/validation")


    ## check values
    expect_equal(
      tag_list,
      list(
       data.frame(
         requirements = "req001",
         editor = "Test User",
         editDate = format(Sys.Date(),"%Y-%m-%d")
       ),
       data.frame(
         requirements = "req002",
         editor = "Test User",
         editDate = format(Sys.Date(),"%Y-%m-%d")
       ) ,
       data.frame(
         requirements = "req003",
         editor = "Test User 2",
         editDate = format(Sys.Date(),"%Y-%m-%d")
       )
      )
    )
  })
})

test_that("Scrape roxygen tags from test cases works", {

  withr::with_tempdir({

    ## test setup
    vt_use_validation_config(
      username_list = list(
        vt_user(
          username = "user1",
          name = "Test User",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user2",
          name = "Test User 2",
          role = "sample",
          title = "Req Writer"
        )
      ))

    vt_use_validation()

    vt_use_test_case("test_case_001.md",username = "Test User")
    vt_use_test_case("test_case_002.md",username = "Test User")
    vt_use_test_case("test_case_003.md",username = "Test User 2")

    ## scrape
    tag_list <- scrape_tags_from(
      type = "test_cases",
      tags = c("editor","editDate"),
      ref = "vignettes/validation")


    ## check values
    expect_equal(
      tag_list,
      list(
        data.frame(
          test_cases = "test_case_001",
          editor = "Test User",
          editDate = format(Sys.Date(),"%Y-%m-%d")
        ),
        data.frame(
          test_cases = "test_case_002",
          editor = "Test User",
          editDate = format(Sys.Date(),"%Y-%m-%d")
        ) ,
        data.frame(
          test_cases = "test_case_003",
          editor = "Test User 2",
          editDate = format(Sys.Date(),"%Y-%m-%d")
        )
      )
    )
  })
})

test_that("Scrape roxygen tags from test code works", {

  withr::with_tempdir({

    ## test setup
    vt_use_validation_config(
      username_list = list(
        vt_user(
          username = "user1",
          name = "Test User",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user2",
          name = "Test User 2",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user3",
          name = "Test User 3",
          role = "sample",
          title = "Test Code Writer"
        ),
        vt_user(
          username = "user4",
          name = "Test User 4",
          role = "sample",
          title = "Test Code Writer"
        )
      ))

    vt_use_validation()

    vt_use_test_code("test_code_001",username = "Test User 3")
    text <- readLines("vignettes/validation/test_code/test_code_001.R")
    writeLines(gsub("TESTNUMBER","1.1", text),"vignettes/validation/test_code/test_code_001.R")

    vt_use_test_code("test_code_002",username = "Test User 4")
    text <- readLines("vignettes/validation/test_code/test_code_002.R")
    writeLines(gsub("TESTNUMBER","2.1", text),"vignettes/validation/test_code/test_code_002.R")

    vt_use_test_code("test_code_003",username = "Test User 4")
    text <- readLines("vignettes/validation/test_code/test_code_003.R")
    writeLines(gsub("TESTNUMBER","3.1", text),"vignettes/validation/test_code/test_code_003.R")

    ## scrape
    tag_list <- scrape_tags_from(
      type = "test_code",
      tags = c("editor","editDate"),
      ref = "vignettes/validation")

    ## check values
    expect_equal(
      tag_list,
      list(
        data.frame(
          test_code = "1.1",
          editor = "Test User 3",
          editDate = format(Sys.Date(),"%Y-%m-%d")
        ),
        data.frame(
          test_code = "2.1",
          editor = "Test User 4",
          editDate = format(Sys.Date(),"%Y-%m-%d")
        ) ,
        data.frame(
          test_code = "3.1",
          editor = "Test User 4",
          editDate = format(Sys.Date(),"%Y-%m-%d")
        )
      )
    )
  })
})

test_that("scrape functions from external dir", {

  withr::with_tempdir({

    capture_output <- capture.output({
      usethis::create_package(path = "example.package" , open = FALSE,rstudio = TRUE)
    })

    vt_use_validation_config()

    this_file1 <- file.path("example.package", "R/hello_world.R")
    fs::file_create(path = this_file1)
    cat(file = this_file1, append = FALSE,c(
        "#' @title Hello World!",
        "#' @description A description",
        "#' @param Param1 definition",
        "#' @return string greeting the argument",
        "#' @export",
        "hello_world <- function(Param1){",
        "  # A function  with no authorship",
        "  paste('Hello', Param1)",
        "}"), sep = "\n")

    # use case: multiple functions in single file
    # export tag is ignored
    # ignores extra spaces
    # accommodates minor variation in date format
    # accommodates minor variation in formatting of last update date
    this_file2 <- file.path("example.package", "R/second_func.R")
    fs::file_create(path = this_file2)
    cat(
      file = this_file2,
      append = FALSE,c(
      "#' @title Second Function",
      "#' @description A description",
      "#' @param Param1 definition",
      "#' @param Param2 definiion",
      "#' @section Last updated by:",
      "#' Author Name",
      "#' @section Last updated date:",
      "#' 2021-01-01",
      "#' @export",
      "second_func <- function(Param1, Param2){",
      "  return('Something')",
      "}",
      "",
      "",
      "#' @title Third Function",
      "#' @description A description",
      "#' @param Param1 definition",
      "#' @param Param2 definiion",
      "#' @section last updated by:",
      "#'   Author2 Name",
      "#' @section Last Update Date:",
      "#' 12/01/2021",
      "third_func <- function(Param1, Param2){",
      "  return('Something')",
      "}"),
      sep = "\n"
    )

    # pulls function authorship from roxygen2 block, even if no function included
    # ignores stray comments
    this_file3 <- file.path("example.package", "R/null_doc.R")
    fs::file_create(path = this_file3)
    cat(file = this_file3, append = FALSE,c(
        "#' @title Fourth_Function",
        "#' @description A description",
        "#' @param Param1 definition",
        "#' @param Param2 definiion",
        "#' @section Last updated by:",
        "#' Author Name",
        "#' @section Last updated date:",
        "#' 2021/12/01",
        "#' @note Defunct",
        "#' @noRd",
        "NULL",
        "",
        "",
        "#' @title Fifth Function",
        "#' @description A help page that won't be flagged for function authorship",
        "#' @param Param1 definition",
        "#' @param Param2 definiion",
        "NULL",
        "",
        "",
        "# some stray comment"),sep = "\n")

    warnings <- capture_warnings({
    tag_list <- scrape_tags_from(
      type = "functions",
      tags = c("editor", "editDate"),
      src = "example.package",
      ref = "vignettes/validation"
      )
    })



    expect_equal(
      tag_list,
      list(
        data.frame(
          functions = "Fourth_Function",
          editor = "Author Name",
          editDate = "2021-12-01"
        ),
        data.frame(
          functions = "second_func",
          editor = "Author Name",
          editDate = "2021-01-01"
        ),
        data.frame(
          functions = "third_func",
          editor = "Author2 Name",
          editDate = "2021-12-01"
        )
     ))

    ## 8 cases of
    expect_equal(
      length(warnings),
      6
    )

  })

})

test_that("Scrape roxygen tags and specific tags are missing throws warnings", {

  withr::with_tempdir({

    ## test setup
    vt_use_validation_config(
      username_list = list(
        vt_user(
          username = "user1",
          name = "Test User",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user2",
          name = "Test User 2",
          role = "sample",
          title = "Req Writer"
        )
      ))

    vt_use_validation()

    vt_use_test_case("test_case_001.md",username = "Test User")

    ## scrape
    warn_val <- capture_warnings({
      tag_list <- scrape_tags_from(
        type = "test_cases",
        tags = c("fake_tag"),
        ref = "vignettes/validation")
    })


    ## check values
    expect_equal(
      tag_list,
      list()
    )

    expect_equal(
      warn_val,
      "No blocks with tags `fake_tag`"
    )

  })
})


test_that("Scrape roxygen tags from function authors", {

  withr::with_tempdir({

    ## test setup
    captured_output <- capture.output({vt_create_package(open = FALSE)})
    usethis::proj_set(force = TRUE)
    usethis::use_r("hello_world.R", open = FALSE)

    writeLines(c(
      "#' A function to greet someone",
      "#' @param name someone's name",
      "#' @editor An author",
      "#' @editDate 2021-01-01",
      "#' @return text greeting",
      "#' @export",
     " hello_world <- function(name){",
     "   paste(\"Hello \", name)",
      "}",
      "",
      "#' A function to greet someone with date",
      "#' @param name someone's name",
      "#' @editor Another author",
      "#' @editDate 2021-02-01",
      "#' @return text greeting with date",
      "hello_world2 <- function(name){",
      "  paste(\"Hello \", name, \" today is: \", Sys.Date())",
      "}"
    ), file.path(usethis::proj_get(), "R", "hello_world.R"))


    expect_equal(vt_scrape_functions(tags = c("editor", "editDate")),
                 data.frame(functions = c("hello_world", "hello_world2"),
                            editor = c("An author", "Another author"),
                            editDate = c("2021-01-01", "2021-02-01")))

    exported_authors <- vt_scrape_functions()
    expect_equal(exported_authors[!is.na(exported_authors$export), c("functions", "editor", "editDate")],
                 data.frame(functions = "hello_world",
                            editor = "An author",
                            editDate = "2021-01-01"))
    expect_equal(strsplit(vt_kable_functions(exported_authors), "\n")[[1]][-1],
                 c(
                   "\\begin{tabular}{|>{}l|l|l|>{}l|}",
                   "\\hline",
                    "functions & editor & editDate & export\\\\",
                   "\\hline",
                    "hello\\_world & An author & 2021-01-01 & \\\\",
                   "\\hline",
                    "hello\\_world2 & Another author & 2021-02-01 & NA\\\\",
                   "\\hline",
                    "\\end{tabular}"
                 ))

    expect_equal(strsplit(vt_kable_functions(exported_authors, format = "html"), "\n")[[1]][-1],
                 c(" <thead>",
                 "  <tr>",
                 "   <th style=\"text-align:left;\"> functions </th>",
                 "   <th style=\"text-align:left;\"> editor </th>",
                 "   <th style=\"text-align:left;\"> editDate </th>",
                 "   <th style=\"text-align:left;\"> export </th>",
                 "  </tr>",
                 " </thead>",
                 "<tbody>",
                 "  <tr>",
                 "   <td style=\"text-align:left;border-left:1px solid;\"> hello_world </td>",
                 "   <td style=\"text-align:left;\"> An author </td>",
                 "   <td style=\"text-align:left;\"> 2021-01-01 </td>",
                 "   <td style=\"text-align:left;border-right:1px solid;\">  </td>",
                 "  </tr>",
                 "  <tr>",
                 "   <td style=\"text-align:left;border-left:1px solid;\"> hello_world2 </td>",
                 "   <td style=\"text-align:left;\"> Another author </td>",
                 "   <td style=\"text-align:left;\"> 2021-02-01 </td>",
                 "   <td style=\"text-align:left;border-right:1px solid;\"> NA </td>",
                 "  </tr>",
                 "</tbody>",
                 "</table>" ))



  })
})
