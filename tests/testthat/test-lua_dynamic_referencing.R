test_that("lua numbering for pdf", {
  ## this test demonstrates how to use lua filter incl in inst/lua for dynamic labeling
  ## does not depend on valtools functions for dynamic labeling
  skip_if_not_installed("valtools")
  withr::with_tempdir({

  ## create test files
  test_input <- tempfile(fileext = ".md", tmpdir = getwd())

  cat(
    file = test_input,
    sep = "\n",
    c('---',
      'title: "Lua filter counter"',
      'output:',
      '  pdf_document:',
      '    pandoc_args:',
      '    - --lua-filter',
      paste0('    - ', system.file(package = "valtools", "lua/counter.lua")),
      '---',
      '\n\n',
      '1. A label `##tc:refA` with a group: ##tc:refA',
      '2. A label `##refB` without a group: ##refB',
      '3. Add second label to the first group using `##tc:refB` : ##tc:refB',
      '4. Another label `##refC` without a group: ##refC',
      '5. A label with a second group using `##req:refC` : ##req:refC',
      '6. Add third label to the first group using `##tc:refC`: ##tc:refC',
      '7. Add a second label to the second group using `##req:refD`: ##req:refD',
      '\n\n',
      'Later reference to (1) - ##tc:refA\n',
      'Later reference to (6) - ##tc:refC\n',
      'Later reference to (2) - ##refB\n',
      'Later reference to (7) - ##req:refD\n',
      'Later reference to (4) - ##refC\n',
      'Later reference to (5) - ##req:refC\n',
      'Later reference to (3) - ##tc:refB\n'
    ))


    rmarkdown::render(input = test_input, clean = FALSE)

    test_output_rendered <-
      strsplit(split = "\r\n",
               pdftools::pdf_text(gsub(test_input, pattern = ".md", replacement = ".pdf")))[[1]]

    expect_equal(c(1, 1, 2, 2, 1, 3, 2),
                 unlist(lapply(strsplit(trimws(test_output_rendered[2:8]), split = ": "),
                               FUN = function(x){as.numeric(trimws(x[2]))})))
    expect_equal(c(1, 3, 1, 2, 2, 1, 2), as.numeric(gsub(test_output_rendered[9:15],
              pattern = "Later\\sreference\\sto\\s\\((\\d)\\)\\s-\\s(\\d)",
              replacement = "\\2")))
    expect_equal(c(1, 6, 2, 7, 4, 5, 3), as.numeric(gsub(test_output_rendered[9:15],
              pattern = "Later\\sreference\\sto\\s\\((\\d)\\)\\s-\\s(\\d)",
              replacement = "\\1")))
})})

test_that("lua numbering for html", {
  ## this test demonstrates how to use lua filter incl in inst/lua for dynamic labeling
  ## does not depend on valtools functions for dynamic labeling
  skip_if_not_installed("valtools")
  withr::with_tempdir({
  ## create test files
  test_input <- tempfile(fileext = ".md", tmpdir = getwd())

  cat(
    file = test_input,
    sep = "\n",
    c('---',
      'title: "Lua filter counter"',
      'output:',
      '  html_document:',
      '    pandoc_args:',
      '    - --lua-filter',
      paste0('    - ', system.file(package = "valtools", "lua/counter.lua")),
      '---',
      '\n\n',
      '1. A label `##tc:refA` with a group: ##tc:refA',
      '2. A label `##refB` without a group: ##refB',
      '3. Add second label to the first group using `##tc:refB` : ##tc:refB',
      '4. Another label `##refC` without a group: ##refC',
      '5. A label with a second group using `##req:refC` : ##req:refC',
      '6. Add third label to the first group using `##tc:refC`: ##tc:refC',
      '7. Add a second label to the second group using `##req:refD`: ##req:refD',
      '\n\n',
      'Later reference to (1) - ##tc:refA\n',
      'Later reference to (6) - ##tc:refC\n',
      'Later reference to (2) - ##refB\n',
      'Later reference to (7) - ##req:refD\n',
      'Later reference to (4) - ##refC\n',
      'Later reference to (5) - ##req:refC\n',
      'Later reference to (3) - ##tc:refB\n'
    ))

  rmarkdown::render(input = test_input, clean = FALSE)
  test_output_rendered <- XML::htmlTreeParse(gsub(test_input, pattern = ".md", replacement = ".html"),
                                            useInternal = TRUE)
  text_rendered <- unlist(XML::xpathApply(test_output_rendered, "//p", XML::xmlValue,
                           recursive = FALSE, trim = TRUE))
  list_rendered <- unlist(XML::xpathApply(test_output_rendered, "//li", XML::xmlValue,
                                          recursive = FALSE, trim = TRUE))


  expect_equal(c(1, 3, 1, 2, 2, 1, 2), as.numeric(gsub(text_rendered,
                          pattern = "Later\\sreference\\sto\\s\\((\\d)\\)\\s-\\s(\\d)",
                          replacement = "\\2")))
  expect_equal(c(1, 6, 2, 7, 4, 5, 3), as.numeric(gsub(text_rendered,
                          pattern = "Later\\sreference\\sto\\s\\((\\d)\\)\\s-\\s(\\d)",
                          replacement = "\\1")))
  expect_equal(c(1, 1, 2, 2, 1, 3, 2),
               as.numeric(unlist(lapply(strsplit(list_rendered, ": "), FUN = function(x){x[2]}))))

})})
