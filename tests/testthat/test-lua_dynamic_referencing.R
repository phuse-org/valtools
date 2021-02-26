test_that("lua numbering for pdf", {
  withr::with_tempdir({

  ## create test files
  test_input <- tempfile(fileext = ".md", tmpdir = getwd())

  cat(
    file = test_input,
    sep = "\n",
    c('---',
      'title: "Lua filter counter"',
      'author: "An author"',
      'output:',
      '  pdf_document:',
      '    pandoc_args:',
      '    - --lua-filter',
      paste0('    - ', system.file(package = "valtools", "lua/counter.lua")),
      '---',
      '\n\n',
      'Reference No. `@@Ref-1`\n\n',
      'Second call to same ref `@@Ref-1`\n\n',
      'Some other code block `something else`\n\n',
      'Ref no. `@@Ref-2`\n\n',
      'First ref: `@@Ref-1`\n\n',
      'Second ref: `@@Ref-2`\n\n'
    ))


    rmarkdown::render(input = test_input, clean = FALSE)

    test_output_rendered <-
      strsplit(split = "\r\n",
               pdftools::pdf_text(gsub(test_input, pattern = ".md", replacement = ".pdf")))[[1]]
    expect_equal("Reference No. 1", test_output_rendered[3])
    expect_equal("Second call to same ref 1", test_output_rendered[4])
    expect_equal("Ref no. 2", test_output_rendered[6])
    expect_equal("First ref: 1", test_output_rendered[7])
    expect_equal("Second ref: 2", test_output_rendered[8])
})})

test_that("lua numbering for html", {
  withr::with_tempdir({
  ## create test files
  test_input <- tempfile(fileext = ".md", tmpdir = getwd())

  cat(
    file = test_input,
    sep = "\n",
    c('---',
      'title: "Lua filter counter"',
      'author: "An Author"',
      'output:',
      '  html_document:',
      '    pandoc_args:',
      '    - --lua-filter',
      paste0('    - ', system.file(package = "valtools", "lua/counter.lua")),
      '---',
      '\n\n',
      'Reference Number `@@Ref-1`\n\n',
      'Second call to same ref `@@Ref-1`\n\n',
      'Some other code block `something else`\n\n',
      'Ref no. `@@Ref-2`\n\n',
      'First ref: `@@Ref-1`\n\n',
      'Second ref: `@@Ref-2`\n\n'
    ))

  rmarkdown::render(input = test_input, clean = FALSE)
  test_output_rendered <-
    unlist(XML::xpathApply(XML::htmlTreeParse(gsub(test_input, pattern = ".md", replacement = ".html"),
                                useInternal = TRUE), "//p", XML::xmlValue,
                           recursive = FALSE, trim = TRUE))



  expect_equal("Reference Number 1", test_output_rendered[1])
  expect_equal("Second call to same ref 1", test_output_rendered[2])
  expect_equal("Ref no. 2", test_output_rendered[4])
  expect_equal("First ref: 1", test_output_rendered[5])
  expect_equal("Second ref: 2", test_output_rendered[6])
})})
