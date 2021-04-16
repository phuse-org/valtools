test_that("finding a file works", {
  withr::with_tempdir({

    ## create folders
    dir.create("folder1/folder2/folder3",recursive = TRUE)
    file.create("folder1/folder2/folder3/hello.R")

    expect_equal(
      find_file("hello.R"),
      "folder1/folder2/folder3/hello.R"
    )

  })
})

test_that("finding a file works relative to reference", {
  withr::with_tempdir({

    ## create folders
    dir.create("folder1/folder2/folder3",recursive = TRUE)
    file.create("folder1/folder2/folder3/hello.R")

    expect_equal(
      find_file("hello.R", ref = "folder1/folder2"),
      "folder3/hello.R"
    )

  })
})

test_that("finding a file can return multiple files", {
  withr::with_tempdir({

    ## create folders
    dir.create("folder1/folder2/folder3",recursive = TRUE)
    file.create("folder1/folder2/folder3/hello.R")
    file.create("folder1/folder2/hello.R")

    expect_true(
      all(c(
        "folder1/folder2/hello.R",
        "folder1/folder2/folder3/hello.R"
      ) %in%
      find_file("hello.R")
    ))

  })
})




