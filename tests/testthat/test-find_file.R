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

test_that("when finding multiple files a preference can be stated", {
  withr::with_tempdir({

    ## create folders
    dir.create("folder1/folder2/folder3",recursive = TRUE)
    dir.create("folder4/folder5/folder6",recursive = TRUE)
    file.create("folder1/folder2/folder3/hello.R")
    file.create("folder1/folder2/hello.R")
    file.create("folder4/folder5/folder6/hello.R")

    expect_equal(
        find_file("hello.R", preference = "folder4"),
        "folder4/folder5/folder6/hello.R"
    )

    expect_equal(
      find_file("hello.R", preference = "folder5"),
      "folder4/folder5/folder6/hello.R"
    )

    expect_equal(
      find_file("hello.R", preference = "folder6"),
      "folder4/folder5/folder6/hello.R"
    )

    expect_equal(
      find_file("hello.R", preference = "folder2/folder3"),
      "folder1/folder2/folder3/hello.R"
    )

    expect_equal(
      find_file("hello.R", preference = "folder2"),
      c(
        "folder1/folder2/folder3/hello.R",
        "folder1/folder2/hello.R"
      )
    )

  })
})




