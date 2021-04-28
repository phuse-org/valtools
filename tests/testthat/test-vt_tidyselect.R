

test_that("tidyselect everything operator works as expected", {

  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("reqA", open = FALSE)
    vt_use_req("reqB", open = FALSE)
    vt_use_req("reqC", open = FALSE)

    vt_use_req("reqBeforeEverything", open = FALSE, add_before = tidyselect::everything())

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqBeforeEverything.md",
        "- reqA.md",
        "- reqB.md",
        "- reqC.md"
      )
    )

    vt_use_req("reqAfterEverything", open = FALSE, add_after = tidyselect::everything())

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqBeforeEverything.md",
        "- reqA.md",
        "- reqB.md",
        "- reqC.md",
        "- reqAfterEverything.md"
      )
    )
  })
})

test_that("tidyselect last_col operator works as expected", {

  withr::with_tempdir({
    quiet <- capture.output({
    vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("reqA", open = FALSE)
    vt_use_req("reqB", open = FALSE)
    vt_use_req("reqC", open = FALSE)

    vt_use_req("reqBeforeLastCol", open = FALSE, add_before = tidyselect::last_col())

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqA.md",
        "- reqB.md",
        "- reqBeforeLastCol.md",
        "- reqC.md"
      )
    )

    vt_use_req("reqAfterLastCol", open = FALSE, add_after = tidyselect::last_col())

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqA.md",
        "- reqB.md",
        "- reqBeforeLastCol.md",
        "- reqC.md",
        "- reqAfterLastCol.md"
      )
    )
  })
})

test_that("tidyselect starts_with operator works as expected", {

  withr::with_tempdir({
    quiet <- capture.output({
    vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("XYZreqA", open = FALSE)
    vt_use_req("XYZreqB", open = FALSE)
    vt_use_req("reqC", open = FALSE)

    vt_use_req("reqBeforeStartsWithXYZ", open = FALSE, add_before = tidyselect::starts_with("XYZ"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqBeforeStartsWithXYZ.md",
        "- XYZreqA.md",
        "- XYZreqB.md",
        "- reqC.md"
      )
    )

    vt_use_req("reqAfterStartsWithXYZ", open = FALSE, add_after = tidyselect::starts_with("XYZ"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqBeforeStartsWithXYZ.md",
        "- XYZreqA.md",
        "- XYZreqB.md",
        "- reqAfterStartsWithXYZ.md",
        "- reqC.md"
      )
    )
  })
})

test_that("tidyselect ends_with operator works as expected", {

  withr::with_tempdir({
    quiet <- capture.output({
      vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("reqA", open = FALSE)
    vt_use_req("reqBXYZ", open = FALSE)
    vt_use_req("reqCXYZ", open = FALSE)

    vt_use_req("reqBeforeEndsWithXYZ", open = FALSE, add_before = tidyselect::ends_with("XYZ.md"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqA.md",
        "- reqBeforeEndsWithXYZ.md",
        "- reqBXYZ.md",
        "- reqCXYZ.md"
      )
    )

    vt_use_req("reqAfterEndsWithXYZ", open = FALSE, add_after = tidyselect::ends_with("XYZ.md"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqA.md",
        "- reqBeforeEndsWithXYZ.md",
        "- reqBXYZ.md",
        "- reqCXYZ.md",
        "- reqAfterEndsWithXYZ.md"
      )
    )
  })
})

test_that("tidyselect contains operator works as expected", {

  withr::with_tempdir({
    quiet <- capture.output({
      vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("reqA", open = FALSE)
    vt_use_req("reqB", open = FALSE)
    vt_use_req("reqC", open = FALSE)

    vt_use_req("reqBeforeContainsA", open = FALSE, add_before = tidyselect::contains("A"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqBeforeContainsA.md",
        "- reqA.md",
        "- reqB.md",
        "- reqC.md"
      )
    )

    vt_use_req("reqAfterContainsA", open = FALSE, add_after = tidyselect::contains("A"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqBeforeContainsA.md",
        "- reqA.md",
        "- reqAfterContainsA.md",
        "- reqB.md",
        "- reqC.md"
      )
    )
  })
})

test_that("tidyselect matches operator works as expected", {

  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("reqA", open = FALSE)
    vt_use_req("reqB", open = FALSE)
    vt_use_req("reqC", open = FALSE)

    vt_use_req("reqBeforeMatchesQC", open = FALSE, add_before = tidyselect::matches("qC"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqA.md",
        "- reqB.md",
        "- reqBeforeMatchesQC.md",
        "- reqC.md"
      )
    )

    vt_use_req("reqAfterMatchesQC", open = FALSE, add_after = tidyselect::matches("qC"))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqA.md",
        "- reqB.md",
        "- reqBeforeMatchesQC.md",
        "- reqC.md",
        "- reqAfterMatchesQC.md"
      )
    )
  })
})

test_that("tidyselect all_of operator works as expected", {

  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )
    vt_use_req("reqA", open = FALSE)
    vt_use_req("reqB", open = FALSE)
    vt_use_req("reqC", open = FALSE)

    vt_use_req("reqBeforeAllOfAC", open = FALSE, add_before = tidyselect::all_of(c("reqA.md", "reqC.md")))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 5),
      c(
        "validation_files:",
        "- reqBeforeAllOfAC.md",
        "- reqA.md",
        "- reqB.md",
        "- reqC.md"
      )
    )

    vt_use_req("reqAfterAllOfAC", open = FALSE, add_after = tidyselect::all_of(c("reqA.md", "reqC.md")))

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 6),
      c(
        "validation_files:",
        "- reqBeforeAllOfAC.md",
        "- reqA.md",
        "- reqB.md",
        "- reqC.md",
        "- reqAfterAllOfAC.md"
      )
    )
  })
})
