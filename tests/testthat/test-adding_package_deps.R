test_that("Able to up-version package dependencies", {
    withr::with_tempdir({

      quite <- capture.output({
        vt_create_package(".", open = FALSE)
      })

      desc::desc_set_dep("rlang","Suggests")

      old_deps <- desc::desc_get_deps()

      add_package_to_desc("rlang","Depends")

      new_deps <- desc::desc_get_deps()

      deps_change <-
        merge(old_deps, new_deps,by = c("package","version"))

      expect_equal(
        deps_change,
        data.frame(
          package = c("rlang","valtools"),
          version = c("*","*"),
          type.x = c("Suggests","Imports"),
          type.y = c("Depends","Imports"),
          stringsAsFactors = FALSE
        )
      )
  })
})

test_that("Won't down-version require package dependencies", {
  withr::with_tempdir({

    quite <- capture.output({
      vt_create_package(".", open = FALSE)
    })

    desc::desc_set_dep("rlang","Depends")

    old_deps <- desc::desc_get_deps()

    add_package_to_desc("rlang","Suggests")

    new_deps <- desc::desc_get_deps()

    deps_change <-
      merge(old_deps, new_deps,by = c("package","version"))

    expect_equal(
      deps_change,
      data.frame(
        package = c("rlang","valtools"),
        version = c("*","*"),
        type.x = c("Depends","Imports"),
        type.y = c("Depends","Imports"),
        stringsAsFactors = FALSE
      )
    )
  })
})
