test_that("evaluate validation report filename within a package", {
  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("test.package",open = FALSE)
    })

    setwd("test.package")

    val_report_name <- evaluate_filename()
    val_report_name_setversion <- evaluate_filename(version = "1.0")

    expect_equal(
      val_report_name,
      paste0("Validation_Report_test.package_v0.0.0.9000_",format(Sys.Date(),"%Y%m%d"))
    )

    expect_equal(
      val_report_name_setversion,
      paste0("Validation_Report_test.package_v1.0_",format(Sys.Date(),"%Y%m%d"))
    )

  })

})

test_that("evaluate validation report filename for external packages", {
  withr::with_tempdir({

    quiet <- capture.output({
      vt_use_validation(package = "rlang")
    })

    val_report_name <- evaluate_filename()
    val_report_name_setversion <- evaluate_filename(version = "1.0")

    expect_equal(
      val_report_name,
      paste0("Validation_Report_rlang_v",packageVersion("rlang"),"_",format(Sys.Date(),"%Y%m%d"))
    )

    expect_equal(
      val_report_name_setversion,
      paste0("Validation_Report_rlang_v1.0_",format(Sys.Date(),"%Y%m%d"))
    )

  })

})
