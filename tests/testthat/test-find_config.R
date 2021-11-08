test_that("Find config when within a package with validation", {

  withr::with_tempdir({
    quiet <- capture.output({
      vt_create_package(
        "example.package",
        open = FALSE)
    })


    withr::with_dir(new = "example.package", {
      expect_equal(
        vt_find_config(),
        normalizePath(file.path(getwd(), "vignettes","validation","validation.yml"),winslash = "/")
      )
    })
  })
})

test_that("Find config when within a package with validation when working dir is non-standard", {

  withr::with_tempdir({
    quiet <- capture.output({
      vt_create_package(
        "example.package",
        working_dir = "inst",
        open = FALSE)
    })


    withr::with_dir(new = "example.package", {
      expect_equal(
        vt_find_config(),
        normalizePath(
          file.path(getwd(), "inst","validation","validation.yml"),
          winslash = "/")
      )
    })
  })
})

test_that("Find config when within a validation packet", {

  withr::with_tempdir({
      quiet <- capture.output({
        vt_create_packet("example_packet",
                         target = "example.package",
                         open = FALSE)
      })


      withr::with_dir(new = "example_packet", {
        expect_equal(
          vt_find_config(),
          normalizePath(
            file.path(getwd(), "validation","validation.yml"),
            winslash = "/")
        )
      })

  })

})


test_that("Informative error when outside a packet or package", {

  withr::with_tempdir({

    expect_error(
      vt_find_config(),
      paste0(
      "Could not find root directory. ",
      "Is your working directory inside a package, validation packet, or project?\n"
      ),
      fixed = TRUE)
  })

})

test_that("Informative error when inside an Rproj, but multiple packets or packages are nested in a subfolders", {
  
  withr::with_tempdir({
    
    quiet <- capture.output({
      usethis::create_project("test_project",open = FALSE)
      
      vt_create_packet("test_project/example_packet", 
                         target = "example.package",
                         open = FALSE)
      vt_create_packet("test_project/example_packet2", 
                       target = "example.package2",
                       open = FALSE)
    })
    
    
    withr::with_dir(new = "test_project", {
      
      expect_error(
        vt_find_config(),
        "Nested projects with validation infrastructures exist. Set the working directory to one of:",
        fixed = TRUE
      )
      
    })
    
  })
  
})


