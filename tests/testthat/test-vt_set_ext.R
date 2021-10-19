test_that("setting extentions", {
  expect_equal(
    vt_set_ext("test_input","R"),
    "test_input.R"
  )

  expect_equal(
    vt_set_ext("test_input","md"),
    "test_input.md"
  )

})

test_that("overriding extentions", {
  expect_equal(
    vt_set_ext("test_input.txt","R"),
    "test_input.R"
  )

  expect_equal(
    vt_set_ext("test_input.zzz","md"),
    "test_input.md"
  )

})

test_that("setting extentions when multiple exist", {
  
  expect_equal(
    vt_set_ext("test_input.md",c("md","Rmd")),
    "test_input.md"
  )
  
  expect_equal(
    vt_set_ext("test_input.Rmd",c("md","Rmd")),
    "test_input.Rmd"
  )
  
  expect_equal(
    vt_set_ext("test_input.zzz",c("md","Rmd")),
    "test_input.md"
  )
  
})
