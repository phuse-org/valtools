test_that("partial match to section names", {
  roxy_block1 <- c("@title Fourth Function",
                  "@description A description",
                  "@param Param1 definition",
                  "@param Param2 definiion",
                  "@section Last updated by:",
                  "Author Name",
                  "@section Last updated date:",
                  "2021/12/01"  )


  testthat::expect_equal("Author Name",
                         vt_scrape_section("updated by", roxy_block1))
  testthat::expect_equal("Author Name",
                         vt_scrape_section("last updated b", roxy_block1))
  testthat::expect_equal("2021/12/01",
                         vt_scrape_section("date:", roxy_block1))
})

test_that("with #'", {
  roxy_block2 <- paste("#'", c("@title Fourth Function",
                   "@description A description",
                   "@param Param1 definition",
                   "@param Param2 definiion",
                   "@section Last updated by:",
                   "Author Name",
                   "@section Last updated date:",
                   "2021/12/01"  ), sep = " ")


  testthat::expect_equal("Author Name",
                         vt_scrape_section("updated by", roxy_block2))
  testthat::expect_equal("Author Name",
                         vt_scrape_section("last updated b", roxy_block2))
  testthat::expect_equal("2021/12/01",
                         vt_scrape_section("date:", roxy_block2))
})
