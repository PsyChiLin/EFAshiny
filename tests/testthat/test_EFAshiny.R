context("EFAshiny")

test_that("EFAshiny work", {
        app <- system.file("efas",package = 'EFAshiny')
        value <- class(app)
        testthat::expect_equal(value, "character")
})