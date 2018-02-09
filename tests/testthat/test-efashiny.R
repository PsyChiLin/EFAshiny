context("EFAshiny.R")

test_that("EFAshiny work", {
        #app <- system.file("efas",package = 'EFAshiny')
        value <- class(EFAshiny(run = F))
        testthat::expect_equal(value, "character")
})
