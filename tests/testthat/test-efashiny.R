context("EFAshiny.R")

test_that("EFAshiny work", {
        value <- class(EFAshiny(run = F,  report_Apppath = T))
        testthat::expect_equal(value, "character")
})

test_that("EFAshiny work", {
        value <- EFAshiny(run = F, report_Apppath = F)
        testthat::expect_equal(value, "Have fun with EFAshiny : https://psychilin.shinyapps.io/EFAshiny/")
})