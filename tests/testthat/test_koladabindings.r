context("koladabindings")

## Required packages
library(httr)
library(jsonlite)

## Testing user inputs
test_that("Wrong user input is detected from get_municipality()", {
  expect_error(get_municipality("a", 2012))
  expect_error(get_municipality(1440, "b"))
  expect_error(get_municipality(1440, list(2012, 2013, 2014)))
  expect_error(get_municipality(list(1440, 1441, 1442), 2012))
  expect_error(get_municipality(data.frame(), 2012))
  expect_error(get_municipality(1440, list()))
})

test_that("Wrong user input is detected from get_kpi()", {
  expect_error(get_kpi("N00914", 1440, list(2010, 2011, 2012)))
  expect_error(get_kpi(c("N00914,U00405"), 1440, list(2010, 2011, 2012)))
  expect_error(get_kpi(list("N00914,U00405"), 1440, c(2010, 2011, 2012)))
  expect_error(get_kpi(list("N00914,U00405"), c(1440, 1440), list(2010, 2011, 2012)))
  expect_error(get_kpi(list("N00914,U00405"), data.frame(), list(2010, 2011, 2012)))
  expect_error(get_kpi(list(), 1440, list(2010, 2011, 2012)))
  expect_error(get_kpi("N00914", 1440))
  expect_error(get_kpi("N00914", c()))
  expect_error(get_kpi(list(), 1440))
})

## Testing functionality
test_that("get_municipality() is working", {
  df = get_municipality()
  expect_true(is.data.frame(df))
  expect_equal(df[5, "values.id"], "1280")
})

test_that("get_kpi() is working", {
  df = get_kpi()
  expect_true(is.data.frame(df))
  expect_equal(df[6, "values.id"], "NO7402")
})

test_that("get_municipality_groups() is working", {
  df = get_municipality_groups()
  expect_true(is.data.frame(df))
  expect_equal(df[5, "values.id"], "V15E128000301")
})

test_that("get_kpi_groups() is working", {
  df = get_kpi_groups()
  expect_true(is.data.frame(df))
  expect_equal(df[6, "member_id"], "U28119")
})

test_that("get_search_results() is working", {
  df = get_search_results(list("N00914", "U00405"), 1440, list(2010, 2011, 2012))
  expect_true(is.data.frame(df))
  expect_equal(df[5, "values.kpi"], "U00405")
})