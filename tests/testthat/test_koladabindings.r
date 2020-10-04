context("koladabindings")

## Required packages
require(httr)
require(jsonlite)


# Examples for testing:
mun = "lund"
mun_group = "skola"
kpi = "kvinnofridskränkning"
kpi_group = "kostnad"
search_res = list(kpi_list = list("N00945"), year_list = list(2009,2010))

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

test_that("Wrong user input is detected from municipality groups", {
  expect_error(get_municipality_groups("N00914", 1440, list(2010, 2011, 2012)))
  expect_error(get_municipality_groups(c("N00914,U00405"), 1440, list(2010, 2011, 2012)))
  expect_error(get_municipality_groups(list("N00914,U00405"), 1440, c(2010, 2011, 2012)))
  expect_error(get_municipality_groups(list("N00914,U00405"), c(1440, 1440), list(2010, 2011, 2012)))
  expect_error(get_municipality_groups(list("N00914,U00405"), data.frame(), list(2010, 2011, 2012)))
  expect_error(get_municipality_groups(list(), 1440, list(2010, 2011, 2012)))
  expect_error(get_municipality_groups("N00914", 1440))
  expect_error(get_municipality_groups("N00914", c()))
  expect_error(get_municipality_groups(list(), 1440))
})

test_that("Wrong user input is detected from search", {
  expect_error(get_search_results("N00914", 1440, list(2010, 2011, 2012)))
  expect_error(get_search_results(c("N00914,U00405"), 1440, list(2010, 2011, 2012)))
  expect_error(get_search_results(list("N00914,U00405"), 1440, c(2010, 2011, 2012)))
  expect_error(get_search_results(list("N00914,U00405"), c(1440, 1440), list(2010, 2011, 2012)))
  expect_error(get_search_results(list("N00914,U00405"), data.frame(), list(2010, 2011, 2012)))
  expect_error(get_search_results(list(), 1440, list(2010, 2011, 2012)))
  expect_error(get_search_results("N00914", 1440))
  expect_error(get_search_results("N00914", c()))
  expect_error(get_search_results(list(), 1440))
})


## Testing functionality
test_that("get_municipality() is working", {
  df = get_municipality(mun)
  expect_true(is.list(df))
  expect_true(is.character(df["values"][[1]][[1]]$id))
  expect_true(is.character(df["values"][[1]][[1]]$title))
  expect_true(is.character(df["values"][[1]][[1]]$type))
})

test_that("get_kpi() is working", {
  df = get_kpi(kpi)
  expect_true(is.list(df))
  expect_true(is.character(df[["values"]][[1]]$auspices))
  expect_true(is.character(df[["values"]][[1]]$municipality_type))
  expect_true(is.character(df[["values"]][[1]]$perspective))
  expect_true(is.numeric(df[["values"]][[1]]$is_divided_by_gender))
  expect_true(is.character(df[["values"]][[1]]$title))
})

test_that("get_municipality_groups() is working", {
  df = get_municipality_groups(mun_group)
  expect_true(is.list(df))
  expect_true(is.character(df[["values"]][[1]]$id))
  expect_true(is.character(df[["values"]][[1]]$type))
  expect_true(is.character(df[["values"]][[1]]$title))
})

test_that("get_kpi_groups() is working", {
  df = get_kpi_groups(kpi_group)
  expect_true(is.list(df))
  expect_true(is.character(df["values"][[1]][[1]]$id))
  expect_true(is.list(df["values"][[1]][[1]]$members))
  expect_true(is.character(df["values"][[1]][[1]]$title))
})

test_that("get_search_results() is working", {
  df = get_search_results(kpi_list= search_res$kpi_list, year_list= search_res$year_list)
  expect_true(is.list(df))
  expect_true(is.character(df[["values"]][[1]]$kpi))
  expect_true(is.character(df[["values"]][[1]]$municipality))
  expect_true(is.numeric(df[["values"]][[1]]$period))
  expect_true(is.list(df[["values"]][[1]]$values))
})