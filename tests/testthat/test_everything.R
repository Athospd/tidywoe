library(tidyverse)
library(magrittr)

set.seed(1)
df <- data.frame(x1 = sample(c("A", "B", "C"), size = 20, replace = TRUE) %>% factor,
                 x2 = sample(c("A", "B", "C"), size = 20, replace = TRUE)) %>%
  mutate(y = rbinom(20, 1, prob = 1/(1 + exp(-1 * (-4 + as.numeric(x1) + as.numeric(x2)))))) %>%
  mutate(y = if_else(y == 1, "A", "B"))

#------------------------------------
context("woe_table")

test_that("woe_table do not accept different length inputs", {
  expect_error(woe_table(rep(c(0, 1), 20), rep(letters[1:4], 5)))
})

test_that("woe_table accepts only response with 2 distinct categories", {
  expect_error(woe_table(rep(c(0, 1, 2), 10), rep(letters[1:3], 10)))
  expect_error(woe_table(rep(c(0), 30), rep(letters[1:3], 10)))
  expect_error(woe_table(df$x1, df$x2))
})

test_that("woe_table returns a proper tibble", {
  expect_equal(woe_table(df$y, df$x1) %>% dim, c(3, 7))
  expect_identical(woe_table(df$y, df$x1) %>% names, c("explanatory", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe"))
})

test_that("logical response variable are well treated", {
  expect_equal(woe_table(c(TRUE, FALSE, TRUE, FALSE), c("A", "A", "A", "B")) %>% dim, c(2, 7))
})

test_that("logical explanatory variable are well treated", {
  expect_equal(woe_table(c("A", "A", "A", "B"), c(TRUE, FALSE, TRUE, FALSE))$explanatory %>% class, "character")
})

test_that("woe_table ruturns no messages nor warnings", {
  expect_silent(woe_table(c("A", "A", "A", "B"), c(TRUE, FALSE, TRUE, FALSE)))
  expect_silent(woe_table(df$y, df$x1))
})


#------------------------------------
context("woe_dictionary")

test_that("woe_dictionary returns a proper tibble", {
  expect_equal(woe_dictionary(df, y) %>% class, c("tbl_df", "tbl", "data.frame"))
  expect_equal(woe_dictionary(df, y) %>% dim, c(6, 8))
  expect_identical(woe_dictionary(df, y) %>% names, c("variable", "explanatory", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe"))
})

test_that("woe_dictionary accepts numeric, logical and character explanatory variables", {
  expect_equal(dim(woe_dictionary(mutate(df,
                                         x3 = rep(c(TRUE, FALSE), 10),
                                         x4 = rep(c(20, 30), 10)), y)), c(10, 8))
})

test_that("woe_dictionary ruturns no messages nor warnings nor errors", {
  expect_silent(woe_dictionary(df, y, x1))
  expect_silent(woe_dictionary(df %>% mutate(x3 = rep(c(TRUE, FALSE), 10)), y, x3))
})


#------------------------------------
context("add_woe")

test_that("add_woe returns a proper tibble", {
  expect_equal(add_woe(df, y) %>% class, c("tbl_df", "tbl", "data.frame"))
  expect_equal(add_woe(df, y) %>% dim, c(20, 5))
  expect_identical(add_woe(df, y) %>% names, c("x1", "x2", "y", "x1_woe", "x2_woe"))
})

test_that("add_woe accepts only response with 2 distinct categories", {
  expect_error(woe_dictionary(df %>% filter(y %in% "B"), y))
})

test_that("add_woe ruturns no messages nor warnings nor errors", {
  expect_silent(add_woe(df, y, x1))
  expect_silent(add_woe(df %>% mutate(x3 = rep(c(TRUE, FALSE), 10)), y, x3))
})

test_that("add_woe accepts numeric, logical and character explanatory variables", {
  expect_equal(add_woe(df %>% mutate(x3 = rep(c(TRUE, FALSE), 10),
                                     x4 = rep(c(20, 30), 10)), y) %>% dim, c(20, 9))
})

test_that("add_woe returns woe only for those variables that exists in both data and dictionary", {
  expect_equal(names(add_woe(df, y, x2, .woe_dictionary = woe_dictionary(df, y, x1))), c("x1", "x2", "y"))
  expect_equal(names(add_woe(df, y, x1, .woe_dictionary = woe_dictionary(df, y, x1))), c("x1", "x2", "y", "x1_woe"))
  expect_equal(names(add_woe(df, y, .woe_dictionary = woe_dictionary(df, y, x1))), c("x1", "x2", "y", "x1_woe"))
  expect_equal(names(add_woe(df, y, x1, x2, .woe_dictionary = woe_dictionary(df, y, x1))), c("x1", "x2", "y", "x1_woe"))
})

