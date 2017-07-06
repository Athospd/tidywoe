library(tidyverse)
library(magrittr)

set.seed(1)
df <- data.frame(x1 = sample(c("A", "B", "C"), size = 20, replace = TRUE) %>% factor,
                 x2 = sample(c("A", "B", "C"), size = 20, replace = TRUE) %>% factor) %>%
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
})

test_that("woe_table returns a proper tibble", {
  expect_equal(woe_table(df$y, df$x1) %>% dim, c(3, 7))
  expect_identical(woe_table(df$y, df$x1) %>% names, c("explanatory", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe"))
})

#------------------------------------
context("woe_dictionary")

test_that("woe_dictionary returns a proper tibble", {
  expect_equal(woe_dictionary(df, y) %>% class, c("tbl_df", "tbl", "data.frame"))
  expect_equal(woe_dictionary(df, y) %>% dim, c(6, 8))
  expect_identical(woe_dictionary(df, y) %>% names, c("variable", "explanatory", "n_tot", "n_A", "n_B", "p_A", "p_B", "woe"))
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
