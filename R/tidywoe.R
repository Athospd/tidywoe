#' Cross table with woe between a (dicotomous) response and a explanatory
#'
#' Calculates some summaries and the WoE between a dicotomous response and a given explanatory variable.
#' Used to biuld the dictionary.
#'
#' @param response The dependent variable. A atomic vector with exactly 2 distinct values.
#' @param explanatory A atomic vector, usualy with few distinct values.
#'
#' @return a tibble with summaries and woe. Warning: WoE can possibly be -Inf.
#'
#' @examples
#'
#' resp <- rep(c("A", "B"), 20)
#' expl <- sample(c("X", "Y", "Z", "W"), size = 40, replace = TRUE)
#' woe_table(resp, expl)
#'
#' @export
woe_table <- function(response, explanatory) {
  response_original_labels <- response %>% unique

  if(length(response_original_labels) != 2) stop(sprintf("response must have exactly 2 categories (has %s)", length(response_original_labels)))

  woe_expr <- parse(text = sprintf("log(p_%s/p_%s)", response_original_labels[1], response_original_labels[2]))

  counts <- tibble::tibble(response, explanatory) %>%
    dplyr::group_by(response, explanatory) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::group_by(explanatory) %>%
    dplyr::mutate(n_tot = sum(n)) %>%
    dplyr::group_by(response) %>%
    dplyr::mutate(p = n/sum(n)) %>%
    tidyr::gather(summary, value, n, p) %>%
    tidyr::unite(summary_response, summary, response) %>%
    tidyr::spread(summary_response, value, fill = 0) %>%
    dplyr::mutate(woe = eval(woe_expr))
  return(counts)
}

#' WoE dictionary of a set of explanatory variables upon a given dicotomous response
#'
#' Builds the WoE dictionary of a set of explanatory variables upon a given dicotomous response.
#' Convenient to make a WoE version of the given set of explanatory variables and also to allow
#' one to tweak some woe values by hand.
#'
#' @param .data A tbl. The data.frame where the variables come from.
#' @param .response unquoted name of the response variable.
#' @param ... unquoted names of explanatory variables, passed as you would pass variables to \code{dplyr::select()}. This means that you can use all the helpers like \code{starts_with()} and \code{matches()}.
#'
#' @return a tibble with summaries and woe for every given explanatory variable stacked up. Warning: WoE can possibly be -Inf.
#'
#' @details You can pass a custom dictionary to \code{add_woe()}. It must have the exactly the same structure of the output of \code{woe_dictionary()}. One easy way to do this is to tweak a output returned from it.
#'
#' @examples
#'
#' mtcars %>% woe_dictionary(am, cyl, gear:carb)
#'
#' @export
woe_dictionary <- function(.data, .response, ...) {
  .response <- enquo(.response)
  .response_vector <- .data %>% dplyr::pull(!!.response)

  purrr::map(.data %>% dplyr::select(..., -!!.response),
      ~ woe_table(.response_vector, .x)) %>%
    dplyr::bind_rows(.id = "variable")
}

#' Add WoE in a data.frame
#'
#' A tidyverse friendly way to plug WoE versions of a set of explanatory variables against a given dicotomous response.
#'
#' @param .data A tbl. The data.frame to plug the new woe version columns.
#' @param .response unquoted name of the response variable.
#' @param ... unquoted names of explanatory variables, passed as you would pass variables to \code{dplyr::select()}. This means that you can use all the helpers like \code{starts_with()} and \code{matches()}.
#' @param .woe_dictionary a tbl. If NULL the function will build a dictionary for those variables passed to \code{...}. You can pass a custom dictionary too, see \link{\code{woe_dictionary()}} for details.
#'
#' @return a tibble with the original columns of .data plus the woe columns wanted.
#'
#' @details You can pass a custom dictionary to \code{add_woe()}. It must have the exactly the same structure of the output of \code{woe_dictionary()}. One easy way to do this is to tweak a output returned from it.
#'
#' @examples
#'
#' mtcars %>% add_woe(am, cyl, gear:carb)
#'
#' @export
add_woe <- function(.data, .response, ..., .woe_dictionary = NULL) {
  .response <- dplyr::enquo(.response)

  if(is.null(.woe_dictionary)) .woe_dictionary <- .data %>% woe_dictionary(!!.response, ...)

  .woe_dictionary %>%
    dplyr::select(variable, explanatory, woe) %>%
    dplyr::group_by(variable) %>%
    tidyr::nest(.key = "woe_table") %>%
    dplyr::mutate(woe_table = purrr::map2(woe_table, variable, ~ purrr::set_names(.x, c(.y, paste0(.y, "_woe")))) %>% purrr::set_names(variable)) %$%
    purrr::map2(woe_table, variable, ~ dplyr::left_join(.data %>% dplyr::select(!!.y), .x, by = .y) %>% dplyr::select(ends_with("woe"))) %>%
    dplyr::bind_cols(.data, .) %>%
    tibble::as_tibble()
}
