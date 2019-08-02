#' @title \code{tidy_lm_add_logical_significance}
#' @description add a logical column of significance comparing p-values to alpha
#' @author Ekarin Eric Pongpipat
#' @param tidy_df a \code{tidy} table of \code{lm} results
#' @param alpha = 0.05 (default). can specify another alpha value or a vector \code{`c()`} of alpha values
#'
#' @return logical column of significance (i.e., TRUE or FALSE) by comparing p
#' to the cut-off alpha. this column is added to a \code{tidy} table of a \code{lm} result
#'
#' @export
#'
#' @examples
#' packages <- c("broom", "bootPermBroom", "dplyr")
#' xfun::pkg_attach(packages, message = F, install = T)
#'
#' lm(salary ~ sex, carData::Salaries) %>%
#'   tidy() %>%
#'   tidy_lm_add_logical_significance(., c("0.01", "0.05"))
tidy_lm_add_logical_significance <- function(tidy_df, alpha = 0.05) {

  # load and install packages if not already ----
  suppressMessages(require(xfun))
  packages <- c("dplyr", "stringr", "tidyr")
  pkg_attach(packages, message = F, install = T)

  # grab column names of p-values ----
  col_p_value_list <- colnames(tidy_df) %>%
    str_subset(., paste0(c("p.value", "p_value"), collapse = "|"))

  # for-loop through each p-value column and specified alpha ----
  for (i in 1:length(col_p_value_list)) {
    for (j in 1:length(alpha)) {
      tidy_df <- tidy_df %>%
        mutate(sig = ifelse(eval(as.name(col_p_value_list[i])) < alpha[j], T, F))

      colnames(tidy_df)[length(tidy_df)] <- str_replace(col_p_value_list[i], "p.value", "sig") %>%
        str_replace(., "p_value", "sig") %>%
        paste0(., "_thr_", alpha[j])
    }

  }
  return(tidy_df)
}


