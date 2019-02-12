#' @title \code{tidy_lm_add_logical_significance}
#' @description add column of logical significance to lm tidy table
#' @author Ekarin Eric Pongpipat
#' @param tidy_df
#' @param p_values
#' @param alpha
#' @param sig_column_name
#'
#' @import tibble dplyr broom
#'
#' @return logical column of significance (i.e., TRUE or FALSE) by comparing p
#' to the cut-off alpha. this column is added to a \code{tidy} table of a \code{lm}
#' @export
#'
#' @examples
add_logical_p_values <- function(tidy_df, p_values = p.value, alpha = 0.05, sig_column_name) {
  tidy_df %>%
    rowwise() %>%
    mutate(!!sig_column_name := ifelse(as.numeric(eval(as.name(p_values))) < alpha, TRUE, FALSE)) %>%
    ungroup()
}
