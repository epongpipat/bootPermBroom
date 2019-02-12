#' @title \code{tidy_lm_add_r_squared}
#' @description add r.squared and adj.r.squared to \code{tidy} table of \code{lm} results.
#' @author Ekarin Eric Pongpipat
#' @param tidy_df a \code{tidy} table of \code{lm} results
#' @param n the number of participants analyzed. Using nrow(data), where data is the dataframe used in \code{lm} is a good option.
#'
#'
#' @return column of r.squared and adj.r.squared added a \code{tidy} table of a \code{lm}
#' @export
#'
#' @examples
#' data <- tibble(
#'   a = scale(sample.int(100), scale = F),
#'   b = scale(sample.int(100), scale = F),
#'   c = b^2,
#'   d = scale(sample.int(100), scale = F)
#' )
#' lm(a ~ b, data) %>%
#'   tidy() %>%
#'   tidy_lm_add_r_squared(., n = nrow(data))
tidy_lm_add_r_squared <- function(tidy_df, n) {
  if (n <= 1) {
    stop(paste0("n must be larger than 1"))
  }

  n_params <- tidy_df$term %>% unique() %>% length()
  df_residual <- n - n_params
  tidy_df %>%
    rowwise() %>%
    mutate(
      r.squared = statistic^2 / (statistic^2 + df_residual),
      adj.r.squared = 1 - (1 - r.squared) * ((n - 1) / (n - n_params))
    )
}
