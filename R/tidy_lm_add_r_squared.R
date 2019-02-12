#' @title \code{tidy_lm_add_r_squared}
#' @description add R^2 and adjusted R^2
#' @author Ekarin Eric Pongpipat
#' @param tidy_df
#' @param n
#'
#' @import tibble dplyr broom
#'
#' @return column of r.squared and adj.r.squared added a \code{tidy} table of a \code{lm}
#' @export
#'
#' @examples
#' df <- tibble(a = scale(sample.int(100), scale = F),
#'                            b = scale(sample.int(100), scale = F),
#'                            c = b^2,
#'                            d = scale(sample.int(100), scale = F))
#' lm(a ~ b, df) %>%
#'   tidy() %>%
#'   tidy_add_r_squared(., n = nrow(df))
tidy_lm_add_r_squared <- function(tidy_df, n) {
  n_params <- tidy$term %>% unique() %>% length()
  df_residual <- n - n_params
  tidy_df %>%
    rowwise() %>%
    mutate(r.squared = statistic^2/(statistic^2 + df_residual),
           adj.r.squared = 1-(1-r.squared)*((n-1)/(n-n_params)))
}


