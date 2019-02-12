#' @title \code{permute_lm_tidy}
#' @description perform permutation testing and output permutation p-value in tidy format
#' @author Ekarin Eric Pongpipat
#' @param data a data.frame to be analyzed
#' @param formula a formula to be analyzed as typically written for the \code{lm} function
#' @param n_permute = 1000 (default) the number of permutations to perform
#' @param var_permute variable(s) to unlink in the permutation
#'
#' @return outputs \code{tidy} table that includes the p.value from the permutation of a \code{lm} test
#' @import broom dplyr modelr tibble
#'
#' @examples
#'  set.seed(2019)
#' data <- tibble(a = scale(sample.int(100), scale = F),
#'               b = scale(sample.int(100), scale = F),
#'               c = b^2,
#'               d = scale(sample.int(100), scale = F))
#' df_tidy <- permute_lm_tidy(data = data, formula = "a ~ b + c + d", n_permute = 1000, var_permute = "a")
#' permute_lm_tidy(data = data, formula = "a ~ b + c", n_permute = 100, "a") %>% tidy_add_r_squared(., n = nrow(data))
#' @export
tidy_lm_permute <- function(data, formula, n_permute = 1000, var_permute) {

  if (n_permute <= 1) {
    stop(paste0("n_permute must be larger than 1"))
  } else if (is.null(var_permute)) {
    stop(paste0("var_permute must be defined"))
  }

  lm <- lm(as.formula(formula), data)
  lm_tidy <- lm %>% tidy()

  df_permute <- permute(df, n_permute, var_permute)
  df_lm_permute <- map(df_permute[["perm"]], ~ lm(as.formula(formula), data = .))
  df_lm_permute_tidy <- map_df(df_lm_permute, broom::tidy, .id = "id")

  for (term_name in unique(df_lm_permute_tidy$term)) {
    lm_tidy_name <- lm_tidy %>%
      filter(term == term_name)
    df_lm_permute_tidy_term <- df_lm_permute_tidy %>%
      filter(term == term_name)

    sign <- lm_tidy_name$estimate / lm_tidy_name$estimate
    if (sign == 1) {
      p_permute <- (sum(df_lm_permute_tidy_term$estimate >= lm_tidy_name$estimate) + 1) / n_permute
    } else {
      p_permute <- (sum(df_lm_permute_tidy_term$estimate <= lm_tidy_name$estimate) + 1) / n_permute
    }

    permute_table <- tibble(
      term = term_name,
      p_permuate = p_permute
    )
    if (term_name == unique(df_lm_permute_tidy$term)[1]) {
      permute_table_full <- permute_table
    } else {
      permute_table_full <- rbind(permute_table_full, permute_table)
    }
  }

  colnames(permute_table_full) <- c("term", paste0("p_permute_", n_permute))
  lm_tidy <- full_join(lm_tidy, permute_table_full, by = "term")
  return(lm_tidy)
}
