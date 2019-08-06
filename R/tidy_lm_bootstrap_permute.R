#' @title \code{tidy_lm_bootstrap_permute}
#' @description \code{permute} \code{lm} and output the results as a \code{tidy} table.
#' @author Ekarin Eric Pongpipat
#' @param data a data.frame to be analyzed
#' @param formula a formula to be analyzed as typically written for the \code{lm} function
#' @param n_bootstrap = 1000 (default) the number of bootstraps to perform
#' @param bootstrap_ci = 0.95 (default)
#' @param n_permute = 1000 (default) the number of permutations to perform
#' @param var_permute variable(s) to unlink in the permutation
#' @param r_squared_ci = 0.95 (default)
#' @return outputs \code{tidy} table that includes the p.value from the permutation of a \code{lm} test
#'
#' @examples
#' packages <- c("broom", "broomExtra", "dplyr", "modelr", "purrr", "tibble")
#' xfun::pkg_attach2(packages, message = F)
#'
#' data <- tibble(
#'   a = scale(sample.int(100), scale = F),
#'   b = scale(sample.int(100), scale = F),
#'   c = b^2,
#'   d = scale(sample.int(100), scale = F)
#' )
#'
#' tidy_lm_bootstrap_permute(data, a ~ b, var_permute = "a")
#' @export
tidy_lm_bootstrap_permute <- function(data, formula, n_bootstrap = 10000, bootstrap_ci = 0.95, n_permute = 10000, var_permute, r_squared_ci = 0.95) {

  # load packages if not already ----
  packages <- c("broom", "dplyr", "modelr", "purrr", "tibble")
  xfun::pkg_attach2(packages, message = F)

  # initial-check ----
  if (n_permute <= 1) {
    stop(paste0("n_permute must be larger than 1"))
  } else if (is.null(var_permute)) {
    stop(paste0("var_permute must be defined"))
  }

  # lm ----
  lm <- lm(as.formula(formula), data)
  lm_tidy <- lm %>% tidy()

  # boostrap ----
  df_bootstrap <- modelr::bootstrap(data, n = n_bootstrap)
  df_bootstrap_lm <- map(df_bootstrap[["strap"]], ~ lm(as.formula(formula), data = .))
  df_bootstrap_tidy <- map_df(df_bootstrap_lm, broom::tidy, .id = "id")
  df_bootstrap_tidy_rsq <- df_bootstrap_tidy %>% tidy_lm_add_r_squared(., nrow(data))

  # permute ----
  df_permute <- modelr::permute(data, n_permute, var_permute)
  df_lm_permute <- map(df_permute[["perm"]], ~ lm(as.formula(formula), data = .))
  df_lm_permute_tidy <- map_df(df_lm_permute, broom::tidy, .id = "id")

  # combine tables ----
  for (term_name in unique(df_lm_permute_tidy$term)) {
    lm_tidy_name <- lm_tidy %>%
      filter(term == term_name)
    df_lm_bootstrap_tidy_term <- df_bootstrap_tidy_rsq %>%
      filter(term == term_name)
    df_lm_permute_tidy_term <- df_lm_permute_tidy %>%
      filter(term == term_name)

    bootstrap_ci_ll <- quantile(df_lm_bootstrap_tidy_term$estimate, (1 - bootstrap_ci) / 2)
    bootstrap_ci_ul <- quantile(df_lm_bootstrap_tidy_term$estimate, ((1 - bootstrap_ci) / 2) + bootstrap_ci)

    bootstrap_rsq_ll <- quantile(df_lm_bootstrap_tidy_term$r.squared, (1 - r_squared_ci) / 2)
    bootstrap_rsq_ul <- quantile(df_lm_bootstrap_tidy_term$r.squared, ((1 - r_squared_ci) / 2) + r_squared_ci)

    bootstrap_adj_rsq_ll <- quantile(df_lm_bootstrap_tidy_term$adj.r.squared, (1 - r_squared_ci) / 2)
    bootstrap_adj_rsq_ul <- quantile(df_lm_bootstrap_tidy_term$adj.r.squared, ((1 - r_squared_ci) / 2) + r_squared_ci)

    sign <- lm_tidy_name$estimate / lm_tidy_name$estimate
    if (sign == 1) {
      p_permute <- (sum(df_lm_permute_tidy_term$estimate >= lm_tidy_name$estimate) + 1) / n_permute
    } else {
      p_permute <- (sum(df_lm_permute_tidy_term$estimate <= lm_tidy_name$estimate) + 1) / n_permute
    }

    bootstrap_permute_tidy <- tibble(
      term = term_name,
      ci.ll.bootstrap = bootstrap_ci_ll,
      ci.ul.bootstrap = bootstrap_ci_ul,
      p.permuate = p_permute,
      r.squared.ci.ll = bootstrap_rsq_ll,
      r.squared.ci.ul = bootstrap_rsq_ul,
      adj.r.squared.ci.ll = bootstrap_adj_rsq_ll,
      adj.r.squared.ci.ul = bootstrap_adj_rsq_ul
    )
    if (term_name == unique(df_lm_permute_tidy$term)[1]) {
      bootstrap_permute_tidy_full <- bootstrap_permute_tidy
    } else {
      bootstrap_permute_tidy_full <- rbind(bootstrap_permute_tidy_full, bootstrap_permute_tidy)
    }
  }

  colnames(bootstrap_permute_tidy_full) <- c(
    "term",
    paste0("estimate_ci_", bootstrap_ci, c("_ll", "_ul"), "_boot_", n_bootstrap),
    paste0("p_value_permute_", n_permute),
    paste0("r_sq_ci_", r_squared_ci, c("_ll", "_ul"), "_boot_", n_bootstrap),
    paste0("r_sq_adj_ci_", r_squared_ci, c("_ll", "_ul"), "_boot_", n_bootstrap)
  )

  lm_tidy <- full_join(lm_tidy, bootstrap_permute_tidy_full, by = "term")
  lm_tidy <- lm_tidy %>% tidy_lm_add_r_squared(., nrow(data))
  return(lm_tidy)
}
