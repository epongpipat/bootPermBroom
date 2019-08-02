#' @title \code{tidy_lm_add_multiple_comparison_correction}
#' @description add adjusted p-values (i.e., multiple comparison correction methods)
#' as a new column using the \code{p.adjust()} function
#' @author Ekarin Eric Pongpipat
#'
#' @param tidy_df a \code{tidy} table of \code{lm} results
#' @param methods = "all" (default). a string or vector \code{c()} of p.adjust methods,
#' which include: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", or "all"
#'
#' @return numeric adjusted p.values column using all or defined mutliple
#' comparison correction methods. this column is added to a \code{tidy} table
#' of a \code{lm} result
#'
#' @export
#'
#' @examples
#' packages <- c("broom", "bootPermBroom", "dplyr")
#' xfun::pkg_attach(packages, message = F, install = T)
#'
#' lm(salary ~ sex, carData::Salaries) %>%
#'   tidy() %>%
#'   tidy_lm_add_multiple_comparison_correction()
tidy_lm_add_multiple_comparison_correction <- function(tidy_df, methods = "all") {

  #  load and install packages if not already ----
  suppressMessages(require(xfun))
  packages <- c("dplyr", "stringr")
  pkg_attach(packages, message = F, install = T)

  # list all available p.adjust methods ----
  # within the p.adjust() within the stats package
  methods_list <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr") %>% sort()

  # subset list to specified options ----
  if (methods[1] != "all") {
    methods_list <- str_subset(methods_list, paste0(methods, collapse = "|"))
  }

  # for loop and add each method specified ----
  if (length(methods_list) > 0) {
    for (i in 1:length(methods_list)) {
      tidy_df <- tidy_df %>%
        mutate(!!paste0("p_value_", methods_list[i]) := p.adjust(p.value, methods_list[i]))
    }
    return(tidy_df)
  } else {
    stop(paste0('No valid method specified. Valid options include: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", or "fdr"'))
  }

}
