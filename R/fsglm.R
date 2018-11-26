#' Execute single variable glm collectively.
#'
#' @param df input data frame
#' @param hier_key group key column name with character type
#' @param y target column name with character type
#' @param x explanatory column name with character type
#' @param family glm family argument
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom dplyr group_by_
#' @importFrom tidyr nest
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @importFrom utils data
#' @importFrom stats glm
#' @importFrom broom tidy
#' @export
#'
fsglm <- function(df, hier_key, y, x, family = "gaussian") {

  res <- df %>%
    select(hier_key, x, y) %>%
    group_by_(hier_key) %>%
    nest() %>%
    mutate(
      reg = map(
        .$data,
        ~ glm(
            sprintf("%s ~ %s", y, x),
            data = .,
            family = family) %>%
          tidy())) %>%
    select(-data) %>%
    unnest()

  return(res)

}
