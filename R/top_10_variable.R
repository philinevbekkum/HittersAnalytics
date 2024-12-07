#' Get the Top 10 of a Variable
#'
#' This function retrieves the top 10 values for a given variable (e.g., `Salary`, `Hits`, `Runs`).
#' It sorts the dataset by the specified variable and returns the top 10 values along with the corresponding player names from the row names.
#'
#' @param dataset A data frame containing the dataset (e.g., `Hitters`).
#' @param variable A character string specifying the column name of the variable (e.g., "Salary", "Hits").
#'
#' @return A data frame containing the top 10 values of the specified variable along with the corresponding player names.
#' @importFrom dplyr arrange desc
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export
#'
#' @examples
#' # Assuming the ISLR package and Hitters dataset are loaded
#' library(ISLR)
#'
#' # Example 1: Get the top 10 players by Salary
#' top_10_salary <- top_10_variable(Hitters, "Salary")
#' print(top_10_salary)
#'
#' # Example 2: Get the top 10 players by Hits
#' top_10_hits <- top_10_variable(Hitters, "Hits")
#' print(top_10_hits)
#'
#' # Example 3: Get the top 10 players by Home Runs
#' top_10_home_runs <- top_10_variable(Hitters, "HmRun")
#' print(top_10_home_runs)
#'
#' # Example 4: Get the top 10 players by Runs
#' top_10_runs <- top_10_variable(Hitters, "Runs")
#' print(top_10_runs)
#'
top_10_variable <- function(dataset, variable) {

  if (!(variable %in% colnames(dataset))) {
    stop(paste("The dataset does not contain a column named '", variable, "'", sep = ""))
  }

  dataset_with_names <- cbind(Player = gsub("^[-]", "", rownames(dataset)), dataset)

  top_10 <- dataset_with_names %>%
    dplyr::arrange(dplyr::desc(!!rlang::sym(variable))) %>%
    head(10)

  return(top_10[, c("Player", variable)])
}
