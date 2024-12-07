#' Calculate the Correlation Between Two Variables
#'
#' This function calculates the Pearson correlation coefficient between two specified variables in a dataset.
#' It checks if both variables exist in the dataset and handles missing values by removing rows with NA values.
#'
#' @param dataset A data frame containing the dataset.
#' @param x_var A character string specifying the name of the first variable (e.g., "Salary").
#' @param y_var A character string specifying the name of the second variable (e.g., "Hits").
#'
#' @return A numeric value representing the Pearson correlation coefficient between the two variables.
#' @importFrom stats cor
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' # Assuming the ISLR package and Hitters dataset are loaded
#' library(ISLR)
#'
#' # Example 1: Calculate correlation between Salary and Hits
#' correlation_result_1 <- calculate_correlation(Hitters, "Salary", "Hits")
#' print(correlation_result_1)
#'
#' # Example 2: Calculate correlation between Years and Walks
#' correlation_result_2 <- calculate_correlation(Hitters, "Years", "Walks")
#' print(correlation_result_2)
#'
#' # Example 3: Calculate correlation between AtBat and HmRun
#' correlation_result_3 <- calculate_correlation(Hitters, "AtBat", "HmRun")
#' print(correlation_result_3)
#'
#' # Example 4: Calculate correlation between CRuns and CRBI
#' correlation_result_4 <- calculate_correlation(Hitters, "CRuns", "CRBI")
#' print(correlation_result_4)
#'
#' # Example 5: Calculate correlation between Runs and RBI
#' correlation_result_5 <- calculate_correlation(Hitters, "Runs", "RBI")
#' print(correlation_result_5)

calculate_correlation <- function(dataset, x_var, y_var) {

  if (!(x_var %in% colnames(dataset)) | !(y_var %in% colnames(dataset))) {
    stop("Both x_var and y_var must be valid column names from the provided dataset.")
  }

  dataset_clean <- dplyr::filter(dataset, !is.na(!!rlang::sym(x_var)), !is.na(!!rlang::sym(y_var)))

  correlation_value <- stats::cor(dataset_clean[[x_var]], dataset_clean[[y_var]])

  return(correlation_value)
}
