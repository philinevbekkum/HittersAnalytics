#' Summarize Baseball Player Salary or Any Other Variable
#'
#' This function provides a summary of any variable (e.g., Salary, Hits) in the dataset,
#' including basic statistics such as mean, median, standard deviation, and summary statistics.
#' The function works on the **Hitters** dataset from the ISLR package by default, but can be
#' applied to any dataset.
#'
#' @param dataset A data frame containing the dataset. Default is the **Hitters** dataset from the ISLR package.
#' @param variable_name A string representing the column name of the variable to be summarized (e.g., "Salary", "Hits").
#'
#' @return A list containing:
#'   \item{summary}{Summary statistics for the chosen variable (e.g., Min, 1st Qu., Median, Mean, 3rd Qu., Max).}
#'   \item{mean}{Mean of the chosen variable.}
#'   \item{median}{Median of the chosen variable.}
#'   \item{sd}{Standard deviation of the chosen variable.}
#'   \item{min}{Minimum value of the chosen variable.}
#'   \item{max}{Maximum value of the chosen variable.}
#'
#' @examples
#' # Load the ISLR package to access the Hitters dataset
#' library(ISLR)
#'
#' # Summarize Salary in the Hitters dataset
#' summarize_variable(Hitters, "Salary")
#'
#' # Summarize Hits in the Hitters dataset
#' summarize_variable(Hitters, "Hits")
#'
#' @importFrom stats median sd na.omit
#' @export
summarize_variable <- function(dataset = ISLR::Hitters, variable_name) {

  if (!(variable_name %in% colnames(dataset))) {
    stop(paste("The dataset does not contain a column named '", variable_name, "'", sep = ""))
  }

  dataset_clean <- na.omit(dataset)

  variable_summary <- summary(dataset_clean[[variable_name]])

  mean_value <- mean(dataset_clean[[variable_name]], na.rm = TRUE)
  median_value <- median(dataset_clean[[variable_name]], na.rm = TRUE)
  sd_value <- sd(dataset_clean[[variable_name]], na.rm = TRUE)
  min_value <- min(dataset_clean[[variable_name]], na.rm = TRUE)
  max_value <- max(dataset_clean[[variable_name]], na.rm = TRUE)

  summary_list <- list(
    summary = variable_summary,
    mean = mean_value,
    median = median_value,
    sd = sd_value,
    min = min_value,
    max = max_value
  )

  return(summary_list)
}
