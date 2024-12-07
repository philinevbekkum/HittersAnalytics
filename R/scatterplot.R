#' Create Scatter Plots for Various Variables
#'
#' This function generates a scatter plot for different pairs of variables. You can specify any pair of numeric variables
#' from the dataset, such as (experience, salary), (salary, hits), or (salary, walks), to explore their relationships.
#'
#' @param x_var A character string specifying the variable for the x-axis.
#' @param y_var A character string specifying the variable for the y-axis.
#' @return A `ggplot2` plot object representing the scatter plot between the two chosen variables.
#' @importFrom ggplot2 ggplot geom_point labs theme_minimal aes
#' @importFrom dplyr filter
#' @importFrom rlang sym
#' @examples
#' # Example 1: Scatter plot of Salary vs Hits
#' scatterplot("Salary", "Hits")
#'
#' # Example 2: Scatter plot of Salary vs Years
#' scatterplot("Salary", "Years")
#'
#' # Example 3: Scatter plot of Hits vs Runs
#' scatterplot("Hits", "Runs")
#'
#' # Example 4: Scatter plot of Years vs Walks
#' scatterplot("Years", "Walks")
#'
#' @export
scatterplot <- function(x_var, y_var) {

  # Check if the variables are valid column names in the Hitters dataset
  if (!(x_var %in% colnames(ISLR::Hitters)) | !(y_var %in% colnames(ISLR::Hitters))) {
    stop("Both x_var and y_var must be valid column names from the 'Hitters' dataset.")
  }

  # Create the plot with proper handling of NAs
  plot <- ggplot2::ggplot(data = dplyr::filter(ISLR::Hitters,
                                               !is.na(!!rlang::sym(x_var)),
                                               !is.na(!!rlang::sym(y_var))),
                          mapping = ggplot2::aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
    ggplot2::geom_point(color = "red", alpha = 0.6) +
    ggplot2::labs(title = paste(x_var, "vs", y_var),
                  x = x_var, y = y_var) +
    ggplot2::theme_minimal()

  return(plot)
}
