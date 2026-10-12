#' Calculate Pythagorean expected winning percentage
#'
#' Estimates the winning percentage a team should have based on the
#' points (or runs) it scored and allowed.
#'
#' @param points_for Points or runs scored. A positive number or numeric vector.
#' @param points_against Points or runs allowed. A positive number or numeric vector.
#' @param exponent Exponent used in the formula. Defaults to 2.
#'
#' @return A numeric vector of expected winning percentages between 0 and 1.
#' @export
#'
#' @examples
#' pythag_win_pct(800, 650)
pythag_win_pct <- function(points_for, points_against, exponent = 2) {
  if (!is.numeric(points_for) || !is.numeric(points_against)) {
    stop("points_for and points_against must be numeric.")
  }
  if (any(points_for <= 0) || any(points_against <= 0)) {
    stop("points_for and points_against must be positive.")
  }
  points_for^exponent / (points_for^exponent + points_against^exponent)
}
