#' Calculate winning percentage
#'
#' Calculates the proportion of games won from wins and losses.
#'
#' @param wins Number of wins. A non-negative number or numeric vector.
#' @param losses Number of losses. A non-negative number or numeric vector.
#'
#' @return A numeric vector of winning percentages between 0 and 1.
#' @export
#'
#' @examples
#' win_pct(103, 59)
win_pct <- function(wins, losses) {
  if (!is.numeric(wins) || !is.numeric(losses)) {
    stop("wins and losses must be numeric.")
  }
  if (any(wins < 0) || any(losses < 0)) {
    stop("wins and losses cannot be negative.")
  }
  wins / (wins + losses)
}
