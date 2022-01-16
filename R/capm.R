#' CAPM regression for various time windows
#'
#' @param x : portfolio level data frame
#' @param window : months or days
#' @param freq : monthly or daily
#'
#' @return beta coefficient
#' @export
#'
#' @examples
capm_regression <- function(x, window, freq){
  # Dropping missing values
  x <- na.omit(x)
  
  # Determine the minimum # of obs to estimate beta
  if(freq == "monthly"){
    if (window == 12) {check <- 10}
    if (window == 24) {check <- 20}
    if (window == 36) {check <- 24}
    if (window == 60) {check <- 24}
  }
  if (freq == "daily") {
    # NOTE: as of 01/13/2022, I don't have daily data
    if (window == 1) {check <- 15}
    if (window == 3) {check <- 50}
    if (window == 6) {check <- 100}
    if (window == 12) {check <- 200}
    if (window == 24) {check <- 450}
  }
  
  # Check if minimum # of obs is satisfied
  if(nrow(x) < check){
    return(NA)
  } else {
    reg <- summary(lm(ret_adj_excess ~ mkt_excess, data = x))
    return(tibble(beta = reg$coefficients[2,1],
                se = reg$coefficients[2,2]))
  }
}


#' Rolling capm regressoin for each window and frequency
#'
#' @param x : portfolio level data frame
#' @param window : months or days
#' @param freq : monthly or daily
#'
#' @return result of the rolling regression
#' @export
#'
#' @examples
rolling_capm_regression <- function(x, window, freq){
  # Nested tibbles of daily data need to be binded
  if(freq == "daily"){
    x <- bind_rows(x)
  }
  out <- slide_period(.x = x, .i = x$month, .period = "month",
                      .f = function(x) {capm_regression(x, window, freq)},
                      .before = (window - 1), .complete = FALSE)
  return(out)
}
