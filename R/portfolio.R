#' Wrapper of \code{weighted.mean()} to deal with NA in weights (\code{w})
#'
#' @param x 
#' @param w 
#' @param ... 
#' @param na.rm 
#'
#' @return
#' @export
#'
#' @examples
weighted_mean <- function(x, w, ..., na.rm = FALSE){
  if(na.rm){
    x_temp <- x[!is.na(x) & !is.na(w)]
    w <- w[!is.na(x) & is.na(w)]
    x <- x_temp
  }
  return(weighted.mean(x, w, ..., na.rm = FALSE))
}

#' Returns quantile of each grouped variable
#'
#' @param x variable
#' @param q quantile cut points
#'
#' @return data frame
#' @export
#'
#' @details Be careful NOT to name the variable when you use this function.
#'
#' @examples
#' # OK
#' as_tibble(mtcars) %>% 
#'   group_by(cyl) %>% 
#'   summarise(quibble(mpg, c(.25, .5, .75)))
#' 
#' # Don't; returns "Data-frame columns"
#' as_tibble(mtcars) %>% 
#'   group_by(cyl) %>% 
#'   summarise(y = quibble(mpg, c(.25, .5, .75)))
#' 
quibble <- function(x, q){
  tibble("{{x}}" := quantile(x, probs = q),
         "{{x}}_q" := q)
}

get_breakpoints_functions <- function(var, n_portfolios = 10){
  # Get relevant percentiles
  percentiles <- seq(0, 1, length.out = (n_portfolios + 1))
  percentiles <- percentiles[percentiles > 0 & percentiles < 1]
  
  # Construct the set of named quantile functions
  percentiles_names <- map_chr(percentiles, ~str_c(rlang::quo_text(enquo(var)), "_q", .x*100))
  percentiles_funs <- map(percentiles, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = percentiles_names)
  
  return(percentiles_funs)
}


#' Given a list of breakpoints, sort stcoks into portfolio
#'
#' @param x 
#' @param breakpoints 
#'
#' @return
#' @export
#'
#' @examples
get_portfolio <- function(x, breakpoints){
  portfolio <- as.integer(1 + findInterval(x, unlist(breakpoints)))
  return(portfolio)
}
