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
    w <- w[!is.na(x) & !is.na(w)]
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
quibble <- function(x, n_portfolios){
  # Get relevant percentiles
  percentiles <- seq(0, 1, length.out = (n_portfolios + 1))
  percentiles <- percentiles[percentiles > 0 & percentiles < 1]
  
  tibble("{{x}}" := quantile(x, probs = percentiles),
         "{{x}}_q" := percentiles)
}


#' Given a list of breakpoints, sort stcoks into portfolio
#'
#' @param x a value that is sorted into portfolio
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

#' Construct univariate portfolios based on beta
#'
#' @param data 
#' @param var 
#' @param n_portfolios 
#' @param nyse_breakpoints 
#' 
#' @details This function takes long data with (stock, month, window)-level
#' data to construct univariate portfolios.
#'
#' @return
#' @export
#'
#' @examples
construct_univariate_portfolios <- function(data, n_portfolios = 10,
                                            nyse_breakpoints = TRUE){
  # Drop NAs
  data <- data %>% 
    filter(!is.na(beta))
  
  # Determine breakpoints based on NYSE only or all stocks
  if(nyse_breakpoints == TRUE){
    data_quantiles <- data %>% 
      filter(exchange == "NYSE") %>% 
      select(month, window, beta)
  } else {
    data_quantiles <- data %>% 
      select(month, window, beta)
  }
  
  # Compute quantiles
  quantiles <- data_quantiles %>% 
    group_by(month, window) %>% 
    summarise(quibble(beta, n_portfolios),
              .groups = "keep") %>% 
    select(-beta_q) %>% 
    nest(beta_breakpoints = beta)
  
  # Independently sort all stocks into portfolios based on breakpoints
  # WARNING: takes about 100 seconds; paralellization on my laptop didn't work well
  portfolios <- data %>%
    left_join(quantiles, by = c("month", "window")) %>%
    mutate(portfolio = map2_dbl(beta, beta_breakpoints, get_portfolio))  
  
  # Construct mean portfolio characteristics
  # NOTE: return is the one-period future return
  portfolio_ts <- portfolios %>% 
    group_by(month, portfolio, window) %>% 
    summarise(ret_ew = mean(ret_adj_excess_f1, na.rm = TRUE),
              ret_vw = weighted_mean(ret_adj_excess_f1, 
                                     mktcap, na.rm = TRUE),
              ret_mkt = mean(mkt_ff_excess_f1, na.rm = TRUE),
              beta = mean(beta, na.rm = TRUE),
              mktcap = mean(mktcap, na.rm = TRUE),
              mktcap_cpi_all = mean(mktcap_cpi, na.rm = TRUE),
              mktcap_cpi_nyse = mean(mktcap_cpi[exchange == "NYSE"],
                                     na.rm = TRUE),
              n_stocks = n(),
              n_stocks_nyse = sum(exchange == "NYSE", na.rm = TRUE),
              .groups = "drop")
  
  # Construct long-short portfolio
  portfolio_ts_ls <- portfolio_ts %>% 
    select(portfolio, month, window, starts_with("ret")) %>% 
    filter(portfolio %in% c(1, n_portfolios)) %>% 
    pivot_wider(names_from = portfolio,
                values_from = c(ret_ew, ret_vw)) %>% 
    mutate(ret_ew = !!sym("ret_ew_" %+% n_portfolios) - 
             !!sym("ret_ew_" %+% 1),
           ret_vw = !!sym("ret_vw_" %+% n_portfolios) -
             !!sym("ret_vw_" %+% 1),
           portfolio = n_portfolios %+% "-1") %>% 
    select(portfolio, month, window, ret_ew, ret_vw, ret_mkt)
  
  # Combine everything
  out <- portfolio_ts %>% 
    mutate(portfolio = as.character(portfolio)) %>% 
    bind_rows(portfolio_ts_ls) %>% 
    mutate(portfolio = factor(portfolio,
                              levels = c(as.character(seq(1, n_portfolios)),
                                         str_c(n_portfolios, "-1"))))
  
  return(out)
}

#' Estimate portfolio returns
#'
#' @param data 
#' @param ret Either \code{ret_ew} or \code{ret_vw}. Note that both variables
#' are excess returns, including \code{ret_mkt}
#' @param n_portfolios 
#' @param lag 
#'
#' @return
#' @export
#'
#' @examples
estimate_portfolio_returns <- function(data, ret, n_portfolios = 10,
                                       lag = 6){
  # Compute average returns per portfolio  
  average_ret <- data %>% 
    select(portfolio, month, window, ret = {{ret}}) %>% 
    group_by(portfolio, window) %>% 
    nest(data = c(month, ret)) %>% 
    mutate(model = map(data, ~ lm("ret ~ 1", data = .)),
           nw_se = map_dbl(model, ~sqrt(diag(sandwich::NeweyWest(., lag = lag)))),
           model = map(model, tidy)) %>% 
    unnest(model) %>% 
    ungroup() %>% 
    mutate(nw_tstat = estimate / nw_se,
           type = "excess return") %>% 
    select(portfolio, window, estimate, nw_tstat, type) 
  
  # Estimate capm alpha per portfolio
  average_capm_alpha <- data %>% 
    select(portfolio, month, window, ret = {{ret}}, ret_mkt) %>% 
    group_by(portfolio, window) %>% 
    nest(data = c(month, ret, ret_mkt)) %>% 
    mutate(model = map(data, ~ lm("ret ~ 1 + ret_mkt", data =.)),
           nw_se = map_dbl(model,
                           ~sqrt(diag(sandwich::NeweyWest(., lag = lag))[1])),
           model = map(model, tidy)) %>% 
    unnest(model) %>% 
    ungroup() %>% 
    filter(term == "(Intercept)") %>% 
    mutate(nw_tstat = estimate / nw_se,
           type = "alpha") %>% 
    select(portfolio, window, estimate, nw_tstat, type)
  
  # Combine together
  out <- bind_rows(average_ret, average_capm_alpha)
  return(out)
}


#' Reshape the result to make it presentable
#'
#' @param x Output of \code{estimate_portfolio_returns()}
#'
#' @return
#' @export
#'
#' @examples
reshape_portfolio_returns <- function(x){
  average_ret <- x %>% 
    filter(type == "excess return") %>% 
    select(-type)
  
  average_capm_alpha <- x %>% 
    filter(type == "alpha") %>% 
    select(-type)
  
  window_vec <- c(1, 2, 3, 5) %+% "Y"
  out <- list()
  for(i in seq_along(window_vec)){
    average_ret_wide <- average_ret %>%
      filter(window == window_vec[i]) %>% 
      select(estimate, nw_tstat) %>%
      t()
    
    average_capm_alpha_wide <- average_capm_alpha %>%
      filter(window == window_vec[i]) %>% 
      select(estimate, nw_tstat) %>%
      t()
      
    out_i <- rbind(average_ret_wide, average_capm_alpha_wide)
    colnames(out_i) <- c(as.character(seq(1, n_portfolios)),
                         str_c(n_portfolios, "-1"))
    rownames(out_i) <- c("Excess Return", "t-stat", 
                         "CAPM Alpha", "t-stat")
    out[[i]] <- out_i
  }
  return(reduce(out, rbind))
}
