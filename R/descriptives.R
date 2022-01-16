#' Compute correlation matrix
#'
#' @param data a matrix or data frame 
#' @param use see ?correlate
#' @param method see ?correlate 
#' @param diagonal see ?correlate
#' @param quiet see ?correlate
#' @param shave_upper see ?correlate
#'
#' @return correlation data fram
#' @export
#'
#' @examples
compute_correlation <- function(data, 
                                use = "complete.obs", 
                                method = "pearson",
                                diagonal = 1,
                                quiet = TRUE, 
                                shave_upper = TRUE){
  temp <- data %>% 
    correlate(use = use, method = method, diagonal = diagonal, 
              quiet = quiet) %>% 
    shave(., upper = shave_upper)
  
  return(temp)
}

#' Compute Persistence
#'
#' @param data 
#' @param var 
#' @param tau 
#' @param probs 
#'
#' @return
#' @export
#'
#' @examples
compute_persistence <- function(data, var, tau, probs){
  # Initialized output
  out <- list() 
  for(t in 1:length(tau)){
    # Create a table with lagged dates
    dates <- data %>% 
      distinct(month) %>% 
      arrange(month) %>% 
      mutate(month_lag = month %m-% months(tau[t]))
    
    # Winsorize the data on a monthly basis
    data_winsorized <- data %>% 
      filter(!is.na({{var}})) %>% 
      group_by(month) %>% 
      mutate(beta = DescTools::Winsorize({{var}}, probs = probs)) %>% 
      ungroup() %>% 
      select(permno, month, beta)
    
    # Compute the correlation to the lagged value
    correlation <- data_winsorized %>% 
      left_join(dates, by = "month") %>% 
      left_join(data_winsorized %>% 
                  select(permno, month, beta_lag = beta),
                by = c("permno", "month_lag" = "month")) %>% 
      select(month, beta, beta_lag) %>% 
      na.omit() %>% 
      nest(betas = c(beta, beta_lag)) %>% 
      mutate(correlations = map(betas, compute_correlation)) %>% 
      unnest(correlations) %>% 
      filter(term == "beta_lag") %>% 
      summarise(mean = mean(beta, na.rm = TRUE))
    
    out[[t]] <- tibble(tau = tau[t],
                       {{var}} := as.numeric(correlation))
  }
  return(bind_rows(out))
}
