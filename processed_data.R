# function processing data used to generate 4 main graphs

processed_data <- function(stock_data, include_timeseries = FALSE) {
  df <- stock_data %>%
    arrange(symbol, date) %>%
    filter(!is.na(adjusted)) %>%
    group_by(symbol) %>%
    mutate(
      daily_return = adjusted / lag(adjusted) - 1,
      cum_return = adjusted / first(adjusted) - 1
    )
  
  if (include_timeseries) {
    return(df %>% ungroup())
  }
  
  df %>%
    summarise(
      Start = first(adjusted),
      End = last(adjusted),
      Return = (End - Start) / Start,
      Volatility = sd(daily_return, na.rm = TRUE),
      Drawdown = min(daily_return, na.rm = TRUE),
      Recovery = max(daily_return, na.rm = TRUE),
      .groups = "drop"
    )
}