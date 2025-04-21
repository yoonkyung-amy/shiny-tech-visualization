# 📈 US Tech Stocks Analysis Dashboard (2020–2025)

As part of the STAT 442 final project at the University of Waterloo, this interactive dashboard uses daily stock price data from Yahoo Finance to explore the post-COVID performance of 20 major U.S. technology companies.
Built in R using **Shiny**, it provides different types of visualization to compare stock returns, volatility, risk, and growth trend.

👉 **[Access the live app here](https://yoonkyung-amy.shinyapps.io/tq_stocks_analysis/)**  
*(Hosted on ShinyApps.io)*

---

## Features

### 1️⃣ Return vs. Volatility (Scatter Plot)
- Compares each stock’s cumulative return and volatility
- Point size reflects return magnitude
- Hover for detailed values (using `plotly`)

### 2️⃣ Cumulative Return Over Time (Line Plot)
- Tracks normalized cumulative returns from the selected start date
- Optional **facet view** for individualized Y-scales
- Great for observing growth patterns over time

### 3️⃣ Radar Chart: Risk & Return Metrics
- Visualizes:
  - Total Return
  - Volatility
  - Drawdown (worst day)
  - Recovery (best day)
- Uses **base R plotting only**

### 4️⃣ Performance Summary Table (gt Table)
- GT table with:
  - Start/End prices
  - Return, Volatility, Risk metrics
---

## 📊 Data Source

Data is retrieved live from Yahoo Finance using the `tidyquant` package. The companies are selected based on:

🔗 [Top 20 U.S. Tech Stocks by Market Cap (Yahoo Finance)](https://finance.yahoo.com/research-hub/screener/sec-ind_sec-largest-equities_technology/)

---

## Technologies Used

- **R** / **Shiny**
- `tidyquant`, `dplyr`, `plotly`, `ggplot2`, `gt`, `scales`, `RColorBrewer`
- `shinyapps.io` for hosting

---

## Files

| File              | Description                             |
|-------------------|-----------------------------------------|
| `app.R`           | Main Shiny app script                   |
| `processed_data.R`| Custom data prepping function           |
| `ticker_info.R`   | includes: ticker, name, logo URL        |
| `README.md`       | information about the app               |
