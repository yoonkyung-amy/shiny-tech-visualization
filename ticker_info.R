# ticker information was separated for the code readability

library(tibble)

ticker_info <- tibble(
  logo = c(
    "https://logo.clearbit.com/apple.com",
    "https://logo.clearbit.com/microsoft.com",
    "https://logo.clearbit.com/nvidia.com",
    "https://logo.clearbit.com/broadcom.com",
    "https://logo.clearbit.com/tsmc.com",
    "https://logo.clearbit.com/oracle.com",
    "https://logo.clearbit.com/sap.com",
    "https://logo.clearbit.com/asml.com",
    "https://logo.clearbit.com/salesforce.com",
    "https://logo.clearbit.com/cisco.com",
    "https://logo.clearbit.com/ibm.com",
    "https://logo.clearbit.com/palantir.com",
    "https://logo.clearbit.com/accenture.com",
    "https://logo.clearbit.com/intuit.com",
    "https://logo.clearbit.com/servicenow.com",
    "https://logo.clearbit.com/uber.com",
    "https://logo.clearbit.com/adobe.com",
    "https://logo.clearbit.com/qualcomm.com",
    "https://logo.clearbit.com/amd.com",
    "https://logo.clearbit.com/ti.com"),
  
  ticker = c("AAPL", "MSFT", "NVDA", "AVGO", "TSM", "ORCL", "SAP", "ASML", "CRM", "CSCO",
             "IBM", "PLTR", "ACN", "INTU", "NOW", "UBER", "ADBE", "QCOM", "AMD", "TXN"),
  
  company = c("Apple Inc.", "Microsoft Corp.", "NVIDIA Corp.", "Broadcom Inc.", "Taiwan Semiconductor",
              "Oracle Corp.", "SAP SE", "ASML Holding", "Salesforce Inc.", "Cisco Systems",
              "IBM Corp.", "Palantir Technologies", "Accenture", "Intuit", "ServiceNow",
              "Uber Technologies", "Adobe Inc.", "Qualcomm Inc.", "AMD", "Texas Instruments")
)

tickers <- c(
  "AAPL", "MSFT", "NVDA", "AVGO", "TSM", "ORCL", "SAP", "ASML", "CRM", "CSCO",
  "IBM", "PLTR", "ACN", "INTU", "NOW", "UBER", "ADBE", "QCOM", "AMD", "TXN"
)