# 📊 Time Series Analysis and Forecasting with R

This project demonstrates advanced statistical modeling for time-series data. I used R to analyze public health trends and birth rates, focusing on seasonal patterns and future forecasting.

## 🚀 Key Project Highlights
- **Data Transformation:** Applied log-transformations to handle outliers and stabilize variance.
- **Statistical Modeling:** Built and compared several **ARIMA** models to find the best fit for forecasting.
- **Forecasting:** Predicted future trends for a 12-month horizon using the `forecast` library.
- **Randomness Testing:** Performed **Ljung-Box** tests to ensure model reliability.

## 🛠 Tech Stack & Tools
- **Language:** R
- **Key Functions Used:** `ts()`, `decompose()`, `acf()`, `pacf()`, `auto.arima()`, `forecast()`
- **Libraries:** `tseries`, `forecast`

## 📂 Included Analysis
The project covers:
1. **Cancer Rate Analysis:** Historical trend identification and modeling.
2. **NYC Birth Data:** Seasonal decomposition and pattern recognition.
