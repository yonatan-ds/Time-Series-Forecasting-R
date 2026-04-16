# Time Series Analysis and Forecasting in R 📊

This repository contains an advanced statistical analysis project focused on time series forecasting for health and birth data. Using R, I implemented models to decompose trends and predict future patterns.

## 🚀 Key Features
- [cite_start]**Data Decomposition:** Analyzed seasonal and trend components using `decompose()`[cite: 1, 11, 54].
- [cite_start]**Statistical Testing:** Applied Ljung-Box tests for randomness and checked stationarity with ACF/PACF plots[cite: 1, 15, 18, 60].
- [cite_start]**Model Comparison:** Compared multiple ARIMA models to find the best fit based on AIC[cite: 1, 29, 31, 73].
- [cite_start]**Future Forecasting:** Used `auto.arima()` and `forecast()` to predict future values for 12-month periods[cite: 1, 38, 42, 81, 85].

## 🛠 Tools Used
- **Language:** R
- **Key Libraries:** `forecast`, `tseries`
- [cite_start]**Techniques:** ARIMA Modeling, Log Transformation, Differencing[cite: 1, 8, 23, 62].

## 📂 Datasets
- [cite_start]**Cancer Rate Data:** Historical trends from 1930 onwards[cite: 1, 4, 9].
- [cite_start]**NYC Birth Data:** Monthly records starting from 1946[cite: 1, 46, 50].
