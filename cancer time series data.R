# ==========================================================
# 📊 END-TO-END HEALTH DATA TIME SERIES ANALYSIS
# Annual Cancer Rates
# ==========================================================

# ==========================================================
# 🔵 LOAD REQUIRED PACKAGES
# ==========================================================
packages <- c(
  "tidyverse", "dplyr", "ggplot2",
  "tseries", "urca", "forecast",
  "timetk", "plotly", "DT","shiny","shinydashboard"
)

installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(tidyverse)
library(dplyr)
library(ggplot2)
library(tseries)
library(urca)
library(forecast)
library(timetk)
library(plotly)
library(DT)

# ==========================================================
# 🔵 TASK 1 — DATA LOADING & CLEANING
# ==========================================================
cat("\n===== LOADING DATA =====\n")
data <- read.csv("C:/Users/DS 1/OneDrive/Desktop/data_cacer.csv")
# Convert types
data$Year <- as.integer(data$Year)
data$Rate <- as.numeric(data$Rate)

# Quick checks
str(data)
summary(data)
colSums(is.na(data))

# ==========================================================
# 🔵 TASK 1.1 — SUMMARY STATISTICS
# ==========================================================
cat("\n================ SUMMARY STATISTICS =================\n")
overall_stats <- data %>%
  group_by(Year) %>%
  summarise(
    n = n(),
    mean_rate   = mean(Rate, na.rm = TRUE),
    median_rate = median(Rate, na.rm = TRUE),
    sd_rate     = sd(Rate, na.rm = TRUE),
    min_rate    = min(Rate, na.rm = TRUE),
    max_rate    = max(Rate, na.rm = TRUE),
    range_rate  = max_rate - min_rate,
    iqr_rate    = IQR(Rate, na.rm = TRUE),
    .groups = "drop"
  )
print(overall_stats)
datatable(overall_stats, options = list(pageLength = 10))

# By Cancer Type & Sex
group_stats <- data %>%
  group_by(Year, Cancer.Type, Sex) %>%
  summarise(
    n = n(),
    mean_rate   = mean(Rate, na.rm = TRUE),
    median_rate = median(Rate, na.rm = TRUE),
    sd_rate     = sd(Rate, na.rm = TRUE),
    min_rate    = min(Rate, na.rm = TRUE),
    max_rate    = max(Rate, na.rm = TRUE),
    range_rate  = max_rate - min_rate,
    iqr_rate    = IQR(Rate, na.rm = TRUE),
    .groups = "drop"
  )
print(group_stats)
datatable(group_stats, options = list(pageLength = 10))

# ==========================================================
# 🔵 TASK 1.2 — INTERACTIVE EXPLORATORY PLOTS
# ==========================================================
p_overall <- ggplot(overall_stats, aes(x = Year, y = mean_rate)) +
  geom_line(color = "steelblue", size = 1.3) +
  geom_point(size = 3) +
  theme_minimal(base_size = 14) +
  labs(title = "Annual Mean Cancer Rate", x = "Year", y = "Mean Cancer Rate")
ggplotly(p_overall)

p_by_cancer <- ggplot(data, aes(x = Year, y = Rate, color = Cancer.Type)) +
  geom_line(size = 1.1) +
  theme_minimal(base_size = 14) +
  labs(title = "Cancer Rates by Cancer Type", x = "Year", y = "Rate")
ggplotly(p_by_cancer)

p_by_sex <- ggplot(data, aes(x = Year, y = Rate, color = Sex)) +
  geom_line(size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(title = "Cancer Rates by Sex", x = "Year", y = "Rate")
ggplotly(p_by_sex)

# ==========================================================
# 🔵 TIME SERIES CREATION
# ==========================================================
overall_ts <- data %>%
  group_by(Year) %>%
  summarise(overall_rate = mean(Rate, na.rm = TRUE)) %>%
  arrange(Year)

ts_data <- ts(overall_ts$overall_rate,
              start = min(overall_ts$Year),
              frequency = 1)

# ==========================================================
# 🔵 TASK 2 — STATIONARITY, RANDOMNESS & SEASONALITY
# ==========================================================
cat("\n================ TASK 2 =================\n")

# ------------------------------------------------
# 2.0 Visual Inspection
# ------------------------------------------------
p_ts <- autoplot(ts_data) +
  theme_minimal(base_size = 14) +
  labs(title = "Annual Cancer Rate (Visual Inspection)",
       x = "Year", y = "Cancer Rate")

ggplotly(p_ts)

# ------------------------------------------------
# 2.1 ADF Test (WITH p-value)
# ------------------------------------------------

# ADF with trend (MacKinnon p-value)
adf_p <- adf.test(ts_data, alternative = "stationary")

print(adf_p)

# ADF with trend (critical-value based, for reporting)
adf_trend <- ur.df(ts_data, type = "trend", selectlags = "AIC")
summary(adf_trend)
library(urca)
library(tseries)

# ------------------------------------------------
# 2.2 KPSS Test with Trend (WITH p-value)
# ------------------------------------------------

# KPSS with trend (p-value based)
kpss_p <- kpss.test(ts_data, null = "Trend")

print(kpss_p)

cat("\nInterpretation:\n")
cat("- ADF H0 = unit root (non-stationary)\n")
cat("- KPSS H0 = trend-stationary\n")

# ==========================================================
# FLEXIBLE DETRENDING: LOESS 
# ==========================================================
cat("\n=== LOESS DETRENDING (Flexible, Non-Parametric) ===\n")

# Fit LOESS — span controls smoothness (0.3–0.7 typical for annual data)
# Try different spans: smaller = more flexible, larger = smoother
loess_fit <- loess(overall_rate ~ Year, data = overall_ts, span = 0.5)

overall_ts <- overall_ts %>%
  mutate(
    loess_trend = predict(loess_fit),
    loess_resid = overall_rate - loess_trend
  )

resid_loess <- ts(overall_ts$loess_resid,
                  start = min(overall_ts$Year),
                  frequency = 1)

# Stationarity tests
cat("\n--- ADF and KPSS on LOESS Residuals ---\n")
print(adf.test(resid_loess))
print(kpss.test(resid_loess))

# Visual check (most important!)
ggplotly(ggplot(overall_ts, aes(x = Year)) +
           geom_line(aes(y = overall_rate, color = "Observed"), linewidth = 1.2) +
           geom_line(aes(y = loess_trend, color = "LOESS Trend"), linewidth = 1.8) +
           theme_minimal(base_size = 14) +
           labs(title = "Cancer Mortality: Observed vs LOESS Trend",
                y = "Rate per 100,000", color = "Series"))

ggplotly(autoplot(resid_loess) +
           geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
           theme_minimal() +
           labs(title = "LOESS Residuals (Should look random around zero)",
                y = "Residual (Rate per 100,000)"))

# ACF/PACF
ggplotly(ggAcf(resid_loess) + ggtitle("ACF - LOESS Residuals"))
ggplotly(ggPacf(resid_loess) + ggtitle("PACF - LOESS Residuals"))

# 2.3 Detrending for Randomness
time_index <- time(ts_data)
trend_model <- lm(ts_data ~ time_index)
trend_component <- fitted(trend_model)
detrended_ts <- ts_data - trend_component

ggplotly(
  autoplot(detrended_ts) +
    theme_minimal(base_size = 14) +
    labs(title = "Detrended Cancer Rate Series", x = "Year", y = "Detrended Rate")
)

# 2.4 Randomness Testing (Ljung-Box)
lb_test <- Box.test(detrended_ts, lag = 10, type = "Ljung-Box")
print(lb_test)

# 2.5 Autocorrelation / PACF of Detrended Series
ggplotly(ggAcf(detrended_ts) + ggtitle("ACF: Detrended Cancer Rates"))
ggplotly(ggPacf(detrended_ts) + ggtitle("PACF: Detrended Cancer Rates"))

# 2.6 Optional: Seasonality Check
# Health data annual → frequency = 1 → No seasonality expected
# But we can check for repeating patterns visually
ggplotly(
  autoplot(ts_data) +
    theme_minimal(base_size = 14) +
    labs(title = "Seasonality Check (Annual Data)", x = "Year", y = "Cancer Rate")
)

# # ==========================================================
# 🔵 TASK 3 — TREND, CYCLE & DECOMPOSITION (FIXED)
# ==========================================================
ts_vec <- as.numeric(ts_data)

# Trend (7-Year Moving Average)
trend_ma <- ma(ts_vec, order = 7, centre = TRUE)

# Align trend and original data
valid_idx <- which(!is.na(trend_ma))
year_aligned <- overall_ts$Year[valid_idx]
cancer_aligned <- ts_vec[valid_idx]
trend_aligned <- trend_ma[valid_idx]

# Cycle & remainder
cycle_aligned <- cancer_aligned - trend_aligned
remainder_aligned <- cancer_aligned - trend_aligned - cycle_aligned

# Tidy decomposition dataframe
decomp_df <- tibble(
  Year = year_aligned,
  Observed = cancer_aligned,
  Trend = trend_aligned,
  Cycle = cycle_aligned,
  Remainder = remainder_aligned
)

# Trend plot
p_trend <- ggplot(decomp_df, aes(x = Year)) +
  geom_line(aes(y = Observed, color = "Observed Cancer Rate"), size = 1) +
  geom_line(aes(y = Trend, color = "7-Year Moving Average (Trend)"), size = 1.3) +
  theme_minimal(base_size = 14) +
  labs(title = "Trend in Annual Cancer Rates",
       x = "Year", y = "Cancer Rate", color = "")
ggplotly(p_trend)

# Cycle plot
p_cycle <- ggplot(decomp_df, aes(x = Year, y = Cycle)) +
  geom_line(color = "darkgreen", size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(title = "Cyclical Deviations from Trend",
       x = "Year", y = "Cycle")
ggplotly(p_cycle)

# Full decomposition plot
decomp_long <- decomp_df %>%
  pivot_longer(cols = c(Observed, Trend, Cycle, Remainder),
               names_to = "Component", values_to = "Value")

p_decomp <- ggplot(decomp_long, aes(x = Year, y = Value, color = Component)) +
  geom_line(size = 1) +
  facet_wrap(~ Component, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Decomposition of Annual Cancer Rates",
       x = "Year", y = "Value")
ggplotly(p_decomp)
# ==========================================================
# 🔵 INTERACTIVE DASHBOARD — HISTORICAL CANCER DATA
# No Forecasting (Future Work)
# ==========================================================

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# -------------------------------
# Prepare dashboard data
# -------------------------------
df_overall <- overall_ts %>%
  rename(Year = Year, Rate = overall_rate)

df_full <- data

# -------------------------------
# UI
# -------------------------------
ui <- dashboardPage(
  
  dashboardHeader(title = "Cancer Rate Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Cancer Type", tabName = "type", icon = icon("layer-group")),
      menuItem("Sex Comparison", tabName = "sex", icon = icon("venus-mars")),
      menuItem("Trend & Decomposition", tabName = "trend", icon = icon("wave-square")),
      menuItem("Data Table", tabName = "table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # =========================
      # OVERVIEW
      # =========================
      tabItem(
        tabName = "overview",
        
        fluidRow(
          valueBox(mean(df_overall$Rate), "Average Rate", icon = icon("heartbeat"), color = "blue"),
          valueBox(min(df_overall$Rate), "Minimum Rate", icon = icon("arrow-down"), color = "green"),
          valueBox(max(df_overall$Rate), "Maximum Rate", icon = icon("arrow-up"), color = "red"),
          valueBox(nrow(df_overall), "Years Covered", icon = icon("calendar"), color = "yellow")
        ),
        
        fluidRow(
          box(
            title = "Overall Cancer Rate Trend (Historical)",
            width = 12,
            plotlyOutput("overall_plot", height = 400)
          )
        )
      ),
      
      # =========================
      # BY CANCER TYPE
      # =========================
      tabItem(
        tabName = "type",
        box(
          title = "Cancer Rates by Cancer Type",
          width = 12,
          plotlyOutput("type_plot", height = 450)
        )
      ),
      
      # =========================
      # BY SEX
      # =========================
      tabItem(
        tabName = "sex",
        box(
          title = "Cancer Rates by Sex",
          width = 12,
          plotlyOutput("sex_plot", height = 450)
        )
      ),
      
      # =========================
      # TREND & DECOMPOSITION
      # =========================
      tabItem(
        tabName = "trend",
        fluidRow(
          box(
            title = "Observed vs LOESS Trend",
            width = 12,
            plotlyOutput("loess_plot", height = 400)
          )
        ),
        fluidRow(
          box(
            title = "7-Year Moving Average Trend",
            width = 12,
            plotlyOutput("ma_plot", height = 400)
          )
        )
      ),
      
      # =========================
      # DATA TABLE
      # =========================
      tabItem(
        tabName = "table",
        box(
          title = "Historical Cancer Rate Data",
          width = 12,
          DTOutput("data_table")
        )
      )
    )
  )
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output) {
  
  output$overall_plot <- renderPlotly({
    ggplotly(
      ggplot(df_overall, aes(Year, Rate)) +
        geom_line(color = "steelblue", linewidth = 1.3) +
        geom_point(color = "darkred", size = 3) +
        theme_minimal(base_size = 14) +
        labs(y = "Cancer Rate per 100,000")
    )
  })
  
  output$type_plot <- renderPlotly({
    ggplotly(
      ggplot(df_full, aes(Year, Rate, color = Cancer.Type)) +
        geom_line(linewidth = 1.2) +
        theme_minimal(base_size = 14)
    )
  })
  
  output$sex_plot <- renderPlotly({
    ggplotly(
      ggplot(df_full, aes(Year, Rate, color = Sex)) +
        geom_line(linewidth = 1.3) +
        theme_minimal(base_size = 14)
    )
  })
  
  output$loess_plot <- renderPlotly({
    ggplotly(
      ggplot(overall_ts, aes(Year)) +
        geom_line(aes(y = overall_rate, color = "Observed"), linewidth = 1.2) +
        geom_line(aes(y = loess_trend, color = "LOESS Trend"), linewidth = 1.6) +
        theme_minimal(base_size = 14) +
        labs(color = "")
    )
  })
  
  output$ma_plot <- renderPlotly({
    ggplotly(p_trend)
  })
  
  output$data_table <- renderDT({
    datatable(df_full, options = list(pageLength = 8))
  })
}

# -------------------------------
# RUN DASHBOARD
# -------------------------------
shinyApp(ui, server)


cat("\n===== END OF SCRIPT — ALL TASKS COMPLETE =====\n")
