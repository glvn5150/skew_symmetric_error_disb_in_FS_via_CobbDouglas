library(readxl)
library(tidyverse)   
library(lmtest)     
library(sandwich)    
library(sn)          
library(car)        
library(plotly)
library(psych)       
library(knitr)

# --------------------------imports---------------------------------
# 0. load data
file_path <- "C:/Users/Os/OneDrive - Bina Nusantara/FlashDIsk/File Kuliah/Thesis and Such/V1_indonesian_company_quarterly_data_2023_2025.xlsx"
data_raw <- read_excel(file_path)

# make a working copy
data <- as_tibble(data_raw)

# convert date if present
if ("Date" %in% names(data)) {
  data <- data %>% mutate(Date = as.Date(Date))
}

# -------------------------data Cleaning----------------------------------
# 1. clean data
num_cols <- c("Total Revenue", "Total Assets", "Number of Employees")
for (col in num_cols) {
  if (!col %in% names(data)) stop(paste("Column missing:", col))
  data[[col]] <- as.numeric(data[[col]])   # NA introduced will be filtered below
}

# filter
data <- data %>%
  filter(
    !is.na(`Total Revenue`) & !is.na(`Total Assets`) & !is.na(`Number of Employees`),
    `Total Revenue` > 0,
    `Total Assets` > 0,
    `Number of Employees` > 0
  )

# log variables
eps <- .Machine$double.eps
data <- data %>%
  mutate(
    log_Revenue   = log(pmax(`Total Revenue`, eps)),
    log_Assets    = log(pmax(`Total Assets`, eps)),
    log_Employees = log(pmax(`Number of Employees`, eps))
  )

# summary
describe(select(data, log_Revenue, log_Assets, log_Employees))

# log_data if preferred
L_data <- data %>% select(log_Revenue, log_Assets, log_Employees)

# --------------------classic Cobb-Douglas------------------------
# 2. linear with firm dummies
model_simple <- lm(log_Revenue ~ log_Assets + log_Employees, data = data)
summary(model_simple)

# 3D surface using plotly (predict over a grid)
la_seq <- seq(min(data$log_Assets), max(data$log_Assets), length.out = 40)
ln_seq <- seq(min(data$log_Employees), max(data$log_Employees), length.out = 40)
grid <- expand.grid(log_Assets = la_seq, log_Employees = ln_seq)
grid$log_Revenue <- predict(model_simple, newdata = grid)

z_matrix <- matrix(grid$log_Revenue, nrow = length(la_seq), ncol = length(ln_seq))

fig <- plot_ly()
fig <- fig %>% add_trace(
  x = data$log_Assets, y = data$log_Employees, z = data$log_Revenue,
  type = "scatter3d", mode = "markers",
  marker = list(size = 3, color = data$log_Revenue, colorscale = "Viridis")
)
fig <- fig %>% add_surface(x = la_seq, y = ln_seq, z = z_matrix, opacity = 0.5)
fig <- fig %>% layout(scene = list(
  xaxis = list(title = "Log(Total Assets)"),
  yaxis = list(title = "Log(Number of Employees)"),
  zaxis = list(title = "Log(Total Revenue)")
))
fig

# --------------------fixed effects----------------------------
# 3. fix effect cobb douglas
if (!"Ticker" %in% names(data)) {
  warning("Ticker column not found — skipping firm dummies model.")
  model_fe <- model_simple
} else {
  model_fe <- lm(log_Revenue ~ log_Assets + log_Employees + as.factor(Ticker), data = data)
  summary(model_fe)
}

# obs vs pred
data <- data %>% mutate(predicted_log_Revenue_fe = predict(model_fe))
ggplot(data, aes(x = log_Revenue, y = predicted_log_Revenue_fe)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Observed vs Predicted (Log Scale)",
       x = "Observed Log Revenue", y = "Predicted Log Revenue") +
  theme_minimal()

# plot (no logs)
ggplot(data, aes(x = `Total Assets`, y = `Total Revenue`)) +
  geom_point(alpha = 0.6) +
  labs(title = "Assets vs Revenue (raw scale)", x = "Assets", y = "Revenue") +
  theme_minimal()

# ----------------------hetero tests------------------------
# 4. heteroskedasticity tests and simple residual diagnostics
bptest(model_fe)
bptest(model_fe, ~ fitted(model_fe) + I(fitted(model_fe)^2), data = data)

# residuals vs fitted
ggplot(data, aes(x = fitted(model_fe), y = residuals(model_fe))) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()

# ---------------------skew normal regression--------------------------
# 5. skew-normal regression (recommended: use sn::selm)
# yhis replaces the fragile custom optim approach
fit_selm <- tryCatch(
  selm(log_Revenue ~ log_Employees + log_Assets, data = data, family = "SN"),
  error = function(e) {
    message("selm() failed: ", e$message)
    NULL
  }
)

if (!is.null(fit_selm)) {
  cat("\nSummary of skew-normal regression (selm):\n")
  print(summary(fit_selm))
  # fitted values and residuals
  data <- data %>% mutate(
    fitted_selm = as.numeric(fitted(fit_selm)),
    resid_selm = as.numeric(residuals(fit_selm))
  )
  
  # residual plot for selm
  ggplot(data, aes(x = fitted_selm, y = resid_selm)) +
    geom_point(alpha = 0.6) +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = "selm: Residuals vs Fitted", x = "Fitted (selm)", y = "Residuals") +
    theme_minimal()
} else {
  message("skew-normal regression not available; consider installing/updating package 'sn' or inspect data.")
}

# --------------------outliers--------------------------
# 6. outliers / influence (Cook's distance) for the FE model
cooksd <- cooks.distance(model_fe)
plot(cooksd, pch = 20, main = "Cook's Distance", col = "blue")
abline(h = 4 / length(cooksd), col = "red")
influential <- which(cooksd > (4 / length(cooksd)))
cat("Influential indices (Cook's):", toString(influential), "\n")

# optionally remove influential points and re-fit
if (length(influential) > 0) {
  data_clean <- data[-influential, ]
  model_clean <- lm(log_Revenue ~ log_Assets + log_Employees + (if ("Ticker" %in% names(data_clean)) as.factor(Ticker) else 1), data = data_clean)
  summary(model_clean)
} else {
  data_clean <- data
  model_clean <- model_fe
}

# -------------------diagnostics---------------------------
# 7. Residual diagnostics & normality tests
# classical model residuals:
ggplot(data, aes(x = residuals(model_fe))) +
  geom_histogram(color = "black", fill = "skyblue", bins = 30) +
  labs(title = "Histogram of Residuals (FE model)", x = "Residuals", y = "Frequency") +
  theme_minimal()

qqnorm(residuals(model_fe)); qqline(residuals(model_fe), col = "red")

# shapiro-wilk tests (on cleaned data where appropriate)
cat("\nShapiro-Wilk tests (using data_clean):\n")
if (nrow(data_clean) >= 3 && nrow(data_clean) <= 5000) {
  print(shapiro.test(data_clean$log_Revenue))
} else {
  message("Shapiro-Wilk not run: sample size outside [3,5000].")
}

# qq comparison of residuals (classical vs cleaned/refitted)
resid_classic <- residuals(model_fe)
resid_clean   <- residuals(model_clean)

# helper to create qq data
generate_qq_data <- function(x, label) {
  x <- na.omit(x)
  n <- length(x)
  tibble(
    Theoretical = qnorm(ppoints(n)),
    Sample = sort(x),
    Model = label
  )
}

qq_classic <- generate_qq_data(resid_classic, "Classical")
qq_clean   <- generate_qq_data(resid_clean, "Cleaned")
qq_combined <- bind_rows(qq_classic, qq_clean)

ggplot(qq_combined, aes(x = Theoretical, y = Sample, color = Model)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Q-Q Plot Comparison", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# -------------------model comparision---------------------------
# 8. model comparison, vif, multicollinearity, autocorrel
aic_fe      <- AIC(model_fe)
aic_simple  <- AIC(model_simple)
bic_fe      <- BIC(model_fe)
bic_simple  <- BIC(model_simple)

cat("AIC (FE):", aic_fe, " AIC (simple):", aic_simple, "\n")
cat("BIC (FE):", bic_fe, " BIC (simple):", bic_simple, "\n")

vif_model_simple <- tryCatch(vif(model_simple), error = function(e) NULL)
if (!is.null(vif_model_simple)) {
  print(vif_model_simple)
} else {
  message("VIF not available for this model (maybe perfect collinearity / factors).")
}

# BP and DW
print(bptest(model_fe))
print(dwtest(model_fe))

