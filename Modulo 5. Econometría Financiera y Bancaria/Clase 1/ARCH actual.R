#install.packages(c("quantmod","rugarch","PerformanceAnalytics","tseries",
                #   "forecast","FinTS"))


# Cargar librerías
library(quantmod)
library(rugarch)
library(PerformanceAnalytics)
library(tseries)
library(forecast)
library(FinTS)
library(fGarch)

help("FinTS-package")
help("quantmod-package")

# Descargar precios de cierre ajustados de AAPL (últimos 5 años)
getSymbols("AAPL", src = "yahoo", from = Sys.Date() - 5*365, auto.assign = TRUE)
# Ctrl + Enter
# web scraping

# Extraer precios de cierre ajustados
aapl_prices <- Ad(AAPL)

# Calcular retornos logarítmicos
aapl_returns <- dailyReturn(aapl_prices, type = "log")
aapl_returns <- na.omit(aapl_returns)
aapl_returns <- aapl_returns[-1,]
colnames(aapl_returns) <- "Returns"

# Gráfico de precios y retornos
par(mfrow = c(2,1))
plot(aapl_prices, main = "Precio de cierre ajustado de Apple", col = "blue")
plot(aapl_returns, main = "Retornos logarítmicos diarios de Apple", col = "red")


# ACF y PACF de retornos y de su cuadrado
par(mfrow = c(2,2))
acf(aapl_returns, main = "ACF de retornos")
pacf(aapl_returns, main = "PACF de retornos")
acf(aapl_returns^2, main = "ACF de retornos al cuadrado")
pacf(aapl_returns^2, main = "PACF de retornos al cuadrado")

# Test de ARCH de Engle
arch_test <- ArchTest(aapl_returns, lags = 12)
print(arch_test)


# Lista para guardar resultados
arch_models <- list()
aic_values <- c()
bic_values <- c()

# Probar modelos ARCH(1) a ARCH(5)
for (q in 3:5) {
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q, 0)),
                     mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                     distribution.model = "norm")
  fit <- ugarchfit(spec = spec, data = aapl_returns)
  arch_models[[paste0("ARCH(", q, ")")]] <- fit
  aic_values[q] <- infocriteria(fit)[1]  # AIC
  bic_values[q] <- infocriteria(fit)[2]  # BIC
}

#correr este código para forecast
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(5, 0)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "norm")
fit <- ugarchfit(spec = spec, data = aapl_returns)



arch_models[[paste0("ARCH(", q, ")")]] <- fit
aic_values[q] <- infocriteria(fit)[1]  # AIC
bic_values[q] <- infocriteria(fit)[2]  # BIC


comparison <- data.frame(
  Modelo = paste0("ARCH(", 1:5, ")"),
  AIC = aic_values,
  BIC = bic_values
)
print(comparison)

# Visualización
barplot(t(as.matrix(comparison[, -1])), beside = TRUE, names.arg = comparison$Modelo,
        col = c("skyblue", "orange"), legend.text = c("AIC", "BIC"),
        main = "Comparación de modelos ARCH por AIC y BIC")




# Identificar modelo con menor AIC
best_index <- which.min(aic_values)
best_model <- arch_models[[best_index]]
show(best_model)

# Diagnóstico de residuos
par(mfrow = c(2,2))
plot(residuals(fit), main = "Residuos del mejor modelo")
acf(residuals(fit), main = "ACF de residuos")
acf(residuals(fit)^2, main = "ACF de residuos^2")
qqnorm(residuals(fit)); qqline(residuals(best_model), col = "red")


# Extraer volatilidad condicional (sigma) del mejor modelo
sigma_best <- sigma(fit)


par(mfrow = c(1,1))
# Graficar la volatilidad condicional

fit <- garchFit(~garch(5,0), data = aapl_returns, trace = FALSE)

volatilidad <- volatility(fit)


plot(volatilidad, type = "l" ,xlab = "Tiempo", ylab = "Volatilidad",
     main = "Volatilidad ARCH(5)", col = "darkgreen", lwd = 2)

# Forecast a 20 días
#correr el primer código de fit

n <- 20
forecast_arch <- ugarchforecast(fit, n.ahead = n)

# Obtener predicción de retornos esperados y volatilidad futura
predicted_returns <- fitted(forecast_arch)
predicted_sigma <- sigma(forecast_arch)

# Mostrar tabla de resultados
pred_df <- data.frame(
  Dia = 1:n.ahead,
  Retorno_Esperado = as.numeric(predicted_returns),
  Volatilidad_Esperada = as.numeric(predicted_sigma)
)
print(pred_df)


# Simular precios futuros suponiendo rendimientos compuestos
last_price <- as.numeric(last(aapl_prices))
future_prices <- last_price * cumprod(1 + pred_df$Retorno_Esperado)

# Graficar precios simulados
plot(1:n.ahead, future_prices, type = "l", lwd = 2, col = "darkblue",
     main = "Proyección de precios de Apple (usando ARCH)", xlab = "Días", ylab = "Precio simulado")
points(1, last_price, col = "black", pch = 16)
text(1, last_price, labels = round(last_price, 2), pos = 3)


#Simulación estocástica

sim <- ugarchsim(fit, n.sim = 20, m.sim = 1)
simulated_returns <- fitted(sim)
simulated_sigma <- sigma(sim)

# Generar precios a partir del último precio
simulated_prices <- last_price * cumprod(1 + as.numeric(simulated_returns))
plot(simulated_prices, type = "l", col = "blue", main = "Simulación estocástica de precios")


