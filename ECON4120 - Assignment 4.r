## Import libraries
library(haven)
library(fpp3)
library(fUnitRoots)
library(tidyquant)
library(quantmod)
library(patchwork)
library(zoo)
library(forecast)
library(rugarch)

## Q1

##i) 

# Importing data and cleaning data
inf = read_dta("infl.dta")


 ti <- seq(as.Date("1970-01-01"), as.Date("2012-03-01"), by= "month")
 ti <- yearmonth(ti)
 inf$date <- ti
 
 inf <- inf %>% as_tsibble(index=days) 
 
 # plot series of returns
 
 inf %>%  gg_tsdisplay(infl, plot_type='partial')
 
 # plot series of return^2
 inf_squared <- inf %>% transmute(infl_sq = infl^2)
 inf_squared %>%  gg_tsdisplay(infl_sq, plot_type='partial')
 
 # Conduct ADF test
 adfTest(inf$infl)
 
 # Conduct box-cox transformation
 lambda <- BoxCox.lambda(inf$infl)

inf$bc_infl <- ((inf$infl^lambda - 1) / lambda)
 inf %>%  gg_tsdisplay(bc_infl, plot_type='partial')
 
 # Apply log-transformation
 inf$log_infl <- log(inf$infl)
 
 # Apply difference
inf <- inf %>% mutate(
	'd.infl' = difference(infl)
	)
inf %>% gg_tsdisplay(d.infl, plot_type='partial')

##ii) 

#fit ARMA model for differenced series
fit_ARMA <- inf %>% model(
	stepwise = ARIMA(d.infl),
	search = ARIMA(d.infl, stepwise = FALSE)
	)

#fit ARIMA model with box-cox transformed series
fit_ARIMA <- inf %>% model(
	stepwise = ARIMA(bc_infl),
	search = ARIMA(bc_infl, stepwise = FALSE)
	)

fit_ARMA %>% select(search) %>% report()

#Residual Analysis of ARMA(1,0) model
fit_ARMA %>% select(search) %>% gg_tsresiduals()
augment(fit_ARMA) %>% filter(.model == 'search') %>% 
	features(.innov, ljung_box)


## iv) estimate ARCH(2) and GARCH(1) model

#Extract Residuals
residuals <- fit_ARMA %>% select(search) %>% augment() %>% select(.innov)
residuals <- drop_na(residuals)

residuals2 <- residuals
residuals2$.innov <- residuals2$.innov^2
residuals2 %>% gg_tsdisplay(.innov, plot_type = 'partial')
#Fit ARCH(2) model

arch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), mean.model = list(armaOrder = c(0,0)), distribution.model = "norm")
arch_fit <- ugarchfit(arch_model, residuals$.innov)
arch_coef <- coef(arch_fit)

#Fit GARCH(1,1) model

garch_model <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = c(0,0)), distribution.model = "norm")
garch_fit <- ugarchfit(garch_model, residuals$.innov)
garch_coef <- coef(garch_fit)

#Predict conditional volatility for ARCH(2)
 arch_pred <- ugarchforecast(arch_fit, n.ahead = 12)
 garch_pred <- ugarchforecast(garch_fit, n.ahead=12)
 
 
 
 ## Plot predicted conditional volatility
 
 #Construct future time index
 ft <- seq(as.Date("2012-04-01"), as.Date("2013-03-01"), by= "month")
 ft <- yearmonth(ft)
 
 #Create data for plotting predicted volatility for 12 months

 plot_data <- tibble(
	time = index(residuals2),
	vol_original = residuals2$.innov,
	arch_vol = arch_fit@fit$var,
	garch_vol = garch_fit@fit$var
	)
# Plot data
 
ggplot(plot_data, aes(x = time)) +
  geom_line(aes(y = vol_original, color = "Original")) +
  geom_line(aes(y = arch_vol, color = "ARCH(2)")) +
  geom_line(aes(y = garch_vol, color = "GARCH(1,1)")) +
  scale_color_manual(values = c("Original" = "black", "ARCH(2)" = "red", "GARCH(1,1)" = "blue")) +
  labs(x = "Time", y = "Volatility", color = "Model") +
  ggtitle("Estimated Conditional Volatility")

	## new change