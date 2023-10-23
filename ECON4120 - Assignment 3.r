## Import necessary libraries
library(haven)
library(fpp3)
library(fUnitRoots)
library(tidyquant)

###Q1

##i)
vol = read_dta("vol.dta")

sum(is.na(vol))
vol <- drop_na(vol)

vol <- vol %>% as_tsibble(index=days) 

vol %>%  gg_tsdisplay(vol, plot_type='partial')
  
# judging from visual inspection, it seems volatility time series is staionary

##ii) perform unit-root tests

fUnitRoots::unitrootTest(vol$vol)
fUnitRoots::unitrootTestadfTest(vol$vol)

#p-value is 0.01291

## iii) plot acf and pacf


## iv) propose appropriate ARMA models

# pacf seems to cut off after lag 2
# ARMA(2,0)
# one could also argue acf either cuts off from lag 3 and pacf displays exponential decay.
# ARMA(0,2), ARMA(0,1) (if we consider acf at lag 2 to obe insignificant)
# ARMA(2,2)
# ARMA(1,1)

## v) estimating ARMA models
fit <- vol %>%
	model(arima200 = ARIMA(vol ~ pdq(2,0,0)),
		arima002 = ARIMA(vol ~ pdq(0,0,2)),
		arima202 = ARIMA(vol ~ pdq(2,0,2)),
		arima101 = ARIMA(vol ~ pdq(1,0,1))
	)
	
#Generate residuals of all four models
augment(fit) %>% select(-vol, -.fitted, -.resid) %>%
	pivot_wider(names_from = .model, values_from = .innov)

# residual analysis

fit %>% select(arima002) %>% gg_tsresiduals()
augment(fit) %>% filter(.model == 'arima002') %>% 
	features(.innov, ljung_box)

fit %>% select(arima200) %>% gg_tsresiduals()
augment(fit) %>% filter(.model == 'arima200') %>% 
	features(.innov, ljung_box)

fit %>% select(arima202) %>% gg_tsresiduals()
augment(fit) %>% filter(.model == 'arima202') %>% 
	features(.innov, ljung_box)

fit %>% select(arima101) %>% gg_tsresiduals()
augment(fit) %>% filter(.model == 'arima101') %>% 
	features(.innov, ljung_box)
	
# According to residual analysis, ARIMA(1,0,1), ARIMA(2,0,2), ARIMA(0,0,2) seem most likely in descending order.

## vi) Generate ICs of models and choose based on ICs

glance(fit)

#ARIMA(1,0,1) model has the smallest AIC, AICC, and BIC.

## vii) Make one step forecasts and plot them on the graph showing the volatility series

#Show one step forecasts ARIMA(1,0,1) model
fit %>% forecast(h=1) %>% 
	filter(.model == 'arima101')

# plot onestep forecast on vol time series
fit %>% forecast(h = 1) %>% 
	filter(.model == 'arima101') %>% 
	autoplot(vol)

# Zoom in on plot by letting start point equal to 200th day and ends at 254th day
fit %>% forecast(h=1) %>% 
	filter(.model == 'arima101') %>%
	autoplot(vol) +
	coord_x_date(xlim = c( 200 , 254))
	
	

###Q2


## ii-c) Inspect forex data and determmine model of friction

forex <- read_dta("forex.dta")

forex <- as_tsibble(forex, index = tradetimes)

# Display log exchange rate time series
forex %>% gg_tsdisplay(forex, plot_type = "partial")
# Take difference of logarithmic exchange rates to find log change (return)
forex %>% 
	mutate(diff_forex = difference(log(forex))) %>%
	gg_tsdisplay(diff_forex, plot_type = "partial")
	
fit2 <- forex %>% 
	mutate(diff_forex = difference(log(forex))) %>%
	model(
		arima100 = ARIMA(diff_forex ~ pdq(1,0,0)),
		arima303 = ARIMA(diff_forex ~ pdq(3,0,3)),
		arima300 = ARIMA(diff_forex ~ pdq(3,0,0)),
		stepwise = ARIMA(diff_forex),
		search = ARIMA(diff_forex, stepwise = FALSE)

	)
	
