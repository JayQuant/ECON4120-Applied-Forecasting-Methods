##Importing Data
#load necessary libraries
library(haven)
library(tidyverse)
library(fpp3)
library(zoo)
library(tidyquant)

par(mfrow=c(2,1))

US_QGDP <- read_dta("C:/Users/idkup/Desktop/Course studies/2022 TERM 2/ECON4120/US_QGDP.dta")
view(US_QGDP)

## Manipulation and Cleaning Data

# Check for NA values
sum(is.na(US_QGDP)) 

# Change GDP column so all three decimal places are shown
US_QGDP$gdp <- as.numeric(US_QGDP$gdp)
US_QGDP$gdp <- format(round(US_QGDP$gdp,3), nsmall=3)

# Create quarterly data sequence
ti <- seq(as.Date("1947-01-01"), as.Date("2019-12-01"), by= "quarter")

# Check time index length is same as that of data
length(ti) == length(US_QGDP$gdp)    

# Change to quarterly data
ti <- as.yearqtr(ti, format="%Y-%m-%d") 
ti <- yearquarter(ti)
US_QGDP$yq <- ti

# Change character to numeric vector
US_QGDP$gdp <- as.numeric(US_QGDP$gdp)

# Swap column order, change to time series object
US_QGDP <- select(US_QGDP, yq, gdp) %>%
	as_tsibble(index = yq)		

autoplot(US_QGDP) #plot time series of US GDP 

### Q1

##i) Detecting stationarity of series visually
autoplot(US_QGDP) #plot time series of US GDP 

##ii) Differencing the series by order 1
plt.d.US_QGDP <- US_QGDP %>% transmute(
	'd.gdp' = difference(gdp)
	) %>%
	autoplot()

##iii) Differencing the log series by order 1

plt.d.logUS_QGDP <- US_QGDP %>% transmute(
	'd.loggdp' = difference(log(gdp))
	) %>%
	
	ggplot(aes(x=yq, y= d.loggdp)) +
	geom_line(col='steelblue') +
	labs( y = "first difference of log gdp", title = "Growth rate of quarterly GDP")

# Show differenced series on top and differenced log series on bottom
plt.d.US_QGDP / plt.d.logUS_QGDP

##iv) Estimate AR(1) model for GDP growth rate

# Save differenced log series
d.logUS_QGDP <- US_QGDP %>% transmute(
	'd.loggdp' = difference(log(gdp))
	)
	


# Fit AR(1) model
fit <- d.logUS_QGDP %>% drop_na() %>%
	model(AR(d.loggdp ~ order(1), na.rm=T)
	)

report(fit)

# Forecast 1 quarter ahead
fit %>% forecast(h = 1)

# Plot forecasted path and distribution along with original data
forecast.1 <- fit %>% forecast() %>% autoplot(d.logUS_QGDP) + 
	coord_x_date(xlim = c("2000-1-1" , "2021-1-1"))

##v) Estimate AR(2) model for GDP growth rate

#Fit AR(2) model
fit2 <- d.logUS_QGDP %>% drop_na() %>%
	model(AR(d.loggdp ~ order(2), na.rm=T)
	)
	
report(fit2)

# Forecast 1 quarter ahead
fit2 %>% forecast(h = 1)

# Plot forecasted path along original data
forecast.2 <- fit2 %>% forecast() %>% autoplot(d.logUS_QGDP) + 
	coord_x_date(xlim = c("2000-1-1" , "2021-1-1"))
	
