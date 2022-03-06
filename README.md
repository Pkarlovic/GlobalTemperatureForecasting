# GlobalTemperatureForecasting
Forecasting Global Temperatures using R and Time Series Forecasting Methods

I. Intro

Climate change is an important issue that we face as a global community in the post-industrial era. Between melting sea ice and climate disasters we are facing the many repercussions of these increasing temperatures in our communities due to the large scale effects of increasing temperatures. In this project I will analyze temperature change across the 6 inhabited continents, and the scope of this temperature change. The data is harvested from the FAOSTAT Temperature change domain which measures the mean change in surface temperature from 1961 to 2019. We have seen the impact of global temperature change in regards to natural resource disasters but in this analysis we will observe the change in temperatures as well as predict the mean surface temperature change for different continents.

II.	Problems

From this data we will want to come to a few conclusions:
Which continents have had the highest increase in mean temperature?
Which continents are forcasted to have the highest increase or decrease in mean temperatures until 2050?
What will be the global mean temperature change in the 20 years?
Do certain continents experience different rates of temperature change?

III.	Methodology and Analysis

The FAOSTAT Temperature Change domain keeps track of monthly, seasonal and annual mean temperature changes from the mean as well as the standard deviation of the temperature change. The data is based of Global Surface Temperature Change Data which was gathered by NASA. Data covers temperature data from 1961 to 2019 in monthly, seasonal, and yearly periods. It is measured in celsius.
The data was obtained via Kaggle and can be accessed via https://www.kaggle.com/sevgisarac/temperature-change?select=FAOSTAT_data_11-24-2020.csv
In order to complete this project we will be using a number of specific packages:
ggplot2 stats forecast tidyr vioplot
First we need to import and clean our data and put it into a time series format:

```{r}
setwd('C:/Users/patri/OneDrive/Desktop')
rawdata <- read.csv("Environment_Temperature_change_E_All_Data_NOFLAG.csv")
library(ggplot2)
library(tidyverse)
library(forecast)
library(vioplot)
library(stats)
library(mgcv)
tempsddf<-(rawdata %>%
  filter(str_detect(Months.Code,'7020')))
tempdf<-(tempsddf%>%
           filter(str_detect(Element.Code, '7271')))
temp.ts<- subset(tempdf, select = -c(Area.Code, Months.Code, Months, Element.Code, Element, Unit))
temp <- pivot_longer(temp.ts, starts_with("Y"))
```

Now that our data is cleaned and cut down into temperatures based on countries we will want to plot it and take a look at our data as well as create a regression model to examine our data.

```{r warning=FALSE}
temp%>% 
  ggplot(aes(x = name, y = value, group = 1)) +
  geom_point() +
  stat_smooth( method = 'gam' ,color = 'red')+
  labs(x = 'Year',
       y = 'Change in Temperature (C)',
       title = "Global yearly Temperature Change for all Countries",
       subtitle = "1961-2019") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![image](https://user-images.githubusercontent.com/70538240/156904456-07314ae6-cade-4f02-ba2f-57975f0f224e.png)

As we can see here we see a general upward trend of a positive change in temperature as years go on. The graph above shows an included general additive model (GAM) which is a method of regression where each individual variable contributes equally to the model.

```{r}
#subsetting our data
tempEurope<-temp%>%
  filter(Area == "Europe")
tempAsia<-temp%>%
  filter(Area == "Asia")
tempAfrica<- temp%>%
  filter(Area == 'Africa')
tempNA <- temp%>%
  filter(Area == 'Northern America')
tempSA<- temp%>%
  filter(Area=='South America')
tempAustralia<-temp%>%
  filter(Area== 'Australia')
tempWorld<- temp%>%
  filter(Area == "World")
#Visualizing our new data 
ggplot(tempEurope, aes(x=name,y=value,group = 1, color="Europe")) +geom_line()+ 
  geom_line(data=tempAsia, aes(name,value,group=1,color="Asia"))+
  geom_line(data=tempAfrica, aes(name,value,group=1, color= "Africa"))+
  geom_line(data=tempNA, aes(name,value,group=1,color = "North America"))+
  geom_line(data=tempSA, aes(name,value,group=1,color= "South America"))+
  geom_line(data=tempAustralia, aes(name,value,group=1,color="Australia"))+
  geom_line(data=tempWorld, aes(name,value,group=1,color="World"))+
  labs(x = 'Year',
       y = 'Change in Temperature (C)',
       title = "Global yearly Temperature Change for Continents",
       subtitle = "1961-2019") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

![image](https://user-images.githubusercontent.com/70538240/156904483-748277b5-aaf0-4100-b5a5-3dfd079053dd.png)

For each continent we are seeing a general upwards trend for the mean change in temperature. From this Time Series plot we do see that North America and Europe have higher variations and extremes as compared to other continents, in order to analyze these variables more closely we will have to look at a different type of graph.

```{r}
vEurope <- tempEurope$value
vAsia<-tempAsia$value
vAfrica<-tempAfrica$value
vNA<-tempNA$value
vSA<-tempSA$value
vAustralia<-tempAustralia$value
vioplot(vEurope,vAsia,vAfrica,vNA,vSA,vAustralia, names= c('Europe', 'Asia', 'Africa', 'North America', 'South America', 'Australia'))
```
![image](https://user-images.githubusercontent.com/70538240/156904506-3cc6e1e5-7de3-40ba-9a8e-7647551d462f.png)

Above we have a Violin Plot which carries some of the same summary statistics as box plots but includes a kernel density estimation to show how our data is distributed. Looking at the violin plot for each Country we can Confirm that Europe and North America have higher temperature change values since 1961. They also carry a higher mean temperature change value as represented by the white dot in the center of the Violins.

Forecasting:

Now that we see that Europe and North America have more drastic changes in temperature as opposed to the other habitted continents, lets take a look at their forecasted values. First lets look at the forecasted values for the World, our continental values will follow the same methodology.

```{r}
#Putting our dataset in a form that works for the forecast package
Worldx <- ts(tempWorld[,3], start=c(1961,1), frequency = 1)
#Plotting our 
autoplot(Worldx)+ ggtitle('Time Series plot for Global World Temperature Change')
#Removing trend
Worlddx <- diff(Worldx)
autoplot(Worlddx)+ ggtitle('Time Series plot for Global World Temperature Change: Trend Stationary')
```
![image](https://user-images.githubusercontent.com/70538240/156904530-ec100779-04a6-4f46-99ce-a0c928bd9027.png)

Testing forecasting models:

Naive Forecasting is the most general method of forecasting which estimates using the last period’s actual values without adjusting or attempting to establish for causal factors. In this dataset we see in the Autocorrelation Function plot we see that our first value leaves out some data.

```{r}
fitworld <- snaive(Worlddx)
print(summary(fitworld))
checkresiduals(fitworld)
```

![image](https://user-images.githubusercontent.com/70538240/156904543-75884fba-ef1c-4aff-9ce5-47d40ea0786e.png)

Lets test another model:
The ETS forecast predicts a value based on existing values which follow a seasonal trend which applies overall smoothing for our data. In this method, our data fits pretty well, but we will test another method.

```{r}
worldfit_ets <-ets(Worldx)
checkresiduals(worldfit_ets)
```

![image](https://user-images.githubusercontent.com/70538240/156904551-8052a5b6-82ce-4f80-b275-d4287b1d6b3b.png)

ARIMA:

```{r}
worldarima <- auto.arima(Worldx, d=1, D=1,stepwise=FALSE, approximation = FALSE, trace = TRUE)
checkresiduals(worldarima)
```

![image](https://user-images.githubusercontent.com/70538240/156904569-0909d05b-1b7e-41bf-be01-e65c6019f1b5.png)


The Arima model seems to fit slightly better as the autoarima function selects the best possible ARIMA model for our data. The Auto Regressive Integrated Moving Average (ARIMA) explains itself by creating a time series forecast based on past values. This is model works best for non seasonal datasets which exhibit patterns. Now let's forecast. 

```{r}
fcstworld<- forecast(worldarima, h=31)
autoplot(fcstworld) + ggtitle('World Temperature Forecast: 1961-2050', subtitle = "Forecast with ARIMA(0,1,1) with drift")+
  ylab('Change in temperature (C)')
summary(fcstworld)
```
The Arima Model above forecasts a general increase in temperatures up until 2050 with the blue line representing the forecast point, the light-blue area representing the forecast point at a 80% confidence interval and the lighter-blue area as the forecast point at a 95% confidence interval.
Our data forecast that by 2050 the mean average surface temperature will be a 2.205621 Celsius increase from the global baseline climatology.

For forecasting North America and Europe we will follow the same methodology and select the best model in order to forecast.

North America:

Here is the Time Series Plot for North America

```{r}
NAx <- ts(tempNA[,3], start=c(1961,1), frequency = 1)
autoplot(NAx) + ylab('Temperature Change (C)') + ggtitle('North America Temperature Change')
```

![image](https://user-images.githubusercontent.com/70538240/156904577-dd688c51-ca3a-417f-9076-511cc85fc316.png)

Here is the forecast model:

```{r}
NAfit_ets <-ets(NAx)
print(summary(NAfit_ets))
checkresiduals(NAfit_ets)
fcstNA<- forecast(NAfit_ets, h=31)
autoplot(fcstNA) + ggtitle('North America Temperature Change Forecast: 1961-2050', subtitle = "Forecast with ETS with drift")+
  ylab('Change in temperature (C)')
summary(fcstNA)
```

![image](https://user-images.githubusercontent.com/70538240/156904586-fd97857a-003b-4e52-b13d-35d30cc67040.png)

In this model we use the Exponential Smoothing Model as it fits our data better. Our data forecasts that for North America the mean temperature change will be at 2.147375 degrees celsius increase from the baseline climatology.

Europe:
Here is out time series plot for Europe

```{r}
EUx <- ts(tempEurope[,3], start=c(1961,1), frequency = 1)
autoplot(EUx)+ ggtitle('Europe Temperature Change')+ ylab('Change in Temperature (C)')
```
![image](https://user-images.githubusercontent.com/70538240/156904612-33226b23-923e-4b8b-9ebd-afea505ca6d0.png)

![image](https://user-images.githubusercontent.com/70538240/156904613-6d6bfbc4-0d5b-40b7-aa42-fc17cb0f9a03.png)

Similar to North America, we use exponential smoothing to forecast our data. In this forecasting model we predict that by 2050, Europe will see it’s mean average surface temperature increase to 3.423310 degrees celsius above the mean baseline Climatology.
IV: Discussion
Viewing our data, we see that there is no question that temperature values are increasing across the globe as time goes on. By our violin plots we can see that Europe and North America carry the highest increase in temperature from baseline climatology as compared to the rest of the world, but these continents are not alone as globally temperatures are forecasted to increase. While some may see Climate Change as a small problem in the scope of global issues, the effects of it could lead to disastrous consequences for large swaths of the population. Sea level rise, coral bleaching, ice-free arctic, heat waves, flooding, and the degredation of wildlife habitat are all consequences to an ever increasing global climate temperature. Currently, the Paris Agreement is a consensus by 196 world leaders to keep temperatures from exceeding 1.5 degrees Celsius from the current average. Europe is already forecasted to feel the most effects of temperature change with a 3.4 degree deviation from its standard climatology which could lead to disastrous effects.
V: Limitations
This project only relied on one time dependent variable: Temperature Change based of baseline climatology. Improvement on this project could be the addition of multiple variables for Multivariate time series forecasting and analysis. Random Forest as a regression tool may also improve on the statistical analysis of the data.




