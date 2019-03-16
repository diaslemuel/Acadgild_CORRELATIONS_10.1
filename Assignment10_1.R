# Import dataset from the following link:
 # https://archive.ics.uci.edu/ml/machine-learning-databases/00360/
 # Perform the below written operations:
#a. Read the file in Zip format and get it into R
setwd("D:/AcadGild")
getwd()
files.temp = "AirQualityUCI.zip"
unzip("AirQualityUCI.zip")
for (i in files.temp)
  unzip(i)

#b. Create Univariate for all the columns.
data1 = read_xlsx("AirQualityUCI.xlsx")
AirQualityUCI <- data1
library(psych)
describe(AirQualityUCI)


#c. Check for missing values in all columns.

col1<- mapply(anyNA,AirQualityUCI)
col1
summary(AirQualityUCI)
is.na(AirQualityUCI)

#d. Impute the missing values using appropriate methods
colSums(is.na(AirQualityUCI))
library(plyr)
AirQualityUCI[AirQualityUCI==-200.0]<-NA
for(i in 1:ncol(AirQualityUCI)){ AirQualityUCI[is.na(AirQualityUCI[,i]),i] <- mean(AirQualityUCI[,i], na.rm = TRUE)} 
summary(AirQualityUCI)

#e. Create bi-variate analysis for all relationships
summary(AirQualityUCI)
plot(AirQualityUCI$`NOx(GT)`~AirQualityUCI$`PT08.S2(NMHC)`)
plot(AirQualityUCI$`PT08.S1(CO)`~AirQualityUCI$`PT08.S3(NOx)`)
plot(AirQualityUCI$`NO2(GT)`~AirQualityUCI$`PT08.S4(NO2)`)
plot(AirQualityUCI$`PT08.S5(O3)`~AirQualityUCI$T)

#f. Test relevant hypothesis for valid relations

t.test(AirQualityUCI$`CO(GT)`, AirQualityUCI$`PT08.S1(CO)`, paired = T)
t.test(AirQualityUCI$`C6H6(GT)`, AirQualityUCI$`PT08.S2(NMHC)`, paired = T)
t.test(AirQualityUCI$`NOx(GT)`, AirQualityUCI$`PT08.S3(NOx)`, paired = T)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$Date1)
summary(mod)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$T)
summary(mod)

mod <- lm(AirQualityUCI$`CO(GT)`~AirQualityUCI$RH)
summary(mod)

#g. Create cross tabulations with derived variables

mydata<-AirQualityUCI
View(mydata)
attach(mydata)
mytable <- table(A,B) # A will be rows, B will be columns 
mytable # print table 
margin.table(mytable, 1) # A frequencies (summed over B) 
margin.table(mytable, 2) # B frequencies (summed over A)
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages 
prop.table(mytable, 2) # column percentages

#h. check for trends and patterns in time series

#plot time series
tsAirqualityUCI <- EuStockMarkets[, 1] # ts data
decomposedRes <- decompose(tsAirqualityUCI, type="mult") # use type = "additive" for additive components
plot (decomposedRes) # see plot below
stlRes <- stl(tsAirqualityUCI, s.window = "periodic")
plot(AirQualityUCI$T, type = "l")
#or
library(xts)

timeseries <- xts(final$`CO(GT)`, final$datetime)
plot(timeseries)
summary(timeseries)

ts (AirQualityUCI, frequency = 4, start = c(1959, 2))# frequency 4 =>Quarterly Data
ts (1:10, frequency = 12, start = 1990) # freq 12 => Monthly data.
ts (AirQualityUCI, start=c(2009), end=c(2014), frequency=1) # Yearly Data
ts (1:1000, frequency = 365, start = 1990) # freq 365 => daily data

#i. Find out the most polluted time of the day and the name of the chemical compound.

names(AirQualityUCI)
library(dplyr)

polluted <- AirQualityUCI%>%group_by(Time)%>%
  select(Time, `CO(GT)`, `C6H6(GT)`, `NO2(GT)`, `NOx(GT)` )%>%
  summarise(CO = mean(`CO(GT)`), C6H6 = mean(`C6H6(GT)`), NO2 = mean(`NO2(GT)`), NOX =mean(`NOx(GT)`))%>%
  
  polluted[c(which.max(polluted$CO),which.max(polluted$C6H6),which.max(polluted$NO2),which.max(polluted$NOX)),]

# 19:00:00 is the most polluted time of the day with CO, C6H6, NO2 & NOx
