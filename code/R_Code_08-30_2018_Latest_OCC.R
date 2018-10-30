library(readr)
library(caret)
library(mgcv)
library(nlme)
library(Epi)
library(date)
library(lubridate)
library(forecast)
library(data.table)
library(Metrics)
library(xts)
library(dplyr)
library(ggthemes)
library(scales)
library(itsadug)
library(visreg)
############################
##### Data Preparation #####
############################
set.seed(123)

# --- read in data --- #
energy = read_csv("D:/occupancy-profiling/code/Modified_Data.csv")
energy$hourly = substr(energy$time,1,nchar(energy$time)-3)
energy <- energy[,c(11,2:10,1)]
head(energy)

# Aggregate kWh and temperature from 30-minutes to hourly
head(sums_kwh <- aggregate(kwh ~ hourly, data = energy, FUN = sum))
colnames(sums_kwh)<- c("hourly","sumKwh")
head(mean_temp <- aggregate(temperature ~ hourly, data = energy, FUN = mean))
colnames(mean_temp)<- c("hourly","meanTemp")

# Merge the dataframes
energy <- merge(sums_kwh, energy,  by="hourly", no.dups = TRUE)
energy <- merge(mean_temp, energy,  by="hourly", no.dups = TRUE)
head(energy,10)

# Modify time to hourly intervals
energy$time <- paste0(energy$time, ":00")
energy$time <- as.POSIXct(energy$time,format= "%m/%d/%Y %H:%M:%S",tz="CST6CDT")
energy <- energy[,c(13,2:12,1)]
energy$hourly <- paste0(energy$hourly, ":00:00")
energy$time <- as.POSIXct(energy$hourly,format= "%m/%d/%Y %H:%M:%S",tz="CST6CDT")
energy <- energy[ order(energy$time , decreasing =FALSE ),]
head(energy,10)


#Remove duplicate rows of 30 minute intervals
energy <- energy[!(energy$hours %in% seq(30,1410,60)),] 


# Drop unused columns
colnames(energy) <- c("time","temperature","kwh","kwh_30mins","temp_30mins","date","hours","day","Day","occ",
                      "year","month","hourly")
energy <- energy[,c("time","temperature","kwh","date","hours","day","Day","occ","year","month")]
str(energy)


# add predictor "Econ"
energy$Econ = ifelse(energy$temperature>40 & energy$temperature<65 & energy$occ =="yes", "yes", "no")

# # transfer date and hour format to numeric format
# for (i in (1:dim(energy)[1]) ){
#   x = strsplit(energy$date[i], split = "/")[[1]]
# 
#   if (x[1]=="0"|x[1]=="1"|x[1]=="2"|x[1]=="3"|x[1]=="4"|x[1]=="5"|x[1]=="6"|x[1]=="7"|x[1]=="8"|x[1]=="9"){
#     x[1] = paste("0", x[1], sep = "")
#   }
#   if (x[2]=="0"|x[2]=="1"|x[2]=="2"|x[2]=="3"|x[2]=="4"|x[2]=="5"|x[2]=="6"|x[2]=="7"|x[2]=="8"|x[2]=="9"){
#     x[2] = paste("0", x[2], sep = "")
#   }
# 
#   x = paste(x[1], x[2], x[3], sep = "")
#   x = mdy(x)
#   x = decimal_date(x)
#   energy$time[i] = x - floor(x)
# 
# }

# modify formats for other variables
#energy$time = as.numeric(energy$time)
energy$hours = energy$hours/60
energy$day = as.factor(energy$day)
energy$Day = as.factor(energy$Day)





#Set the Occupancy
Holidays <- c("1/1/2015","1/19/2015", "5/25/2015", "7/3/2015","11/26/2015",
              "12/25/2015", "1/1/2016", "1/18/2016")
energy$occ <- ifelse(energy$day == "Sunday" | energy$hours <= 6 |energy$hours >18 |
                       energy$day == "Saturday" & energy$hours > 13 |energy$date %in% Holidays,
                     "0", "1" )

energy$occ = as.factor(energy$occ)
energy$Econ = as.factor(energy$Econ)
#energy$month = as.factor(energy$month)
str(energy)
# --- create additional predictors to check dependence --- #

# overall length
n = length(energy$kwh)

# create "prev_30mins" predictor to see dependence on the 30 mins before
energy$prev_hour = c(NA,energy$kwh[1:(n-1)])

# Check for lag in between the temperature and kwh
ccfvals = ccf(energy$kwh,energy$temperature)
ccfvals
# create "temp_delay" predictor to see dependence of kwh on the temperature before 3 hours
energy$temp_2delay = c(rep(NA, 2), energy$temperature[1:(n-2)])
energy$temp_3delay = c(rep(NA, 3), energy$temperature[1:(n-3)])
energy$temp_4delay = c(rep(NA, 4), energy$temperature[1:(n-4)])
energy$temp_6delay = c(rep(NA, 6), energy$temperature[1:(n-6)])
energy$temp_8delay = c(rep(NA, 8), energy$temperature[1:(n-8)])
# create "temp_delay" predictor to see dependence of kwh on the temperature before 3 hours
energy$temp_delay = c(rep(NA, 9), energy$temperature[1:(n-9)])


# create "prev_day" predictor to see dependence on the day before
energy$prev_day = c(rep(NA, 23), energy$kwh[1:(n-23)])
#energy$temp_prev_day = c(rep(NA, 24), energy$temperature[1:(n-24)])

# create "prev_week" predictor to see dependece on the week before
energy$prev_week = c(rep(NA, 24*7), energy$kwh[1:(n-24*7)])

# create "prev_2week" predictor to see dependece on the 2 weeks before
energy$prev_2week = c(rep(NA, 24*14), energy$kwh[1:(n-24*14)])

# create "prev_3week" predictor to see dependece on the 3 weeks before
energy$prev_3week = c(rep(NA, 24*21), energy$kwh[1:(n-24*21)])

# create "prev_month" predictor to see dependece on the month before (28 days)
energy$prev_month = c(rep(NA, 24*28), energy$kwh[1:(n-24*28)])

# create "solar" predictor to see dependece on the solar radiation
energy$solar = ifelse(energy$hours >= 7 & energy$hours <=18,1, 0)
energy$solar <- as.factor(energy$solar)

head(energy,48)
# 
# # create "prev_season" predictor to see dependence on the season before (90 days)
# energy$prev_season = c(rep(NA, 24*90), energy$kwh[1:(n-24*90)])
# 
# # create "prev_year" predictor to see dependence on the year before (365 days)
# energy$prev_year = c(rep(NA, 24*365), energy$kwh[1:(n-24*365)])

energy = na.omit(energy)
str(energy)

# Create a new temperature column with mean temperature per day
energy$date<-as.factor(energy$date)
setDT(energy)[, temp.new := mean(temperature), by = date]

# Create weekly hours
energy$weekday_number <- (as.POSIXlt(energy$time)$wday)
energy$hours2 <- (energy$weekday_number*24+(1+energy$hours))

#Set the Pre-heating or Pre-cooling
energy$optimalstart <- as.factor(ifelse(energy$Day == "weekday" & energy$hours >= 4 & 
                                          energy$hours <=8,
                                        "1", "0" ))



# Scatter Plot
xlab1 <- expression(paste('Temperature (',~degree,'F)',sep=''))
ggplot(data = energy, aes(x = temperature, y=kwh,color=occ)) + geom_point()+
  theme_bw() +labs(title = "Effect of temperature on Energy Consumption", subtitle = "Occupied and Unoccupied periods", 
                   y="Electricity consumption (kWh)")+labs(x=xlab1) +  theme_calc() +
  theme(plot.title = element_text(hjust = 0.5,size=21,face="bold"), plot.subtitle = element_text(hjust = 0.5,size=18)) +
  theme(axis.title.x = element_text(size=16,face="bold"),axis.title.y = element_text(size=16,face="bold"),
        axis.text.x=element_text(hjust=1,size=12),axis.text.y=element_text(hjust=1,size=12),
        legend.text=element_text(size=12),
        legend.justification=c(1,0.8),legend.position=c(1,0.95),legend.title=element_blank(),
        legend.key = element_rect(colour = NA, fill = NA))+ scale_colour_manual("",breaks = c("0", "1"),
                                                                                values = c("red", "#339999"),
                                                                                labels=c("Unoccupied","Occupied")) 









#####################################
##### Fit Models:  HOURLY #####
#####################################

mopt1 = gam(kwh ~ s(temperature, by=occ) + s(hours, by=day) + 
             Econ*optimalstart*solar+ s(prev_week, by=occ) + s(prev_2week, by=occ)+
             s(temp_4delay, by=occ)+ s(temp_6delay, by=occ), 
           data = energy[1:(nrow(energy)-336),])
summary(mopt1)
#plot.gam(mopt1)


mopt2 = gam(kwh ~ s(temperature, by=occ) + s(hours, by=day) + 
             Econ*optimalstart*month*solar + s(prev_week, by=day)+
             + s(prev_day, by=occ), 
           data = energy[1:(nrow(energy)-336),])
summary(mopt2)
#plot.gam(mopt2)

###########################******************

mopt3 = gam(kwh ~ s(temperature, by=occ) + s(hours2, k=13, bs="cc") + 
             Econ*optimalstart*solar+ s(prev_week, by=occ) + s(prev_2week, by=occ)+
             s(temp_4delay, by=occ)+ s(temp_6delay, by=occ), 
           data = energy[1:(nrow(energy)-336),])
summary(mopt3)
#plot.gam(mopt3)


mopt4 = gam(kwh ~ s(temperature, by=occ) + s(hours2, bs="cc", k=40) + 
             Econ*optimalstart*month*solar + s(prev_week, by=day), 
           data = energy[1:(nrow(energy)-336),])
summary(mopt4)
#plot.gam(mopt4)

occ_vals <- as.data.frame(predict(mopt4, type = "terms", se=TRUE))


#######Confidence intervals for "hours2" smooth:

preds   <- predict(mopt4, type = "terms", se.fit = TRUE)
my_data <- data.frame(mu   = occ_vals$fit.s.hours2.,
                      low  = occ_vals$fit.s.hours2. - 1.96 * occ_vals$se.fit.s.hours2.,
                      high = occ_vals$fit.s.hours2. + 1.96 * occ_vals$se.fit.s.hours2.)

week_data <- my_data[c(73:168, 1:72),]
plot(week_data$mu, type="l", col="red", lwd=2, main="Time of Day v/s Occupancy profile - Thursday to Wednesday",
     xlab="Time of day (24 hours each day)", ylab="Occupancy Profile")
lines(week_data$low, type="l", col="blue", lty=2, lwd=2)
lines(week_data$high, type="l", col="blue", lty=2, lwd=2)

write.csv(week_data, file="Occ values with Confidence Intervals.csv")



write.csv(occ_vals, file="OCC_VALUES_overall.csv")



# Variable plots for Simple model  ----------------------------------------------------------
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
plot.gam(mopt4, col = "red",
         shade = TRUE, shade.col="rosybrown1", lwd=3,
         panel.abline=grid(),select=3, main = "Effect of Time of Day on Energy Consumption \n Weekly",
         xlab="Time of the day (24 hours each from Sunday to Saturday)",
         ylab = "Smooth Function of Time of Day",
         font.lab=2, rug = FALSE,cex.axis=1,cex.main=1.6,cex.lab=1.4)





#options(repos="https://CRAN.R-project.org")