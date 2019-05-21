# clear workspace variables
rm(list = ls());
# it means ctrl+L. clear window
cat("\014") 
# close all plots
graphics.off() 


setwd("C:/Users/HP/Downloads")
# Reading the file

library(rpart)
library(gam)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(reshape)
library('plyr')

# Reading the data

data = read.csv("hour.csv")


# Variable type identification
str(data)

head(data)

# Finding missing values
table(is.na(data))

# Understanding and analyzing the distribution of numerical variables 
# Season has four categories of almost equal distribution
prop.table(table(data$season))

processedData<-data
processedData$weeknum<-week(processedData$dteday)
processedData$dteday<- NULL
processedData<-processedData[,c(1,17,2:16)]

for(i in unique(processedData$season)) {
  processedData[[paste0("season_",i)]] <- ifelse(processedData$season==i,1,0)
}
for(i in unique(processedData$weekday)) {
  processedData[[paste0("weekday_",i)]] <- ifelse(processedData$weekday==i,1,0)
}
for(i in unique(processedData$weathersit)) {
  processedData[[paste0("weathersit_",i)]] <- ifelse(processedData$weathersit==i,1,0)
}
for(i in unique(processedData$mnth)) {
  processedData[[paste0("mnth_",i)]] <- ifelse(processedData$mnth==i,1,0)
}
for(i in unique(processedData$hr)) {
  processedData[[paste0("hour_",i)]] <- ifelse(processedData$hr==i,1,0)
}

for(i in unique(processedData$weeknum)) {
  processedData[[paste0("weeknum_",i)]] <- ifelse(processedData$weeknum==i,1,0)
}

# Checkign ACF
acf(x=processedData$cnt, lag.max=10 , plot=TRUE)

#creating lag values
processedData$lag1<-0
processedData$lag2<-0
processedData$lag3<-0

processedData$lag1[2:17379]=processedData$cnt[1:17378]
processedData$lag2[3:17379]=processedData$cnt[1:17377]
processedData$lag3[4:17379]=processedData$cnt[1:17376]


# Fix cyclic variables
processedData$weeknum<-pmin(abs(3-processedData$weeknum),56-processedData$weeknum);  
processedData$mnth<-pmin(abs(1-processedData$mnth),13-processedData$mnth);  
processedData$hr<-pmin(abs(4-processedData$hr),28-processedData$hr);  


# move count to end
processedData<- processedData[c(1:16, 18:124,17)]
# remove excessive columns
processedData<- processedData[-c(1,3,8,10,15,16)]

processedData<- processedData[3:17379,]


# Exploratory data analysis -----------------------------------------------


# plot  atemp
p<-ggplot(processedData,aes(x=atemp,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Adjusted Temperature', y= 'Hourly Usage Count')+
  geom_smooth(method='auto', qr=TRUE)+
  geom_smooth(method='lm',color= 'red', qr=TRUE)+
  geom_smooth(method='loess', color ='blue')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))


# plot temp
p<-ggplot(processedData,aes(x=temp,y=cnt))+
  geom_point(alpha=0.07,color='orange')+
  labs(x='Temperature', y= 'Hourly Usage cnt')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')+
  geom_smooth(method='loess', color ='blue')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot humidity
p<-ggplot(processedData,aes(x=hum,y=cnt))+
  geom_point(alpha=0.07, color='violet')+
  labs(x='Humidity', y= 'Hourly Usage Count')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')+
  geom_smooth(method='loess', color ='blue')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot weather situation
p<-ggplot(data,aes(x=weathersit,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color='green')+
  labs(x='Weather situation', y= 'Hourly Usage Count')+
  geom_smooth(method='loess')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# plot wind-speed
p<-ggplot(processedData,aes(x=windspeed,y=cnt))+
  geom_point(alpha=0.07, color='green')+
  labs(x='Wind speed', y= 'Hourly Usage Count')+
  geom_smooth(method='lm',color= 'red')+
  geom_smooth(method='loess', color ='blue')
ggMarginal(p, type = "histogram", fill="transparent", margins=c("x"))

# correlation plot
data$weekday=as.integer(data$weekday)
data$hr=as.integer(data$hr)
data$holiday=as.integer(data$holiday)
data$workingday=as.integer(data$workingday)
cont_data<-data[c(6:17)]
o=corrplot(cor(cont_data),method='number') # this method can be changed try using method='circle'

# plot 24 hours
ggplot(data,aes(x=hr,y=cnt))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'coral')+
  labs(x='Hours', y= 'Usage count')+
  geom_smooth()+
  geom_smooth(method='loess', color ='blue')

# plot hours from 4am using processed Data
ggplot(processedData,aes(x=hr,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Hours from 4am', y= 'Hourly Usage Count')+
  geom_smooth(method='lm', color='red')+
  geom_smooth(method='loess', color ='blue')

# Plot Months
ggplot(data,aes(x= mnth,y=cnt))+
  geom_count(color= 'yellow1')+
  geom_point(alpha=0.05, color= 'green')+
  labs(x='Month number', y= 'Hourly Usage Count')+
  geom_smooth()+
  geom_smooth(method='loess', color ='blue')

# plot months from January
ggplot(processedData,aes(x=mnth,y=cnt))+
  geom_count(color='yellow')+
  geom_point(alpha=0.07, color= 'pink')+
  labs(x='Months from January', y= 'Usage count')+
  geom_smooth(method='lm', color ='black')+
  geom_smooth(method='loess', color ='blue')

# Boxplots -plot year vs count
rawData$yr<-factor(rawData$yr)
ggplot(data=rawData,aes(x=yr,y=cnt,  fill=yr ))+
  geom_boxplot(outlier.colour="Black",  outlier.size=1, notch=FALSE)+
  labs(x='Year', y= 'Hourly Usage Count')





# We can see that for weather situation , most bikes are rented out in 1 category of weather and least in 4 th
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(as.integer(data$season))
hist(data$weathersit)
hist(data$hum)
hist(as.integer(data$holiday))
hist(as.integer(data$workingday))
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

# Convert discrete variables into factor (season, weather, holiday, workingday)

data$season=as.factor(data$season)
data$weather=as.factor(data$weathersit)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

# plotting the box plot hourly trend of count over hours

ggplot(data, aes(x = hr, y = cnt, fill = hr)) +
  geom_boxplot() +
  theme_light(base_size = 11) +
  xlab("hour") +
  ylab("count of users") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))


# plotting the box plot hourly trend of count over hours( contains outliers)

boxplot(data$cnt~data$hr,xlab="hour", ylab="count of users")

# We can see the trend of bike demand over hours. Quickly, Segregatating the bike demand in three categories:
  
#  High       : 7-9 and 17-19 hours
#  Average    : 10-16 hours
#  Low        : 0-6 and 20-24 hours

#  Looking at the distribution of registered and casual users separately(black and white).
boxplot(data$registered~data$hr,xlab="hour", ylab="registered users")
boxplot(data$casual~data$hr,xlab="hour", ylab="Casual users")

#  Looking at the distribution of registered and casual users separately(Coloured).
ggplot(data, aes(x = hr, y = registered, fill = hr)) +
  geom_boxplot() +
  theme_light(base_size = 11) +
  xlab("hour") +
  ylab("registered users") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))

ggplot(data, aes(x = hr, y = casual, fill = hr)) +
  geom_boxplot() +
  theme_light(base_size = 11) +
  xlab("hour") +
  ylab("casual users") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))

# Above we can see that registered users have similar trend as count. Whereas, casual users have different trend.

# Taking log
boxplot(log(data$cnt)~data$hr,xlab="hour",ylab="log(count)")


ggplot(data, aes(x = hr, y = log(cnt), fill = hr)) +
  geom_boxplot() +
  theme_light(base_size = 11) +
  xlab("hour") +
  ylab("casual users") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))


# Plots showing registered and casual users??? demand over days

date=substr(data$dteday,1,10)
days<-weekdays(as.Date(date))
data$day=days

boxplot(data$registered~data$day,xlab="day", ylab="registered users")
boxplot(data$casual~data$day,xlab="day", ylab="Casual users")


ggplot(data, aes(x = day, y = registered, fill = day)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Day") +
  ylab("Registered users") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))

ggplot(data, aes(x = day, y = casual, fill = day)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Day") +
  ylab("Casual users") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))


# Observing the trend with weather 

boxplot(data$registered~data$weathersit,xlab="day", ylab="registered users")
boxplot(data$casual~data$weathersit,xlab="day", ylab="Casual users")

# Finding Correlations

sub=data.frame(data$registered,data$casual,data$cnt,data$temp,data$hum,data$atemp,data$windspeed)
cor(sub)

# Variable atemp is highly correlated with temp
#Windspeed has lower correlation as compared to temp and humidity
# Variable temp is positively correlated with dependent variables (casual is more compare to registered)
# Variable registered is highly correlated with count


# Analyzing trnd of bikes over year
data$year = substr(data$dteday,1,4)
data$year = as.factor(data$year)

ggplot(data, aes(x = year, y = cnt, fill = year)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("year") +
  ylab("count") +
  ggtitle("\n")   +
  theme(plot.title = element_text(size = 11, face="bold"))

boxplot(data$cnt~data$year,xlab="year", ylab="count")




# Binning

data$hour=as.integer(data$hr)


d=rpart(registered~hour,data=data)
fancyRpartPlot(d)

data$dp_reg=0
data$dp_reg[data$hr<8]=1
data$dp_reg[data$hr>=22]=2
data$dp_reg[data$hr>9 & data$hour<18]=3
data$dp_reg[data$hr==8]=4
data$dp_reg[data$hr==9]=5
data$dp_reg[data$hr==20 | data$hour==21]=6
data$dp_reg[data$hr==19 | data$hour==18]=7

# Create buckets for casual 

d=rpart(casual~hour,data=data)
fancyRpartPlot(d)

data$dp_cas=0
data$dp_cas[data$hr<=8]=1
data$dp_cas[data$hr==9]=2
data$dp_cas[data$hr>=10 & data$hr<=19]=3
data$dp_cas[data$hr>19]=4



###created bins for temperature for both registered and casuals users. Variables created are (temp_reg and temp_cas).

f=rpart(registered~temp,data=data)
fancyRpartPlot(f)

f=rpart(casual~temp,data=data)
fancyRpartPlot(f)

data$temp_reg=0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>=13 & data$temp<23]=2
data$temp_reg[data$temp>=23 & data$temp<30]=3
data$temp_reg[data$temp>=30]=4

data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>=15 & data$temp<23]=2
data$temp_cas[data$temp>=23 & data$temp<30]=3
data$temp_cas[data$temp>=30]=4

## Year Bins

data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$mnth>3]=2
data$year_part[data$year=='2011' & data$mnth>6]=3
data$year_part[data$year=='2011' & data$mnth>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$mnth>3]=6
data$year_part[data$year=='2012' & data$mnth>6]=7
data$year_part[data$year=='2012' & data$mnth>9]=8
table(data$year_part)


# creating another variable day_type which may affect our accuracy as weekends and weekdays are important in deciding rentals
data$day_type=0

data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

#Creating another variable for weekend
data$weekend=0
data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1


#predicting the cnt


set.seed(123)
sample = sample.split(data,SplitRatio = 0.80) 
train = subset(data,sample==TRUE)
test = subset(data, sample==FALSE)
# Creating random forest model 1 

fit1 <- randomForest(cnt~hr, data=train,importance=TRUE, ntree=250)
print(fit1)

# Predicting on test set
pred1 <- predict(fit1, test, type = "class")

# Visualizing the Random Forest Plot
plot(data$instant, data$cnt, type = "l", col = "red", xlab = "Day", ylab = "Number of Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Estimated"), lty = c(1, 1), col = c("red", "blue"))
lines(test$instant, pred1, type = "l", col = "blue")
rm(pred1, fit1)


# Creating random forest model 2

# converting all relevant categorical variables into factors to feed to our random forest model
data$season=as.factor(data$season)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)
data$weather=as.factor(data$weathersit)
data$hour=as.factor(data$hr)
data$month=as.factor(data$month)
data$day_part=as.factor(data$dp_cas)
data$day_type=as.factor(data$dp_reg)
data$day=as.factor(data$day)
data$temp_cas=as.factor(data$temp_cas)
data$temp_reg=as.factor(data$temp_reg)


### Changing the number of ntree

#fit2 <- randomForest(cnt~hr + workingday + holiday  + temp_reg+hum+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,data=train,importance=TRUE, ntree=10)
#print(fit2)

#fit2 <- randomForest(cnt~hr + workingday + holiday  + temp_reg+hum+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,data=train,importance=TRUE, ntree=100)
#print(fit2)

fit2 <- randomForest(cnt~hr + workingday + holiday  + temp_reg+hum+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,data=train,importance=TRUE, ntree=300)
print(fit2)
# Predicting on test set
pred2 <- predict(fit2, test, type = "class")

# Visualizing the Random Forest Plot 2
plot(data$instant, data$cnt, type = "l", col = "red", xlab = "data$instant", ylab = "Number of Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Estimated"), lty = c(1, 1), col = c("red", "blue"))
lines(test$instant, pred2, type = "l", col = "blue")
rm(pred2, fit2)

# Linear Model
linear_model <- lm(cnt ~ hr, data)

# put the actual and predicted counts in a data frame
results <- tibble(cnt_actual = data$cnt,
                  cnt_pred=linear_model$fitted.values)

results %>% 
  mutate(resid=cnt_actual - cnt_pred) %>% 
  mutate(resid_sq = resid^2) %>% 
  summarise(MSE=mean(resid_sq))


#Linerar Reggression
scatter.smooth(x=data$temp, y=data$cnt, main="cnt ~ temperature")

# linear regression
bike.lm <- lm(data = train, cnt ~ season  + weekday + hr + temp + hum + windspeed)
summary(bike.lm)
bike.lm.step <- step(bike.lm)
summary(bike.lm.step)

# generalized linear model
bike.glm2 <- glm(data = train, cnt ~ as.factor(season)  + as.factor(weekday) + as.factor(hr) + as.factor(workingday) + 
                   as.factor(holiday) + as.factor(weathersit) + temp + hum + windspeed)
summary(bike.glm2)
bike.glm2.step <- step(bike.glm2)
summary(bike.glm2.step)

# gam1
bike.gam1 <- gam(data = train, cnt ~ as.factor(season)  + as.factor(weekday) + as.factor(hr) + as.factor(workingday) + 
                   as.factor(holiday) + as.factor(weathersit) + temp + hum + windspeed)
summary(bike.gam1)

bike.gam1.step <- gam(data = train, cnt ~ as.factor(season) + as.factor(weekday) + as.factor(hour) + 
                        + as.factor(weathersit) + temp + hum + windspeed)
summary(bike.gam1.step)


# gam2
bike.gam2 <- gam(data = train, cnt ~ as.factor(season)  + as.factor(weekday) + as.factor(hour) + as.factor(workingday) + 
                   as.factor(holiday) + as.factor(weathersit) + s(temp) + s(hum) + s(windspeed))
summary(bike.gam2)

bike.gam2.step <- gam(data = train, cnt ~ as.factor(season)  + as.factor(weekday) + as.factor(hour) + as.factor(weathersit) + 
                        s(temp) + s(hum) + s(windspeed))
summary(bike.gam2.step)


# gam3
bike.gam3 <- gam(data = train, cnt ~ as.factor(season)  + as.factor(weekday) + as.factor(hr) + as.factor(workingday) + 
                   as.factor(holiday) + as.factor(weathersit) + s(temp, 4) + s(hum, 10) + s(windspeed, 6))
summary(bike.gam3)

bike.gam3.step <- gam(data = train, cnt ~ as.factor(season)  + as.factor(weekday) + as.factor(hour) + as.factor(weathersit) + 
                        s(temp, 4) + s(hum, 10) + s(windspeed, 6))
summary(bike.gam3.step)



# svm 
install.packages("e1071")
library('e1071')
fitsvm <- svm(cnt~hr + workingday + holiday  + temp_reg+hum+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part,data=train,kernel = "linear", cost = 10)
print(fitsvm)

anova(bike.gam1.step, bike.gam2.step, bike.gam3.step,fit2, test = "F")

### predictions ###
# lm
plot(test$cnt, main = "Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(bike.lm.step, newdata = test), col = "red", pch = 20)

# glm
plot(test$cnt, main = "Generalized Linear Model", ylab = "Test Set Rental Count", pch = 20)
points(predict(bike.glm2.step, newdata = test), col = "red", pch = 20)

# gam3
plot(test$cnt, main = "GAM3", ylab = "Test Set Rental Count", pch = 20)
points(predict(bike.gam3.step, newdata = test), col = "red", pch = 20)

#randomforest

plot(test$cnt, main = "RandomForest", ylab = "Test Set Rental Count", pch = 20)
points(predict(fit2, newdata = test), col = "red", pch = 20)

#svm

plot(test$cnt, main = "RandomForest", ylab = "Test Set Rental Count", pch = 20)
points(predict(fitsvm, newdata = test), col = "red", pch = 20)

# Some visualizations

data$season  <- factor(data$season, labels = c("Spring", "Summer", "Fall", "Winter"))
data$weather <- factor(data$weather, labels = c("Good", "Normal", "Bad", "Very Bad"))
data$hr <- as.factor(data$hr)

season_summary <- ddply(data,.(season,hr),
                        summarise, cnt = mean(cnt))
ggplot(data, aes(x = hr, y = cnt, colour = season)) +
  geom_point(data = season_summary, aes(group = season)) +
  geom_line(data = season_summary, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more in Fall, and much less in Spring.\n") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(data,.(weather,hr),
                         summarise, cnt = mean(cnt))
ggplot(data, aes(x = hr, y = cnt, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more when the weather is Good.\n") + 
  theme(plot.title=element_text(size=18))

## Other Visualizations

month <- as.integer(format(as.POSIXlt(data$dteday), format = "%m"))
weekday <- as.integer(format(as.POSIXlt(data$dteday), format = "%u"))

bike <- data.frame(data$season, month, weekday, data$hr, as.factor(data$workingday), as.factor(data$holiday), 
                   as.factor(data$weather), data$temp, data$hum, data$windspeed, data$cnt)
names(bike) <- c("season", "month", "weekday", "hour", "isweekday", 
                 "isholiday", "weathertype", "temperature", "humidity", "windspeed", "count")

bike <- bike[which(bike$windspeed != 0.0000),]

head(bike, 5)

count.summary <- ddply(bike,.(season, month, weekday, hour, isweekday, isholiday, weathertype), summarise, 
                       temperature = mean(temperature), 
                       humidity = mean(humidity), 
                       windspeed = mean(windspeed), 
                       count = mean(count))

head(count.summary)

### assesments ###

### box plot of count of bike rentals vs season 
ggplot(count.summary, aes(x = season, y = count, fill = factor(season))) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_fill_manual(values = c("#F39C12","#2ECC71", "#E74C3C","#D6EAF8" ), 
                    name="Season:")
            

# line plot of rentals v.s. hour of day
ggplot(count.summary, aes(x = hour, y = count)) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") 
  

# boxplot of rental v.s. holiday 
ggplot(count.summary, aes(x = isholiday, y = count, fill = season)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Is it Holiday?") +
  ylab("Number of Bike Rentals") 


# boxplot of rentals v.s. weather
ggplot(count.summary, aes(x = weathertype, y = count, fill = weathertype)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Type of Weather") +
  ylab("Number of Bike Rentals") 
  


# line plot of rental v.s. temperature

ggplot(count.summary, aes(x = temperature, y = count, color = weathertype)) +
  geom_smooth(fill = NA, size = 1, method = 'loess' , stat = 'smooth') +
  theme_light(base_size = 11) +
  xlab("Temperature") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow" 
                                  )) +
  theme(plot.title = element_text(size = 11, face="bold"))


# line plot of rental v.s. humidity
ggplot(count.summary, aes(x = humidity, y = count, color = weathertype)) +
  geom_smooth(method = 'glm', fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Humidity") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "Bad weather")) +
  theme(plot.title = element_text(size = 11, face="bold"))



# line plot of rental v.s. wind speed
ggplot(count.summary, aes(x = windspeed, y = count, color = weathertype)) +
  geom_smooth(fill = NA, size = 1, method = 'loess') +
  theme_light(base_size = 11) +
  xlab("Wind Speed") +
  ylab("Number of Bike Rentals") +
  ggtitle("\n") +
  scale_color_discrete(name = "Type of Weather:",
                       breaks = c(1, 2, 3, 4),
                       labels = c("Clear or Cloudy", 
                                  "Mist", 
                                  "Light Rain or Snow", 
                                  "Bad wearther")) +
  theme(plot.title = element_text(size = 11, face="bold"))




      
