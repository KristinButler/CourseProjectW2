# Week 2 - Project
## Aanalysis of personal activity monitoring device

Libraries needed
```{r, results = "hide"}
library(lubridate)
library(ggplot2)
library(tidyverse)
library(xtable)
library(dplyr)
```


### Code for reading in the dataset and/or processing the data
```{r}
#Set Working Directory
setwd("C:/Users/Kristin.Butler/Desktop/Coursera/ReproduciblaScience/W2")

#Import data
data <-read.csv("./activity.csv", sep = ",", header = TRUE)

#Change format of the data
data$date <- as.Date(data$date)
data$interval <- as.numeric(data$interval)
data$steps <- as.numeric(data$steps)

#Look at imported data
str(data)
```

### Histogram of the total number of steps taken each day
```{r}
#Total number of steps taken each day
data2 <-data %>% group_by(date) %>% summarise(TotalSteps = sum(steps, na.rm = T)) 

#Histogram of total number of steps taken each day
ggplot(data2, aes(x = TotalSteps))+
  geom_histogram(fill = "red")+
  ggtitle("Histogram of total number of steps taken each day")+
  labs(x = "Total steps")+
  theme_classic()+
  theme(plot.title = element_text( face = "bold", size = (15), hjust = 0.5)) 

```

### Mean and median number of total steps taken each day
```{r}
Mean_ste <-  mean(data2$TotalSteps, rm.na = T) %>% format(digits=1)
Med_ste <- median(data2$TotalSteps, rm.na = T)

Mean_ste
Med_ste
```
The mean of total of steps taken each day are `r Mean_ste` and median is `r Med_ste`. 

### Time series plot of the average number of steps taken

```{r}
data4 <- data %>% group_by(interval) %>% summarise(Mean.Steps.Int = mean(steps, na.rm = T)) 

ggplot(data = data4, aes(x = interval, y = Mean.Steps.Int), group = 1)+
  theme_bw()+
  geom_line(color = "blue", size = 2)+
  ggtitle("Average steps taken per interval")+
  labs(x = "Interval", y = "Average Steps")+
  theme(plot.title = element_text( face = "bold", size = (15), hjust = 0.5)) 
```

### The 5-minute interval that, on average, contains the maximum number of steps
```{r}
data4[which.max(data4$Mean.Steps.Int),]
```


### Code to describe and show a strategy for imputing missing data

Total numbers and percentages of missing values in data:
```{r}
table(is.na(data$steps))

nrow(data[!complete.cases(data$steps), ])/nrow(data)*100
```
We have 2304 missing values in the data which is about 13% of the values.

Lets replace missing data using a library called mice:
```{r}
#Turn intervals into factors
dat <- data %>% mutate(interval = as.factor(interval))

#Initiate mice package
library(mice)

#Run the imputation
imputed = mice(dat, m=5, maxit=50, method="mean", seed = 500)

#Create a data set with new values
data6 <- complete(imputed)

#Check for missing in the imputed data set
sapply(data6, function(x) sum(is.na(x)))
```

### Histogram of the total number of steps taken each day after missing values are imputed

```{r}
#Total number of steps taken each day
data7 <-data6 %>% group_by(date) %>% summarise(TotalSteps = sum(steps, na.rm = T)) 
data7

#Histogram of total number of steps taken each day
ggplot(data2, aes(x = TotalSteps))+
  geom_histogram(fill = "red")+
  ggtitle("Histogram of total number of steps taken each day no missing values")+
  labs(x = "Total steps")+
  theme_classic()+
  theme(plot.title = element_text( face = "bold", size = (15), hjust = 0.5)) 
```

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```{r}
data7 <- data %>% mutate(day = wday(date, label = T))

data7b <- mutate(data7, WD = ifelse(data7$day == c("Mon", "Tue", "Wed", "Thu", "Fri"), "weekday", "weekend"))

data7C <- select(data7b, interval, steps, WD)

data7_wd <-filter(data7C, WD == "weekday")
data7_we <- filter(data7C, WD == "weekend")

#calculate average
data7_wd_avg <- data7_wd %>% group_by(interval) %>% summarise(avg.step = mean(steps, na.rm = T)) %>% mutate(WD = "wd")

data7_we_avg <-data7_we %>% group_by(interval) %>% summarise(avg.step = mean(steps, na.rm = T)) %>% mutate(WD = "we")

#Combine data in one data frame
data7_All_avg <- rbind(data7_wd_avg, data7_we_avg)


ggplot(data = data7_All_avg, aes(x = interval, y = avg.step, group = 1))+
  geom_line(color = "blue")+
  facet_wrap(~WD, labeller = labeller(WD=c("wd" = "Weekdays", "we" = "Weekend")))+
  theme_classic()+
  labs(title = "Comparison of average steps on weekends and weekdays")+
  xlab("Time interval (min)")+
  ylab("Average steps")+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18), axis.text.x = element_text(angle = 90))
```

