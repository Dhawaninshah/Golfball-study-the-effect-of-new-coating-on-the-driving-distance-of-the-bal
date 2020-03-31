####Setting up working Directory
setwd("")
getwd()


#reading library readxl for reading excel files
library(readxl)

#importng dataset
mydata<-read_excel("Golf.xlsx")

View(mydata)

#Variable identification
dim(mydata)
names(mydata)
class(mydata$Current)
class(mydata$New)



#################CCheck Top 6 adn bottom 6##############################
head(mydata)
tail(mydata)

######missing value
colSums(is.na(mydata))

###########interpreting mode from table function
############from histogram current follows normal distribution
col_Head<-c("Mean", "SD", "Variance", "Remarks")
current_Stats<-c(round(mean(mydata$Current), digits = 2), 
                 round(sd(mydata$Current), digits = 2), 
                  round(var(mydata$Current), digits = 2),
                   "curent ball")
New_Stats<-c(round(mean(mydata$New), digits = 2), 
                 round(sd(mydata$New), digits = 2), 
                 round(var(mydata$New), digits = 2),
                 "New ball")
combined_STats<-rbind(col_Head, current_Stats, New_Stats)
combined_STats

##test of means/variance/proportions   answers - mean
##type of sample - one/two/paired  answers - two
## tails (left/right/paired)  - Both
###Confidence intervals - p5%
####is sigma known?   n>30
####Type of test   - 2 sample t test , independent sample
####null hypothesis
###Alternative hypothesis

####understand rejection region using graph
###########################################################################
str(mydata)

table(mydata$Current)
table(mydata$New)

#univariate Analysis

summary(mydata)

range_Current<-max(mydata$Current) - min(mydata$Current)
range_Current

range_New<-max(mydata$New) - min(mydata$New)
range_New

IQR_Current<-IQR(mydata$Current)
IQR_Current

IQR_New<-IQR(mydata$New)
IQR_New

sd(mydata$Current)
sd(mydata$New)

var(mydata$Current)

var(mydata$New)


#Bivariate Analysis
library(corrplot)
cor(mydata$Current, mydata$New)

#Graphical Representations
par(mfrow=c(2,2))


#Histogram
hist(mydata$Current, main="Driving distance of Current balls", xlab = "Driving distance", ylab = "number of balls", col="Blue")
hist(mydata$New, main="Driving distance of New balls", xlab = "Driving distance", ylab = "number of balls", col="Blue")

#Boxplot
boxplot(mydata$Current, main="Driving distance of Current balls", xlab = "Driving distance", ylab = "number of balls", col = "blue", horizontal = TRUE)
boxplot(mydata$New, main="Driving distance of New balls", xlab = "Driving distance", ylab = "number of balls", col = "blue", horizontal = TRUE)




###Hypothesis Formulation
##Ho: There is no significant difference between the driving distance of Current and New golf balls.
##Ho: mucurrent - munew = 0
##    V/s
##H1: There is  a significant difference between the driving distance of current and new golf balls.
##H1:mucurrent - musigma not equal to 0

ttest<-t.test(mydata$Current,mydata$New, paired = F, conf.level = 0.95, alternative = "t") 
ttest

ttest1Current<-t.test(mydata$Current, paired = F, conf.level = 0.95, alternative = "t") 
ttest1Current

ttest1New<-t.test(mydata$New, paired = F, conf.level = 0.95, alternative = "t") 
ttest1New

ftest<-var.test(mydata$Current, mydata$New, ratio = 1, alternative = "t")
ftest

##To find power of the test
Delta<- mean(mydata$New) - mean(mydata$Current)
Delta

SD<-sd(mydata$Current - mydata$New)
SD

Delta/SD

library(ggplot2)
library(pwr)
poweoftest<-pwr.t.test(n = 40, d = Delta/SD, sig.level = 0.05)
poweoftest



##to determine sample size

samplesize<-pwr.t.test(power = .95, d= 0.5, type = "t", alternative = "t", sig.level = .05)
samplesize

