#' ---
#' title: "STAT 344 Project"
#' date: "Date field"
#' author: ""
#' output: html_document
#' # Or use output: pdf_document
#' # Or don't use either, in which case you will be prompted to choose
#' ---

#x <- read.csv("STAT 344/project/Melbourne_housing_FULL.csv")

#summary(x)

#y <- x[(is.na(x$Car) == FALSE) & is.na(x$HouseHousePrice) == FALSE, ]

#summary(y)

#write.csv(y, "M_housing_new.csv", row.names = FALSE)

x <- read.csv("STAT 344/project/M_housing_new.csv")
head(x)
summary(x)

nrow(x)

x$HousePrice <- x$Price
x
table(x$Regionname)

# data summary(part ...)
# Since we create the binary by ourselves, we need to add that column
x$CarSpotSatisfaction <- 0
for (i in 1:nrow(x)){
  if (x$Car[i] >= 2){
    x$CarSpotSatisfaction[i] = 1
  }
}

# code for drawing SRS and the two graph (section ...)
# Choose the Simple Random Sample with size 378
set.seed(20) # set seed for reproducibility
samples <- sample(1:nrow(x), 378)
y <- x[samples, ] # here y would be our SRS. 

# for plots
par(mfrow=c(1,2)) # crate side by side plots

# draw histogram for all the HousePrices in our sample
plot_HousePrice <- hist(y$HousePrice, xlab = "HousePrice of House (in AUD)",
                   main = "Histogram of House HousePrices in Sample" ) 

# bar plot for our binary variable
count <- table(y$CarSpotSatisfaction)
barplot(count,names.arg=c("< 2 parking spaces", ">= 2 parking spaces"),
        main = "Bar Plot for House Parking Spaces")

# Summary of SRS
mean(y$HousePrice)
var(y$HousePrice)
mean(y$CarSpotSatisfaction)

#Stratified sample
set.seed(1000)
eachN <- table(x$Type)

samforh <- sample(1:eachN[1], 284)
samfort <- sample(1:eachN[2], 29)
samforu <- sample(1:eachN[3], 65)

#get the population information for each stratum
Hp <- x[x$Type == 'h', ]
Tp <- x[x$Type == 't', ]
Up <- x[x$Type == 'u', ]

# draw the sample
h <- Hp[samforh,]
t <- Tp[samfort,]
u <- Up[samforu,]

# get summary of Stratified Sample
var(h$HousePrice)
var(t$HousePrice)
var(u$HousePrice)

mean(h$CarSpotSatisfaction)
mean(t$CarSpotSatisfaction)
mean(u$CarSpotSatisfaction)


###############################################
# Data Analysis

# House Price
# SRS
# Vanilla

ybarvani <- mean(y$HousePrice)
varybar <- var(y$HousePrice)
n <- nrow(y)
N <- nrow(x)
#SE
SE <- sqrt((1-(n/N))*varybar/n)

# 95% CI
ybarvani - 1.96*SE
ybarvani + 1.96*SE


# ratio and regression

mean(y$HousePrice)/mean(y$Rooms) * mean(x$Rooms)
var(y$HousePrice - mean(y$HousePrice)/mean(y$Rooms)* y$Rooms)
sqrt((1-n/N) * var(y$HousePrice - mean(y$HousePrice)/mean(y$Rooms)* y$Rooms)/n)
ci_upper <- mean(y$HousePrice)/mean(y$Rooms) * mean(x$Rooms) + 1.96*sqrt((1-n/N) * var(y$HousePrice - mean(y$HousePrice)/mean(y$Rooms)* y$Rooms)/n)
ci_lower <- mean(y$HousePrice)/mean(y$Rooms) * mean(x$Rooms) - 1.96*sqrt((1-n/N) * var(y$HousePrice - mean(y$HousePrice)/mean(y$Rooms)* y$Rooms)/n)
c(ci_lower, ci_upper)

##############################################################################
library(repr)
library(tidyverse)
library(dplyr)
library(digest)
library(cowplot)
library(installr)
updateR()
library(infer)

bootstrap_dist <- y %>%
  rep_sample_n(size = 378, reps = 1000) %>%
  group_by(replicate)  %>%
  summarize(sample_mean = mean(HousePrice)) %>%
  select(sample_mean) %>%
  as.numeric()

(1107566 - 1046703)/1046703
(30723 - 27576)/30723

ci_plot <- 
  bootstrap_dist %>% 
  ggplot(aes(x = HousePrice)) +
  geom_histogram(bins = 50, colour = "white", fill = "grey") +
  annotate("rect", xmin = ci_lower, xmax = ci_upper, ymin = 0, ymax = Inf, #shaded range of CI
           fill = "deepskyblue",
           alpha = 0.3)


# Stratified for house Price
N1 <- table(x$Type)
Nh <- 15364
Nt <- 1577
Nu <- 3482
nh <- nrow(h)
nt <- nrow(t)
nu <- nrow(u)
n <- 378

# estimate
mean(h$HousePrice)
mean(t$HousePrice)
mean(u$HousePrice)
ystr <-  (Nh/N)*mean(h$HousePrice) + (Nt/N)*mean(t$HousePrice) + (Nu/N)*mean(u$HousePrice)

# SE
var(h$HousePrice)
var(t$HousePrice)
var(u$HousePrice)

varstr <- (Nh/N)^2*(1-(nh/Nh))*var(h$HousePrice)/nh + 
  (Nt/N)^2*(1-(nt/Nt))*var(t$HousePrice)/nt +
  (Nu/N)^2*(1-(nu/Nu))*var(u$HousePrice)/nu
# CI  
ystr + 1.96*sqrt(varstr)
ystr - 1.96*sqrt(varstr)

# For Car Spot Satisfication Rate
# SRS
# proportion
pva <- mean(y$CarSpotSatisfaction)

# SE
sep <- sqrt((1-n/N)*pva*(1-pva)/n)

# CI
pva + 1.96*sep
pva - 1.96*sep

# Stratified Sample
# proprotion
pstr <- (Nh/N)*mean(h$CarSpotSatisfaction) + (Nt/N)*mean(t$CarSpotSatisfaction) + (Nu/N)*mean(u$CarSpotSatisfaction)
a <-mean(h$CarSpotSatisfaction)
b <- mean(t$CarSpotSatisfaction)
c <- mean(u$CarSpotSatisfaction)
# SE
varstr1 <- (Nh/N)^2*(1-(nh/Nh))*a*(1-a)/nh + 
  (Nt/N)^2*(1-(nt/Nt))*b*(1-b)/nt +
  (Nu/N)^2*(1-(nu/Nu))*c*(1-c)/nu
sqrt(varstr1)

# CI
pva + 1.96*sqrt(varstr1)
pva - 1.96*sqrt(varstr1)




