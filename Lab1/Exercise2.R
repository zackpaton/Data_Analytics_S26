# Zachary Paton
# ITWS-4600 Data Analytics
# Lab 1
# The variables I chose to work with were ECO.new (ECO) and BDH.new (BDH)

library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
#setwd("~/Courses/Data Analytics/Spring26/labs/lab 1/")

# read data
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

# view dataframe
#View(epi.data)

# Copy variables and remove NAs
ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new

ECO <- na.omit(ECO)
BDH <- na.omit(BDH)


# Print variable summaries
summary(ECO)
summary(BDH)


# Create variable boxplots
boxplot(ECO, BDH, names = c("ECO","BDH"))


# Create histograms with overlayed theoretical probability distributions

# Histogram and probability distribution for ECO
hist(ECO, prob=TRUE)
curve(dnorm(x, mean=mean(ECO), sd=sd(ECO)), add=TRUE)

# Histogram and probability distribution for BDH
hist(BDH, prob=TRUE)
curve(dnorm(x, mean=mean(BDH), sd=sd(BDH)), add=TRUE)


# Plot ECDFs for each variable
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 
plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 


# QQ plots of each variable against the normal distribution
qqnorm(ECO); qqline(ECO)
qqnorm(BDH); qqline(BDH)


# QQ plot of the 2 variables ECO and BDH against each other
qqplot(ECO, BDH, xlab = "Q-Q plot for ECO & BDH") 


# Statistic tests for each variable
shapiro.test(ECO)
shapiro.test(BDH)

ad.test(ECO)
ad.test(BDH)

ks.test(ECO,BDH)

wilcox.test(ECO,BDH)

var.test(ECO,BDH)
t.test(ECO,BDH)
