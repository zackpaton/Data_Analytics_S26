# Zachary Paton
# ITWS-4600 Data Analytics
# Lab 2
# The models I chose to use were sqft + beds, sqft + bath, and sqft + beds + bath

library("ggplot2")
library("readr")



## Read and initialize data
NY_House_Dataset <- read_csv("NY-House-Dataset.csv")

dataset <- NY_House_Dataset



# Clean the data
# Remove outliers
dataset <- dataset[dataset$PRICE<195000000,]

## Remove NAs
dataset <- na.omit(dataset)

# Transform some of the variables for a better fit
dataset$logPRICE <- log10(dataset$PRICE)
dataset$logSQFT  <- log10(dataset$PROPERTYSQFT)



# Create the 3 models

# Model 1: Price with PropertySqFt and Beds
lmod1 <- lm(logPRICE ~ logSQFT + BEDS, data = dataset)

# Model 2: Price with PropertySqFt and Bath
lmod2 <- lm(logPRICE ~ logSQFT + BATH, data = dataset)

# Model 3: Price with PropertySqFt, Beds, and Bath
lmod3 <- lm(logPRICE ~ logSQFT + BEDS + BATH, data = dataset)



# Perform the desired operations on all 3 models

# Print summaries
summary(lmod1)
summary(lmod2)
summary(lmod3)


# Plot most significant variables vs Price with best fit
ggplot(dataset, aes(x = logSQFT, y = logPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Model 1: logPRICE vs logSQFT")

ggplot(dataset, aes(x = logSQFT, y = logPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  ggtitle("Model 2: logPRICE vs logSQFT")

ggplot(dataset, aes(x = logSQFT, y = logPRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col = "green") +
  ggtitle("Model 3: logPRICE vs logSQFT")



# Plot scatter plots of residuals
ggplot(lmod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ggtitle("Model 1 Residual Plot")

ggplot(lmod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ggtitle("Model 2 Residual Plot")

ggplot(lmod3, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ggtitle("Model 3 Residual Plot")



# Compare the 3 models

# R Squared summaries
summary(lmod1)$r.squared
summary(lmod2)$r.squared
summary(lmod3)$r.squared

# Adjusted R Squared summaries
summary(lmod1)$adj.r.squared
summary(lmod2)$adj.r.squared
summary(lmod3)$adj.r.squared



### THE END ###