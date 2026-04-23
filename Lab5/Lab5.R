##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)
library(caret)

## set working directory so that files can be referenced without the full path
#setwd("~/Courses/Data Analytics/Fall25/labs/lab 4/")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## visualize variables
pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

ggpairs(wine, ggplot2::aes(colour = Type))



### 



# Train/Test split
train_index <- createDataPartition(wine$Type, p = 0.7, list = FALSE)
train_wine <- wine[train_index, ]
test_wine  <- wine[-train_index, ]


# Choose a feature subset using variable importance on training set
predictor_names <- setdiff(names(train_wine), "Type")
x_train <- train_wine[, predictor_names, drop = FALSE]
imp_scores <- filterVarImp(x = x_train, y = train_wine$Type)
imp_scores$mean_importance <- rowMeans(imp_scores, na.rm = TRUE)

# Keep 6 best predictors
k_features <- 6
feat_order <- order(imp_scores$mean_importance, decreasing = TRUE)
feat_subset <- colnames(x_train)[feat_order]
feat_subset <- head(feat_subset, k_features)


# Build model frame with only Type and selected features
train_model <- train_wine[, c("Type", feat_subset), drop = FALSE]
test_model  <- test_wine[, c("Type", feat_subset), drop = FALSE]

# Deal with variable names with spaces or special characters
rhs <- paste(paste0("`", feat_subset, "`"), collapse = " + ")
svm_formula <- as.formula(paste("Type ~", rhs))

# Grids for tuning
cost_grid  <- 10^seq(-1, 2, by = 0.5)
gamma_grid <- 10^seq(-2, 1, by = 0.5)
tune_ctrl  <- tune.control(sampling = "cross", cross = 5)

# Build first SVM with linear kernel using cost vector created above 
tune_linear <- tune.svm(
  svm_formula,
  data = train_model,
  kernel = "linear",
  cost = cost_grid,
  tunecontrol = tune_ctrl
)

# Build second SVM with radial basis kernel using cost and gamma vectors created above
tune_radial <- tune.svm(
  svm_formula,
  data = train_model,
  kernel = "radial",
  cost = cost_grid,
  gamma = gamma_grid,
  tunecontrol = tune_ctrl
)

svm_linear_best <- tune_linear$best.model
svm_radial_best <- tune_radial$best.model

pred_svm_linear <- predict(svm_linear_best, test_model)
pred_svm_radial <- predict(svm_radial_best, test_model)


# kNN on the same predictors
train_x <- train_model[, feat_subset, drop = TRUE]
test_x  <- test_model[, feat_subset, drop = TRUE]
knn_ctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)
knn_grid <- data.frame(k = seq(1, 25, 2))
knn_model <- train(
  x = train_x,
  y = train_model$Type,
  method = "knn",
  preProcess = c("center", "scale"),
  tuneGrid = knn_grid,
  trControl = knn_ctrl
)
pred_knn <- predict(knn_model, test_x)

# Print the statistics for each model for comparison
print(confusionMatrix(pred_svm_linear, test_model$Type))
print(confusionMatrix(pred_svm_radial, test_model$Type))
print(confusionMatrix(pred_knn, test_model$Type))
