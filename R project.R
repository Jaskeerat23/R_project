install.packages("caTools")
library(caTools)

read_data <- function(file_path){
  data <- read.csv(file_path)
  return(data)
}

encodeGender <- function(data){
  #Lowering the strings
  data$sex <- tolower(data$sex)
  #0 for Male, 1 for Female
  data$sex <- as.numeric(data$sex == 'female')
  return(data$sex)
}

encodeSmoker <- function(data){
  #Lowering the strings
  data$smoker <- tolower(data$smoker)
  #0 for Male, 1 for Female
  data$smoker <- as.numeric(data$smoker == 'yes')
  return(data$smoker)
}

encodeRegion <- function(data){
  data$region <- as.factor(data$region)
  return((as.numeric(data$region)))
}

normalizeCol <- function(col){
  min_ <- min(col)
  max_ <- max(col)
  normalized <- (col - min_)/(max_ - min_)
  return(normalized)
}

denormalize <- function(col, min_, max_){
  denorm <- col * (max_ - min_) + min_
  return(denorm)
}

file_path = "C:/Users/JASKEERAT SINGH/Downloads/Medical_insurance.csv"
data = read_data(file_path)
regions <- unique(data$region)
print(cat("unique values in region is ", toString(regions)))
 
data$sex <- encodeGender(data)
data$smoker <- encodeSmoker(data)
data$region <- encodeRegion(data)

print("Eg of data")
print(data[1:10,])

charges_min <- min(data$charges)
charges_max <- max(data$charges)

data$age <- normalizeCol(data$age)
data$bmi <- normalizeCol(data$bmi)
data$charges <- normalizeCol(data$charges)

print("Eg of normalized data")
print(data[1:10,])

set.seed(42)
split <- sample.split(data$charges, SplitRatio = 0.7)
train_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)

model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train_set)

predictions <- predict(model, newdata = test_set)

pred_denorm <- denormalize(predictions, charges_min, charges_max)

mse <- mean((pred_denorm - test_set$charges)^2)
print(paste("Test MSE:", mse))