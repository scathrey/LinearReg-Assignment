#Geely Auto - Linear Regression Assignment

#Required Mandatory Packages (Mass and Car for StepAIC and VIF respectively)s
library(MASS)
library(car)

#Data Import - Data Understanding
carPrice <- read.csv("CarPrice_Assignment.csv")
View(carPrice)
str(carPrice)

#Data Preparation
#"CarName" variable - extract and consider only "Car" as independent variable
library(tidyr)
carData <- separate(carPrice, CarName,into = c("CarName", "Type"),sep = " ")
View(carData)

#Separated "CarName" spell correction
library(plyr)
levels(as.factor(carData$CarName))
carMake <- mapvalues(carData$CarName, from = c("maxda", "porcshce", "vokswagen"), to = c("mazda","porsche","volkswagen"))
carMake <- mapvalues(carData$CarName, from = c("vw", "Nissan", "toyouta"), to = c("volkswagen", "nissan", "toyota"))

carData <- cbind(carData[,-3],carMake)
carData$carMake <- as.factor(carData$carMake)
View(carData$carMake)


#Creating dummy variables
carbody_dummy <- data.frame(model.matrix( ~carbody, data = carData))
drivewheel_dummy <- data.frame(model.matrix( ~drivewheel, data = carData))
cylnum_dummy <- data.frame(model.matrix( ~cylindernumber, data = carData))
fuelsys_dummy <- data.frame(model.matrix( ~fuelsystem, data = carData))
comp_dummy <- data.frame(model.matrix( ~carMake, data = carData))
enginetype_dummy <- data.frame(model.matrix( ~enginetype, data = carData))

carbody_dummy <- carbody_dummy[,-1]
drivewheel_dummy <- drivewheel_dummy[,-1]
cylnum_dummy <- cylnum_dummy[,-1]
fuelsys_dummy <- fuelsys_dummy[,-1]
comp_dummy <- comp_dummy[,-1]
enginetype_dummy <- enginetype_dummy[,-1]

carPrices <- cbind(carData[,setdiff(names(carData),
                                           c("carbody","drivewheel",
                                             "cylindernumber","fuelsystem","company","enginetype"))], 
                   carbody_dummy, drivewheel_dummy, cylnum_dummy, fuelsys_dummy, comp_dummy, enginetype_dummy)


#Data conversion to numeric data type (to 0 & 1)

# diesel, gas - 0,1
levels(carPrice$fueltype) <- c(0,1) 
carPrice$fueltype <- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

# std, turbo - 0,1
levels(carPrice$aspiration) <- c(0,1) 
carPrice$aspiration <- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]

# four, two - 0,1
levels(carPrice$doornumber) <- c(0,1)
carPrice$doornumber <- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]

# front, rear - 0,1
levels(carPrice$enginelocation) <- c(0,1) 
carPrice$enginelocation <- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]

#Deriving variables
#1. Overall mpg
carPrice$Ompg <- round(mean(carPrice$citympg + carPrice$highwaympg),2)

#2. Stroke2Bore Ratio
carPrice$sbr <- round(carPrice$stroke/carPrice$boreratio,2)

#4. Overall mpg to Horsepower ratio
carPrice$Ohp <- round(carPrice$Ompg/carPrice$horsepower, 2)

#5. Overall mpg to curbweight ratio (FE)
carPrice$FE <- round(carPrice$Ompg/carPrice$curbweight, 4)

View(carPrice)


set.seed(9999)
#Setting up training and testing datasets
tind= sample(1:nrow(carPrice), 0.7*nrow(carPrice))
train = carPrice[tind,]
test = carPrice[-tind,]

#linear regression model
geelyModel <-lm(price~.,data=train)
summary(geelyModel)

#stepAIC Geely Model
geelyModel_step <- lm(price ~ car_ID + fueltype + aspiration + doornumber + enginelocation + 
                wheelbase + carlength + carwidth + carheight + curbweight + 
                enginesize + sbr + Ohp, data = train)
summary(geelyModel_step)
vif(geelyModel_step)

#Performing prediction
geelyPredict <- predict(geelyModel_step,test[,-20])
test$test_price <- geelyPredict

#Actual Vs Predicted Geely Car sales test
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2


#Adjusted R-Square value (Greater than 80%)
rsquared*100


