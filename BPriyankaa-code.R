
Title:  'ASSESSMENT 2 - MID-TERM'

# Using aggregation functions for data analysis. The provided zip file contains the datafile [ENB_2023.txt ] and the R code [AggWaFit718.R ] 
## to use with the following tasks, include these in your R working directory.

##    Energy Appliances Dataset:

#The Dataset for this assignment is a modified version of a subset of data used in Candanedo et al, 2017. 
#The experimental data have been used to create models of energy use of appliances in a low-energy house. 

##    The modified Dataset provides the energy use of Appliances (denoted as Y) using 671 samples.
##    The Dataset comprises 5 features (variables), which are denoted as X1, X2, X3, X4 and X5. 

#The details about these variables are given below:
  
'X1: Temperature in kitchen area, in Celsius
X2: Humidity in kitchen area, given as a percentage
X3: Temperature outside (from weather station), in Celsius
X4: Humidity outside (from weather station), given as a percentage
X5: Visibility (from weather station), in km
Y: Appliances, energy use, in Wh'

# For more information about the variables see Candanedo et al, 2017.## 


# Setting the working directory to import the dataset.

setwd('C:/PriyankaaB/1.1 MS DATASCIENCE/2. SIG718 Real World Analytics/MidTerm Assess')

## Get the working directory
getwd()

library("knitr")
library("rmarkdown")
library(readr)

# citation('readr')
# citation('knitr')
# citation('rmarkdown')

#     These Libraries are used to combine the R code and the written commentary into a formal document.

############################################################################################################################
                                                      'TASK 1 - Understand the dataset '
############################################################################################################################

# Assigning the 'txt' dataset as a 'matrix' to a variable.
the.data <- as.matrix(read.table("ENB_2023.txt"))

##  Viewing the dataset:
View(the.data)

'Inference': 'The dataset has 671 rows and 6 columns in total'

# Generating the subset of 340 numerical data and store it a variable "my.data"

my.data <- the.data[sample(1:671,340), c(1:6)]

## Viewing the dataset:
View(my.data)

'Inference': 
  'The dataset has 340 rows and 6 columns. This a sub-dataset from a large dataset'

## Renaming the columns for understanding

colnames(my.data) <- c("X1", "X2", "X3", "X4", "X5", "Y")

# Converting the matrix into a dataframe:
my.data.df <- data.frame(my.data)



               'Creating 5 Scatter plots for the variables X1, X2, X3, X4, X5 against Y'


# Generally Scatter Plots are used to display relationship between the 2 numerical data:

 "X1 VS Y"
plot(my.data.df$X1, my.data.df$Y, xlab= 'X1: Kitchen Temperature in Celsius', ylab= 'Appliances Energy Comsumption (Wh)', main= 'Kitchen Temperature (X1) Vs Appliance Energy Consumption (Y)', col = 'green')

# ******Inference Made:**************
# Energy consumption by the appliance is high when Kitchen Tempertaure is high, here Wh is higher between 17  to 21 degree Celsius 


"X2 VS Y"
plot(my.data.df$X2, my.data.df$Y, xlab= 'X2: Humidity in Kitchen (%)', ylab= 'Appliances Energy Comsumption (Wh)', main= 'Humidity in Kitchen (%) (X2) Vs Appliance Energy Consumption (Y)', col = 'blue')

# *********Inference Made:*********** 
# Energy consumption by the appliance is high when Humidity in the Kitchen ranges between 32 - 43%. 


"X3 VS Y"
plot(my.data.df$X3, my.data.df$Y, xlab= 'X3: Weather Station Temperature in Celsius', ylab= 'Appliances Energy Comsumption (Wh)', main= 'Weather Station Temperature (X3) Vs Appliance Energy Consumption (Y)', col = 'pink')

# *********Inference: ***************
# Energy consumption ranges from 20 - 110 Wh when the Tempertaure of the Weather Station ranges from 1 - 4 degree Celsius.


"X4 VS Y"
plot(my.data.df$X4, my.data.df$Y, xlab= 'X4: Weather Station Humidity (%)', ylab= 'Appliances Energy Comsumption (Wh)', main= 'Weather Station Humidity (%) (X4) Vs Appliance Energy Consumption (Y)', col = 'red')

# *********Inference:****************
# Appliance's Energy Consumption ranges from 30 - 100 Wh for the Weather Station's Humidity ranging between 73 - 91 %


"X5 VS Y"
plot(my.data.df$X5, my.data.df$Y, xlab= 'X5: Visibility from Weather Station (km)', ylab= 'Appliances Energy Comsumption (Wh)', main= 'Visibility from Weather Station (km) (X5) Vs Appliance Energy Consumption (Y)', col = 'yellow')

# *********Inference:****************
# Most of the datapoints has Energy Consumption ranging between 20 - 100 Wh and Visibility ranging between 25 - 40 km.



# ************************************* DATA EXPLORATION ****************************** #

# Checking the correlation and the skewness of the data in order to understand the distribution od the dataset:

round(cor(my.data.df),digits = 2)

'X1 Variable Vs Y'
# Value is 0.32 which means it is moderate degree positive correlation and the data is normally distributed.

'X2 Variable Vs Y'
# Value is 0.14 which means it is low degree positive correlation and the data is positively skewed.

'X3 Variable Vs Y'
# Value is 0.49 which means it is high degree positive correlation and the data is normally distributed.

'X4 Variable Vs Y'
# Value is -0.01 which means it is non-linear correlation and the data is negatively skewed.

'X5 Variable Vs Y'
# Value is 0.31 which means it is low degree positive correlation and the data is positively skewed.                

# ******************************************************************************************** #

                           'Creating 6 Histogram plots for the variables X1, X2, X3, X4, X5, Y'

# Generally Histogram denotes the frequency of the value.

'Histogram of X1'

hist(my.data.df$X1, xlab='X1: Kitchen Temperature in Celsius', main= 'Kitchen Temperature Distribution in Celsius')

# Inference: The highest frequency is reached for the Kitchen Temperature ranging from 16 - 18 degree Celsius.

'Histogram of X2'

hist(my.data.df$X2, xlab='X2: Humidity in Kitchen (%)', main= 'Distribution of the Kitchen Humidity')

# Inference: The highest frequency is reached for the Kitchen Humidity ranges from 32 - 43%.

'Histogram of X3'

hist(my.data.df$X3, xlab='X3: Weather Station Temperature in Celsius', main= 'Distribution of the Temperature of Weather Station (Celsius)')

# Inference: The highest frequency of the Temperature of the Weather Stations is 3 - 4 Celsius.

'Histogram of X4'

hist(my.data.df$X4, xlab='X4: Weather Station Humidity (%)', main= 'Distribution of the Humidity of Weather Station (%)')

# Inference: The highest frequency for the Weather Station Humidity ranges from 75 - 90%.

'Histogram of X5'

hist(my.data.df$X5, xlab='X5: Visibility from Weather Station (km)', main= 'Distribution of the Visibility from Weather Station (km)')

# Inference: The highest frequency for the Distance from Weather Station ranges between 35 - 40 km.

'Histogram of Y'

hist(my.data.df$Y, xlab='Y: Appliance Energy Consumption (Wh)', main= 'Distribution of the Appliance Energy Consumption (Wh)')

# Inference: From the histogram, it seems that most appliances consumes less energy ranging between 20 - 70 Wh. 
##    There are very few appliances consuming higher energy of above 120 Wh.



##########################################################################################################################
                                                          'TASK 2 - Transform the Data'
##########################################################################################################################



# Each column may represent certain things that are measured differently through different scales. 
# In order to bring uniformity, these variables are scaled to 0 - 1 scale so that it can compared easily.

## *** Out of 5 variables, 4 variables are chosen that is except X4, remaining variables are chosen.*** ##

"X1 Variable is transformed using linear feature scaling"

min.X1 <- min(my.data.df$X1)
min.X1
# Minimum value of X1 is 15
max.X1 <- max(my.data.df$X1)
max.X1
# Maximum value of X1 is 23.33

# Transforming the X1 variable
transformed.X1 <- (my.data.df$X1 - min.X1)/ (max.X1 - min.X1)


"X2 Variable is tarnsformed using polynomial and linear feature scaling"

p.X2 = (my.data.df$X2)^(1/100) # first the X2 column from the data-set is scaled suing polynomial transformation
min.X2 <- min(p.X2)
min.X2
# Minimum value of X2 is 1.0345
max.X2 <- max(p.X2)
max.X2
# Maximum value of X2 is 1.0406

# Transforming the X2 variable using linear feature scaling
transformed.X2 <- (p.X2 - min.X2)/ (max.X2 - min.X2)


"X3 Variable is transformed using linear feature scaling"

min.X3 <- min(my.data.df$X3)
min.X3
# Minimum value of X3 is 0
max.X3 <- max(my.data.df$X3)
max.X3
# Maximum value of X3 is 7.2044

# Transforming the X3 variable
transformed.X3 <- (my.data.df$X3 - min.X3)/ (max.X3 - min.X3)



"X5 Variable is transformed using polynomial and linear feature scaling"

p.X5 = (my.data.df$X5)^(1/100) # first the X2 column from the data-set is scaled suing polynomial transformation
min.X5 <- min(p.X5)
min.X5
# Minimum value of X2 is 1.0267
max.X5 <- max(p.X5)
max.X5
# Maximum value of X2 is 1.0426

# Transforming the X2 variable using linear feature scaling
transformed.X5 <- (p.X5 - min.X5)/ (max.X5 - min.X5)


"Y Variable is transformed using log and linear feature scaling"
log.Y <- log10(my.data.df$Y)

min.Y <- min(log.Y)
min.Y
# Minimum value of X4 is 1.3010
max.Y <- max(log.Y)
max.Y
# Maximum value of X4 is 2.2787


# Transforming the Y variable using log and linear feature scaling
transformed.Y <- (log.Y - min.Y)/ (max.Y - min.Y)

"These transformations mostly done on the extremely skewed dataset, so that it becomes normally distributed"

column.names <- c("Y", "X1", "X2", "X3", "X5")
your.data <- array(c(transformed.Y, transformed.X1, transformed.X2, transformed.X3, transformed.X5), dim = c(340,5), dimnames= list(NULL, column.names))
View(your.data)

# *********************************DATA EXPLORATION AFTER TRANSFORMATION ****************************** #
round(cor(your.data),digits = 2)

'X1 Variable Vs Y'
# Value is 0.36 which means it is moderate degree positive correlation and the data is normally distributed.

'X2 Variable Vs Y'
# Value is 0.11 which means it is non-linear correlation and the data is positively skewed.

'X3 Variable Vs Y'
# Value is 0.46 which means it is high degree positive correlation and the data is normally distributed.

'X5 Variable Vs Y'
# Value is 0.31 which means it is low degree positive correlation and the data is positively skewed.


# Converting the above array dataset "new.data" into a text file.

write.table(your.data, "BPriyankaa_Transformed.txt")



########################################################################################################################
                              'TASK 3 - Build models and investigate the importance of each variable'
########################################################################################################################

# Loading AggWaFit R file onto the R space
source("AggWaFit718.R")

# install.packages("lpSolve")
library(lpSolve)
# The above package is used to solve linear programming problems and get statistical analysis.

t3.data <- as.matrix(read.table("BPriyankaa_Transformed.txt"))

#TASK 3 - (ii) Use the fitting functions to learn the parameters for
##    a. A weighted arithmetic mean (WAM)

# Parameters for WAM 
## Results generated in "Stat_AM" text-file

fit.QAM(t3.data[,c(1:5)], "out_AM.txt", "stat_AM.txt",g= AM, g.inv= invAM)

##    b. Weighted power means (WPM) with p = 0.5

# Parameters for WPM, p=0.5
## Results generated in "stat_PM05" textfile

fit.QAM(t3.data[,c(1:5)], "out_PM05.txt", "stat_PM05.txt", g= PM05, g.inv= invPM05)

##    c. Weighted power means (WPM) with p = 2,

# Parameters for WPM, p=2
## Results generated in "stat_PM" textfile
fit.QAM(t3.data[,c(1:5)], "out_QM.txt", "stat_QM.txt", g= QM, g.inv= invQM)


##    d. An ordered weighted averaging function (OWA)

# Parameters for OWA
## Results generated in "stat_OWA" textfile

fit.OWA(t3.data[,c(1:5)], "out_OWA.txt", "stat_OWA.txt")


##    e. The Choquet integral

# Parameters for Choquet
## Results generated in "stat_CQ" textfile

fit.choquet(t3.data[,c(1:5)], "out_CQ.txt", "stat_CQ.txt")



#########################################################################################################################
                                                      'TASK 4 - Using Models for Prediction'
#########################################################################################################################

# T4. Use your model for prediction.
## ************  Using your best fitting model from T3, predict Y (the area) for the following input *************

"X1=22; X2=38; X3=4; X4=88.2, X5=34"
# The min and max values are taken from Task 2 and X1 in the min-max formula is replaced by 22 as already given.

"X1 Variable is transformed using linear feature scaling"

min.X1 <- 15
# Minimum value of X1 is 15
max.X1 <- 23.33
# Maximum value of X1 is 23.33

# Transforming the X1 variable
transformed4.X1 <- (22 - min.X1)/ (max.X1 - min.X1)
transformed4.X1

"X2 Variable is tarnsformed using polynomial and linear feature scaling"

p4.X2 = (38)^(1/100) # first the X2 column from the data-set is scaled suing polynomial transformation

min.X2 <- 1.0345
# Minimum value of X2 is 1.0345

max.X2 <- 1.0406
# Maximum value of X2 is 1.0406

# Transforming the X2 variable using linear feature scaling
transformed4.X2 <- (p4.X2 - min.X2)/ (max.X2 - min.X2)
transformed4.X2

"X3 Variable is transformed using linear feature scaling"

min.X3 <- 0

max.X3 <- 7.2044

# Transforming the X3 variable
transformed4.X3 <- (4 - min.X3)/ (max.X3 - min.X3)
transformed4.X3

"X5 Variable is transformed using polynomial and linear feature scaling"

p4.X5 = (34)^(1/100) # first the X2 column from the data-set is scaled suing polynomial transformation
min.X5 <- 1.0267

max.X5 <-  1.0426

# Transforming the X2 variable using linear feature scaling
transformed4.X5 <- (p4.X5 - min.X5)/ (max.X5 - min.X5)
transformed4.X5

# /////////////// CONVERTING TRANSFORMED VALUES INTO A VECTOR ////////////// #
transformed4.values.prediction <- c(transformed4.X1, transformed4.X2, transformed4.X3, transformed4.X5)

' APPLYING THE BEST FITTING MODEL, HERE IT IS WAM'

WAM.weights <- c(0.4011, 0.2419, 0.1466, 0.2102)
Y.predicted.transformed.value <- QAM(transformed4.values.prediction, WAM.weights)

Y.predicted.transformed.value

# TO PREDICT THE VALUE OF Y

## Reversing the transformations:

min.Y <- 1.3010
# Minimum value of X4 is 1.3010
max.Y <- 2.2787
# Maximum value of X4 is 2.2787

reverse.lin <- (Y.predicted.transformed.value * (max.Y - min.Y)) + min.Y

reverse.log <- 10^reverse.lin

final.predicted.y <- reverse.log
final.predicted.y

# 84.65176
' APPLYING THE BEST FITTING MODEL, HERE IT IS WPM, P=0.5'
WPM05.weights <- c(0.3882, 0.2132, 0.1813, 0.2171)
Y.predicted.transformed.value1 <- QAM(transformed4.values.prediction, WPM05.weights)

Y.predicted.transformed.value1

# TO PREDICT THE VALUE OF Y

## Reversing the transformations:

min.Y <- 1.3010
# Minimum value of X4 is 1.3010
max.Y <- 2.2787
# Maximum value of X4 is 2.2787

reverse.lin1 <- (Y.predicted.transformed.value1 * (max.Y - min.Y)) + min.Y

reverse.log1 <- 10^reverse.lin1

final.predicted.y1 <- reverse.log1
final.predicted.y1

# 84.73534

'APPLYING THE BEST FITTING MODEL, HERE IT IS WPM, P=2'

WPM.weights <- c(0.2696, 0.2705, 0.1803, 0.2794)
Y.predicted.transformed.value2 <- QAM(transformed4.values.prediction, WPM.weights)

Y.predicted.transformed.value2

# TO PREDICT THE VALUE OF Y

## Reversing the transformations:

min.Y <- 1.3010
# Minimum value of X4 is 1.3010
max.Y <- 2.2787
# Maximum value of X4 is 2.2787

reverse.lin2 <- (Y.predicted.transformed.value2 * (max.Y - min.Y)) + min.Y

reverse.log2 <- 10^reverse.lin2

final.predicted.y2 <- reverse.log2
final.predicted.y2

# 77.38821

'APPLYING THE BEST FITTING MODEL, HERE IT IS OWA'

OWA.weights <- c(0.0273, 0.2074,0.1886, 0.5762)
Y.predicted.transformed.value3 <- QAM(transformed4.values.prediction, OWA.weights)

Y.predicted.transformed.value3

# TO PREDICT THE VALUE OF Y

## Reversing the transformations:

min.Y <- 1.3010
# Minimum value of X4 is 1.3010
max.Y <- 2.2787
# Maximum value of X4 is 2.2787

reverse.lin3 <- (Y.predicted.transformed.value3 * (max.Y - min.Y)) + min.Y

reverse.log3 <- 10^reverse.lin3

final.predicted.y3 <- reverse.log3
final.predicted.y3

# 68.56996

'APPLYING THE BEST FITTING MODEL, HERE IT IS Choquet Integral'

CI.weights <- c(0.2106, 0.2142, 0.1746, 0.4004)
Y.predicted.transformed.value4 <- QAM(transformed4.values.prediction, CI.weights)

Y.predicted.transformed.value4

# TO PREDICT THE VALUE OF Y

## Reversing the transformations:

min.Y <- 1.3010
# Minimum value of X4 is 1.3010
max.Y <- 2.2787
# Maximum value of X4 is 2.2787

reverse.lin4 <- (Y.predicted.transformed.value4 * (max.Y - min.Y)) + min.Y

reverse.log4 <- 10^reverse.lin4

final.predicted.y4 <- reverse.log4
final.predicted.y4

# 76.30156

#########################################################################################################################
                    'TASK 5 - Summarizing the data analysis in up to 20 slides for a 5-minutes presentation'
#########################################################################################################################
 
# A presentation .pptx file is prepared in the name of BPriyankaa-slides. Kindly refer that.



