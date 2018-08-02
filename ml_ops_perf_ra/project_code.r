
############################################################
####Class Project Starts here
####

#####NOTES
###
# execute each line of code one at a time by selecting from the menu "Code->Run Selected Line(s)" or by using the keyboard shortcut - 
# Command-Enter for Mac or 
# Ctrl-Enter for Windows
###
# Each lesson builds on the previous, so if you close your R-studio session and don't save the workspace, simply re-execute the lines of code 
# for all the previous lessons
###
#To clear out the environment and delete all your variables in memory
#execute this command by copying it to the console and executing it 
# rm(list=ls())
#or
#select "Session->Clear Workspace" from the menu
#you can select R help or click on the help tab in the lower right pane to get more information on 
# any of the R functions used in the code



#####
#############################################################
#####LESSON 2 - Feature Engineering
#####
#read in the project exercise data
#from the menu select "Session->Set Working Directory->To Source File Location" OR "Choose Directory" to 
#make sure you read from the correct directory
project.metrics <- read.csv("project.data.csv")

#inspect the data to understand what metrics you have in your data set
View(project.metrics)

#start your feature engineering by looking at the relationships between the data by plotting them
#How does load and memory utilization relate? Is there a correlation?
plot(project.metrics$load.rps, project.metrics$memory.util, ylim = c(0,100))
#How does load and response time relate? Is there a correlation?
plot(project.metrics$load.rps, project.metrics$response.time, ylim = c(0,max(project.metrics$response.time)+2))
#How does load and cpu utilization relate? Is there a correlation?
plot(project.metrics$load.rps, project.metrics$cpu.util, ylim = c(0, 100))
####
####
####Which features are relevant in this data set? Which have a correlation?
####We will use those for modeling
##
############################################################
#####LESSON 3 - Exploring the math of lines
#####
#####The formula for a line is y= a+ bx
##### You can change the values of a and b below and see how they modify the lines
##### hint: if you want to draw more lines, you can change the colors too and then change line 2 or 3 values for a and b and re-execute the code
##line 1
a <- 0
b <- 3
x <- seq(0,10)
y <- a+b*x
plot(x,y, type="l", ylim = c(0, max(y)), xlim = c(0,max(x)+2))
title('Linear Plots')

##line 2
a <- 4
b <- 5
lines(x,a+b*x, col="red")

##line 3
a <- 2
b <- 1
lines(x,a+b*x, col="blue")
#####
#####
############################################################
############################################################
#####LESSON 4 - Build the Model 
#####

##uncomment (delete the "###') the line that builds the model using the correct features 
## use the feature that was correlated with the load
##select only one line from the next 3 lines to uncomment

#project.feature <- paste("cpu.util")
#project.feature <- paste("memory.util")
#project.feature <- paste("response.time")

#define the feature relationship
model.features <- paste(project.feature, "~ load.rps")

##build the model
model.project <- lm(model.features, data=project.metrics)

print(paste("The linear model is y=", round(model.project$coefficients[1],1), "+", round(model.project$coefficients[2],1), "* x"))

############################################################

#####
#####
############################################################
############################################################
#####LESSON 5 - Making Predictions with the Model , part 1
#####
##make predictions based on the data you built the model with, to sanity check your code & model
project.predictions <- predict(model.project, data.frame(load.rps = project.metrics$load.rps), interval = "prediction")
#change the data type to make it easier to work
project.predictions <- as.data.frame(project.predictions)

#plot your features
plot(project.metrics$load.rps, project.metrics[[project.feature]], col="red", ylim = c(0,100))
#now plot your model to see how well it fits
lines(project.metrics$load.rps, project.predictions$fit, col="blue")

##How well does it fit?
##
##


#####
#####
############################################################
############################################################
#####LESSON 6 - Making Predictions with the Model , part 2
#####
##### This is a sample production use case, checking actual metrics against a model to look for anomalies or issues
#####

#read in a sample of production data from your monitoring tool
#assume this data is in time sequence

prod.data <- read.csv("project.proddata.csv")

plot(prod.data$prod.time, prod.data$production.load, xlab="time", ylab="load.rps" )
title("Production Load")

prod.predictions <- predict(model.project, data.frame(load.rps = prod.data$production.load), interval = "prediction")
prod.predictions <- data.frame(prod.predictions)
####prod.interval <- predict(model.project, data.frame(load.rps = prod.data$production.load), interval = "confidence")
#This will plot the acutal production values as measured
plot(prod.data$prod.time, prod.data$production.cpu, xlab="Time", ylab="CPU Util %", col="blue")
title("Model Predictions")
#this will plot the upper limit of the model predictions
lines(prod.data$prod.time, prod.predictions$lwr, col="red")
#this will plot the lower limit of the model predictions
lines(prod.data$prod.time, prod.predictions$upr, col="red")
#this will plot the predicted values based on the model, given the actual system load 
lines(prod.data$prod.time, prod.predictions$fit, col="green")

###How did you do? were many points outside of the model predictions?
### is there any large anomaly where the actual values significanlty varied outside of the model predictions?
## Do you see any odd values in the CPU utilization? Sometimes data is suspect or corrupted, always be on the lookout for it!


