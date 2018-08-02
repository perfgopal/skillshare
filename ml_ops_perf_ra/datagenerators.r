

#clear the environment so you can start your project
rm(list=ls())



###generate the example data set

##generate load values
##generate response time values

#generate projectdata set
#generate response times (constant)
#generate load (xxx)
#generate CPU  (linear)
#generate memory (uniform distribution)

numdatapoints <- 100
maxvalue <- 100
load.rps <- runif(numdatapoints, 0, maxvalue)
memory.util <- runif(numdatapoints, .4,.5)
noise <- rnorm(numdatapoints, 0, .40*maxvalue)

avg.response.time <- 3*load.rps + noise

#read in the csv

plot(load.rps, avg.response.time)
#metrics <- data.frame(load, response)



plot(load.rps, memory.util, ylim = c(0, max(memory.util)+.1))




########################
###generate data for the class project  
#max memory stable,
#load increases
#cpu linear with load
#response time stable
numdatapoints <- 300
maxvalue <- 100
load.rps <- runif(numdatapoints, 0, maxvalue)
memory.util <- runif(numdatapoints, 40,60)
noise <- rnorm(numdatapoints, 3, .05*maxvalue)
response.time <- rlnorm(numdatapoints, mean= log(1.2), sdlog = .2) 
plot(response.time)
project.a <- .5
project.fix <- 6
cpu.util <- project.a *load.rps + noise+ project.fix

project.metrics.gen <- data.frame(load.rps, memory.util, response.time, cpu.util)
project.metrics.gen <- round(project.metrics.gen,1)

write.csv(project.metrics.gen, "project.data.csv", row.names = FALSE)



#plot the metrics
plot(load.rps, avg.response.time)
metrics <- data.frame(load, response)



###################
###generate prediction data
###
###
#production.load <- (seq(0,200) + 80) ^2
sd <- 50
mu <- 300
ex <- seq(-200,200)
production.load <- -(ex^2)/100
production.load <- production.load - min(production.load)
#production.load <- 300000*(1/(sd*2.506627))*2.71828^((-(ex-mu)^2)/2*sd^2)
plot(production.load)

ex <- seq(0,numdatapoints-1)
production.load <- ex^2 / 1000
plot(production.load)
noise <- rnorm(numdatapoints, 3, .05*maxvalue)
#production.cpu <- project.a * production.load + noise + project.fix
production.cpu <- model.project$coefficients[2] * production.load + noise + model.project$coefficients[1] -4


 production.cpu[75] <- 45
 production.cpu[76] <- 44
 production.cpu[77] <- 46
 production.cpu[78] <- 44
# production.cpu[126] <- 89
# production.cpu[127] <- 87
# production.cpu[128] <- 85
 production.cpu <- abs(production.cpu)
points(production.cpu, col="red")
production.load <- round(production.load,2)
production.cpu <- round(production.cpu,1)

prod.time <- Sys.time() + ex*60 - 60*60*8
prod.time <- format(prod.time, "%H:%M")
prod.data <- data.frame(prod.time,production.load, production.cpu)
write.csv(prod.data,"project.proddata.csv", row.names = FALSE)