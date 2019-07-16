rm(list = ls())
# Loading packages ####
require(fields)
require(deSolve)
require(ggplot2)
require(reshape2)
require(raster)
require(maptools)
require(scales)
require("binom")
require("dplyr")
require("lazyeval")


# PARAMETER DEFINITIONS ####
# N = population size
# ß = per capita birth rate (Beta)
# d = per capita death rate (Delta)
# G	 = population density where birth rate becomes 0 (Gamma) (Not K)
# ff = female fertility 0 < ff < 1
# mf = male fertility 0 < mf < 1
# b = offspring number per female
# dN = change in N
# r = mating rate

# FUNCTION to calculate population change ####

pop_growth <-function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
       
  #Calculation for ß
  ß<- ff*b*(r/(r+(1/mf)))
  
  # Main formula
  dN<- N +(ß*N)-(d*N*(1+N/G))
  
  list(c(dN))
})
}

# Initial parameter values
b = 50
d = 10
G	= 1000

# Mating rate of 1 ####
r = 1


start.state <- c(N=10) # vector with starting conditions
times  <- seq(0, 1000, by = 100)

# Length of mf and ff values
mf_array<- seq(0,1, length=101)
ff_array<-seq(0,1, length=101)

#Empty matrix for results
equilibria1<- matrix(data=NA, nrow = length(mf_array), ncol= length(ff_array))

# LOOP to create data for matrix
for(i in 1:length(mf_array)){
  print(i)
  for(k in 1:length(ff_array)){
    mf <- mf_array[i]
    ff <- ff_array[k]
    out <- ode(y = start.state, times = times, func = pop_growth, parms = c(mf=mf,ff=ff))
    equilibria1[i,k] <- out[which.max(times),"N"]
  }
}

image(equilibria1, col = "blue",zlim=c(10,10000))


# Mating rate of 1.5 ####
r= 1.5

#Empty matrix for results
equilibria1_5<- matrix(data=NA, nrow = length(mf_array), ncol= length(ff_array))

# LOOP to create data for matrix
for(i in 1:length(mf_array)){
  print(i)
  for(k in 1:length(ff_array)){
    mf <- mf_array[i]
    ff <- ff_array[k]
    out <- ode(y = start.state, times = times, func = pop_growth, parms = c(mf=mf,ff=ff))
    equilibria1_5[i,k] <- out[which.max(times),"N"]
  }
}

image(equilibria1_5, col = "blue",zlim=c(10,10000))


# Mating rate of 2 ####
r= 2

#Empty matrix for results
equilibria2<- matrix(data=NA, nrow = length(mf_array), ncol= length(ff_array))

# LOOP to create data for matrix
for(i in 1:length(mf_array)){
  print(i)
  for(k in 1:length(ff_array)){
    mf <- mf_array[i]
    ff <- ff_array[k]
    out <- ode(y = start.state, times = times, func = pop_growth, parms = c(mf=mf,ff=ff))
    equilibria2[i,k] <- out[which.max(times),"N"]
  }
}

image(equilibria2, col = "blue",zlim=c(10,10000))


# Mating rate of 3 ####
r= 3

#Empty matrix for results
equilibria3<- matrix(data=NA, nrow = length(mf_array), ncol= length(ff_array))

# LOOP to create data frames
for(i in 1:length(mf_array)){
  print(i)
  for(k in 1:length(ff_array)){
    mf <- mf_array[i]
    ff <- ff_array[k]
    out <- ode(y = start.state, times = times, func = pop_growth, parms = c(mf=mf,ff=ff))
    equilibria3[i,k] <- out[which.max(times),"N"]
  }
}

image(equilibria3, col = "red",zlim=c(10,10000))



# Mating rate of 5 ####
r= 5


#Empty matrix for results
equilibria5<- matrix(data=NA, nrow = length(mf_array), ncol= length(ff_array))

# LOOP to create data for matrix
for(i in 1:length(mf_array)){
  print(i)
  for(k in 1:length(ff_array)){
    mf <- mf_array[i]
    ff <- ff_array[k]
    out <- ode(y = start.state, times = times, func = pop_growth, parms = c(mf=mf,ff=ff))
    equilibria5[i,k] <- out[which.max(times),"N"]
  }
}
# Mating rate of 15 ####
r= 15


#Empty matrix for results
equilibria15<- matrix(data=NA, nrow = length(mf_array), ncol= length(ff_array))

# LOOP to create data for matrix
for(i in 1:length(mf_array)){
  print(i)
  for(k in 1:length(ff_array)){
    mf <- mf_array[i]
    ff <- ff_array[k]
    out <- ode(y = start.state, times = times, func = pop_growth, parms = c(mf=mf,ff=ff))
    equilibria15[i,k] <- out[which.max(times),"N"]
  }
}

image(equilibria15, col = "blue",zlim=c(10,10000))

# Saving datasets ####
#write.csv(equilibria1, file = "Figures/equilibria1_V2.csv")
#write.csv(equilibria1_5, file = "Figures/equilibria1_5_V2.csv")
#write.csv(equilibria2, file = "Figures/equilibria2_V2.csv")
#write.csv(equilibria3, file = "Figures/equilibria3_V2.csv")
#write.csv(equilibria5, file = "Figures/equilibria5_V2.csv")
#write.csv(equilibria15, file = "Figures/equilibria15_V2.csv")