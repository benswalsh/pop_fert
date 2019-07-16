rm(list = ls())

# PARAMETERS ####
# N = population size
# ?? = per capita birth rate (Beta)
# ?? = per capita death rate (Delta)
# ??	 = population density where birth rate becomes 0 (Gamma) (Not K)
# ff = female fertility 0 < ff < 1
# mf = male fertility 0 < mf < 1
# b = offspring number per female
# dN = change in N

# FUNCTION to calculate population change ####

pop_growth <-function(mf, ff, r){

#Calculation for β
  ?? <- ff* min(b,mf*b*r)

# Main formula
dN<- N +  (?? * (N) * (1-N/??	) ) -  (??*N)

return(dN)
}

# DEFINE PARAMETERS ####
N = 10
?? = 0.5
??	 = 1000
b = 1
t = 100

#Empty vector for results
NVec <- c()

# LOOP to create data frames ####
for(i in 1:t){
  nextGen <- pop_growth(mf=1,ff=0.3, r=1) #pop_growth function only calculates change in N, so total pop size is N+ pop_growth
  N <- nextGen[1]
  NVec[i] <- N
}

# PLOT ####
plot(1:t, NVec, type = 'n')
lines(1:t, NVec)



