rm(list = ls())
require(ggplot2)

# PARAMETERS ####
# N = population size
# ß = per capita birth rate (Beta)
# d = per capita death rate (Delta)
# G = population density where birth rate becomes 0 (Gamma) (Not K)
# ff = female fertility 0 < ff < 1
# mf = male fertility 0 < mf < 1
# r = female remating rate 0 < r
# b = offspring number per female 0 < b
# dN = change in N
# dt = time step
# t = iterations

# FUNCTION ####
pop_fert<-function(N=10,b=50,ff=1,mf=1,r=1,K=1000,d=10,
                         dt = 0.01, t=10000)
  
{
  graph_frame <- c()
  
  #Full function within for loop
  for(i in 1:t) {
     
    #Calculation for ??
    ß<- ff*b*(r/(r+(1/mf)))
    G<- K/((ff*b*r)/(d*r*(1/mf))-1)
    
    # Main formula
    dN<- N + (ß*N*dt) -(d*N*(1+N/G)*dt)
    
    new_data<-c(i,dN) 
    
    graph_frame<-rbind(graph_frame,new_data)
    
    N <-dN
  }
  return(graph_frame)
  
  }

# FIGURE 1 High polyandry vs male and female fertility ####

# Create data frames
fert1 <- pop_fert(ff=0.3, mf=1, r=3)
fert2 <- pop_fert(ff=1, mf=0.3, r=1)
fert3 <- pop_fert(ff=1, mf=1, r=1)
fert4 <- pop_fert(ff=0.6, mf=0.6, r=3)


# Plot population size over time

pop_dyn_high_polyandry<-  ggplot()+
  scale_x_continuous("Time")+
  scale_y_continuous("Population Density")+
  geom_line(size= 1, linetype= "longdash",aes(fert1[,1],fert1[,2]),colour= '#EA1E15') +
  geom_text(aes(x = 90, y = -20, label = "IV"), colour= '#EA1E15')+
  geom_line(size= 1, linetype= "twodash", aes(fert2[,1],fert2[,2]),colour= '#76EA15')+
  geom_text(aes(x = 90, y = 600, label = "II"), colour= '#76EA15')+
  geom_line(size= 1, aes(fert3[,1],fert3[,2]), colour= '#15E1EA')+
  geom_text(aes(x = 90, y = 700, label = "I"),colour= '#15E1EA')+
  geom_line(size=1, linetype="dashed", aes(fert4[,1],fert4[,2]), colour = '#8915EA')+
  geom_text(aes(x = 90, y = 380, label = "III"), colour = '#8915EA')+
  scale_color_hue()+
  theme(panel.background = element_rect(fill = "white",colour = "gray90"),
        axis.line=element_line(colour='gray90'),
        axis.text=element_text(colour='black'),
        panel.grid.major = element_line("gray90"),
        axis.title.y = element_text(vjust=1.5),
        plot.title = element_text(face="bold"),
        plot.background = element_rect(fill = "transparent",colour = NA))

pop_dyn_high_polyandry

#Save image
#png(filename="C:/Users/bwalsh/Documents/R docs/Pop_dynamics_high_poly.png", type="cairo", units="in", width=6, height=5, pointsize=12, res=2000)
#print(pop_dyn_high_polyandry)
#dev.off() 


# FIGURE 2: Low polyandry vs male and female fertility ####

# Create data frames
fert5<- pop_fert(ff=0.3, mf=1, r=1.2)
fert6<- pop_fert(ff=1, mf=0.3, r=1.2)
fert7<- pop_fert(ff=1, mf=1, r=1.2)
fert8<- pop_fert(ff=0.6, mf=0.6, r=1.2)

# Plot population size over time
pop_dyn_low_polyandry<-  ggplot()+
  scale_x_continuous("Time")+
  scale_y_continuous("Population Density")+
  geom_line(size= 1, linetype= "longdash",aes(fert5[,1],fert5[,2]),colour= '#EA1E15') +
  geom_text(aes(x = 90, y = -20, label = "IV"), colour= '#EA1E15')+
  geom_line(size= 1, linetype= "twodash", aes(fert6[,1],fert6[,2]),colour= '#76EA15')+
  geom_text(aes(x = 90, y = 50, label = "II"), colour= '#76EA15')+
  geom_line(size= 1, aes(fert7[,1],fert7[,2]), colour= '#15E1EA')+
  geom_text(aes(x = 90, y = 700, label = "I"),colour= '#15E1EA')+
  geom_line(size=1, linetype="dashed", aes(fert8[,1],fert8[,2]), colour = '#8915EA')+
  geom_text(aes(x = 90, y = 150, label = "III"), colour = '#8915EA')+
  scale_color_hue()+
  theme(panel.background = element_rect(fill = "white",colour = "gray90"),
        axis.line=element_line(colour='gray90'),
        axis.text=element_text(colour='black'),
        panel.grid.major = element_line("gray90"),
        axis.title.y = element_text(vjust=1.5),
        plot.title = element_text(face="bold"),
        plot.background = element_rect(fill = "transparent",colour = NA),
        legend.position="none")

pop_dyn_low_polyandry

# Save image
#png(filename="C:/Users/bwalsh/Documents/R docs/Pop_dynamics_low_poly.png", type="cairo", units="in", width=6, height=5, pointsize=12, res=2000)
#print(pop_dyn_low_polyandry)
#dev.off() 



