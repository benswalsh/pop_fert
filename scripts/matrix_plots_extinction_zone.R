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


# Loading data frames back in
equilibria1<- as.matrix(read.csv("data_outputs/equilibria1_V2.csv",header=FALSE))
equilibria1_5 <- as.matrix(read.csv("data_outputs/equilibria1_5_V2.csv", header = FALSE))
equilibria2<- as.matrix(read.csv("data_outputs/equilibria2_V2.csv", header = FALSE))
equilibria3<- as.matrix(read.csv("data_outputs/equilibria3_V2.csv", header = FALSE))
equilibria5<- as.matrix(read.csv("data_outputs/equilibria5_V2.csv", header = FALSE))
equilibria15<- as.matrix(read.csv("data_outputs/equilibria15_V2.csv", header = FALSE))

# Converting matricies to longform to be able to plot in geom_raster ####

equi_A<-equilibria1
equi_B<-equilibria1_5
equi_C<-equilibria2
equi_D<-equilibria3
equi_E<-equilibria5
equi_G<-equilibria15

mat_array<- seq(0,1, length= nrow(equi_A))


#For equi_A
colnames(equi_A) <-  mat_array
rownames(equi_A) <- mat_array
longDataA<-melt(equi_A)
longDataA<-longDataA[longDataA$value!=0,]

binary_DataA<-longDataA

binary_DataA$value<- ifelse(binary_DataA$value>10,1,0)

#For equi_B
colnames(equi_B) <-  mat_array
rownames(equi_B) <- mat_array

longDataB<-melt(equi_B)
longDataB<-longDataB[longDataB$value!=0,]

binary_DataB<-longDataB

binary_DataB$value<- ifelse(binary_DataB$value>10,1,0)

#For equi_C

colnames(equi_C) <-  mat_array
rownames(equi_C) <- mat_array

longDataC<-melt(equi_C)
longDataC<-longDataC[longDataC$value!=0,]

binary_DataC<-longDataC

binary_DataC$value<- ifelse(binary_DataC$value>10,1,0)

#For equi_D

colnames(equi_D) <-  mat_array
rownames(equi_D) <- mat_array

longDataD<-melt(equi_D)
longDataD<-longDataD[longDataD$value!=0,]

binary_DataD<-longDataD

binary_DataD$value<- ifelse(binary_DataD$value>10,1,0)


#For equi_E
colnames(equi_E) <-  mat_array
rownames(equi_E) <- mat_array

longDataE<-melt(equi_E)
longDataE<-longDataE[longDataE$value!=0,]

binary_DataE<-longDataE

binary_DataE$value<- ifelse(binary_DataE$value>10,1,0)


#For equi_G

colnames(equi_G) <-  mat_array
rownames(equi_G) <- mat_array

longDataG<-melt(equi_G)
longDataG<-longDataG[longDataG$value!=0,]

binary_DataG<-longDataG

binary_DataG$value<- ifelse(binary_DataG$value>10,1,0)


# PLOT 1: EXTINCTION ZONE using GGPLOT ####

library(RColorBrewer)

binary_Data_list<- list(binary_DataA,binary_DataB,binary_DataC,binary_DataD,binary_DataE, binary_DataG)
binary_Data_all<- (Reduce('+', binary_Data_list))/6


#display.brewer.all(n=5, type="seq", select=NULL, exact.n=TRUE, colorblindFriendly=TRUE)
cols<- brewer.pal(7,"RdPu")
cols[1]<- "white"

extinction_zone <- ggplot() + 
  geom_raster(data=binary_Data_all, aes(x = Var1, y = Var2, fill=factor(value))) +
  scale_fill_manual("  r", values = cols, guide = guide_legend(reverse=TRUE),labels=c("", "15", "5", "3","2", "1.5", "1"))+
  labs(x="Male fertility", y="Female fertility") +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
  scale_x_continuous(breaks = seq(0, 1, by=0.2),expand= c(0,0))+
  scale_y_continuous(breaks = seq(0, 1, by=0.2),expand= c(0,0))+
  geom_vline(xintercept=seq(0, 1, by=0.2), alpha = 0.08) +
  geom_hline(yintercept=seq(0, 1, by=0.2), alpha = 0.08) +
  theme(axis.text.x=element_text(size=10,vjust =0.1),
        axis.text.y=element_text(size=10),
        axis.title = element_text(size=12),
        legend.title= element_text(size=16,face= "italic"),
        panel.border = element_rect(colour = "black", size=1.1,linetype="solid", fill=NA),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

extinction_zone


#png(filename="figures/extinction_zone_RdPu_V3.png", type="cairo", units="in", width=6.9, height=6, pointsize=12, res=2000)
#print(extinction_zone)
#dev.off()

# PLOT 2: Growth rate vs male fertility (female fert=1) ####

start.state= 10 #What N did they start on?

male_GRA <- filter(longDataA, longDataA$Var2 == 1 )
male_GRA[,"growth_rate"]<- male_GRA$value/start.state
male_GRA[,"r"]<- 1

male_GRB <- filter(longDataB, longDataB$Var2 == 1 )
male_GRB[,"growth_rate"]<- male_GRB$value/start.state
male_GRB[,"r"]<- 1.5

male_GRC <- filter(longDataC, longDataC$Var2 == 1 )
male_GRC[,"growth_rate"]<- male_GRC$value/start.state
male_GRC[,"r"]<- 2

male_GRD <- filter(longDataD, longDataD$Var2 == 1 )
male_GRD[,"growth_rate"]<- male_GRD$value/start.state
male_GRD[,"r"]<- 3

male_GRE <- filter(longDataE, longDataE$Var2 == 1 )
male_GRE[,"growth_rate"]<- male_GRE$value/start.state
male_GRE[,"r"]<- 5

male_GR_all<- rbind(male_GRA,male_GRB,male_GRC, male_GRD, male_GRE)

plot(male_GRE$Var1, y=male_GRE$growth_rate, type='n')
lines(male_GRA$Var1, y=male_GRA$growth_rate)
lines(male_GRB$Var1, y=male_GRB$growth_rate)
lines(male_GRC$Var1, y=male_GRC$growth_rate)
lines(male_GRD$Var1, y=male_GRD$growth_rate)
lines(male_GRE$Var1, y=male_GRE$growth_rate)

#display.brewer.all(n=5, select=NULL, exact.n=TRUE, colorblindFriendly=TRUE)
cols2<-brewer.pal(9,"PuRd")

male_GR_plot<- ggplot(male_GR_all, aes(x=Var1 , y=growth_rate, colour=factor(r)))+
  scale_color_manual(values=cols2[5:9])+
  geom_line(size=2)+
  scale_x_continuous('Male fertility') +
  scale_y_continuous('Population growth rate') +
  labs(colour = "  r")+
  guides(color = guide_legend(reverse = TRUE))+
  theme(axis.text.x=element_text(size=10,vjust =0.1),
        axis.text.y=element_text(size=10),
        axis.title = element_text(size=12),
        legend.title= element_text(size=16,face= "italic"),
        panel.border = element_rect(colour = "black", fill=NA, size=1.1),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())
male_GR_plot

?scale_fill_manual

#png(filename="C:/Users/bwalsh/Documents/R docs/male_fert_GR.png", type="cairo", units="in", width=8, height=6, pointsize=12, res=2000)
#print(male_GR_plot)
#dev.off() 

# PLOT 3: Growth rate vs female fertility (male fert=1) ####
female_GRA <- filter(longDataA, longDataA$Var1 == 1 )
female_GRA[,"growth_rate"]<- female_GRA$value/start.state
female_GRA[,"r"]<- 1

female_GRB <- filter(longDataB, longDataB$Var1 == 1 )
female_GRB[,"growth_rate"]<- female_GRB$value/start.state
female_GRB[,"r"]<- 1.5

female_GRC <- filter(longDataC, longDataC$Var1 == 1 )
female_GRC[,"growth_rate"]<- female_GRC$value/start.state
female_GRC[,"r"]<- 2

female_GRD <- filter(longDataD, longDataD$Var1 == 1 )
female_GRD[,"growth_rate"]<- female_GRD$value/start.state
female_GRD[,"r"]<- 3

female_GRE <- filter(longDataE, longDataE$Var1 == 1 )
female_GRE[,"growth_rate"]<- female_GRE$value/start.state
female_GRE[,"r"]<- 5

female_GR_all<- rbind(female_GRA,female_GRB,female_GRC, female_GRD, female_GRE)


female_GR_plot<- ggplot(female_GR_all, aes(x=Var2 , y=growth_rate, colour=factor(r)))+
  geom_line(size=2)+
  scale_x_continuous('Female fertility') +
  scale_y_continuous('Population growth rate') +
  theme_bw()+
  labs(colour = "Mating rate")+
  guides(color = guide_legend(reverse = TRUE))

female_GR_plot

#png(filename="C:/Users/bwalsh/Documents/R docs/female_fert_GR.png", type="cairo", units="in", width=8, height=6, pointsize=12, res=2000)
#print(female_GR_plot)
#dev.off() 

