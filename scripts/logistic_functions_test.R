rm(list=ls) #Clear R

# FUNCTION ####
# Function to show how likely it is that a female is fertile and has mated to a fertile male
# as a function of remating rate.
# Function run several times for different values of either male or female fertility

logistic_fertility<- function(x, mf, ff){

  
  y<-ff*((x)/(x+(1/mf))) # Basic equation
  output<- c(y)          # Stores y value in vector
  return(output)         # Returns y value as output
  }


r_array<- seq(0,15, length = 10001) # array of mating values to use
mf_array<- seq(0.2,1, length = 5)   # array of fertility values to use
ff_array<- seq(0.2,1, length = 5)

# 1. Male fertility and r vs β ####

logistic_all_mf = data.frame()        # Empty data frame for results


for(i in 1:length(mf_array)){                                     # For every iteration of fertility,
  print(i)
  for(k in 1:length(r_array)){                                    # and every iteration of mating rate,
    mf<-mf_array[i]                                               # mf becomes i'th value of mf_array
    x<-r_array[k]                                                 # x becomes k'th value of r_array
   logistic_fun_mf<- data.frame(logistic_fertility(mf, x, ff=1))  # calculate function and put in data frame
   logistic_fun_mf<- cbind(logistic_fun_mf, mf, x)                # bind together y, mf, and x
   logistic_all_mf <- rbind(logistic_all_mf,logistic_fun_mf)      # bind together each iteration of data
}
}

colnames(logistic_all_mf)<- c("prop_female", "male_fert", "r")

# 2. Female fertility and r vs β ####

logistic_all_ff = data.frame()

for(i in 1:length(ff_array)){
  print(i)
  for(k in 1:length(array)){
    ff<-ff_array[i]
    x<-array[k]
    logistic_fun_ff<- data.frame(logistic_fertility(mf=1, x, ff))
    logistic_fun_ff<- cbind(logistic_fun_ff, ff, x)
    logistic_all_ff <- rbind(logistic_all_ff,logistic_fun_ff)
  }
}

colnames(logistic_all_ff)<- c("prop_female", "female_fert", "r")

# 3. PLOTS ####

library(grid)
library(ggplot2)
library(RColorBrewer)


r_fert_cols<- brewer.pal(8, "YlGnBu")

#Grob to put text onto ggplot
male_text <- "mf"
male_grob <- grid.text(male_text, x=0.9,  y=0.08, gp=gpar(col="black", fontsize=16, fontface="italic"))


r_male_fert<- ggplot(logistic_all_mf, aes(x=r, y=prop_female, colour=factor(male_fert))) +
                     geom_line(size=1)+
                     scale_color_manual(values=r_fert_cols[4:8])+
                     labs(x="r", y="Proportion of females with fertile matings") +
                     coord_cartesian(ylim = c(0,1))+
                    # guides(color = guide_legend(reverse = TRUE), fill=NA)+
                     annotation_custom(male_grob)+
                     theme(axis.text.x=element_text(size=10,vjust =0.1),
                           axis.text.y=element_text(size=10),
                           legend.position ="none",
                           legend.key=element_blank(),
                           axis.title.x =  element_text(size=16,face= "italic"),
                           axis.title.y =  element_text(size=12),
                           legend.title= element_text(size=10,face= "bold"),
                           panel.border = element_blank(),
                           axis.line = element_line(size=1.1),
                           panel.grid.major = element_line(size = 0.5, linetype = NULL,
                                        lineend = NULL, color = "lightgrey", arrow = NULL,
                                        inherit.blank = FALSE),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank())

r_male_fert

#Female grob
female_text <- "ff"
female_grob <- grid.text(female_text, x=0.9,  y=0.08, gp=gpar(col="black", fontsize=16, fontface="italic"))


r_female_fert<- ggplot(logistic_all_ff, aes(x=r, y=prop_female, colour=factor(female_fert))) +
                geom_line(size=1)+ 
                scale_color_manual("Fertility", values=r_fert_cols[4:8])+
                labs(x="r", y="Proportion of females with fertile matings") +
               # guides(color = guide_legend(reverse = TRUE), fill=NA)+
                coord_cartesian(ylim = c(0,1))+
                annotation_custom(female_grob)+
                theme(axis.text.x=element_text(size=10,vjust =0.1),
                      axis.text.y=element_text(size=10),
                      legend.position ="none",
                      legend.key=element_blank(),
                      axis.title.x =  element_text(size=16,face= "italic"),
                      axis.title.y =  element_text(size=12),
                      legend.title= element_text(size=10,face= "bold"),
                      panel.border = element_blank(),
                      axis.line = element_line(size=1.1),
                      panel.grid.major = element_line(size = 0.5, linetype = NULL,
                                         lineend = NULL, color = "lightgrey", arrow = NULL,
                                         inherit.blank = FALSE),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank())
r_female_fert

require(gridExtra)
require(cowplot)


# Extra plot for legend use only
r_fert_legend<- ggplot(logistic_all_ff, aes(x=r, y=prop_female, colour=factor(female_fert))) +
  geom_line(size=1)+ 
  scale_color_manual("", values=r_fert_cols[4:8])+
  labs(x="r", y="Proportion of females with fertile matings") +
  guides(color = guide_legend(reverse = TRUE), fill=NA)+
  coord_cartesian(ylim = c(0,1))+
  annotation_custom(female_grob)+
  theme(axis.text.x=element_text(size=10,vjust =0.1),
        axis.text.y=element_text(size=10),
        legend.key=element_blank(),
        axis.title.x =  element_text(size=16,face= "italic"),
        axis.title.y =  element_text(size=12),
        legend.title= element_text(size=10,face= "bold"),
        panel.border = element_blank(),
        axis.line = element_line(size=1.1),
        panel.grid.major = element_line(size = 0.5, linetype = NULL,
                                        lineend = NULL, color = "lightgrey", arrow = NULL,
                                        inherit.blank = FALSE),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Grab legend from legend plot
legend <- cowplot::get_legend(r_fert_legend)

# Combining all plots and legend
#png(filename="Figures/r_fert_m_f.png", type="cairo", units="in", width=12, height=4, pointsize=10, res=1500)
#plot_grid(r_male_fert, r_female_fert, legend, ncol = 3, rel_widths = c(1, 1, 1/4))
#dev.off() 