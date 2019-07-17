


logistic_solv<- function(r, mf){
  
  
  ff<- (d*(r+1/mf))/(b*r) # Basic equation
  output<- c(ff)          # Stores y value in vector
  return(output)         # Returns y value as output
}

b = 50
d = 10


mf_array <- seq(0.0001,1, length=10000)
r_array <- c(1, 1.5, 2, 3, 5, 15)


logistic_all = data.frame() 


for(i in 1:length(mf_array)){                                     # For every iteration of fertility,
  print(i)
  for(k in 1:length(r_array)){                                    # and every iteration of mating rate,
    mf<-mf_array[i]                                               # mf becomes i'th value of mf_array
    r<-r_array[k]                                                 # x becomes k'th value of r_array
    logistic_fun<- data.frame(logistic_solv(mf, r))  # calculate function and put in data frame
    logistic_fun <- cbind(logistic_fun, mf, r)                # bind together y, mf, and x
    logistic_all <- rbind(logistic_all,logistic_fun)      # bind together each iteration of data
  }
}

library(RColorBrewer)
cols<- brewer.pal(7,"RdPu")

colnames(logistic_all)<- c("ff", "mf", "r")
solv_extinction <- ggplot(logistic_all, aes( x= mf, y=ff, colour=factor(r)))+
                          geom_line(size=1) +
                          scale_color_manual("  r", values=cols[7:2])+
                          coord_cartesian(xlim = c(0,1), ylim = c(0,1))+
                          scale_x_continuous(breaks = seq(0, 1, by=0.2),expand= c(0,0))+
                          scale_y_continuous(breaks = seq(0, 1, by=0.2),expand= c(0,0))+
                          labs(x="Male fertility", y="Female fertility") +
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

png(filename="figures/stability_extinction.png", type="cairo", units="in", width=6.9, height=6, pointsize=12, res=2000)
print(solv_extinction)
dev.off()
