getwd()
library(plyr)
library (graphics)
library (scales)
library (car)
library(ggplot2)

#need to change working directory depending on where files are
#files need to be csv with columns labelled time, media and LOGCFU
#list.dirs("C:/Users/Corrie/Dropbox/Grad School/Hancock Lab/Experiments//TKC/", recursive = FALSE) [1]
setwd(list.dirs("C:/Users/Corrie/Documents/data/TKC/", recursive = FALSE) [1])

#where R must look for the files
#this does not work. The loop only works if current directory is correct
#need both the path and the current directory set?
path = list.dirs("C:/Users/Corrie/Documents/data/TKC/", recursive = FALSE) [1]
print (path)

#empty data object,don'tknow why
out.file<-""
MHB <-""
MHB.AZM <-""
RPMI <-""
RPMI.AZM <-""
meanCFU <-""

#standard error calculation for error bars
se <- function(y) (sd(y)/sqrt(length(y)))

#read the path to the files and a loop for reading each existing file of type ".csv" as table
#count the files in the path and make a list of the names to rename each output later
file.names <- dir(path, pattern=".csv")
lst <- vector("list", length(file.names))
names(lst)<- file.names
#print(names(lst))

#colours and shapes for plotting
myPallete <- c("#000000","#000000", "#808080", "#808080", "#C1B8B8", "#C1B8B8")
myshapes <- c(15,17,15,17,15,17)

#loop to open each csv file in the current directory as a table, 
#calculate and append mean and standarderror and make a time course plot
for (i in 1:length(file.names)){
  file<- read.table(file.names[i], header=TRUE, sep=",")
  
  meanLOGCFU <- ddply(file, .(media, time), summarize, mean_CFU = mean(LOGCFU), stderr = se(LOGCFU))
  
  mypath <- file.path("C:", "Users", "Corrie", "Documents", "data", "TKC", paste("plot_", names(lst)[i], ".jpeg", sep=""))
  
  ggplot(data = meanLOGCFU, aes(x = time, y = mean_CFU, group = media, shape = media, colour = media)) +
    geom_line(size = 4) + geom_point(size = 9) +
    xlab("time(h)") + ylab("Log CFU/mL") +
    geom_errorbar(data = meanLOGCFU, mapping = aes(ymax = mean_CFU + stderr, ymin = mean_CFU - stderr),
                  width = 0.75, size=0.75, colour = "black") +
    theme(panel.background = element_blank(), legend.text= element_text(size = 30),
          legend.key = element_blank(), legend.title = element_blank(),
          axis.line.x = element_line(), axis.line.y = element_line(),
          axis.text = element_text(size = 30), axis.title = element_text(size = 30),
          legend.key = element_rect(size=5, colour = NA), legend.key.size = unit(3.5, "lines")) +
    scale_colour_manual(values=myPallete) +
    scale_shape_manual(values=myshapes) +
    
    #geom_segment(data = vertlines,aes( x = x, xend = xend, y = y, yend = yend))
    ggsave(file = mypath, device = "jpg", width = 16, height = 9, dpi = 120)
}

#Anova using car package
#makes a linear model and calculates analysis of variance
for (i in 1:length(file.names)){
  file<- read.table(file.names[i], header=TRUE, sep=",")
  m3 <- lm(LOGCFU~media,data=file)
  mypath2 <- file.path("C:", "Users", "Corrie", "Documents", "data", "TKC", "TKCstats", paste("ANOVA_", names(lst)[i], ".txt", sep=","))
  textfile = Anova(m3)
  capture.output(textfile, file = mypath2)
}