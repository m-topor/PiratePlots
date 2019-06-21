# Use this page for further options and instructions:
# https://cran.r-project.org/web/packages/yarrr/vignettes/pirateplot.html

# The following code will check if your R has the package 'YaRrr' installed. 
# If not, it will be installed.

if (!require(yarrr)) install.packages('yarrr')
library(yarrr)

# Make sure you set your working directory to the same file where your data is.
setwd("~/Uni/PhD/Year 1/Training/NDAS workshop")

# Read in the data that you want to use for your pirate plot. 
# In this case the datafile is called NDAS.csv

df <- read.csv("NDAS.csv")

# Specify the names of your groups for R
# Here we will check what makes people eat more cookies, if they had no dinner, 
# if they had dinner, if they are stressed or a comparison group sampled at random

df$Group[df$Group == "1"] <- "NoDinner"
df$Group[df$Group == "2"] <- "Dinner"
df$Group[df$Group == "3"] <- "Comparison"
df$Group[df$Group == "4"] <- "Stressed"

# Tell R to use the newly created objects as factors for the plots
as.factor(df$Group)

#PART I

# Here is the code for a pirate plot, pay attention to the first argument stating the variables.
# After that, you can change and specify all different types of features, look at the instructions
# in the link above to explore lots more options, or type in ?pirateplot and press "Run". 
?pirateplot

# Remember that each new instruction starts in a new indented line and has to end with a comma.
# The last command has to have a closing bracket.

par(mfrow=c(1,1)) # this is not necessary, just specifies that there will be 1 graph in the picture
pirateplot(formula = Cookies~Group, #which variables are you using
           data = df,
           theme = 0, #check out different themes in the guide page linked at the top, theme 0 is starting from scratch
           main = "Cookies eaten in each Group", #title of the graph
           par(cex.main = 1.2), #set the size of your title
           xlab="Experimental Group", #label of the x axis
           ylab="Number of cookies", #label of the y axis
           cex.lab = 1.2, #size of the axis labels
           pal = "basel", # color palette, look at the instruction link to identify different palettes
           inf.method = 'hdi', # method of inference - either High Density Interval (hdi, from Bayesian stats), or standard Confidence Interval (ci)
           bean.f.o = 0.5, # Bean fill darkness
           point.o = .3, # points darkness
           inf.f.o = .5, # Inference fill
           inf.b.o = .5, # Inference border
           avg.line.o = 0.7, # Average line darkness
           inf.f.col = "white", # Inf fill col
           inf.b.col = "black", # Inf border col
           avg.line.col = "black", # avg line col
           point.pch = 20, #point shape
           point.col = "black", #point colour
           point.cex = 1, #point size
           gl.col = "#FFCCFF", #gridlines and colour
           yaxt ="n", # remove the automatic scale on the axis Y
           quant = c(.05, .95) #set the quantile, here within 5% on each side to indicate outliers
           )
#here are some additional commands that add onto the plot
axis(2, at = seq(0, 10, by = 1)) #set the numbers on the y axis
par(mar = rep(4, 4)) #set the margin size, i.e. where the the graph fits within the picture 


# PART II

# We are now going to make 6 different plots and see whether there are significant differences between the groups

# Make different comparison pairs for R so that it can run t-tests

# Pair 1
df$PairA[df$Group == "Dinner"] <- "Dinner"
df$PairA[df$Group == "NoDinner"] <- "NoDinner"

myt1 <- t.test(df$Cookies ~ df$PairA, is.na = TRUE)

# Pair 2
df$PairB[df$Group == "Dinner"] <- "Dinner"
df$PairB[df$Group == "Comparison"] <- "Comparison"

myt2 <- t.test(df$Cookies ~ df$PairB, is.na = TRUE)

# Pair 3
df$PairC[df$Group == "NoDinner"] <- "NoDinner"
df$PairC[df$Group == "Comparison"] <- "Comparison"

myt3 <- t.test(df$Cookies ~ df$PairC, is.na = TRUE)

# Pair 4
df$PairD[df$Group == "Dinner"] <- "Dinner"
df$PairD[df$Group == "Stressed"] <- "Stressed"

myt4 <- t.test(df$Cookies ~ df$PairD, is.na = TRUE)

# Pair 5
df$PairE[df$Group == "NoDinner"] <- "NoDinner"
df$PairE[df$Group == "Stressed"] <- "Stressed"

myt5 <- t.test(df$Cookies ~ df$PairE, is.na = TRUE)

# Pair 6
df$PairF[df$Group == "Comparison"] <- "Compairson"
df$PairF[df$Group == "Stressed"] <- "Stressed"

myt6 <- t.test(df$Cookies ~ df$PairC, is.na = TRUE)


# Make 6 quick pirate plots and add information regarding the t-test statistics 

par(mfrow=c(2,3)) #the following picture will contain 6 graphs in two rows
# the header of each graph will be the t-test result
myheader=paste0('t = ', format(myt1$statistic,digits=2),'; p = ',format(myt1$p.value,digits=3))
# each graph will be marked with stars if the difference between the groups is significant
mystars<-''
if(myt1$p.value<.05) {mystars<-'*'}
if(myt1$p.value<.01) {mystars<-'**'}
if(myt1$p.value<.001) {mystars<-'***'}
# this is a much shorter version for making a less pretty pirate plot
pirateplot(Cookies~PairA,theme=1,cex.lab=1,data=df,main = myheader, point.o=1, xlab="Experimental Group", ylab="Number of cookies", bean.f.col = "blue")
par(col.main ="black", cex.main = 1) # size and colour of the heading which is the t-test stats
text(1.5,10,mystars,col='red',cex=3) # size and colour of the significance stars

#now the few lines above will be repeated for each pair of groups and added to the overall picture

myheader=paste0('t = ', format(myt2$statistic,digits=2),'; p = ',format(myt2$p.value,digits=3))
mystars<-''
if(myt2$p.value<.05) {mystars<-'*'}
if(myt2$p.value<.01) {mystars<-'**'}
if(myt2$p.value<.001) {mystars<-'***'}
pirateplot(Cookies~PairB,theme=1,cex.lab=1,data=df,main = myheader, point.o=1, xlab="Experimental Group", ylab="Number of cookies", bean.f.col = "green")
par(col.main ="black", cex.main = 1)
text(1.5,9,mystars,col='red',cex=3)

myheader=paste0('t = ', format(myt3$statistic,digits=2),'; p = ',format(myt3$p.value,digits=3))
mystars<-''
if(myt3$p.value<.05) {mystars<-'*'}
if(myt3$p.value<.01) {mystars<-'**'}
if(myt3$p.value<.001) {mystars<-'***'}
pirateplot(Cookies~PairC,theme=1,cex.lab=1,data=df,main = myheader, point.o=1, xlab="Experimental Group", ylab="Number of cookies", bean.f.col = "red")
par(col.main ="black", cex.main = 1)
text(1.5,10,mystars,col='red',cex=3)

myheader=paste0('t = ', format(myt4$statistic,digits=2),'; p = ',format(myt4$p.value,digits=3))
mystars<-''
if(myt4$p.value<.05) {mystars<-'*'}
if(myt4$p.value<.01) {mystars<-'**'}
if(myt4$p.value<.001) {mystars<-'***'}
pirateplot(Cookies~PairD,theme=1,cex.lab=1,data=df, main=myheader, point.o=1, xlab="Experimental Group", ylab="Number of cookies", bean.f.col = "yellow")
par(col.main ="black", cex.main = 1)
text(1.5,9,mystars,col='red',cex=3)

myheader=paste0('t = ', format(myt5$statistic,digits=2),'; p = ',format(myt5$p.value,digits=3))
mystars<-''
if(myt5$p.value<.05) {mystars<-'*'}
if(myt5$p.value<.01) {mystars<-'**'}
if(myt5$p.value<.001) {mystars<-'***'}
pirateplot(Cookies~PairE,theme=1,cex.lab=1,data=df,main = myheader, point.o=1, xlab="Experimental Group", ylab="Number of cookies", bean.f.col = "brown")
par(col.main ="black", cex.main = 1)
text(1.5,9,mystars,col='red',cex=3)

myheader=paste0('t = ', format(myt6$statistic,digits=2),'; p = ',format(myt6$p.value,digits=3))
mystars<-''
if(myt6$p.value<.05) {mystars<-'*'}
if(myt6$p.value<.01) {mystars<-'**'}
if(myt6$p.value<.001) {mystars<-'***'}
pirateplot(Cookies~PairF,theme=1,cex.lab=1,data=df,main = myheader, point.o=1, xlab="Experimental Group", ylab="Number of cookies", bean.f.col = "grey")
par(col.main ="black", cex.main = 1)
text(1.5,9,mystars,col='red',cex=3)




