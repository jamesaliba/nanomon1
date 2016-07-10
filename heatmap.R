
#this is a heatmap code that takes the volume of the tumor and the max temperature to kill the tumor and not the cells (tumor dies at 42 cells die at 47)
#so it takes volume and max temp and the amount of nanoparticles and can simulate how the heat will dissipate
#the numbers are general and not actual data but we can easily get the info online, this is more of a proof of concept
library(heatmap.plus)
x<-c(1:200)
y<-c(1:200)
tmax<-100
decay<-0.25

distance.matrix<-matrix(NA, nrow=200, ncol=200)
distance.matrix[,]<-0

gold<-list(c(50,0),c(20,20),c(190,190),c(190,20),c(20,190),c(190,190))#y,x

for (k in 1:(length(gold))){

for (i in 1:200) {for (j in 1:200){
  distance.matrix[i,j]<-((gold[[k]][1]-x[i])^2+(gold[[k]][2]-y[j])^2 )^(-decay)}}

heat.matrix<-matrix(NA, nrow=200, ncol=200)
heat.matrix[,]<-0
for (i in 1:200) {for (j in 1:200){heat.matrix[i,j]<-tmax*exp(-as.numeric(distance.matrix[i,j]))}}}


heatmap.plus((as.matrix(heat.matrix)),Rowv=NA, Colv = NA,col = colorRampPalette(c("red","yellow","green"))(50))



heatmap.plus((as.matrix(heat.matrix)),Rowv=NA, Colv = NA,col = heat.colors(50, alpha = 1))
