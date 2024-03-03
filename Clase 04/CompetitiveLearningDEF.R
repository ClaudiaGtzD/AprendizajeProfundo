rm(list=ls())

library(openxlsx, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(scales)
library(officer)
library(flextable)
library(Distance)
library(cluster)
library(ClusterR)
library(fpc)
library(plotly)
library(scatterplot3d)
library(kohonen)
library(cclust)
set.seed(100)

################################################################################
#STEP 0. UPLOAD DATA BASE AND ROUTES.

ruta.1 <- "C:/Users/riemm/OneDrive/Escritorio/DEEP LEARNING CLASS/"
BaseAumentada <- read.csv("C:/Users/riemm/OneDrive/Hazel and Big Data/DEEP LEARNING CLASS/BaseLD.csv",header=TRUE, sep=",")
pertenencia<-matrix(0,nrow=27751, ncol=1, byrow=F)

################################################################################
#STEP 2. ALGORITHM.

m<-nrow(BaseAumentada)
n<-ncol(BaseAumentada)
colnames(BaseAumentada)
neuronas<-matrix(0, nrow=3, ncol=n, byrow=F)
promedio<-matrix(0, nrow=1, ncol=n, byrow=F)
minimo<-matrix(0, nrow=1, ncol=n, byrow=F)
maximo<-matrix(0, nrow=1, ncol=n, byrow=F)

#Initialize the locations.

for(i in 1:n)
{minimo[1,i]=min(BaseAumentada[,i])
}

for(i in 1:n)
{maximo[1,i]=max(BaseAumentada[,i])
}

for(i in 1:n)
{promedio[1,i]=mean(BaseAumentada[,i])
}

neuronas[1,]<-minimo[1,]
neuronas[2,]<-promedio[1,]
neuronas[3,]<-maximo[1,]

#Synaptic Potential

distancia<-matrix(0, nrow=1, ncol=3, byrow=F)
base <- as.matrix(BaseAumentada)

#Cluster

cluster<-matrix(0, nrow=1, ncol=3, byrow=F)

for (k in 1:3) {
  
  cluster<-matrix(0, nrow=1, ncol=3, byrow=F)
  
  for(i in 1:m)
  {
    
    for(j in 1:3)
    {
      distancia[,j]<-norm(as.matrix(base[i,]-neuronas[j,]),"f")
    }
    
    un<-0.5*(1-i/m)
    
    min<-min(distancia[,1],distancia[,2],distancia[,3])
    
    if(distancia[,1]==min){neuronas[1,]=neuronas[1,]+un*(base[i,]-neuronas[1,]);cluster[,1]<-cluster[,1]+1;pertenencia[i]<-1}
    if(distancia[,2]==min){neuronas[2,]=neuronas[2,]+un*(base[i,]-neuronas[2,]);cluster[,2]<-cluster[,2]+1;pertenencia[i]<-2}
    if(distancia[,3]==min){neuronas[3,]=neuronas[3,]+un*(base[i,]-neuronas[3,]);cluster[,3]<-cluster[,3]+1;pertenencia[i]<-3}
  }
  
  k <- k+1
  
}

################################################################################
#STEP 3. RESULTS.

#Print Neurons

neuronas[1,]
neuronas[2,]
neuronas[3,]
nombre.neurona<-c("Neurona1","Neurona2","Neurona3")
neuronas.p<-cbind(nombre.neurona,neuronas)
neuronas.p<-data.frame(neuronas.p)
write.table(
  x         = neuronas.p,
  file      = paste0(ruta.1,"NEURONAS.csv"),
  sep       = ",",
  row.names = FALSE
)

#Print means

sump1<-sum(neuronas[1,])/30
sump2<-sum(neuronas[2,])/30
sump3<-sum(neuronas[3,])/30
suma.promedio<-c(sump1,sump2,sump3)
suma.promedio<-cbind(nombre.neurona,suma.promedio)
suma.promedio<-data.frame(suma.promedio)
write.table(
  x         = suma.promedio,
  file      = paste0(ruta.1,"SUMA.PROMEDIO.csv"),
  sep       = ",",
  row.names = FALSE
)

#Print Clusters.

cluster
cluster<-rbind(nombre.neurona,cluster)
cluster<-data.frame(cluster)
write.table(
  x         = cluster,
  file      = paste0(ruta.1,"Total.Elementos.Cluster.csv"),
  sep       = ",",
  row.names = FALSE
)

#Print Ownership

pertenencia<-data.frame(pertenencia)
pertenencia

base.actualizada<-cbind(pertenencia,BaseAumentada)
base.actualizada

write.table(
  x         = base.actualizada,
  file      = paste0(ruta.1,"Pertenencia.csv"),
  sep       = ",",
  row.names = FALSE
)





