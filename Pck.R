library(tidyverse)##loading package
library(lattice)#loading package
library(survival)#loading package
library(MASS)#loading package
library(caret)#loading package
library(cluster)#loading package
library(factoextra)#loading package
library(FactoMineR)#loading package
library(mclust)#loading package
# Part 1
data.training.valid<-read.csv("C:/Users/ALVONIAH.000/Downloads/data_training_valid.csv")
Newdata<-na.omit(data.training.valid)#Cleaning dataset by removing NAs
age<-Newdata$age#Assining variable
diplome.secteur.2010<-Newdata$diplome_secteur_2010#Assining variable
emprunt<-Newdata$emprunt#Assining variable
dette.2013<-Newdata$dette_2013#Assining variable
secteur.emploi.2013<-Newdata$secteur_emploi_2013#Assining variable
sur.sous.qualification<-Newdata$sur_sous_qualification#Assining variable
lien.emploi.etude<-Newdata$lien_emploi_etude#Assining variable
salaire.2013<-Newdata$salaire_2013#Assining variable
poids<-Newdata$poids#Assining variable
mydata<-cbind.data.frame(age,diplome.secteur.2010,emprunt,dette.2013,secteur.emploi.2013,sur.sous.qualification,lien.emploi.etude,salaire.2013,poids)#Combining quantitative variables
#PCA
res.pca<-prcomp(mydata, scale = TRUE)
fviz_eig(res.pca)

#Multiple correspondence analysis
satisfaction.emploi<-Newdata$satisfaction_emploi#Assigning variable
etude.temps.plein<-Newdata$etude_temps_plein#Assigning variable
scolarite.obtenue.2010<-Newdata$scolarite_obtenue_2010#Assigning variable
emprunt.gouv<-Newdata$emprunt_gouv#Assigning variable
minorite.visible<-Newdata$minorite_visible#Assigning variable
genre<-Newdata$genre#Assigning variable
mydata1<-cbind.data.frame(satisfaction.emploi,etude.temps.plein,scolarite.obtenue.2010,emprunt.gouv,minorite.visible,genre)
res.mca<-MCA(mydata1,graph=FALSE)
print(res.mca)

# K-means clustering
set.seed(123)
#function to compute total within-cluster sum of square
wss<-function(k){kmeans(mydata,k,nstart=10)$tot.withinss}
#compute and plot wss for k=1 to k=15
k.values<-1:15
#extract wss for 2-15 clusters
wss_values<-map_dbl(k.values,wss)
plot(k.values,wss_values,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")


#Hirearchical clustering
# Dissimilarity matrix
d<-dist(mydata, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1<-hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.001, hang = -1)


#Model based clustering 
mydat<-scale(mydata)#Standardizing data
mc<-Mclust(mydat)#model based clustering
summary(mc)#printing a summary
#plot showing clustering
fviz_mclust(mc,"classification",geom="point",pointsize=1.5,palette="jco")

