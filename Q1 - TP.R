#Q1

#chargemente des libraries

library(tidyverse)
library(FactoMineR)
library(plotly)
library(ggplot2)
library(rgl)



library(rglwidget)
library(htmltools)
library(Factoshiny)
library(dplyr)
library(magrittr)
library(leaflet)
library(mice)
library(DT)
library(graphics)


# chargement des données et quelques verifications


entrain_valid

summary(entrain_valid)



hist(entrain_valid$age,freq=TRUE, labels=TRUE)

hist(entrain_valid$emprunt,freq=TRUE, labels=TRUE)

hist(entrain_valid$poids,freq=TRUE, labels=TRUE)

hist(entrain_valid$salaire_2013,freq=TRUE, labels=TRUE)

hist(entrain_valid$dette_2013,freq=TRUE, labels=TRUE)

boxplot(entrain_valid$salaire_2013)

boxplot(entrain_valid$poids)

boxplot(entrain_valid$age)

boxplot(entrain_valid$emprunt)

boxplot(entrain_valid$dette_2013)



for (x in seq(1,20)){ 
  print(paste(colnames(entrain_valid[,x]),'a',sum(is.na(entrain_valid[,x])),'valeurs manquantes')) 
  }


#exclusion du ID

df<-entrain_valid %>%  
  select( salaire_2013,dette_2013,age, emprunt,poids,genre
          ,langue,minorite_visible,scol_mere,scol_pere,
          scolarite_avant,scolarite_obtenue_2010,diplome_secteur_2010,
          emprunt_gouv,secteur_emploi_2013,etude_temps_plein,
          satisfaction_emploi,sur_sous_qualification,lien_emploi_etude)



#Vérification des doublons 

nrow(distinct(df))

df

summary(df)

#valuers manquantes

for (x in seq(1,19)){ 
  print(paste(colnames(df[,x]),'a',sum(is.na(df[,x])),'valeurs manquantes'))
}

#variables qualitatives

#remplacer missing par inconnu


df<- df%>% 
  mutate(langue = replace_na(langue, "inconnu"),
         minorite_visible = replace_na(minorite_visible, "inconnu"),
         scol_mere = replace_na(scol_mere, "inconnu"),
         scol_pere = replace_na(scol_pere, "inconnu"),
         scolarite_avant = replace_na(scolarite_avant, "inconnu"),
         scolarite_obtenue_2010 = replace_na(scolarite_obtenue_2010, "inconnu"),
         emprunt_gouv = replace_na(emprunt_gouv, "inconnu"),
         etude_temps_plein = replace_na(etude_temps_plein, "inconnu"),
         satisfaction_emploi = replace_na(satisfaction_emploi, "inconnu"))



#convertir  facteur
df <- df %>%
  mutate(genre = as.factor(genre),
         langue= as.factor(langue),
         minorite_visible= as.factor(minorite_visible),
         scol_mere=as.factor(scol_mere),
         scol_pere= as.factor(scol_pere),
         scolarite_avant= as.factor(scolarite_avant),
         scolarite_obtenue_2010= as.factor(scolarite_obtenue_2010),
         diplome_secteur_2010= as.factor(diplome_secteur_2010),
         emprunt_gouv= as.factor(emprunt_gouv),
         secteur_emploi_2013= as.factor(secteur_emploi_2013),
         etude_temps_plein= as.factor(etude_temps_plein),
         satisfaction_emploi= as.factor(satisfaction_emploi),
         sur_sous_qualification= as.factor(sur_sous_qualification),
         lien_emploi_etude= as.factor(lien_emploi_etude)) 

#ACP imputation automatique par la moyenne

v<-as.matrix(df[,5])


result_pca <- PCA(df[1:4], 
                  ncp = 5,
                  row.w = as.vector(v),
                  graph = TRUE)


#ACP avec les var qualitative impute par prediction avec mice


df_mice <- mice(df[1:4], 
                m =4 ,
                method = 'norm.predict',
                print = FALSE)

df_complete <- complete(df_mice)

df_imputees <- df %>%
  mutate(age = df_complete$age,
         emprunt= df_complete$emprunt,
         salaire_2013= df_complete$salaire_2013,
         dette_2013= df_complete$dette_2013 )



v1<-as.matrix(df_imputees[,5])


result_pca_imput <- PCA(df_imputees[1:4], 
                  ncp = 5,
                  row.w = as.vector(v1),
                  graph = TRUE)


#sans variable dette_2013

result_pca_sans_dette <- PCA(df %>% select(1,3,4), 
                  ncp = 5,
                  row.w = as.vector(v),
                  graph = TRUE)

#sans variable salaire_2013

result_pca_sans_salaire <- PCA(df[2:4], 
                  ncp = 5,
                  row.w = as.vector(v),
                  graph = TRUE)

#============================================================================
  
  
  #Analyse des correspondances multiples
  
  
  #DF_socio_dem  
res.MCA1<-MCA(df[1:19][,-c(2,3,4)],quanti.sup=c(1,2),quali.sup=c(3,4,5,6,7,8),graph=FALSE,
                row.w=as.vector(v))

plot.MCA(res.MCA1, choix='var')

plot.MCA(res.MCA1,invisible= 'var',select= 'contrib 10342',selectMod= 'contrib  8 ',habillage='contrib',title="DF_NAture Sociodem",label ='none')

plot.MCA(res.MCA1, choix='quanti.sup')


#df_diplome_2010_emprunt

res.MCA2<-MCA(df[1:19][,-c(1,3,4,5)],quanti.sup=c(1),quali.sup=c(8,9,10),graph=FALSE,
              row.w=as.vector(v))

plot.MCA(res.MCA2, choix='var',col.quali.sup='#006400')

plot.MCA(res.MCA2,select= 'contrib 10342',selectMod= 'contrib  11 ',habillage='contrib',col.quali.sup='#006400',title="DF_diplo_2010_et_emprunt",label =c('var'))

plot.MCA(res.MCA2, choix='quanti.sup')


#df_statut_2013


res.MCA3<-MCA(df[1:19][,-c(1,2,5)],quanti.sup=c(1,2),quali.sup=c(12,13,14,15,16),graph=FALSE,
              row.w=as.vector(v))

plot.MCA(res.MCA3, choix='var',col.quali.sup='#006400')

plot.MCA(res.MCA3,select= 'contrib 10342',selectMod= 'contrib  9 ',habillage='contrib',col.quali.sup='#006400',title="Df_statut_2013",label =c('var'))

plot.MCA(res.MCA3, choix='quanti.sup')

#============================================================================


#Regroupement


#Cluster avec ACP 

res.PCA<-PCA(df_imputees[,-c(13,14,15,16,17,18,19)],quali.sup=c(6,7,8,9,10,11,12),quanti.sup=c(5),graph=FALSE)
plot.PCA(res.PCA,choix='var')
plot.PCA(res.PCA)


res.PCA<-PCA(df_imputees[,-c(13,14,15,16,17,18,19)],ncp=3,quali.sup=c(6,7,8,9,10,11,12),quanti.sup=c(5),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=4,kk=100,consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Hierarchical tree on the factor map')




res.PCA<-PCA(df_imputees[,-c(13,14,15,16,17,18,19)],ncp=3,quali.sup=c(6,7,8,9,10,11,12),quanti.sup=c(5),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=4,kk=100,consol=TRUE,graph=FALSE)
summary(res.HCPC)



