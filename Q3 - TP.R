# Q3

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
library(caret)
library(MLmetrics)









#exclusion du ID

df<-entrain_valid %>%  
  select( salaire_2013,dette_2013,age, emprunt,poids,genre
          ,langue,minorite_visible,scol_mere,scol_pere,
          scolarite_avant,scolarite_obtenue_2010,diplome_secteur_2010,
          emprunt_gouv,secteur_emploi_2013,etude_temps_plein,
          satisfaction_emploi,sur_sous_qualification,lien_emploi_etude)




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


summary(df)

#Exlcusion ID

test_numero_3_sans_ID <- test_numero_3 %>%  select(1,2,3,4,5,6,7,8,9,10,11,13)

#selections seulement de variables qu'il ya dans le fichier test

myvars_3 <- names(test_numero_3_sans_ID)

measurevar_3 <- "salaire_2013"

groupvars_3  <- myvars_3

groupvars_3[1:11]


myvars_3 <- c(myvars_3,'salaire_2013')

df_ex_3 <- df[myvars_3]

names(df_ex_3)

summary(df_ex_3)

#imputation

df_ex_3 <- df_ex_3 %>% 
  filter(!is.na(salaire_2013),
         salaire_2013>0)


df_mice_ex3 <- mice(df_ex_3 %>%  select(1,10), 
                    m =2 ,
                    method = 'norm.predict',
                    print = FALSE)

df_complete_ex3 <- complete(df_mice_ex3)

df_imputees_ex3 <- df_ex_3 %>%
  mutate(emprunt=df_complete_ex3$emprunt,
         age=df_complete_ex3$age)


summary(df_imputees_ex3)



# division entrain test

n_int_3 <- nrow(df_imputees_ex3)

index <- sample.int(n_int_3,n_int_3*0.8,replace=FALSE)

df_imputees_ex3_train <- df_imputees_ex3[index,]

df_imputees_ex3_test <- df_imputees_ex3[-index,]

names(df_imputees_ex3_train)

summary(df_imputees_ex3_train)

summary(df_imputees_ex3_test)



formula_3 <- as.formula(paste(measurevar_3, paste(groupvars_3[1:11], collapse=" + "), sep=" ~ "))

set.seed(3)


model_ex_3 <- lm(formula_3,
              data = df_imputees_ex3_train,
              weights =df_imputees_ex3_train$poids ) 


summary(model_ex_3)


#Prédiction

p_ex_3_train <- predict(model_ex_3, df_imputees_ex3_train, type = 'response',weights=df_imputees_ex3_train$poids)

p_ex_3_test <- predict(model_ex_3, df_imputees_ex3_test, type = 'response',weights=df_imputees_ex3_test$poids)

min(df_imputees_ex3$salaire_2013)

#MAE MSE train

MSE(y_pred=p_ex_3_train, y_true = df_imputees_ex3_train$salaire_2013)


MLmetrics::MAE(y_pred=p_ex_3_train, y_true = df_imputees_ex3_train$salaire_2013)


#MAE MSE test


MSE(y_pred=p_ex_3_test, y_true = df_imputees_ex3_test$salaire_2013)

MLmetrics::MAE(y_pred=p_ex_3_test, y_true = df_imputees_ex3_test$salaire_2013)



y_chapeau_3 <- predict(model_ex_3)

y_chapeau_3


#transformation en facteur


test_numero_3_fact <- test_numero_3 %>%
  mutate(genre = as.factor(genre),
         langue= as.factor(langue),
         minorite_visible= as.factor(minorite_visible),
         scol_mere=as.factor(scol_mere),
         scol_pere= as.factor(scol_pere),
         scolarite_avant= as.factor(scolarite_avant),
         scolarite_obtenue_2010= as.factor(scolarite_obtenue_2010),
         diplome_secteur_2010= as.factor(diplome_secteur_2010),
         emprunt_gouv= as.factor(emprunt_gouv),
         ID=as.factor(ID)) 

#prediction sur le vraie echantillon de test

names(test_numero_3_fact)

summary(test_numero_3_fact)

#enlever les données manquantes de l'echantillon de test

test_numero_3_no_miss_2 <- test_numero_3_fact %>% 
  filter(!is.na(age),
         !is.na(langue),
         !is.na(minorite_visible),
         !is.na(scol_mere),
         !is.na(scol_pere),
         !is.na(scolarite_avant),
         !is.na(scolarite_obtenue_2010),
         !is.na(emprunt),
         !is.na(emprunt_gouv))

summary(test_numero_3_no_miss_2)

names(test_numero_3_no_miss_2)


resp_ex_3_test <- predict(model_ex_3, test_numero_3_no_miss_2, type = 'response',weights=test_numero_3_no_miss$poids)

names(test_numero_3_no_miss_2)

test_numero_3_no_miss <- test_numero_3_no_miss_2

test_numero_3_no_miss$prediction <-resp_ex_3_test

test_numero_3_no_miss

names(test_numero_3_no_miss)


