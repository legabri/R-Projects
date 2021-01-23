#Q4

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

# Calcul de la moitie de la valeur de l'enmpunt en 2010

df_ex_4 <- df

df_ex_4$moitie_emprunt=(df_ex_4['emprunt']/2)

names(df_ex_4)

df_ex_4_2 <- df_ex_4



#creation variable moitie_paye si oui sa  veut dire que la personne a paye moitie de l'emprunt
# si dette_2013<moitie_emprunt alors 'Oui' sinon 'Non'

df_ex_4_2$moitie_paye= ifelse(df_ex_4$dette_2013<df_ex_4$moitie_emprunt,"oui","non")


names(df_ex_4_2)


#creation variable moitie_paye si oui sa  veut dire que la personne a paye moitie de l'emprunt
# si dette_2013<moitie_emprunt alors 'Oui' sinon 'Non'


#Exlcusion ID

test_numero_4_sans_ID <- test_numero_4[1:11]

#selections seulement de variables qu'il y a dans le fichier test

myvars_4 <- names(test_numero_4_sans_ID)

measurevar_4 <- "moitie_paye"

groupvars_4  <- myvars_4

groupvars_4


myvars_4 <- c(myvars_4,'moitie_paye','poids')

df_ex_4_3 <- df_ex_4_2[myvars_4]

names(df_ex_4_3)

summary(df_ex_4_3)




#df_ex_4$moitie_paye = as.factor(df_ex_4$moitie_paye)

#df_ex_4$moitie_emprunt = as.factor(df_ex_4$moitie_emprunt)

#exclusion valeurs manquantes pour la variable emprunt car je me base sur elle pour calculer la 
#variable reponse moitie_paye

df_ex_4_3 <- df_ex_4_3 %>% 
  filter(!is.na(emprunt),
         !is.na(moitie_paye))

#imputation de l'age

df_mice_ex4 <- mice(df_ex_4_3[1:2], 
                    m =1 ,
                    method = 'norm.predict',
                    print = FALSE)

df_complete_ex4 <- complete(df_mice_ex4)

df_imputees_ex4 <- df_ex_4_3 %>%
  mutate(age=df_complete_ex4$age)


summary(df_imputees_ex4)

# division entrain test

n_int_4 <- nrow(df_imputees_ex4)

index <- sample.int(n_int_4,n_int_4*0.8,replace=FALSE)

df_imputees_ex4_train <- df_imputees_ex4[index,]

df_imputees_ex4_test <- df_imputees_ex4[-index,]

names(df_imputees_ex4_train)

names(df_imputees_ex4_test)

summary(df_imputees_ex4_train)

summary(df_imputees_ex4_test)

formula_4 <- as.formula(paste(measurevar_4, paste(groupvars_4, collapse=" + "), sep=" ~ "))

set.seed(4)

n4 <- trunc(sqrt(nrow(df_imputees_ex4_train)))

model_ex_4 <- ranger(formula_4,
                     df_imputees_ex4_train,
                     mtry = 2, # Nombre de variables évaluées à chaque noeud
                     num.trees = 500, # Nombre d'arbres
                     sample.fraction = 1, # Fraction de l'échantillon pour chaque arbre
                     splitrule = 'gini', # Critère de séparation
                     case.weights = df_imputees_ex4_train$poids,
                     min.node.size = n4, #racine care du nb de obbs 
                     importance = 'permutation', # Pour obtenir l'importance des variables 
                     scale.permutation.importance = TRUE
)
#dificuldade missings da divida e do salario antes de rodar o random forest

#3 medidas

p_ex_4_train <- predict(model_ex_4, df_imputees_ex4_train, type = 'response',weights=df_imputees_ex4_train$poids)

p_ex_4_test  <- predict(model_ex_4, df_imputees_ex4_test, type = 'response', weights=df_imputees_ex4_test$poids)

df_imputees_ex4_test$prediction <- p_ex_4_test$predictions

df_imputees_ex4_test


#evaluation entrain

Accuracy(y_pred = p_ex_4_train$predictions, y_true = df_imputees_ex4_train$moitie_paye)

Precision(y_pred = p_ex_4_train$predictions, y_true = df_imputees_ex4_train$moitie_paye)

Recall(y_pred = p_ex_4_train$predictions, y_true = df_imputees_ex4_train$moitie_paye)

Specificity(y_pred = p_ex_4_train$predictions, y_true = df_imputees_ex4_train$moitie_paye)

round(model_ex_4$variable.importance)



#evaluation teste

Accuracy(y_pred = p_ex_4_test$predictions, y_true = df_imputees_ex4_test$moitie_paye)

Precision(y_pred = p_ex_4_test$predictions, y_true = df_imputees_ex4_test$moitie_paye)

Recall(y_pred = p_ex_4_test$predictions, y_true = df_imputees_ex4_test$moitie_paye)

Specificity(y_pred = p_ex_4_test$predictions, y_true = df_imputees_ex4_test$moitie_paye)


# evaluation sur les var sensibles quelles?

library(fairness)



test_4_no_miss_fact <- test_4_no_miss %>%
  mutate(genre = as.factor(genre),
         langue= as.factor(langue),
         scol_mere=as.factor(scol_mere),
         scol_pere=as.factor(scol_pere),
         scolarite_avant=as.factor(scolarite_avant),
         scolarite_obtenue_2010=as.factor(scolarite_obtenue_2010),
         emprunt_gouv=as.factor(emprunt_gouv))


test_4_no_miss <- na.exclude(test_numero_4)

names(test_4_no_miss)

sum(is.na(test_4_no_miss_fact$prediction))

#la prediction sur le vraie echantillon teste

resp_ex_4_test <- predict(model_ex_4, test_4_no_miss,weights=1)

test_4_no_miss$prediction <- resp_ex_4_test$predictions

summary(test_4_no_miss)

View(test_4_no_miss)


```
