
# Q2

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

#Créer un modèle permettant de prédire la satisfaction par rapport 
#à l’emploi et discuter des facteurs qui affectent ce niveau de satisfaction. 
#Votre rapport doit décrire brièvement votre méthodologie. (1 page)

# avec toutes les variables

#imputation

df_ex_2 <- df %>% 
  filter(satisfaction_emploi != 'inconnu')

df_ex_2$satisfaction_emploi <- df_ex_2$satisfaction_emploi %>% droplevels


df_mice_ex2 <- mice(df_ex_2[1:4], 
                   m =4 ,
                   method = 'norm.predict',
                   print = FALSE)

df_complete_ex2 <- complete(df_mice_ex2)

df_imputees_ex2 <- df_ex_2 %>%
  mutate(dette_2013 = df_complete_ex2$dette_2013,
         salaire_2013= df_complete_ex2$salaire_2013,
         emprunt=df_complete_ex2$emprunt,
         age=df_complete_ex2$age)



summary(df_imputees_ex2)


#separation 80% entrain 20% test



n_int_2 <- nrow(df_imputees_ex2)

index <- sample.int(n_int_2,n_int_2*0.8,replace=FALSE)

df_imputees_ex2_train <- df_imputees_ex2[index,]

df_imputees_ex2_test <- df_imputees_ex2[-index,]

nrow(df_imputees_ex2_train)

set.seed(2)

model_ex_2 <- ranger(satisfaction_emploi~., 
                         df_imputees_ex2_train,
                         mtry = 8, # Nombre de variables évaluées à chaque noeud
                         num.trees = 500, # Nombre d'arbres
                         sample.fraction = 1, # Fraction de l'échantillon pour chaque arbre
                         case.weights=df_imputees_ex2_train$poids, # poids d'echantillonage
                         splitrule = 'gini', # Critère de séparation
                         min.node.size = 81, # racine carré de 6586
                         importance = 'permutation', # Pour obtenir l'importance des variables 
                         scale.permutation.importance = TRUE
)

#dificuldade missings da divida e do salario antes de rodar o random forest

#prediction entrain

p_ex_2_train <- predict(model_ex_2, df_imputees_ex2_train, type = 'response',weights=df_imputees_ex2_train$poids)

#exactitude, precision et importance entrain

Accuracy(y_pred = p_ex_2_train$predictions, y_true = df_imputees_ex2_train$satisfaction_emploi)

Precision(y_pred = p_ex_2_train$predictions, y_true = df_imputees_ex2_train$satisfaction_emploi)

round(model_ex_2$variable.importance)


#prediction test

p_ex_2_test <- predict(model_ex_2, df_imputees_ex2_test, type = 'response',weights=df_imputees_ex2_test$poids)

#exactitude, precision et importance test


Accuracy(y_pred = p_ex_2_test$predictions, y_true = df_imputees_ex2_test$satisfaction_emploi)

Precision(y_pred = p_ex_2_test$predictions, y_true = df_imputees_ex2_test$satisfaction_emploi)



#avec les variables concerrnant le statut en 2013

names(df_imputees_ex2_train)

#selections des colonnes concernées

df_ex_2_var_spec_train <- df_imputees_ex2_train %>%  select(3,4,5,15,16,17,18,19)

df_ex_2_var_spec_test <- df_imputees_ex2_test %>%  select(3,4,5,15,16,17,18,19)


names(df_ex_2_var_spec_train)


set.seed(22)

model_ex_2_var_spec <- ranger(satisfaction_emploi~., 
                    df_ex_2_var_spec_train,
                     mtry = 2, # Nombre de variables évaluées à chaque noeud
                     num.trees = 10, # Nombre d'arbres
                     sample.fraction = 1, # Fraction de l'échantillon pour chaque arbre
                     case.weights=df_ex_2_var_spec_train$poids, # poids d'echantillonage
                     splitrule = 'gini', # Critère de séparation
                     min.node.size = 3, # racine carré de 6586
                     importance = 'permutation', # Pour obtenir l'importance des variables 
                     scale.permutation.importance = TRUE
)

#dificuldade missings da divida e do salario antes de rodar o random forest

#prediction entrain

p_ex_2_train_var_spec <- predict(model_ex_2_var_spec, df_ex_2_var_spec_train, type = 'response',weights=df_ex_2_var_spec_train$poids)

#exactitude, precision et importance entrain

Accuracy(y_pred = p_ex_2_train_var_spec$predictions, y_true = df_ex_2_var_spec_train$satisfaction_emploi)

Precision(y_pred = p_ex_2_train_var_spec$predictions, y_true = df_ex_2_var_spec_train$satisfaction_emploi)

round(model_ex_2_var_spec$variable.importance)


#prediction test

p_ex_2_test_var_spec <- predict(model_ex_2_var_spec, df_ex_2_var_spec_test, type = 'response',weights=df_ex_2_var_spec_test$poids)

#exactitude, precision et importance test


Accuracy(y_pred = p_ex_2_test_var_spec$predictions, y_true = df_ex_2_var_spec_test$satisfaction_emploi)

Precision(y_pred = p_ex_2_test_var_spec$predictions, y_true = df_ex_2_var_spec_test$satisfaction_emploi)

