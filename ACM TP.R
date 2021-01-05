library(Factoshiny)
res1 <- Factoshiny(entrain_valid[1:19])

#nature sociodem

res.MCA<-MCA(entrain_valid[1:19][,-c(8,10,12,13,16,17,18)],quanti.sup=c(1,12),quali.sup=c(2,3,4,5,6,7),graph=FALSE)
plot.MCA(res.MCA, choix='var',col.quali.sup='#006400')
plot.MCA(res.MCA,invisible= 'var',select= 'contrib 10342',selectMod= 'contrib  4 ',habillage='contrib',col.quali.sup='#006400',title="Nature Sociodémographique",label ='none')
plot.MCA(res.MCA, choix='quanti.sup')

# diplome obtenue et les empruunt




library(Factoshiny)

res2 <- Factoshiny(df[1:19])


#DF_socio_dem  
res.MCA<-MCA(df[1:19][,-c(2,3,4)],quanti.sup=c(1,2),quali.sup=c(3,4,5,6,7,8),graph=FALSE)
plot.MCA(res.MCA, choix='var')
plot.MCA(res.MCA,invisible= 'var',select= 'contrib 10342',selectMod= 'contrib  8 ',habillage='contrib',title="DF_NAture Sociodem",label ='none')
plot.MCA(res.MCA, choix='quanti.sup')


#df_diplome_2010_emprunt

res.MCA<-MCA(df[1:19][,-c(1,3,4,5)],quanti.sup=c(1),quali.sup=c(8,9,10),graph=FALSE)
plot.MCA(res.MCA, choix='var',col.quali.sup='#006400')
plot.MCA(res.MCA,select= 'contrib 10342',selectMod= 'contrib  11 ',habillage='contrib',col.quali.sup='#006400',title="DF_diplo_2010_et_emprunt",label =c('var'))
plot.MCA(res.MCA, choix='quanti.sup')


#df_statut_2013


res.MCA<-MCA(df[1:19][,-c(1,2,5)],quanti.sup=c(1,2),quali.sup=c(12,13,14,15,16),graph=FALSE)
plot.MCA(res.MCA, choix='var',col.quali.sup='#006400')
plot.MCA(res.MCA,select= 'contrib 10342',selectMod= 'contrib  9 ',habillage='contrib',col.quali.sup='#006400',title="Df_statut_2013",label =c('var'))
plot.MCA(res.MCA, choix='quanti.sup')

