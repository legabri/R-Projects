library(Factoshiny)

cluster <- Factoshiny(entrain_valid[1:19])

cluster <- Factoshiny(df[1:19])


mca <- Factoshiny(df[1:19])




#cluster_entrain_valid

peso <- (df$poids-min(df$poids))/(max(df$poids)-min(df$poids))

summary(peso)

df

hc_ave=hclust(dist(df), method='average',members=peso)

plot(hc_ave, hang=1, main='cluster average')  

summary(hc_ave$call)



res.PCA<-PCA(entrain_valid[1:19],ncp=Inf, scale.unit=FALSE,quali.sup=c(2,3,4,5,6,7,9,11,14,15),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,consol=TRUE,graph=FALSE)
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Hierarchical tree on the factor map')


res.PCA<-PCA(df[1:19],ncp=Inf, scale.unit=FALSE,quali.sup=c(6,7,8,9,10,11,12,13,14,15,16,17,18,19),graph=FALSE)
res.HCPC<-HCPC(res.PCA,nb.clust=3,kk=100,consol=FALSE,graph=FALSE,metric='manhattan')
plot.HCPC(res.HCPC,choice='tree',title='Hierarchical tree')
plot.HCPC(res.HCPC,choice='map',draw.tree=FALSE,title='Factor map')
plot.HCPC(res.HCPC,choice='3D.map',ind.names=FALSE,centers.plot=FALSE,angle=60,title='Hierarchical tree on the factor map')
