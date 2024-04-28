rm(list=objects()) ; graphics.off()
library(ggplot2)
################################
#question1: lire les données
################################





xtrain=read.table("rapport/gasolineTrain.txt",header=T,sep="")
xtest=read.table("rapport/gasolineTest.txt",header=T,sep="")

#formation des valeurs y 
ytrain=xtrain$octane
ytest=xtest$octane
#formation des matrices des variables explicatives
xtrain=xtrain[,-1]#on enlève la colonne des indices d'octane
xtest=xtest[,-1]

ncol=ncol(xtrain)
nrow=nrow(xtrain)


##tracer des boxplots des variables explicatives 

par(mfrow = c(1, 1) ) # Définir la disposition des graphiques
for (i in 1:ncol) {
  boxplot(xtrain[, i], main = colnames(xtrain)[i], ylab = "Valeur")
}

##tracer des courbes 

for (i in 1:nrow) {
  matplot(1:ncol, t(xtrain[i,]), type = "l", main = paste("Observation", i), xlab = "Fréquence", ylab = "Intensité")
}

##etude de la corrélation 
correlation= cor(xtrain)
library(corrplot)
corrplot(cor(scale(xtrain)),method="circle")




##########################
#question2: etude de l'ACP 
##########################
library(FactoMineR)
res.acp=PCA(xtrain)
res.acp$eig
## on a 35 valeurs propres

##tracer le graphe des valeurs propres
eigenvalues=res.acp$eig[,1]
barplot(eigenvalues,names.arg=1:length(eigenvalues),main="Graphe des valeurs propres", xlab = "Composantes", ylab = "Valeurs propres")
# A partir de la 8-9ème valeur propre, les valeurs sont quasiment dérisioires voire nulles 

##représentation nuage 
# Représenter les nuages de points dans les six premiers axes principaux
plot(res.acp, axes = c(1, 2), choix = "ind", habillage = "none")  # Premier et deuxième axes principaux
plot(res.acp, axes = c(3, 4), choix = "ind", habillage = "none")  # Troisième et quatrième axes principaux
plot(res.acp, axes = c(5, 6), choix = "ind", habillage = "none")  # Cinquième et sixième axes principaux
# les deux premiers axes sont les principaux 



############################
#question 3
############################

reconstruct=function(res,nr,Xm,Xsd)
{
  coord=res.acp$ind$coord[,1:nr]
  recons=coord%*% t(res.acp$var$coord[,1:nr])
  for (i in 1:ncol(recons))
  {
    recons[,i]=recons[,i]*Xsd[i]+ Xm[i]
  }
  return (recons)
}
reconstruct_xtrain=reconstruct(res.acp,
                               nr=6,
                               Xm=colMeans(xtrain),
                               Xsd=apply(xtrain, 2, sd))
RMSE <- sqrt(mean((reconstructed_xtrain - xtrain)^2))




#############################################################
########### regression penalisee ##################
######################################################

######
#question1
######
library(glmnet)

# Préparer les matrices xtrain et ytrain
xtrain_matrix <- as.matrix(xtrain)
ytrain_vector <- as.vector(ytrain)

# Créer une grille de paramètres pour lambda
grid = 10^seq(6, -10, length = 100)
# Estimer le modèle de régression ridge avec glmnet
ridge_model= glmnet(x = xtrain_matrix, y = ytrain_vector, alpha = 0, lambda = grid)
# Afficher les coefficients
coef(ridge_model)
#variation de l'intercept 
plot(ridge_model, xvar = "lambda", label = TRUE)
## on remarque bien une convergence vers 0 lorsque lambda augmente 
## avec lambda le paramètre de pénalisation 

##recalculer 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
## si on normalise ytrain seulement 
ytrain_centered=ytrain-mean(ytrain)
ridge_model_2= glmnet(x = xtrain_matrix, y = ytrain_centered, alpha = 0, lambda = grid)
plot(ridge_model_2, xvar = "lambda", label = TRUE)
## ca ne change rien 

## si on centre xtrain 
xtrain_centered=scale(xtrain,center=TRUE,scale=FALSE)
ridge_model_3= glmnet(x = xtrain_centered, y = ytrain_vector, alpha = 0, lambda = grid)
plot(ridge_model_3, xvar = "lambda", label = TRUE)
coef(ridge_model_3)


## si on fait les deux 
ridge_model_4= glmnet(x = xtrain_centered, y = ytrain_centered, alpha = 0, lambda = grid)
plot(ridge_model_4, xvar = "lambda", label = TRUE)


####################"
# il reste à faire 




################
# question 2.
################
library(MASS)
#standardiser nos données 
xtrain_stand=scale(xtrain)
ytrain_stand=scale(ytrain)

?lm.ridge
