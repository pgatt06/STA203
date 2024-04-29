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
plot(res.acp, axes = c(4, 5), choix = "ind", habillage = "none")  # Cinquième et sixième axes principaux
# les deux premiers axes sont les principaux 



############################
#question 3
############################

reconstruct=function(res,nr,Xm,Xsd)
{
  coord=res$ind$coord[,1:nr]
  recons=coord%*% t(res.acp$var$coord[,1:nr])
  for (i in 1:ncol(recons))
  {
    recons[,i]=recons[,i]*Xsd[i]+ Xm[i]
  }
  return (recons)
}
reconstruct_xtrain=reconstruct(res.acp,
                               nr=2,
                               Xm=colMeans(xtrain),
                               Xsd=apply(xtrain, 2, sd))

par(mfrow = c(1, 1) ) # Définir la disposition des graphiques
# Initialiser les listes pour stocker les valeurs de RMSE et de MAE
RMSE_values=list()
MAE_values=list()
for (i in (1:5))
{
  reconstruct_xtrain=reconstruct(res.acp,
                                 nr=i,
                                 Xm=colMeans(xtrain),
                                 Xsd=apply(xtrain, 2, sd))

  # Convertir xtrain en matrice
  xtrain_matrix=as.matrix(xtrain)
  error_squared=(reconstruct_xtrain - xtrain_matrix)^2  # Calcul de l'erreur au carré pour chaque élément
  RMSE=sqrt(mean(error_squared, na.rm = TRUE))  # Calcul du RMSE en prenant la moyenne des erreurs au carré et en prenant la racine carrée
  MAE=mean(abs(reconstruct_xtrain-xtrain_matrix))
  RMSE_values[[i]]=RMSE
  MAE_values[[i]]=MAE

}
# Tracer les deux listes sur le même graphe avec une échelle de l'axe des ordonnées adaptée
plot(1:5, unlist(RMSE_values), type = "l", col = "blue", lwd = 2, 
     main = "Evolution de RMSE et MAE en fonction de nr", xlab = "Nombre de composantes principales (nr)", ylab = "Valeur", ylim = range(c(unlist(RMSE_values), unlist(MAE_values))))
lines(1:5, unlist(MAE_values), type = "l", col = "red", lwd = 2)
legend("topright", legend = c("RMSE", "MAE"), col = c("blue", "red"), lty = 1, lwd = 2, cex = 1.2)



#affichage de la reconstruction

for (i in (1:5))
{
  reconstruct_xtrain=reconstruct(res.acp,
                                 nr=i,
                                 Xm=colMeans(xtrain),
                                 Xsd=apply(xtrain, 2, sd))
  
  # Convertir xtrain en matrice
  xtrain_matrix=as.matrix(xtrain)
  error_squared=(reconstruct_xtrain - xtrain_matrix)^2  # Calcul de l'erreur au carré pour chaque élément
  RMSE=sqrt(mean(error_squared, na.rm = TRUE))  # Calcul du RMSE en prenant la moyenne des erreurs au carré et en prenant la racine carrée
  MAE=mean(abs(reconstruct_xtrain-xtrain_matrix))
  RMSE_values[[i]]=RMSE
  MAE_values[[i]]=MAE
  
  # Tracer la reconstruction
  plot.new()
  plot.window(xlim = c(1, ncol(xtrain)), ylim = range(c(xtrain, reconstruct_xtrain)))
  title(main = paste("Reconstruction pour nr =", i, "\nRMSE =", round(RMSE, 4), ", MAE =", round(MAE, 4)))
  for (i in 1:nrow(xtrain)) {
    lines(1:ncol(xtrain), xtrain[i,], type = "l", col = "blue")
    lines(1:ncol(xtrain), reconstruct_xtrain[i,], type = "l", col = "red")
  }
  legend("topright", legend = c("Original", "Reconstructed"), col = c("blue", "red"), lty = 1, lwd = 2)
}


##courbes à ces 6 niveaux pour le premier spectre 
# Initialiser une nouvelle fenêtre graphique
par(mfrow = c(1, 1))

# Créer un vecteur de couleurs pour chaque niveau de nr
colors <- rainbow(length(c(1:5, 39)))

# Tracer les courbes pour le premier spectre
plot(1:ncol(xtrain), xtrain[1,], type = "l", col = "blue", lwd = 2, 
     main = "Courbes pour le premier spectre", xlab = "Fréquence", ylab = "Intensité")
for (nr in c(1:5, 39)) {
  lines(1:ncol(xtrain), reconstruct(res.acp, nr = nr, Xm = colMeans(xtrain), Xsd = apply(xtrain, 2, sd))[1,], 
        type = "l", col = colors[nr], lwd = 2)
}
legend("topright", legend = c(1:5, 39), col = colors, lty = 1, lwd = 2, title = "nr")


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
# Examiner la variation de la valeur estimée du paramètre d'interception
intercept_values=coef(ridge_model)[1, ]
plot(log10(grid), intercept_values, type = "l", xlab = "log(lambda)", ylab = "Estimation de l'intercept", main = "Variation de l'intercept en fonction de lambda")
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
ridge_model_lm=lm.ridge(ytrain_stand~xtrain_stand,lambda=grid)
plot(ridge_model_lm)
# Comparer les coefficients estimés par lm.ridge avec ceux calculés directement
coefficients_lm_ridge=coef(ridge_model_lm)
coefficients_calculated=coef(ridge_model)
plot(coefficients_calculated,col="purple")
lines(coefficients_lm_ridge,col="pink")



##
# Calculer les estimations directes pour la régression ridge
lambda_min <- min(grid)
theta_b_lambda_min <- coefficients_calculated[, which.min(grid)]
estimated_coefficients <- theta_b_lambda_min / (1 + lambda_min)

# Afficher les estimations calculées directement
print(estimated_coefficients)

# Comparer avec les estimations fournies par glmnet
print(coef(ridge_model_lm)[, which.min(grid)])

# Vérifier en lançant lm.ridge avec une grille modifiée
ridge_model_lm_modified <- lm.ridge(ytrain_stand ~ xtrain_stand, lambda = lambda_min)
print(coef(ridge_model_lm_modified))
