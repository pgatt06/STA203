rm(list=objects()) ; graphics.off()
library(ggplot2)
################################
#question1: lire les données
################################

xtrain=read.table("/Users/p.gatt/Documents/2A/maths/STA203/TP_Supervise/gasolineTrain.txt",header=T,sep="")
xtest=read.table("/Users/p.gatt/Documents/2A/maths/STA203/TP_Supervise/gasolineTest.txt",header=T,sep="")

#formation des valeurs y 
ytrain=xtrain$octane
ytest=xtest$octane
#formation des matrices des variables explicatives
xtrain=xtrain[,-1]#on enlève la colonne des indices d'octane
xtest=xtest[,-1]

ncol=ncol(xtrain)
nrow=nrow(xtrain)


##tracer des boxplots des variables explicatives 
par(mfrow=c(1, 1))
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
corrplot(cor(xtrain),method="circle")




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

reconstruct = function(res, nr, Xm, Xsd) {
  
  cord_ind = res$ind$coord[, 1:nr]
  reconstructed_data = cord_ind %*% t(res$var$coord[, 1:nr])
  for (i in 1:ncol(reconstructed_data)) {
    reconstructed_data[, i] <- Xsd[i] *  reconstructed_data[, i]  + Xm[i]
  }
  return(reconstructed_data)
}


Xm <- apply(xtrain, 2, mean)
Xsd <- apply(xtrain, 2, sd)

list_n <- c(1, 2, 3, 4)

plots <- list()
par(mfrow = c(2, 2))
error <- data.frame(nr = integer(), RMSE = numeric(), MAE = numeric())

for (n in list_n) {
  reconstructed <- corrige(res.acp, n, Xm, Xsd)
  error_squared=(reconstruct_xtrain - xtrain_matrix)^2  # Calcul de l'erreur au carré pour chaque élément
  RMSE=sqrt(mean(error_squared, na.rm = TRUE))  # Calcul du RMSE en prenant la moyenne des erreurs au carré et en prenant la racine carrée
  MAE=mean(abs(reconstruct_xtrain-xtrain_matrix))
  error <- rbind(error, data.frame(nr = n, RMSE = RMSE, MAE = MAE))

  plot(reconstructed, main = paste('reconstruction nr= ', n, '\n RMSE = ', round(RMSE, 4), "\n MAE =", round(MAE, 4)))
}



#par(mfrow = c(1, 1) ) # Définir la disposition des graphiques
# Initialiser les listes pour stocker les valeurs de RMSE et de MAE
#RMSE_values=list()
#MAE_values=list()
#for (i in (1:5)
#{
#reconstruct_xtrain=reconstruct(res.acp,
#                                 nr=i,
#                                 Xm=colMeans(xtrain),
#                                 Xsd=apply(xtrain, 2, sd))
#
#  # Convertir xtrain en matrice
#  xtrain_matrix=as.matrix(xtrain)
#  error_squared=(reconstruct_xtrain - xtrain_matrix)^2  # Calcul de l'erreur au carré pour chaque élément
#  RMSE=sqrt(mean(error_squared, na.rm = TRUE))  # Calcul du RMSE en prenant la moyenne des erreurs au carré et en prenant la racine carrée
#  MAE=mean(abs(reconstruct_xtrain-xtrain_matrix))
#  RMSE_values[[i]]=RMSE
#  MAE_values[[i]]=MAE
#
#}
# Tracer les deux listes sur le même graphe avec une échelle de l'axe des ordonnées adaptée
#plot(1:5, unlist(RMSE_values), type = "l", col = "blue", lwd = 2, 
#     main = "Evolution de RMSE et MAE en fonction de nr", xlab = "Nombre de composantes principales (nr)", ylab = "Valeur", ylim = range(c(unlist(RMSE_values), unlist(MAE_values))))
#lines(1:5, unlist(MAE_values), type = "l", col = "red", lwd = 2)
#legend("topright", legend = c("RMSE", "MAE"), col = c("blue", "red"), lty = 1, lwd = 2, cex = 1.2)



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
xtrain_matrix=as.matrix(xtrain)
ytrain_vector=as.vector(ytrain)
xtest_matrix=as.matrix(xtest)
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


X=as.matrix(xtrain)

#Calcul à la main 
XtX=t(X)%*%X
#dim 36 x 36 

resultat_eigen= eigen(XtX)
sigma_XtX= resultat_eigen$values[resultat_eigen$values >1e-6]
Sigma_XtX =diag(sigma_XtX)

#décompo en valeur singulière
svd_X = svd(X)
U = svd_X$u
V = svd_X$v

V=as.matrix(V)
sigma = svd_X$d
Sigma = diag(sigma)

XtX_reconstructed= V %*% Sigma%*%Sigma %*% t(V)
difference= abs(XtX - XtX_reconstructed)

#difference nulle c'est bien les deux mêmes matrices

lambda= 0
XtX_lambda_inv = V %*% diag(1 / (sigma^2 + lambda)) %*% t(V)
A0 = V %*% diag(1 / (sigma^2)) %*% t(V)

#verification pseudo-inverse ? 

# Définir lambda très proche de zéro
lambda = 1e-10

# Calculer l'estimateur theta_lambda avec lambda tendant vers 0
theta_lambda = solve(XtX + lambda * diag(ncol(XtX))) %*% t(as.matrix(xtest)) %*%as.matrix(xtest)



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
plot(coefficients_lm_ridge,col="pink")



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



##3.

par(mfrow = c(1, 1))
library(pls)
?cv.glmnet
??cvsegments
## on définit le germe du générateur aléatoire
set.seed(123)
#nombre de plis vaut 4
B=4
cv_indices=cvsegments(nrow(xtrain),B)

X_train = as.matrix(xtrain)
library(glmnet)
library(Matrix)


lambda_values = 10^seq(-10,1, length = 100)

cv_mse = matrix(NA, nrow = B, ncol = length(lambda_values))
cv_mae = matrix(NA, nrow = B, ncol = length(lambda_values))

for (i in 1:B) {
  
  train_idx = unlist(cv_indices[-i])
  valid_idx = unlist(cv_indices[i])
  
  
  X_train_cv = X_train[train_idx, ]
  y_train_cv = ytrain[train_idx]
  X_valid_cv= X_train[valid_idx, ]
  y_valid_cv = ytrain[valid_idx]
  
  
  for (j in seq_along(lambda_values)) {
    
    ridge_model= glmnet(X_train_cv, y_train_cv, alpha = 0, lambda = lambda_values[j])
    
    
    y_pred = predict(ridge_model, newx = X_valid_cv)
    
    # Calcul de l'erreur quadratique moyenne (MSE) et de l'erreur absolue moyenne (MAE)
    cv_mse[i, j] = mean((y_pred - y_valid_cv)^2)
    cv_mae[i, j] = mean(abs(y_pred - y_valid_cv))
  }
}

# Calcul de l'erreur moyenne et de l'intervalle de confiance pour chaque valeur de lambda
mean_cv_mse =apply(cv_mse, 2, mean)
mean_cv_mae = apply(cv_mae, 2, mean)
se_cv_mse = apply(cv_mse, 2, sd) / sqrt(B)
se_cv_mae = apply(cv_mae, 2, sd) / sqrt(B)

plot(lambda_values, mean_cv_mse, type = "l", ylim = range(mean_cv_mse - 2 * se_cv_mse, mean_cv_mse + 2 * se_cv_mse), 
     xlab = "Lambda", ylab = "MSE", main = "Erreur moyenne avec intervalle de confiance", col = "blue", lwd = 2)
lines(lambda_values, mean_cv_mse - 2 * se_cv_mse, lty = 2, col = "red", lwd = 2)
lines(lambda_values, mean_cv_mse + 2 * se_cv_mse, lty = 2, col = "red", lwd = 2)


legend("topright", legend = c("MSE moyen", "Intervalle de confiance"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)


plot(lambda_values, mean_cv_mae, type = "l", ylim = range(mean_cv_mae - 2 * se_cv_mae, mean_cv_mae + 2 * se_cv_mae), 
     xlab = "Lambda", ylab = "MAE", main = "Erreur moyenne avec intervalle de confiance (MAE)", col = "blue", lwd = 2)
lines(lambda_values, mean_cv_mae - 2 * se_cv_mae, lty = 2, col = "red", lwd = 2)
lines(lambda_values, mean_cv_mae + 2 * se_cv_mae, lty = 2, col = "red", lwd = 2)


legend("topright", legend = c("MAE moyen", "Intervalle de confiance"), col = c("blue", "red"), lty = c(1, 2), lwd = 2)


cv.glmnet(X_train,as.matrix(ytrain))


#Measure: Mean-Squared Error 

#Lambda Index Measure       SE Nonzero
#min 0.01455    96 0.05648 0.010231      13
#1se 0.02790    82 0.06651 0.009804      12





















##########################
# PARTIE 4
##########################
#1.
##pour créer les variables z et z test on pourrait leur donner soit 1 ou -1
## en fonction de si la teneur en octane est sup ou inf au seuil qui vaut 88

## pour l'apprentissage 
#1 c'est si teneur > 88 
#-1 sinon
z=ifelse (ytrain>88,1,-1)
ztest=ifelse(ytest>88,1,-1)
table(z)
#-1  1 
#20 16 
table(ztest)
#-1  1 
#13 11 

## dans les deux cas on a le nbre d'observation en teneur en octane <88 superieur à celles au dessus 



##2.
?cv.glmnet
## on nous dit de veiller à foldid 
## en gros, foldid c'est pour spécifier les plis à utiliser lors de la validation croisée
foldid=sample(rep(1:4, length.out=nrow(xtrain)))
reglog_ridge=cv.glmnet(x=xtrain_matrix,
                 y=z,
                 alpha=0,
                 lambda=grid,
                 type.measure="class",
                 foldid=foldid)
reglog_lasso=cv.glmnet(x=xtrain_matrix,
                       y=z,
                       alpha=1,
                       lambda=grid,
                       type.measure="class",
                       foldid=foldid)
erreur_ridge=min(reglog_ridge$cvm)
erreur_ridge 

erreur_lasso=min(reglog_lasso$cvm)
erreur_lasso 





##3.
library(ROCR)
##apprentissage
predproba_ridge=predict(reglog_ridge,type="response",newx=xtrain_matrix)
pred_ridge=prediction(predproba_ridge,z)
plot(performance(pred_ridge,"sens","fpr"),xlab="",col=2)


predproba_lasso=predict(reglog_lasso,type="response",newx=xtrain_matrix)
pred_lasso=prediction(predproba_lasso,z)
plot(performance(pred_lasso,"sens","fpr"),xlab="",col=2)


##test
predproba_ridge_t=predict(reglog_ridge,type="response",newx=xtest_matrix)
pred_ridge_t=prediction(predproba_ridge_t,ztest)
plot(performance(pred_ridge_t,"sens","fpr"),xlab="",col=2)


predproba_lasso_t=predict(reglog_lasso,type="response",newx=xtest_matrix)
pred_lasso_t=prediction(predproba_lasso_t,ztest)
plot(performance(pred_lasso_t,"sens","fpr"),xlab="",col=2)

