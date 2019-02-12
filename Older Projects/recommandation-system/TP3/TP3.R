##### TP3 ######
# Auteurs : Solenne de Pellegars-Malhortie && Doriane Olewicki
# Date    : 7 novembre 2018

##### PREPARATION DE LA MATRICE #####
u.data <- read.csv(file='u.data.csv', sep='|', header=T)
library(Matrix)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')

#############################################################################
##### FONCTIONS UTILES #####
meanC <- function (x) {
  x[1:length(x)]=mean(x,na.rm=T)
  x
}

rmse <- function (pred, real) {
  sqrt(sum((real-pred)^2,na.rm=T)/length(real))
}

mae <- function (pred, real) {
  sum(abs(real-pred),na.rm=T)/length(real)
}

svd.dim.N <- function(N,MY.SVD,FULL.MATRIX,id=NA){
  if (is.na(id)){
    id <- 1:(dim(FULL.MATRIX)[1]*dim(FULL.MATRIX)[2])
  }
  #N est le nombre de dimension
  estimation <- MY.SVD$u %*% diag(c(MY.SVD$d[1:N],rep(0,length(MY.SVD$d)-N)))%*%t(MY.SVD$v)
  data.frame(rmse=rmse(estimation[id],FULL.MATRIX[id]),mae=mae(estimation[id],FULL.MATRIX[id]))
}

our.svd.iter <- function (MY.MATRIX, iter,id=NA) { #TODO virer
  FULL.MATRIX <- as.matrix(MY.MATRIX)
  FULL.MATRIX[FULL.MATRIX==0] <- NA
  m.norm.FULL <- apply(FULL.MATRIX,1, function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  m.norm <- m.norm.FULL
  m.norm[is.na(m.norm)] <- 0
  votes <- as.matrix(m.norm)
  votes.svd <- svd(votes)
  
  res <- data.frame(i=c(), rmse=c(),mae=c())
  for (i in iter) {
    i <- min(i, length(votes.svd$d))
    res.loc <- svd.dim.N(i,votes.svd,m.norm.FULL,id)
    res <- data.frame(i=c(res$i, i), rmse=c(res$rmse,res.loc$rmse), mae= c(res$mae,res.loc$mae))
  }
  res
}

#############################################################################
#Question 1#
#On cherche a trouver des points de comparaisons pour l'evaluation de nos methodes a venir
FULL.MATRIX <- as.matrix(m)
FULL.MATRIX[FULL.MATRIX==0] <- NA

#On doit avoir une matrice normalisee pour appliquer SVD
m.norm.FULL <- FULL.MATRIX /5 #apply(FULL.MATRIX,1, function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))

#On fait une moyenne selon respectivement : les items, les utilisateurs et la matrice totale. 
#On peut alors avoir une prediction des votes bases sur les moyennes pour avoir un ordre de grandeu
#pour les mesures d'evaluation.
m.mean.item <- apply(FULL.MATRIX /5,2,meanC)
m.mean.user <- t(apply(FULL.MATRIX /5,1,meanC))
m.mean.all <- mean(FULL.MATRIX /5, na.rm=T)

#On calcule ici le rmse pour les 3 moyennes
rmse.mean.item <- rmse(m.mean.item,FULL.MATRIX /5)
rmse.mean.user <- rmse(m.mean.user,FULL.MATRIX /5)
rmse.mean.all <- rmse(m.mean.all,FULL.MATRIX /5)
rmse.mean.item
#[1] 0.05004545
rmse.mean.user
#[1] 0.05157745
rmse.mean.all
#[1] 0.05632692

#On calcule ici le mae pour les 3 moyennes
mae.mean.item <- mae(m.mean.item,FULL.MATRIX /5)
mae.mean.user <- mae(m.mean.user,FULL.MATRIX /5)
mae.mean.all <- mae(m.mean.all,FULL.MATRIX /5)
mae.mean.item
#[1] 0.0100039
mae.mean.user
#[1] 0.01034448
mae.mean.all
#[1] 0.01182948

#############################################################################
#Question 2#

# On ne peut pas avoir de NA dans svd, 
#On fait donc les traitements associes sur la matrice normalisee precedemment
m.norm <- m.norm.FULL
m.norm[is.na(m.norm)] <- 0
#On applique svd sur notre matrice de votes modifiee
votes <- as.matrix(m.norm)
votes.svd <- svd(votes)
str(votes.svd) #affichage du svd
# List of 3
# $ d: num [1:943] 127.2 48.7 43.3 31.6 31.4 ...
# $ u: num [1:943, 1:943] -0.06627 -0.01414 -0.00568 -0.00604 -0.03276 ...
# $ v: num [1:1682, 1:943] -0.0936 -0.0354 -0.0201 -0.0599 -0.0218 ...

#############################################################################
#Question 3#
#On estime les votes a l'aide des resultats du svd obtenus precedemment sur 10 dimensions
votes.10dim <- votes.svd$u %*% diag(c(votes.svd$d[1:10],rep(0,length(votes.svd$d)-10)))%*%t(votes.svd$v)

#############################################################################
#Question 4#

rmse(votes.10dim,m.norm.FULL)
#[1] 0.1143068
mae(votes.10dim,m.norm.FULL)
#[1] 0.02467995

#On remarque qu'on obtient des resultats sensiblement moins bons que ceux estimes en question 1
#############################################################################
#Question 5#

#On estime la dimension appropriee pour obtenir les meilleurs resultats possibles
#On cherche a tracer une courbe afin de definir le nombre optimal de dimension. 
#Pour ce faire on definit les points d'iterations de maniere exponentielle etant 
#donne l'allure de la courbe obtenue lors de nos tests
iter <- round(exp(0:14*0.5))

#Definition du dataframe contenant les resultats
res.svd <- data.frame(i=c(), rmse=c(),mae=c())
#Iteration sur nos differentes dimensions etablies ci dessus permettant de calculer
#RMSE et MAE du svd
for (i in iter) {
  i <- min(i, length(votes.svd$d))
  res.loc <- svd.dim.N(i,votes.svd,m.norm.FULL)
  res.svd <- data.frame(i=c(res.svd$i, i), rmse=c(res.svd$rmse,res.loc$rmse), mae= c(res.svd$mae,res.loc$mae))
}
res.svd
# i         rmse          mae
# 1    1 1.393953e-01 3.139357e-02
# 2    2 1.327124e-01 2.963036e-02
# 3    3 1.276309e-01 2.826321e-02
# 4    4 1.248480e-01 2.756625e-02
# 5    7 1.181671e-01 2.572103e-02
# 6   12 1.125056e-01 2.420967e-02
# 7   20 1.070271e-01 2.275509e-02
# 8   33 1.004557e-01 2.112067e-02
# 9   55 9.121779e-02 1.887961e-02
# 10  90 7.904519e-02 1.597576e-02
# 11 148 6.270980e-02 1.214226e-02
# 12 245 4.252756e-02 7.561429e-03
# 13 403 2.163647e-02 3.257516e-03
# 14 665 5.074723e-03 5.573268e-04
# 15 943 5.921981e-16 1.078866e-16

# On constate qu'on obtient des resultats meilleurs que ceux predits a la question 1 uniquement a partir de 148 
#dimensions considerees pour le rmse et le mae.

#Affichage graphique des resultats
plot(res.svd$i, res.svd$rmse, type="l", ylim=c(min(c(res.svd$rmse, res.svd$mae)), max(c(res.svd$rmse, res.svd$mae))), col="blue")
lines(res.svd$i, res.svd$mae, col="red")

#Calcul de la derivee (sous forme differentielle) de la fonction obtenue precedemment afin d'obtenir
#le point a partir duquel l'amelioration du rmse (et mae) n'est plus significatif lorsqu'on augmente 
#le nombre de dimensions
dif_rmse<- res.svd$rmse[2:length(res.svd$rmse)] - res.svd$rmse[1:length(res.svd$rmse)-1]
dif_i <- res.svd$i[2:length(res.svd$i)] - res.svd$i[1:length(res.svd$i)-1]
diff.rmse <- dif_rmse/dif_i
dif_mae<- res.svd$mae[2:length(res.svd$rmse)] - res.svd$mae[1:length(res.svd$rmse)-1]
diff.mae <- dif_mae/dif_i

#Affichage graphique des derivees
plot(res.svd$i[2:length(res.svd$i)], diff.rmse, type="l", ylim=c(min(c(diff.rmse, diff.mae)), max(c(diff.rmse, diff.mae))), col="blue")
lines(res.svd$i[2:length(res.svd$i)], diff.mae, col="red")
#Affichage de la droite associee au point pour lequel on estime que l'augmentation n'est plus significative
lines(c(res.svd$i[8],res.svd$i[8]), c(min(c(diff.rmse, diff.mae)),0))


#On obtient une dimension optimale d'environ  :
res.svd$i[8]
#[1] 33 #Dimensions

#On peut noter qu'on obtient des resultats un peu moins bons que ceux de la question 1
#Toutefois, augmenter les dimensions n'ameliore pas de facon suffisamment importante le rmse
#pour que l'on considere cela rentable.
#TODO check avec le prof pour les dimensions moins bonnes que la moyenne

#############################################################################
#Question 6#

vector <- as.vector(m.norm.FULL)
index <- which(!is.na(vector))
sampled.index <- sample(index,length(index))
N <- 10
group <- rep(1:N, length(sampled.index)/N)

group

iter <- round(exp(0:14*0.5))

res <- data.frame(i=c(), rmse=c(),mae=c())
for(i in 1:N) {
  new.m <- m
  new.m[sampled.index[group == i]] <- 0

  FULL.MATRIX <- as.matrix(new.m)
  FULL.MATRIX[FULL.MATRIX==0] <- NA
  m.norm.FULL <- apply(FULL.MATRIX,1, function(x) (x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)))
  m.norm <- m.norm.FULL
  m.norm[is.na(m.norm)] <- 0
  votes <- as.matrix(m.norm)
  votes.svd <- svd(votes)
  for (j in iter) {
    j <- min(j, length(votes.svd$d))
    res.loc <- svd.dim.N(j,votes.svd,m.norm.FULL,sampled.index[group == i])
    res <- data.frame(i=c(res$i, j), rmse=c(res$rmse,res.loc$rmse), mae= c(res$mae,res.loc$mae))
  }
}
plot(res$i[1:length(iter)], res$rmse[1:length(iter)], type="l", ylim=c(min(c(res$rmse, res$mae)), max(c(res$rmse, res$mae))), col="blue")
for(i in 0:(N-1)) {
  lines(res$i[1:length(iter) + i * length(iter)], res$rmse[1:length(iter) + i * length(iter)], col="blue")
  lines(res$i[1:length(iter) + i * length(iter)], res$mae[1:length(iter) + i * length(iter)], col="red") 
}


#Calcul de la derivee (sous forme differentielle) de la fonction obtenue precedemment afin d'obtenir
#le point a partir duquel l'amelioration du rmse (et mae) n'est plus significatif lorsqu'on augmente 
#le nombre de dimensions
dif_rmse<- res$rmse[2:length(res$rmse)] - res$rmse[1:length(res$rmse)-1]
dif_i <- res$i[2:length(res$i)] - res$i[1:length(res$i)-1]
diff.rmse <- dif_rmse/dif_i
dif_mae<- res$mae[2:length(res$rmse)] - res$mae[1:length(res$rmse)-1]
diff.mae <- dif_mae/dif_i

#Affichage graphique des derivees
plot(res$i[2:(length(iter))], diff.rmse[1:(length(iter)-1)], type="l", ylim=c(min(c(diff.rmse, diff.mae)), max(c(diff.rmse, diff.mae))), col="blue")
for(i in 0:(N-1)) {
  lines(res$i[2:(length(iter))], diff.rmse[1:(length(iter)-1) + i * length(iter)], col="blue")
  lines(res$i[2:(length(iter))], diff.mae[1:(length(iter)-1) + i * length(iter)], col="red")
}
#Affichage de la droite associee au point pour lequel on estime que l'augmentation n'est plus significative
lines(c(res.svd$i[8],res.svd$i[8]), c(min(c(diff.rmse, diff.mae)),0))

#On obtient une dimension optimale d'environ  :
res$i[8]
#[1] 33 #Dimensions

#On obtient un resultat similaire à la question précédente

#############################################################################
#Question 7#

cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2));
  (v %*% m)/(n * sqrt(sum(v^2)))
}

## Correlation entre un vecteur v et chaque colonne de la matrice m
correlation <- function(v,m) {
  mean.v <- mean(v);
  mean.m <- apply(m, 2, mean);
  centered.v <- v - mean.v;
  centered.m <- m - matrix(rep(mean.m, dim(m)[1]), nr=dim(m)[1], nc=dim(m)[2], byrow=T);
  cosinus.vm(centered.v, centered.m)
}

## Trouve les indexes des premieres 'n' valeurs maximales d'une matrice
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

## Prediction item-item
item.item.pred = function(MY.MATRIX, item.nbr, change.to.NA = T) {
  #Initialisation de la matrice pleine dont on va predire toutes les valeurs
  FULL.MATRIX <- as.matrix(MY.MATRIX)
  FULL.MATRIX[FULL.MATRIX==0] <- NA
  
  distance.item <- sqrt(colSums((MY.MATRIX[,item.nbr] - MY.MATRIX)^2))
  n.voisins = 21
  i.distance.item <- min.nindex(distance.item, n.voisins) # 20 voisins les plus proches
  
  #Calcul des parametres pour la prediction
  weight <- cosinus.vm(MY.MATRIX[,item.nbr], MY.MATRIX)[i.distance.item]
  mean.item <- mean(FULL.MATRIX[,item.nbr], na.rm=T)
  Kappa <- 1 / sum(abs(weight))
  
  #Centrage de donnees
  data.centered <- FULL.MATRIX
  data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item] <- t(apply(data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item], 1, function(x) x - colMeans(FULL.MATRIX[,i.distance.item], na.rm=T)))
  data.centered[is.na(data.centered)] <- 0
  vote.commun <- weight %*% t(data.centered[, i.distance.item])
  
  #Prediction
  estimation.item.item <- mean.item + Kappa * vote.commun
  #Attribution de la valeur NA aux predictions des utilisateurs sans vote commun
  if(change.to.NA) {
    estimation.item.item[vote.commun == 0] <- NA
  }
  
  estimation.item.item
}

## Prediction user-user
user.user.pred = function(MY.MATRIX, user.nbr, change.to.NA = T) {
  #transposition de la matrice pour en faire un probleme item item
  item.item.pred(t(MY.MATRIX), user.nbr, change.to.NA)
}

# Recommandation de n films pour un user defini
pred.n.movies <- function(MY.MATRIX, user.nbr, n) {
  pred <- user.user.pred(MY.MATRIX,user.nbr)
  pred[mod.m[dim(mod.m)[1],] != 0] <- NA #TODO : Delete??
  #N'affiche que les titres des films selectionnes
  u.item$movie.title[max.nindex(pred,n)]
}

## RMSE : implementation d'une fonction pour calculer le RMSE
rmse_fun <- function(real, pred) {
  sqrt(sum((real - pred)^2) / length(pred))
}

## RMSE avec iteration sur les users pour un item en particulier
#  On intere un nombre "iter" de fois sur un item definit par "iter.nbr" ou un
#  pourcentage "percent" d'user on leur vote mis a 0 pour cet item.
rmse_iter <- function(MY.MATRIX, item.nbr, percent, iter) {
  rmse <- rep(0, iter)
  for (i in 1:iter) {
    user.ids <- 1:dim(MY.MATRIX)[1]
    
    #selection uniquement d'un echantillon. Cela permet de supprimer une partie des votes
    #connus pour les predire et tester ensuite la validite de la prediction.
    to.remove <- sample(user.ids, length(user.ids) * percent / 100)
    m.modiv <- MY.MATRIX
    m.modiv[to.remove, item.nbr] <- 0
    
    pred <- item.item.pred(m.modiv, item.nbr, F)
    rmse[i] <- sqrt(sum((MY.MATRIX[to.remove, item.nbr] - pred[to.remove])^2) / length(to.remove))
  }
  c(mean(rmse), sd(rmse))
}


## RMSE avec iteration sur les users et sur les items approche item-item
#  On itere l'iteration de rmse_item sur un pourcentage "percent.item" des 
#  item.
rmse_iter_item_item <- function(MY.MATRIX, percent.item, percent.user, iter) {
  item.ids <- 1:dim(MY.MATRIX)[2]
  to.test <- sample(item.ids, length(item.ids) * percent.item / 100)
  
  if (length(item.ids) * percent.item / 100 < 1) {
    "ERROR : to small sample" 
  }
  else {
    rmse <- c(0,0)
    for (i in to.test) {
      iterated <- rmse_iter(MY.MATRIX, i, percent.user, iter)
      rmse <- rmse + iterated
    }
    
    rmse / length(to.test)
  }
}

## RMSE avec iteration sur les users et sur les items approche user-user
#  On transpose simplement la matrice et on inverse les percentages pour que 
#  le probleme devienne item-item.
rmse_iter_user_user <- function(MY.MATRIX, percent.item, percent.user, iter) {
  rmse_iter_item_item(t(MY.MATRIX), percent.user, percent.item, iter)
}


res.uu <- rmse_iter_user_user(m/5,5,5,5)
res.uu[1]
#[1] 0.69284188

#On observe que les resultats sont moins bons avec cette methode. On peut supposer que certaines 
#methodes sont plus ou moins adaptees a des situations precises et que c'est a nous d'etre a meme d'evaluer 
#quelle methode utiliser dans chaque cas.

