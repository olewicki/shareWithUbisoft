##### TP2 ######
# Auteurs : Solenne de Pellegars-Malhortie && Doriane Olewicki
# Date    : 2 Novembre 2018

##### PREPARATION DE LA MATRICE #####
library(Matrix)

# m = read.table("http://www.cours.polymtl.ca/inf6304/Public/citeseer.rtable") # Pour importer en ligne
m = read.table("citeseer.rtable") # Pour import en local

#############################################################################
##### FONCTIONS UTILES #####
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}
min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

#Implementation de la fonction page_rank
page_rank <- function(m, article, r.init, iter){
  d <- 0.85 #facteur d'ajustement
  
  s <- rowSums(m)
  s[s==0] <-1
  
  r <- r.init
  N <- dim(m)[1]
  #Calcul du PageRank
  for (i in 1:iter) {
    r <- (1-d)/N + d * (as.matrix(m)) %*% ((as.matrix(r / s)))
  }
  
  r
}

## Cosinus entre un vecteur v et chaque colonne de la matrice m
cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2));
  (v %*% m)/(n * sqrt(sum(v^2)))
}

## Prediction item-item, MY.MATRIX etant la matrice etudiee, item.nbr, l'item sur lequel se base la prediction
item.item.pred = function(MY.MATRIX, item.nbr, change.to.NA = F) {
  where.NA <- is.na(MY.MATRIX) # localise les NA
  MY.MATRIX[where.NA] <- 0 # mets la valeur des NA a 0 
  #Recherche des voisins les plus proches
  distance.item <- sqrt(colSums((MY.MATRIX[,item.nbr] - MY.MATRIX)^2))
  n.voisins = 21
  i.distance.item <- min.nindex(distance.item, n.voisins) # 20 voisins les plus proches
  
  #Pour eviter d'avoir des zeros nous remplacons les valeurs de chaque colonne de la matrice par leur moyenne
  #certaines colonnes ne sont remplies que par des zeros, nous verrons ensuite comment gerer ce probleme
  for(i in colnames(MY.MATRIX)) {
    MY.MATRIX[MY.MATRIX[,i]==0, i] <- mean(MY.MATRIX[,i])
  }
  MY.MATRIX[where.NA] <- 0 # remet a zero les valeurs NA 
  
  #Initialisation de la matrice pleine dont on va predire toutes les valeurs
  FULL.MATRIX <- as.matrix(MY.MATRIX)
  FULL.MATRIX[FULL.MATRIX==0] <- NA
  #item.nbr <- paste("X", item.nbr, sep="")
  
  # Calcul des parametres pour la prediction
  weight <- cosinus.vm(MY.MATRIX[,item.nbr], t(t(MY.MATRIX)))[i.distance.item]
  weight[is.nan(weight)] <- 0
  mean.item <- mean(FULL.MATRIX[,item.nbr], na.rm=T)
  Kappa <- 1 / sum(abs(weight))

  #Centrage de donnees
  data.centered <- FULL.MATRIX
  #
  # data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item]
  if (is.null(dim(data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item]))) { # cas ou i y a qu'une seule colonne a traiter
    data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item] <- data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item] - colMeans(FULL.MATRIX[,i.distance.item], na.rm=T)
  }
  else { # cas ou il y a plusieurs colonnes a traiter
    data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item] <- t(apply(data.centered[is.na(FULL.MATRIX[,item.nbr]), i.distance.item], 1, function(x) x - colMeans(FULL.MATRIX[,i.distance.item], na.rm=T)))
  }
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

#Fonction permettant de recuperer les noms des colonnes commencant par un X
grepX <- function(sX) {
  s <- gregexpr("[0-9]+", sX)
  s <- unlist(regmatches(sX, s))
  s
}

#Recupere les liens sortant d un article donne : item.nbr
sortant <- function(item.nbr) {
  grepX(names(m[item.nbr, m[item.nbr,]==1])) 
}

#Recupere les liens entrants d un article donne : item.nbr
entrant <- function(item.nbr) {
  rownames(m)[which(m[,paste('X', item.nbr,sep="")]==1)]
}

#Permet de calculer la distance de Manhattan entre la position dun article dans une liste A et une liste B.
#Cela permet d'evaluer la similitude entre le classement des articles donne par A et celui donne par B.
manhattan <- function(A, B) {
  dist <- c()
  for (i in 1:length(A)) {
    dist <- c(dist, abs(i-which(B == A[i])))
  }
  dist
}

#Retire un element 0 et un element 1 de la matrice aleatoirement afin de predire sa valeur avec 
#lapproche item item.
leave.one.out <- function(MY.MATRIX, change.to.NA = F) {
  #tri des articles etant reference au moins deux fois. Permet d'eviter les cas avec des colonnes 
  #de zero comme evoque precedemment.
  two_links <- colnames(MY.MATRIX)[which(colSums(MY.MATRIX) > 2)]
  rnd_col <- sample(two_links, 1)
  rnd_row_0 <- sample(rownames(MY.MATRIX)[which(MY.MATRIX[,rnd_col] != 1)],1)
  rnd_row_1 <- sample(rownames(MY.MATRIX)[which(MY.MATRIX[,rnd_col] == 1)],1)
  
  #On remplace la valeur a predire par NA
  m_new <- MY.MATRIX
  m_new[rnd_row_0, rnd_col] <- NA
  m_new[rnd_row_1, rnd_col] <- NA
  
  i <- item.item.pred(m_new, rnd_col, change.to.NA = F)
  
  # ici on normalise la prediction, qui servira alors plus comme un ordre que comme une prediction.
  # Cela permet de mettre en valeur les articles qui ont gagne des points par rapport a leur vote commun. 
  # Ainsi, ceux ayant une prediction equivalente a la moyenne sont plus proche de zero et ceux ayant une valeur
  # superieur a la moyenne se rapprochent plus vite de 1.
  i <- (i - min(i)) / (max(i) - min(i)) #normalisation

  val_0 <- i[1,(rnd_row_0)]
  val_1 <- 1 - i[1,(rnd_row_1)]

  data.frame(e0 = val_0, e1 = val_1)
}

#Fonction equivalente a la precedente permettant de realiser loperation plusieurs fois : n= nombre d'iterations
leave.one.out.iter <- function(MY.MATRIX, n) {
  res <- data.frame(e0=c(), e1=c())
  for (i in 1:n) {
    curr.iter <- leave.one.out(MY.MATRIX,change.to.NA = F)
    res <- data.frame(e0=c(res$e0, curr.iter$e0), e1=c(res$e1, curr.iter$e1))
  }
  res
}

#############################################################################
#Question 1#

#on cherche les liens entrant et sortant de l'article 422908
sortant.422908 <- sortant('422908')
entrant.422908 <- entrant('422908')
N <- dim(m)[1] #nombre de documents

## PageRank de degre 1 : valeur 1 pour les liens sortants
r.init.deg1 <- matrix(0, N, 1) #r vide
rownames(r.init.deg1) <- rownames(m) #initialisation des noms de rangs de r
r.init.deg1[sortant.422908,1] <- 1 #valeur 1 pour tous liens sortants de l'article
r.init.deg1 <- r.init.deg1 / sum(r.init.deg1) #normalisation

PR.deg1 <- page_rank(m,'422908', r.init.deg1, 50) #application de l'algorithm PageRank sur 50 iterations

#Classement des articles obtenus
PR.deg1.val <- PR.deg1[order(PR.deg1, decreasing=TRUE)] # valeurs ordonnees en ordre decroissant
PR.deg1.norm <- (PR.deg1 - min(PR.deg1)) / (max(PR.deg1) - min(PR.deg1)) # valeurs ordonnees en ordre decroissant normalisees
PR.deg1.art <- rownames(m)[order(PR.deg1, decreasing=TRUE)] # noms des articles dans l'ordre decroissant de leur valeur

#Affichage des valeurs
head(PR.deg1.val)
# [1] 0.010308628 0.005935208 0.005295870 0.004134258 0.003822359 0.003590751
head(PR.deg1.art)
# [1] "38085"  "164643" "368281" "184608" "500980" "26913"

## PageRank de degre 2 : valeur 2 pour les liens sortants directs et valeur 1 pour les liens sortants des liens sortants directs
r.init.deg2 <- matrix(0, N, 1) #r vide
rownames(r.init.deg2) <- rownames(m) #initialisation des noms de rangs de r
for(i in sortant.422908) {
  s.local <- sortant(i)
  r.init.deg2[s.local,1] <- 1 #valeur 1 pour tous liens sortants de l'article 
}
r.init.deg2[sortant.422908,1] <- 2 #valeur 2 pour tous liens sortants de l'article
r.init.deg2 <- r.init.deg2 / sum(r.init.deg2) #normalisation

#Reduction de la matrice m aux articles ayant des liens de references de degre 1 ou 2
reduced.m <- m[r.init.deg2 != 0, r.init.deg2 != 0]
reduced.r <- r.init.deg2[r.init.deg2 != 0]

PR.deg2 <- page_rank(reduced.m,'422908', reduced.r, 50) #application de l'algorithm PageRank

#Classement des valeurs de page rank
PR.deg2.val <- PR.deg2[order(PR.deg2, decreasing=TRUE)] # valeurs ordonnees en ordre decroissant
PR.deg2.norm <- (PR.deg2 - min(PR.deg2)) / (max(PR.deg2) - min(PR.deg2)) # valeurs ordonnees en ordre decroissant normalisees
PR.deg2.art <- rownames(m)[order(PR.deg2, decreasing=TRUE)] # noms des articles dans l'ordre decroissant de leur valeur

#Affichage des valeurs
head(PR.deg2.val)
#[1] 0.002659377 0.001458014 0.001315988 0.001205039 0.001164815 0.001132713
head(PR.deg2.art)
#[1] "105376" "108055" "102886" "10151"  "10394"  "105655"

#############################################################################
#Question 2#
#Prediction des votes de '422908' avec l'approche item item
ii <- item.item.pred(t(m), '422908', change.to.NA = F) 
ii.reduced <- item.item.pred(t(reduced.m), '422908', change.to.NA = F) 
table(ii)
#On obtient en majorite des votes proches de la moyenne. Ceci est du a la forte presence de 0. 
# 0.0367725109671539 0.0686518540347916 0.0777869880507023 0.0815527708154259  0.085244400409084 0.0882496247041605  0.144605179506461 
# 1060                  1                  1                  5                  1                  2                  4 
# 0.171725653167996  0.189385439354733  0.190511209261223  0.193077068948392  0.236126819999954   0.24086255309174  0.248308424021847 
# 1                  1                  1                  1                  1                  1                  1 
# 0.326293648764362  0.359421393172279  0.391742252331009  0.490386999715226  0.497095395032233  0.534660452838622  0.547128128459315 
# 1                  1                  1                  1                  1                  1                  1 
# 0.596958064362082  0.789012727705053 
# 1                  1 

ii <- t(as.matrix(ii)) 
rownames(ii) <- rownames(m) # nommage des ligne de ii, semblable a PR
ii.reduced <- t(as.matrix(ii.reduced)) 
rownames(ii.reduced) <- rownames(reduced.m) # nommage des ligne de ii.reduced, semblable a PR

#Classement des predictions  
ii.val <- ii[order(ii, decreasing=TRUE)] # valeurs ordonnees en ordre decroissant
ii.norm <- (ii - min(ii)) / (max(ii) - min(ii))  # valeurs ordonnees en ordre decroissant normalisees
ii.art <- rownames(m)[order(ii, decreasing=TRUE)] # noms des articles dans l'ordre decroissant de leur valeur
#Affichage des valeurs
head(ii.val)
#[1] 0.7890127 0.5969581 0.5471281 0.5346605 0.4970954 0.4903870
head(ii.art)
#[1] "17094"  "64835"  "110303" "19422"  "522428" "155792"

ii.reduced.val <- ii.reduced[order(ii.reduced, decreasing=TRUE)] # valeurs ordonnees en ordre decroissant
ii.reduced.norm <- (ii.reduced - min(ii.reduced)) / (max(ii.reduced) - min(ii.reduced))  # valeurs ordonnees en ordre decroissant normalisees
ii.reduced.art <- rownames(m)[order(ii.reduced, decreasing=TRUE)] # noms des articles dans l'ordre decroissant de leur valeur
#Affichage des valeurs
head(ii.reduced.val)
#[1] 1.568346 1.471940 1.467970 1.380226 1.379292 1.342388
head(ii.reduced.art)
#[1] "102966" "106034" "105962" "102886" "100299" "105376"


# Dans un premier temps, pour comparer PageRank et item.item, on a compter le nombre d'article sortant ou
# entrant de "422908".
N.10 <- max(1, round(0.1 * length(m)))
sum(sortant.422908 %in% ii.art[1:N.10]) / length(sortant.422908)
# [1] 1 (%) => Tous les articles sortant se retrouvent dans les predictions hautes de item.item
sum(sortant.422908 %in% PR.deg1.art[1:N.10]) / length(sortant.422908)
# [1] 0 (%) => Aucun article sortant ne se retrouve dans les predictions hautes de PageRank de degre 1
sum(sortant.422908 %in% PR.deg2.art[1:N.10]) / length(sortant.422908)
# [1] 0 (%) => Aucun article sortant ne se retrouve dans les predictions hautes de PageRank de degre 2
sum(entrant.422908 %in% ii.art[1:N.10]) / length(entrant.422908)
# [1] 0.1956522 (%) => 1/5 des articles entrant se retrouvent dans les predictions hautes de item.item
sum(entrant.422908 %in% PR.deg1.art[1:N.10]) / length(entrant.422908)
# [1] 0.4130435 (%) => 2/5 des articles entrant se retrouvent dans les predictions hautes de PageRank de degre 1
sum(entrant.422908 %in% PR.deg2.art[1:N.10]) / length(entrant.422908)
# [1] 0.04347826 (%) => 2/5 des articles entrant se retrouvent dans les predictions hautes de PageRank de degre 2

# Cependant ces resultats ne nous informe pas vraiment sur la qualite des predictions, donnant plus d'importance
# aux articles deja lies.

# Nous avons donc voulu comparer les predictions en tant qu'ordre en utilisant la distance de manhattan comme
# comparaison entre les articles dans les predictions ordonnees des deux algorithmes.

#Calcul avec la distance manhattan  
dist1 <- manhattan(PR.deg1.art,  ii.art) / length(PR.deg1.art)
dist2 <- manhattan(PR.deg2.art,  ii.reduced.art) / length(PR.deg2.art)
par(mfrow = c(1, 2))
boxplot(dist1, main='Distance de Manhattan \n PR deg1 avec ii')
boxplot(dist2, main='Distance de Manhattan \n PR deg2 avec ii')
# On observe que la distance de Manhattan tourne autour de 20-40% du nombre d'article dans les deux cas.

# La distance de Manhattan a toute fois comme desavantages de ne pas prendre en compte les valeurs donnees 
# par les algorithmes. On veut donc finalement comparer les valeurs normalises.
#Calcul avec la distance rmse
OK_rmse_deg1 <- c()
OK_rmse_deg2 <- c()
for (i in rownames(ii)) {
  OK_rmse_deg1 <- c(OK_rmse_deg1, sqrt((ii.norm[i,1] - PR.deg1.norm[i,1])^2))
}
for(i in rownames(PR.deg2.norm)) {
  OK_rmse_deg2 <- c(OK_rmse_deg2, sqrt((ii.reduced.norm[i,1] - PR.deg2.norm[i,1])^2))
}
head(OK_rmse_deg1)
# 0.02300115 0.08504099 0.19829853 0.01150057 0.01512232 0.12905412 
head(OK_rmse_deg2)
# 0.47766120 0.01974215 0.36166952 0.20991843 0.09404541 0.23446759 

par(mfrow = c(1, 2))
boxplot(OK_rmse_deg1, main='RMSE sur pred. normees\n PR deg1 avec ii')
boxplot(OK_rmse_deg2, main='RMSE sur pred. normees\n PR deg2 avec ii')
# On observe a peu pres les meme resultats qu'avec la distance de Manhattan ce qui est rassurant. On peut
# quand meme observer que le PageRank de deg1 est tres proche des predictions de item.item (75% des articles,
# predit a moins de 10% pres a la meme position).

#############################################################################
#Question 3#

# On enleve les colonnes qui n'ont pas assez de liens sortants qui sont donc consideres comme non-pertinent
small_m <- m[which(rowSums(m) > 2), which(rowSums(m) > 2)]

err <- leave.one.out.iter(small_m, 50)
err
# e0        e1
# 1  0 1.0000000
# 2  0 0.7495373
# 3  0 1.0000000
# 4  0 0.5982484
# 5  0 0.8070644
# On observe comme resultat que en general, on tombe sur une ligne avec des voisins peu semblable (resultat 1 et 3). 
# Ce qui fait que l'erreur sur zero (e0) est egale a zero et l'erreur sur un (e1) est a un. La prediction ne predit 
# pas assez d'article au dessus de la prediction moyenne. Du coup on ne repere pas l'article qui aurait du etre a 1.


data.frame(mean.err.0 = mean(err$e0), mean.err.1 = mean(err$e1))
# mean.err.0 mean.err.1
# 1          0  0.9460459
data.frame(sd.err.0 = sd(err$e0), sd.err.1 = sd(err$e1))
# sd.err.0  sd.err.1
# 1        0 0.1353838

# CORRECTION => http://www.groupes.polymtl.ca/log6308/Tp/20183/tp2-sol2.html

