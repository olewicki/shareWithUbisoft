##### TP1 ######
# Auteurs : Solenne de Pellegars-Malhortie && Doriane Olewicki
# Date    : 5 Septembre 2018

##### PREPARATION DES MATRICES #####
# m : Matrice de données de 100 000 votes faits par 943 utilisateurs et portant sur 1682 items.
u.data <- read.csv(file='u.data.csv', sep='|', header=T)
library(Matrix)
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')

# u.item : Matrice de données sur les films
u.item <- read.csv(file='u.item.csv', sep='|', header=T)

# u.user : Matrice de données sur les utilisateurs
u.user <- read.csv(file='u.user.csv', sep='|', header=T)

#############################################################################
##### FONCTIONS UTILES #####
## Cosinus entre un vecteur v et chaque colonne de la matrice m
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

## Trouve les indexes des premières 'n' valeurs maximales d'une matrice
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
  #Initialisation de la matrice pleine dont on va prédire toutes les valeurs
  FULL.MATRIX <- as.matrix(MY.MATRIX)
  FULL.MATRIX[FULL.MATRIX==0] <- NA

  distance.item <- sqrt(colSums((MY.MATRIX[,item.nbr] - MY.MATRIX)^2))
  n.voisins = 21
  i.distance.item <- min.nindex(distance.item, n.voisins) # 20 voisins les plus proches

  #Calcul des parametres pour la prédiction
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
  #Attribution de la valeur NA aux prédictions des utilisateurs sans vote commun
  if(change.to.NA) {
    estimation.item.item[vote.commun == 0] <- NA
  }
  
  estimation.item.item
}

## Prediction user-user
user.user.pred = function(MY.MATRIX, user.nbr, change.to.NA = T) {
  #transposition de la matrice pour en faire un problème item item
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
    #connus pour les prédire et tester ensuite la validité de la prediction.
    to.remove <- sample(user.ids, length(user.ids) * percent / 100)
    m.modiv <- MY.MATRIX
    m.modiv[to.remove, item.nbr] <- 0
    
    pred <- item.item.pred(m.modiv, item.nbr, F)
    rmse[i] <- sqrt(sum((MY.MATRIX[to.remove, item.nbr] - pred[to.remove])^2) / length(to.remove))
  }
  c(mean(rmse), sd(rmse))
}
rmse_iter(m, item.item.pred, 2, 2, 5) 

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

# Bayes : prediction des films conseilles en fonction de l'age, du genre et du metier
bayes <- function(mx, MY.AGE, MY.GENDER, MY.JOB) {
  list.age <- (1:15)*5
  list.gender <- c('M', 'F')
  list.job <- unique(u.user$job)
  
  #Creation des classes d'age
  list.class.age <- cut(mx$age, breaks=list.age, include.lowest = TRUE)
  my.class.age <- cut(MY.AGE, breaks=list.age, include.lowest = TRUE)
  like.my.age <- list.class.age == my.class.age[1] # booleen disant si l'utilisateur est de la meme classe que moi
  
  # Pour une re-execution plus rapide nous avons mis le nombre de film a 100.
  # Pour obtenir le meme resultat que celui affiche plus bas, mettre le nombre de film total
  # (le commentaire).
  K.movie <- 100 #dim(u.item)[1]
  K.rating <- 5
  
  #[ P(H) ] Calcul de la probabilite pour chaque film d'avoir comme vote 1, 2.. K.rating
  P.rating <- matrix(0, K.movie, K.rating)
  for (i in 1:K.movie) {
    for (j in 1:K.rating) {
      P.rating[i,j] <- (length(mx[mx$rating==j & !is.na(mx$rating) & mx$item.id==i,'rating']) +1) / (length(mx[mx$item.id==i & !is.na(mx$rating),'rating']) + K.rating)
    }
  }
  #[ P(!H) ] Calcul de la probabilite pour chaque film de ne pas avoir comme vote 1, 2.. K.rating
  P.not.rating <- 1-P.rating
  
  #Initialisation des differentes probabilites conditionnelles 
  P.age.if.rating        <- matrix(0, K.movie, K.rating) # [ P(Age | H) ]
  P.age.if.not.rating    <- matrix(0, K.movie, K.rating) # [ P(Age | !H) ]
  P.gender.if.rating     <- matrix(0, K.movie, K.rating) # [ P(Gender | H) ]
  P.gender.if.not.rating <- matrix(0, K.movie, K.rating) # [ P(Gender | !H) ]
  P.job.if.rating        <- matrix(0, K.movie, K.rating) # [ P(Job | H) ]
  P.job.if.not.rating    <- matrix(0, K.movie, K.rating) # [ P(Job | !H) ]
  
  #Calcul des differentes probabilites conditionnelles 
  for (i in 1:K.movie) {
    for (j in 1:K.rating) {
      cond.rating.age <- mx[mx$item.id==i & mx$rating==j & like.my.age, "rating"]
      P.age.if.rating[i,j] <- (length(cond.rating.age) +1) / (length(mx[mx$item.id==i & mx$rating==j,'rating']) + K.rating)
      cond.not.rating.age <- mx[mx$item.id==i & mx$rating!=j & like.my.age, "rating"]
      P.age.if.not.rating[i,j] <- (length(cond.not.rating.age) +1) / (length(mx[mx$item.id==i & mx$rating!=j,'rating']) + K.rating)
      
      cond.rating.gender <- mx[mx$item.id==i & mx$rating==j & mx$gender==MY.GENDER, "rating"]
      P.gender.if.rating[i,j] <- (length(cond.rating.gender) +1) / (length(mx[mx$item.id==i & mx$rating==j,'rating']) + K.rating)
      cond.not.rating.gender <- mx[mx$item.id==i & mx$rating!=j & mx$gender==MY.GENDER, "rating"]
      P.gender.if.not.rating[i,j] <- (length(cond.not.rating.gender) +1) / (length(mx[mx$item.id==i & mx$rating!=j,'rating']) + K.rating)
      
      cond.rating.job <- mx[mx$item.id==i & mx$rating==j & mx$job==MY.JOB, "rating"]
      P.job.if.rating[i,j] <- (length(cond.rating.job) +1) / (length(mx[mx$item.id==i & mx$rating==j,'rating']) + K.rating)
      cond.not.rating.job <- mx[mx$item.id==i & mx$rating!=j & mx$job==MY.JOB, "rating"]
      P.job.if.not.rating[i,j] <- (length(cond.not.rating.job) +1) / (length(mx[mx$item.id==i & mx$rating!=j,'rating']) + K.rating)
    }
  }
  
  #Calcul des chances de votes par film
  O <- P.rating/P.not.rating * P.age.if.rating/P.age.if.not.rating #* P.gender.if.rating/P.gender.if.not.rating * P.job.if.rating/P.job.if.not.rating
  #Calcul des probabilites de votes par film
  P <- O/(1+O)
  
  #Calcul du vote pese
  weighted.P <- 1:5 %*% t(P)
  
  #Affichage des predictions de films
  u.item[order(weighted.P, decreasing=TRUE)[1:10], 'movie.title']
}

#############################################################################
# QUESTION 1 #
#Regroupement des tables u.data et u.user sur la base de l'identifiant de
#l'utilisateur.
u.data.id.user.rating <- data.frame(u.data[,1], u.data[,3])
colnames(u.data.id.user.rating) <- c("id", "rating")
u.user.id.job <- data.frame(u.user[,1], u.user[,2],u.user[,4])
colnames(u.user.id.job) <- c("id", "age", "job")

merged <- merge(u.data.id.user.rating, u.user.id.job, by="id")

#Calcul de la moyenne des votes des utilisateurs selon leur age
class.age <- cut(merged$age, breaks = 0:8 * 10)
merged.modiv <- merged
merged.modiv$age <- class.age
table.age <- aggregate(merged.modiv[, 2], list(merged.modiv$age), mean)
table.age
# Group.1        x
# 1  (0,10] 3.608108
# 2 (10,20] 3.548002
# 3 (20,30] 3.443404
# 4 (30,40] 3.573333
# 5 (40,50] 3.574118
# 6 (50,60] 3.691524
# 7 (60,70] 3.612121
# 8 (70,80] 3.982143

#Affichage de la moyenne des votes en fonction de l'age de l'utilisateur, 
#les ages etant regroupes en classes
jpeg('q1_votes_moyens_age.jpg')
plot(table.age, breaks = 0:8 * 10, main= "Votes moyens des utilisateurs\n en fonction des classes d'âge", xlab="Ages", ylab="Rating")
dev.off()
# Voir annexe pour observer le graphe

#Calcul de la moyenne des votes des utilisateurs selon leur profession
aggregate(merged[, 2], list(merged$job), mean)
# RESULTAT :
# Group.1        x
# 1  administrator 3.635646
# 2         artist 3.653380
# 3         doctor 3.688889
# 4       educator 3.670621
# 5       engineer 3.541407
# 6  entertainment 3.441050
# 7      executive 3.349104
# 8     healthcare 2.896220
# 9      homemaker 3.301003
# 10        lawyer 3.735316
# 11     librarian 3.560781
# 12     marketing 3.485641
# 13          none 3.779134
# 14         other 3.552377
# 15    programmer 3.568260
# 16       retired 3.466750
# 17      salesman 3.582944
# 18     scientist 3.611273
# 19       student 3.515143
# 20    technician 3.532230
# 21        writer 3.375723

#############################################################################
# QUESTION 2 #
#Calcul des films similaires a Star Trek en terme de vote grace au calcul du cosinus
val.cosinus <- cosinus.vm(m[,450], m) # 450 : id de "Star Trek V: The Final Frontier (1989)"
u.item[order(val.cosinus, decreasing = TRUE)[2:11],2] # affiche les titres des films voisins
# RESULTAT :
# [1] Star Trek: The Motion Picture (1979)          Star Trek VI: The Undiscovered Country (1991)
# [3] Star Trek III: The Search for Spock (1984)    Star Trek IV: The Voyage Home (1986)
# [5] Star Trek: The Wrath of Khan (1982)           Stargate (1994)
# [7] Star Trek: Generations (1994)                 Die Hard 2 (1990)
# [9] Escape from New York (1981)                   Conan the Barbarian (1981)

val.cosinus[order(val.cosinus, decreasing = TRUE)[2:11]] # mesures du cosinus utilisees
# RESULTAT :
# [1] 0.6080518 0.5533220 0.5455677 0.5144135 0.4702339 0.4540497 0.4204334 0.3984260 0.3804155 0.3779706

#Calcul des films similaires a Star Trek en terme de vote grace au calcul des correlations
val.correlation <- correlation(m[,450], m) # 450 : id de "Star Trek V: The Final Frontier (1989)"
u.item[order(val.correlation, decreasing = TRUE)[2:11],2] # affiche les titres des films voisins
# RESULTAT :
# [1] Star Trek: The Motion Picture (1979)          Star Trek VI: The Undiscovered Country (1991)
# [3] Star Trek III: The Search for Spock (1984)    Star Trek IV: The Voyage Home (1986)
# [5] Star Trek: The Wrath of Khan (1982)           Stargate (1994)
# [7] Star Trek: Generations (1994)                 Die Hard 2 (1990)
# [9] Escape from New York (1981)                   Conan the Barbarian (1981)

val.correlation[order(val.correlation, decreasing = TRUE)[2:11]] # mesures de la colleration utilisees
# RESULTAT :
# [1] 0.5786053 0.5164891 0.5075631 0.4718242 0.4202140 0.4090821 0.3735034 0.3424054 0.3348971 0.3295714

## On obtient les mêmes films au final mais les résultats des calcules sont légèrement différents,
## plus faible pour la correlation.

#############################################################################
# QUESTION 3 #
#Prediction des votes des utilisateurs n'ayant pas vote pour le film Star Trek V
#Utilisation de l'approche item-item
estimated <- item.item.pred(m, 450)
table(estimated) #table des predictions attribuees

# RESULTAT : 
# 2.31903458372851 2.32259451153572 2.34565788525071 2.35054195883255 2.35380713238811 2.35439891199862 
# 1                1                5                1                1                1 
# 2.36297090434914  2.3741151128692 2.38059714029759 2.38830387039212 2.40147698878673 2.40208697632784 
# 1                2                1                1                1                1 
# 2.40542904971285 2.40722198059422  2.4073414389081 2.41418168607272  2.4211677816171 2.43517226577514 
# 1                1                1                2                1                2 
# 2.44043384527758  2.4570509670376 2.46349558726748 2.51311519585879 2.59688761710572 2.62845709412036 
# 1                1                1                1               14                1 
# 2.78356834780048 2.79694983738604 2.90019367203554 2.91221148195096 2.95181558936028  2.9638333992757 
# 1               12                1                1                1                1 
# 2.9800483954024 2.98705209844021 2.99701205766636 3.01601130260062 3.05283116120238 3.06963140932224 
# 1                1               11                1                1                1 
# 3.18258927089054 3.19707427794668 3.23228785253462 3.39285608045301   3.397136498227 3.41809052319306 
# 1                5                1                1                2                1 
# 3.55433879996836 3.67623201590713 3.85703230125203 5.10955815319508 
# 1                1                1                1 

#############################################################################
# QUESTION 4 #
#Calcul des erreurs moyennes quadratiques sur la prediction des votes connus
rmse_fun(estimated[m[,450] != 0], m[m[,450] != 0,450])
# RESULTAT :
# [1] 1.068045

#############################################################################
# QUESTION 5 #
# recuperer les film star trek et star wars
indices.star.trek <- grep("trek", as.character(u.item$movie.title), ignore.case=T)
indices.star.wars <- c(172, 181)
indices.stars <- c(indices.star.trek, indices.star.wars)

# vecteur representant les votes de notre nouvel user
u.stars <- rep(0, dim(m)[2]) 
u.stars[indices.star.trek] <- 5
u.stars[indices.star.wars] <- 1

# matrice a laquelle on ajoute notre nouvel user
mod.m <- rbind(m, u.stars)

# Calcul des predictions
pred.n.movies(mod.m, dim(mod.m)[1], 10)
# RESULTAT :
# [1] Titanic (1997)              Contact (1997)              Air Force One (1997)       
# [4] Tomorrow Never Dies (1997)  Anastasia (1997)            Stargate (1994)            
# [7] Mr. Magoo (1997)            Jungle2Jungle (1997)        George of the Jungle (1997)
# [10] Face/Off (1997) 

#############################################################################
# QUESTION 6 #
mx <- merge(u.user, u.data, 1)
mx[mx==0] <- NA

# Parametre que l'on a defini pour la prediction. On peut les changer ici ou directement 
# lors de l'appel de la fonction bayes.
MY.AGE = 21
MY.GENDER = 'M'
MY.JOB = 'engineer'

bayes(mx, MY.AGE = 21, MY.GENDER = 'M', MY.JOB = 'engineer')

# RESULTAT : 
# [1] Fly Away Home (1996)                                             
# [2] Meet John Doe (1941)                                             
# [3] Umbrellas of Cherbourg, The (Parapluies de Cherbourg, Les) (1964)
# [4] Oscar & Lucinda (1997)                                           
# [5] Inspector General, The (1949)                                    
# [6] For Whom the Bell Tolls (1943)                                   
# [7] Sweet Hereafter, The (1997)                                      
# [8] Ghost and Mrs. Muir, The (1947)                                  
# [9] Picnic (1955)                                                    
# [10] Gay Divorcee, The (1934)  

#############################################################################
# QUESTION 7 #
rmse_item_item <- rmse_iter_item(m, percent.item=1, percent.user=5, iter=5)
rmse_item_item
#RESULTAT : 
#    RMSE :     Ecart-type :
#[1] 2.86185517 0.05062506

rmse_user_user <- rmse_iter_user_user(m, percent.item=5, percent.user=1, iter=5)
rmse_user_user
#RESULTAT : 
#     RMSE :     Ecart-type :
#[1]  3.44533775 0.03947685