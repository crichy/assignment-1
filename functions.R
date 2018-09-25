# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string

# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  x <- d[[var]] # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (!is.null(x)) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
  if (is.numeric(x)){
     result <- sum(x)
  }
  }
    # You will need to do a bit of research in order to figure out how
    # to test whether a vector contains numbers.
  # YOUR CODE HERE: return the result
  return(result)
}

# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL

# création de la fonction my_sum qui va ajouter les valeurs au vecteur
  my_sum <- function(x) {
# Vérifier que x n'est pas vide  
  if (!is.null(x)){
# Vérifier que x contient des chiffres 
  if (is.numeric(x)){
#création de la variable result par défaut. On ajoute le nombre de la case suivante à la case pré-
#cédente. Le premier résultat: rien n'a été additionné, donc il s'agit de 0
  result <- 0
# Pour chaque case i de x: 
  for (i in x) {
# La variable result devient result additionné au nombre dans le i suivant
       result <- result + i
  }
#Return le résultat mis à jour
    return(result)
  }
  }
#si les conditions précédentes ne sont pas respectée, la fonction returns NULL
      else 
  return(NULL)  
}

# création de la fonction sum_divided_by
  sum_divided_by <- function(x,k) {
#création d'une variable division que l'on va pouvoir appeler dans la fonction my_sum plus tard
# division divise le vecteur x par le nombre k.
# le vecteur et le nombre ont été convertis en valeurs numériques grâce à as.numeric
  division <- as.numeric(x) / as.numeric(k)
#result est la variable ayant le résultat final: à savoir la somme des divisions.
# la somme des nouveaux x (les divisions) est faite grâce la fonction my_sum
  result <- my_sum(division)
#si x ou k n'est pas numérique  
  if (!is.numeric(x) | !is.numeric(k)) {
#la sortie sera NULL    
    return(NULL)
  }
#si x et k sont numériques, la sortie sera la somme des divisions.  
  else {
    return(result)
  }
  }
  


#Création de la fonction my_mean, qui ne prend qu'un argument (x)
my_mean <- function(x){
#Comme sum_divided_by divise la somme des x par k, il suffit de remplacer le second argument de
# sum_divided_by (k) par le nombre d'occurrences de x, à savoir sa longueur.
result <- sum_divided_by(x,length(x))  
#Si une valeur n'est pas numérique pour x, le résultat sera NULL
if (!is.numeric(x)) {
  return(NULL)
}
#sinon, le résultat sera la moyenne.
else {
  return(result)
}
 
}

  
  
  
  
  
  