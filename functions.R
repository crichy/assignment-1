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
#création de la variable result par défaut. On ajoute le nombre de la case suivante à la case précédente. 
#Le premier résultat: rien n'a été additionné, donc il s'agit de 0
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


# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable,
#               provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var, x=grouping_var, fill=grouping_var))
                                              
# YOUR CODE HERE: Create a violin plot
p <- p + ggplot2::geom_violin()
  
return(p) 
}

p <- grouped_violin_plot(iris, "Sepal.Length", "Species")
# Pour changer les couleurs il faut appeler la fonction "remplissage" de couleurs chargée depuis
# le package ggplot2, et choisir n'importe quel modèle donné soit dans la rubrique "help" soit
# dans le tutoriel http://r-statistics.co/Complete-Ggplot2-Tutorial-Part1-With-R-Code.html#5.%20Changing%20the%20color%20and%20size%20of%20points
p <- p + ggplot2::scale_fill_brewer(palette = "Spectral")
# Puis on donne un titre grâce à la fonction labs() chargée depuis le même package "Iris data".
p <- p + ggplot2::labs(title= "Iris data") 
print(p)

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  #création de nouvelles variables d1_var & d2_var, qui sont des vecteurs qui ont une valeur par
  #défaut 0, et qui ont un nombre de rangs correspondant à chaque groupe 1 et 2.
  d1_var <- rep(0, nrow(d_1))
  d2_var <- rep(0, nrow(d_2))
  #création de deux boucles identiques qui assignent toutes les valeurs comprises dans notre 
  #pour chaque rang i dans la colonne var. Nous aurons donc deux groupes: d1_var rempli et
  #d2_rempli aussi.
  
  for (i in 1:nrow(d_1)){
    d1_var[i] <- d_1[i, var]
  } 
  # le code marche et trouve les bons résultats, mais il n'y a pas le message warning.
  for (i in 1:nrow(d_2)){
  d2_var[i] <- d_2[i, var]
  }
  result<- median(d1_var) - median(d2_var)
  return(result)
}
  
# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
#      provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  n <- nrow(d[[var]])
  d[[var]] <- sample(d[[var]], replace = F)
    return(d) 
  }

# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
#      provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
#            a data frame, the name of a variable on which to calculate the
#            test statistic, the name of a grouping variable, the value of
#            the grouping variable corresponding to the first group, and
#            the value of the grouping variable corresponding to the second
#            group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    #                 fill in the vector permutation_statistics with the
    #                 value of statistic(...) for this new permutation
  new_d <- randomize(d, var)  
  permutation_statistics[i] <- statistic(new_d, var, grouping_var, group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}
  