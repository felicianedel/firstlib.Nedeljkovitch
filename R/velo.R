# Filtre anomalie ----
filtre_anomalie <- function(df){
  df_filtre <- df[!(df$Probabilité.de.présence.d.anomalies =="Faible"|
                      df$Probabilité.de.présence.d.anomalies =="Forte"),]
  return(df_filtre)
}

# Compter nombre trajtes ----
compter_nombre_trajets <- function(df){
  return(sum(df$Total))
}

# Compter nombre boucle ----
compter_nombre_boucle <- function(df){
  nb_boucle<- length(unique(df$Boucle.de.comptage))
  return(nb_boucle)
}

# Trouver trajet max ----
trouver_trajet_max <- function(df){
  max <- which.max(df$Total)
  nom <- df$Boucle.de.comptage[max]
  jour <- df$Jour[max]
  nb_passage <-df$Total[max]

  resultat <- data.frame(
    nom=nom,
    jour=jour,
    nb_passage=nb_passage)


  return(resultat)
}

# Calcul distribution semaine ----
library(dplyr)
calcul_distribution_semaine <- function(df){

  nb_trajet <- df |>
    group_by(Jour.de.la.semaine) |>
    summarise(nb_trajets = sum(Total, na.rm = TRUE))
  return(nb_trajet)
}

# Plot distribution semaine ----
plot_distribution_semaine <- function(df){
  calcul_distribution_semaine(df)
  graph <- barplot(tapply(df$Total,
                          df$Jour.de.la.semaine,sum),
                   xlab="Jour de la semaine",
                   ylab="Nombre de trajes par jour",
                   col="royalblue")
  return(graph)
}


