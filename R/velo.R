#' Filtrer les anomalies
#'
#' Cette fonction filtre les lignes correspondant aux anomalies faibles ou fortes
#' dans un jeu de données de trajets vélo.
#'
#' @param df Un data.frame contenant les trajets vélos avec les colonnes suivantes :
#' \describe{
#'   \item{Probabilité.de.présence.d.anomalies}{Niveau d'anomalie ("Faible", "Forte", etc.)}
#' }
#'
#' @return Un data.frame sans les observations présentant des anomalies faibles ou fortes.
#' @export
filtre_anomalie <- function(df){
  df_filtre <- df[!(df$Probabilité.de.présence.d.anomalies %in% c("Faible", "Forte")), ]
  return(df_filtre)
}


#' Compter le nombre total de trajets
#'
#' Cette fonction calcule le nombre total de trajets.
#'
#' @param df Un data.frame contenant les trajets vélos avec les colonnes suivantes :
#' \describe{
#'   \item{Total}{Nombre de passages enregistrés}
#' }
#'
#' @return Un entier correspondant au nombre total de trajets.
#' @export
compter_nombre_trajets <- function(df){
  return(sum(df$Total, na.rm = TRUE))
}


#' Compter le nombre de boucles de comptage
#'
#' Cette fonction calcule le nombre de boucles de comptage distinctes.
#'
#' @param df Un data.frame contenant les trajets vélos avec les colonnes suivantes :
#' \describe{
#'   \item{Boucle.de.comptage}{Identifiant ou nom de la boucle de comptage}
#' }
#'
#' @return Un entier correspondant au nombre de boucles uniques.
#' @export
compter_nombre_boucle <- function(df){
  nb_boucle <- length(unique(df$Boucle.de.comptage))
  return(nb_boucle)
}


#' Trouver le trajet avec le maximum de passages
#'
#' Cette fonction identifie le trajet ayant le plus grand nombre de passages.
#'
#' @param df Un data.frame contenant les trajets vélos avec les colonnes suivantes :
#' \describe{
#'   \item{Total}{Nombre de passages}
#'   \item{Boucle.de.comptage}{Nom de la boucle}
#'   \item{Jour}{Jour du relevé}
#' }
#'
#' @return Un data.frame avec les colonnes :
#' \describe{
#'   \item{nom}{Nom de la boucle de comptage}
#'   \item{jour}{Jour correspondant}
#'   \item{nb_passage}{Nombre maximal de passages}
#' }
#' @export
trouver_trajet_max <- function(df){
  max_id <- which.max(df$Total)

  resultat <- data.frame(
    nom = df$Boucle.de.comptage[max_id],
    jour = df$Jour[max_id],
    nb_passage = df$Total[max_id]
  )

  return(resultat)
}


#' Calculer la distribution des trajets par jour de la semaine
#'
#' Cette fonction calcule le nombre total de trajets pour chaque jour de la semaine.
#'
#' @param df Un data.frame contenant les trajets vélos avec les colonnes suivantes :
#' \describe{
#'   \item{Jour.de.la.semaine}{Jour de la semaine}
#'   \item{Total}{Nombre de passages}
#' }
#'
#' @return Un data.frame contenant :
#' \describe{
#'   \item{Jour.de.la.semaine}{Jour de la semaine}
#'   \item{nb_trajets}{Nombre total de trajets}
#' }
#'
#' @importFrom dplyr group_by summarise
#' @export
calcul_distribution_semaine <- function(df){
  nb_trajet <- df |>
    dplyr::group_by(Jour.de.la.semaine) |>
    dplyr::summarise(nb_trajets = sum(Total, na.rm = TRUE))

  return(nb_trajet)
}


#' Représenter la distribution hebdomadaire des trajets
#'
#' Cette fonction génère un graphique en barres représentant le nombre
#' de trajets par jour de la semaine.
#'
#' @param df Un data.frame contenant les trajets vélos avec les colonnes suivantes :
#' \describe{
#'   \item{Jour.de.la.semaine}{Jour de la semaine}
#'   \item{Total}{Nombre de passages}
#' }
#'
#' @return Un objet graphique (barplot).
#' @export
plot_distribution_semaine <- function(df){

  graph <- barplot(
    tapply(df$Total, df$Jour.de.la.semaine, sum),
    xlab = "Jour de la semaine",
    ylab = "Nombre de trajets par jour",
    col = "royalblue"
  )

  return(graph)
}
