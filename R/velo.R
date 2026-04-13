#' Filtrer les anomalies
#'
#' Supprime les lignes avec anomalies faibles ou fortes.
#'
#' @param df Un data.frame.
#' @return Un data.frame filtre.
#' @export
filtre_anomalie <- function(df){
  df_filtre <- df[!(df$Probabilite.de.presence.d.anomalies %in% c("Faible", "Forte")), ]
  return(df_filtre)
}


#' Compter le nombre de trajets
#'
#' @param df Un data.frame.
#' @return Un entier.
#' @export
compter_nombre_trajets <- function(df){
  return(sum(df$Total, na.rm = TRUE))
}


#' Compter le nombre de boucles
#'
#' @param df Un data.frame.
#' @return Un entier.
#' @export
compter_nombre_boucle <- function(df){
  return(length(unique(df$Boucle.de.comptage)))
}


#' Trouver le trajet maximum
#'
#' @param df Un data.frame.
#' @return Un data.frame.
#' @export
trouver_trajet_max <- function(df){
  max_id <- which.max(df$Total)

  data.frame(
    nom = df$Boucle.de.comptage[max_id],
    jour = df$Jour[max_id],
    nb_passage = df$Total[max_id]
  )
}


#' Distribution par jour
#'
#' @param df Un data.frame.
#' @return Un data.frame.
#' @importFrom dplyr group_by summarise
#' @importFrom rlang .data
#' @export
calcul_distribution_semaine <- function(df){
  df |>
    dplyr::group_by(.data$Jour.de.la.semaine) |>
    dplyr::summarise(nb_trajets = sum(.data$Total, na.rm = TRUE))
}


#' Plot distribution
#'
#' @param df Un data.frame.
#' @return Un graphique.
#' @importFrom graphics barplot
#' @export
plot_distribution_semaine <- function(df){

  barplot(
    tapply(df$Total, df$Jour.de.la.semaine, sum, na.rm = TRUE),
    xlab = "Jour",
    ylab = "Nombre",
    col = "royalblue"
  )
}
#' Filtrer les trajets par boucle
#'
#' @param df Un data.frame.
#' @param boucles Un vecteur de numeros de boucle.
#' @return Un data.frame filtre.
#' @export
filtrer_trajet <- function(df, boucles) {
  df[as.character(df$Numero.de.boucle) %in% as.character(boucles), , drop = FALSE]
}
