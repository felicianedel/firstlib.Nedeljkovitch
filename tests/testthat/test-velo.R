library(testthat)

test_that("filtre_anomalie enleve Faible et Forte", {
  df <- data.frame(
    "Probabilité de présence d'anomalies" = c("Faible", "Forte", "Moyenne"),
    check.names = FALSE
  )

  res <- filtre_anomalie(df)

  expect_equal(nrow(res), 1)
})

test_that("compter_nombre_trajets calcule correctement", {
  df <- data.frame(Total = c(10, 20, 30))
  expect_equal(compter_nombre_trajets(df), 60)
})

test_that("compter_nombre_boucle compte les boucles uniques", {
  df <- data.frame(
    "Boucle de comptage" = c("A", "A", "B", "C"),
    check.names = FALSE
  )

  expect_equal(compter_nombre_boucle(df), 3)
})

test_that("trouver_trajet_max trouve le max", {
  df <- data.frame(
    Total = c(10, 50, 30),
    "Boucle de comptage" = c("A", "B", "C"),
    Jour = c("Lundi", "Mardi", "Mercredi"),
    check.names = FALSE
  )

  res <- trouver_trajet_max(df)

  expect_equal(res$nb_passage, 50)
  expect_equal(res$nom, "B")
})

test_that("calcul_distribution_semaine fonctionne", {
  df <- data.frame(
    "Jour de la semaine" = c("Lundi", "Lundi", "Mardi"),
    Total = c(10, 20, 30),
    check.names = FALSE
  )

  res <- calcul_distribution_semaine(df)

  expect_equal(nrow(res), 2)
})

test_that("filtrer_trajet filtre correctement", {
  df <- data.frame(
    "Numéro de boucle" = c(767, 768, 880),
    check.names = FALSE
  )

  res <- filtrer_trajet(df, c(767, 768))

  expect_equal(nrow(res), 2)
})

test_that("filtrer_trajet renvoie tout si boucles est NULL", {
  df <- data.frame(
    "Numéro de boucle" = c(767, 768, 880),
    check.names = FALSE
  )

  res <- filtrer_trajet(df, NULL)

  expect_equal(nrow(res), nrow(df))
})
