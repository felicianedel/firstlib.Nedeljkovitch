library(testthat)

# Test filtre_anomalie
test_that("filtre_anomalie enleve Faible et Forte", {
  df <- data.frame(
    Probabilite.de.presence.d.anomalies = c("Faible", "Forte", "Moyenne")
  )

  res <- filtre_anomalie(df)

  expect_equal(nrow(res), 1)
})

# Test compter_nombre_trajets
test_that("compter_nombre_trajets calcule correctement", {
  df <- data.frame(Total = c(10, 20, 30))

  expect_equal(compter_nombre_trajets(df), 60)
})


# Test compter_nombre_boucle
test_that("compter_nombre_boucle compte les boucles uniques", {
  df <- data.frame(Boucle.de.comptage = c(1, 1, 2, 3))

  expect_equal(compter_nombre_boucle(df), 3)
})


# Test trouver_trajet_max
test_that("trouver_trajet_max trouve le max", {
  df <- data.frame(
    Total = c(10, 50, 30),
    Boucle.de.comptage = c(1, 2, 3),
    Jour = c("Lundi", "Mardi", "Mercredi")
  )

  res <- trouver_trajet_max(df)

  expect_equal(res$nb_passage, 50)
})


# Test calcul_distribution_semaine
test_that("calcul_distribution_semaine fonctionne", {
  df <- data.frame(
    Jour.de.la.semaine = c("Lundi", "Lundi", "Mardi"),
    Total = c(10, 20, 30)
  )

  res <- calcul_distribution_semaine(df)

  expect_true(nrow(res) > 0)
})


# Test filtrer_trajet
test_that("filtrer_trajet filtre correctement", {
  df <- data.frame(Boucle.de.comptage = c("767", "769"))

  res <- filtrer_trajet(df, c("767", "768"))

  expect_equal(nrow(res), 2)
})




