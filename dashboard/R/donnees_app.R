# Chargement au démarrage et calculs propres au tableau de bord.

PROVINCE <- "__PROVINCE__"

charger_app <- function() {
  # Sans base réelle (clone du dépôt), l'application démarre sur les données synthétiques.
  reel <- length(list.files(here::here("data/raw"), pattern = "\\.xlsx$")) > 0
  if (!reel) Sys.setenv(ITURI_DEMO = "1")
  cfg <- lire_config()
  if (mode_demo() && !dir.exists(here::here("data/synthetic/raw"))) generer_donnees_synthetiques(cfg)
  ids <- nettoyer_ids(trouver_base(cfg), cfg)
  list(cfg = cfg, ids = ids, demo = mode_demo(), S_max = ids$semaine_max,
       zones = sort(unique(ids$donnees$zs)), expo = exposition_zones(ids, cfg),
       modele = charger_modele(ids))
}

# Résultats déjà calculés par run_all.R pour cette même base, sans jamais lancer de calcul long ici.
charger_modele <- function(ids) {
  fichiers <- list.files(here::here("outputs/modeles"), pattern = "\\.rds$", full.names = TRUE)
  fichiers <- fichiers[order(file.mtime(fichiers), decreasing = TRUE)]
  for (f in fichiers) {
    r <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.null(r) && identical(r$sha256, ids$sha256)) return(r)
  }
  NULL
}

choix_semaines <- function(app) {
  sem <- rev(sort(unique(app$ids$donnees$semaine)))
  etiq <- paste0("S", sem, " (", format(date_semaine(sem, app$cfg), "%d/%m"), ")")
  stats::setNames(sem, etiq)
}

choix_maladies <- function(app) {
  codes <- c(unlist(app$cfg$tableau_principal), unlist(app$cfg$tableau_autres))
  codes <- codes[codes %in% unique(app$ids$donnees$maladie)]
  stats::setNames(codes, libelle(codes, app$cfg))
}

choix_zones <- function(app) {
  msf <- intersect(unlist(app$cfg$zones_msf), app$zones)
  autres <- setdiff(app$zones, msf)
  list(`Zones d'intervention MSF` = stats::setNames(msf, nom_zone(msf)),
       `Autres zones` = stats::setNames(autres, nom_zone(autres)))
}

# Évolution sur les zones ayant rapporté les deux semaines : un total provincial brut baisse
# mécaniquement lorsque des zones n'ont pas encore rapporté.
evolution_comparable <- function(d, code, S) {
  rapportees <- function(s) unique(d$zs[d$semaine == s])
  communes <- intersect(rapportees(S), rapportees(S - 1L))
  x <- d[d$maladie == code & d$zs %in% communes, ]
  a <- sum(x$cas[x$semaine == S])
  b <- sum(x$cas[x$semaine == S - 1L])
  list(valeur = sum(d$cas[d$maladie == code & d$semaine == S]),
       variation = if (b > 0) (a - b) / b else NA_real_, n_zones = length(communes))
}

texte_evolution <- function(e, S) {
  if (is.na(e$variation)) return("Pas de comparaison possible")
  sprintf("%s%s par rapport à S%d, sur %d zones comparables",
          if (e$variation >= 0) "+" else "−", pct_fr(abs(e$variation)), S - 1L, e$n_zones)
}

serie_cible <- function(d, code, S, zone = NULL) {
  province <- is.null(zone) || zone == PROVINCE
  if (!province) d <- d[d$zs == zone, ]
  x <- serie_province(d, code, S)
  if (province) x$zones <- zones_par_semaine(d, x$semaine)
  x
}

zones_par_semaine <- function(d, semaines) {
  n <- tapply(d$zs, d$semaine, dplyr::n_distinct)
  as.integer(n[as.character(semaines)])
}

# Points d'attention de la semaine, du plus actionnable au plus général.
points_attention <- function(ids, S, app) {
  d <- ids$donnees
  cfg <- app$cfg
  sortie <- list()
  ajouter <- function(niveau, titre, detail) {
    sortie[[length(sortie) + 1L]] <<- list(niveau = niveau, titre = titre, detail = detail)
  }
  rg <- alertes_rougeole(d, S, cfg)
  if (nrow(rg)) ajouter("critique", "Rougeole : seuil d'alerte franchi", enumerer(paste0(nom_zone(rg$zs), " (", rg$cas_S, " cas)")))
  mg <- alertes_meningite(d, S, cfg)
  if (nrow(mg)) ajouter("critique", "Méningite : seuil franchi", enumerer(paste0(nom_zone(mg$zs), " (", mg$statut, ")")))
  rup <- ids$ruptures |> dplyr::filter(!is.na(debut_rupture)) |> dplyr::arrange(debut_rupture)
  if (nrow(rup)) {
    ajouter("serieux", sprintf("Paludisme : confirmation interrompue dans %d zones", nrow(rup)),
            paste0(enumerer(nom_zone(rup$zs)), ". Rupture de TDR ou de déclaration à vérifier."))
  }
  absentes <- setdiff(app$zones, unique(d$zs[d$semaine == S]))
  if (length(absentes)) {
    ajouter("attention", sprintf("%d zone%s sans donnée en S%d", length(absentes), if (length(absentes) > 1) "s" else "", S), enumerer(nom_zone(absentes)))
  }
  sit <- dernier_sitrep(S, cfg)
  if (!is.null(sit) && sit$age_sem >= 2) {
    ajouter("attention", "Sitrep Ebola ancien", sprintf("Dernier sitrep : %s. Mettre à jour data/external/.", format(sit$date, "%d/%m/%Y")))
  }
  if (!length(sortie)) ajouter("ok", "Aucun point d'attention automatique", "Vérification de routine conseillée.")
  sortie
}
