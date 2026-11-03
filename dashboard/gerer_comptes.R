# Crée ou remplace la base des comptes de connexion.
#   Rscript dashboard/gerer_comptes.R prenom.nom autre.compte
#
# Remplace TOUTE la base : indiquer tous les comptes à conserver. Chaque compte reçoit un
# nouveau mot de passe aléatoire, écrit dans dashboard/comptes_mots_de_passe.csv et jamais
# affiché. Transmettre chaque mot de passe individuellement, puis supprimer ce fichier.
# Rien de tout cela n'est versionné (voir .gitignore).

comptes <- commandArgs(trailingOnly = TRUE)
if (!length(comptes)) stop("Indiquer au moins un nom de compte.", call. = FALSE)
if (anyDuplicated(comptes)) stop("Noms de compte en double.", call. = FALSE)
if (any(!grepl("^[A-Za-z0-9._-]{3,}$", comptes))) stop("Noms : au moins 3 caractères, lettres, chiffres, point, tiret.", call. = FALSE)

aleatoire <- function(n) {
  alphabet <- c(letters, LETTERS, 0:9)
  # Rejet des octets hors plage : tirage uniforme sur l'alphabet.
  octets <- as.integer(openssl::rand_bytes(n * 4L))
  octets <- octets[octets < 248L]
  paste(alphabet[octets[seq_len(n)] %% 62L + 1L], collapse = "")
}

fichier_env <- here::here(".Renviron")
secret <- Sys.getenv("ITURI_COMPTES_SECRET")
if (!nzchar(secret)) {
  secret <- aleatoire(48L)
  cat(sprintf("ITURI_COMPTES_SECRET=%s\n", secret), file = fichier_env, append = TRUE)
}

mots <- vapply(comptes, function(.) aleatoire(20L), character(1))
base <- here::here("dashboard/comptes.sqlite")
if (file.exists(base)) file.remove(base)
shinymanager::create_db(
  credentials_data = data.frame(user = comptes, password = unname(mots), admin = FALSE, stringsAsFactors = FALSE),
  sqlite_path = base, passphrase = secret
)
utils::write.csv(data.frame(compte = comptes, mot_de_passe = unname(mots)),
                 here::here("dashboard/comptes_mots_de_passe.csv"), row.names = FALSE)
message(length(comptes), " compte(s) créé(s). Mots de passe : dashboard/comptes_mots_de_passe.csv (à supprimer après transmission).")
