# Situation épidémiologique de l'Ituri : rapport automatisé et analyse pour publication

Une seule commande produit le rapport hebdomadaire, l'article et son supplément à partir de la base IDS de la DPS.

| Sortie | Fichier |
|---|---|
| Rapport hebdomadaire (Word) | `outputs/hebdo/Rapport_Ituri_S##.docx` |
| Projet d'article, en anglais (Word) | `outputs/article/Article_Ebola_Ituri.docx` |
| Supplément de l'article (Word) | `outputs/article/Supplement_Ebola_Ituri.docx` |

## Essai rapide, sans données réelles

Les données ne sont pas versionnées (voir [data/README.md](data/README.md)). Pour tout exécuter sur une base synthétique :

```
Rscript -e "install.packages('renv'); renv::restore()"
Rscript run_all.R --demo
```

Les sorties vont dans `outputs/demo/` et portent la mention « données synthétiques ». Ce contrôle tourne aussi à chaque envoi via [GitHub Actions](.github/workflows/verification.yml). Il prouve que le code s'exécute, pas que les résultats réels sont reproduits : cela exige les données de la DPS.

## Chaque semaine

1. Déposer la nouvelle base dans `data/raw/`, sans la modifier : `Base_DPS_Ituri_Week44.xlsx`.
2. Si disponibles, compléter :
   - `data/external/mve_provinces.csv` et `mve_zones.csv` : nouveau sitrep (lignes ajoutées, anciennes conservées) ;
   - `data/external/completude_as.csv` : complétude par aire de santé ;
   - `notes/S44.md` : informations de terrain, texte libre.
3. Lancer :

```
Rscript run_all.R --cible=hebdo --semaine=44
```

4. Relire le rapport, puis le diffuser. Il indique lui-même ce qui manque, par exemple un sitrep trop ancien.

## Reproduire l'article

```
Rscript -e "renv::restore()"   # une seule fois : installe les versions exactes
# placer les données selon data/README.md
Rscript run_all.R --cible=tests
Rscript run_all.R --cible=article
```

Le premier calcul prend plusieurs minutes ; le résultat est ensuite mis en cache dans `outputs/modeles/`. Il n'est recalculé que si les données, la configuration ou le code changent. `--forcer` impose un recalcul. La graine aléatoire est fixée dans `config.yml`.

Après une mise à jour des données, les chiffres de l'article changent seuls, mais pas les phrases d'interprétation. L'article affiche un avertissement tant que `SEM_VERROU` (début de `rapports/article.Rmd`) n'a pas été mis à jour après relecture.

## Organisation

| Élément | Rôle |
|---|---|
| `config.yml` | Tous les paramètres : seuils, zones MSF, résultats analysés, règles de nettoyage |
| `run_all.R` | Point d'entrée |
| `R/donnees.R` | Lecture, validation, nettoyage journalisé |
| `R/donnees_synthetiques.R` | Base factice du mode `--demo` |
| `R/indicateurs.R` | Indicateurs du rapport |
| `R/modeles.R` | Modèles de l'article |
| `R/figures.R`, `R/modele_docx.R` | Figures et modèles Word |
| `rapports/` | Sources R Markdown et bibliographie |
| `data/raw/` | Bases brutes, jamais modifiées |
| `data/external/` | Données transcrites à la main, absentes de la base |
| `data/corrections_manuelles.csv` | Corrections validées par la DPS, appliquées avant le nettoyage et traçables |
| `tests/testthat/` | Contrôles automatiques |
| `docs/` | Plan d'analyse, liste de contrôle STROBE/RECORD |
| `.github/workflows/` | Exécution automatique du mode `--demo` |

## Hors dépôt

Bases brutes, PDF, transcriptions, notes de terrain, rapports générés et logo MSF restent uniquement en local (`.gitignore`). Avant de rendre ce dépôt public : obtenir l'autorisation de la DPS et de MSF, choisir une licence, puis relire `docs/` et `rapports/article.Rmd`.

## Limites à connaître

- **Pas de comparaison à 2025 calculée :** la base 2025 n'est pas disponible. Les cumuls 2025 ne s'affichent que pour S32, depuis le rapport transcrit.
- **Sitrep MVE :** saisie manuelle ; un seul sitrep est disponible pour l'instant.
- **Cartes :** aucune, faute de limites de zones de santé.
- **Positivité du paludisme :** rapport confirmés/suspects, car le nombre de patients testés n'est pas dans la base.
- **Seuil méningite et seuil rougeole :** à valider avec la DPS (`config.yml`).
