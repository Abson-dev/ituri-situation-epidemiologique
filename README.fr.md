# Surveillance épidémiologique de l'Ituri : rapport automatisé et analyse reproductible

[English version](README.md)

Une seule commande produit le rapport hebdomadaire et l'analyse à partir de la base IDS de la DPS.

## Essai rapide, sans données réelles

```
Rscript -e "install.packages('renv'); renv::restore()"
Rscript run_all.R --demo
```

Les sorties vont dans `outputs/demo/` et portent la mention « données synthétiques ».

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

## Reproduire l'analyse

```
Rscript -e "renv::restore()"
# placer les données selon data/README.md
Rscript run_all.R --cible=tests
Rscript run_all.R --cible=article
```

Le premier calcul prend plusieurs minutes ; le résultat est ensuite mis en cache dans `outputs/modeles/`. Il n'est recalculé que si les données, la configuration ou le code changent.

## Fichiers locaux, hors dépôt

Ces fichiers restent sur le poste de travail :

| Fichier | Contenu |
|---|---|
| `config.local.yml` | Auteur du rapport et zones d'intervention ; complète `config.yml` |
| `data/raw/`, `data/external/` | Bases et transcriptions |
| `notes/` | Notes de terrain |
| `rapports/article.Rmd` | Texte du manuscrit |
| `outputs/` | Rapports générés |
| `assets/logo_msf.png` | Logo |

Après une mise à jour des données, les chiffres de l'article changent seuls, mais pas les phrases d'interprétation. L'article affiche un avertissement tant que `SEM_VERROU` n'a pas été mis à jour après relecture.

## Limites à connaître

- **Pas de comparaison à 2025 calculée :** la base 2025 n'est pas disponible.
- **Sitrep MVE :** saisie manuelle.
- **Cartes :** aucune, faute de limites de zones de santé.
- **Positivité du paludisme :** rapport confirmés/suspects, car le nombre de patients testés n'est pas dans la base.
- **Seuils méningite et rougeole :** à valider avec la DPS (`config.yml`).
