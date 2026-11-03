# Tableau de bord

## Lancer

Double-cliquer sur **`Lancer_tableau_de_bord.bat`**. Le navigateur s'ouvre ; gardez la fenêtre noire ouverte et fermez-la pour arrêter. Au premier lancement, les paquets manquants s'installent automatiquement.

L'application n'est accessible que depuis cet ordinateur. Sans base réelle, elle démarre sur des données synthétiques, signalées par un bandeau. `Lancer_tableau_de_bord.bat --demo` force ce mode.

## Publier avec connexion

Le lancement par le `.bat` reste sans connexion, car il n'est accessible que depuis cet ordinateur. Toute autre exécution exige une connexion et refuse de démarrer sans base de comptes.

**Une seule fois,** configurer le compte shinyapps.io avec le jeton de sa page *Tokens* :

```
Rscript -e "rsconnect::setAccountInfo(name='COMPTE', token='JETON', secret='SECRET')"
```

**Créer les comptes.** Cette commande remplace toute la base, donc il faut lister tous les comptes à conserver :

```
Rscript dashboard/gerer_comptes.R prenom.nom autre.compte
```

Les mots de passe sont dans `dashboard/comptes_mots_de_passe.csv`. Transmettez-les individuellement, par un canal sûr, puis supprimez ce fichier.

**Publier :**

```
Rscript dashboard/publier.R            # affiche ce qui serait envoyé
Rscript dashboard/publier.R --publier  # envoie
```

Pour changer de mots de passe ou mettre à jour les données, recommencer les deux dernières étapes. Les données réelles et les résultats provisoires sont visibles par tout compte connecté.

## Pages

| Page | Question à laquelle elle répond |
|---|---|
| Vue d'ensemble | Que faut-il regarder en priorité cette semaine ? |
| Maladies | Comment évolue une maladie, où, et avec quelle incidence ? |
| Zones de santé | Quelle est la situation d'une zone donnée ? |
| Paludisme | La tendance reflète-t-elle la maladie ou une rupture de confirmation ? |
| Ebola | Que dit le dernier sitrep, et concorde-t-il avec l'IDS ? |
| Qualité des données | Quelles zones manquent, et quels points arbitrer avec la DPS ? |
| Analyse d'impact | Quels résultats sont solides ? (nécessite `Rscript run_all.R --cible=article`) |
| Données | Filtrer et exporter en CSV ou Excel |

Le sélecteur de semaine s'applique à toutes les pages, sauf « Analyse d'impact », calculée sur toute la base. Le bouton **Rapport Word** génère le rapport de la semaine choisie.

## Mettre à jour

Déposer la nouvelle base dans `data/raw/`, puis relancer : la base la plus récente est utilisée automatiquement.

## Organisation

```text
dashboard/
├── Lancer_tableau_de_bord.bat   Lanceur Windows
├── lancer.R                     Prépare l'environnement et démarre
├── app.R                        Navigation et sélecteur de semaine
├── R/donnees_app.R              Chargement et calculs
├── R/graphiques.R               Graphiques et tableaux
├── R/identite.R                 Logo, en-têtes de page et chiffres clés
├── R/mod_*.R                    Une page par fichier
└── www/style.css                Présentation
```

Le logo MSF (`assets/logo_msf.png`) n'est pas versionné. Il s'affiche seulement sur les données réelles, si le fichier est présent : la démonstration n'en a pas.

Le nettoyage et les indicateurs viennent de `../R/` : tableau de bord et rapports affichent les mêmes chiffres.
