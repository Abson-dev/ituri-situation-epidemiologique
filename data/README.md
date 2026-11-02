# Données

**Aucune donnée réelle n'est versionnée.** La base IDS appartient à la DPS Ituri ; sa diffusion requiert son autorisation. Sans données réelles, `Rscript run_all.R --demo` exécute le pipeline complet sur une base synthétique générée dans `data/synthetic/`.

## Pour reproduire l'analyse réelle

Après autorisation, placer les fichiers ainsi :

| Fichier | Contenu | Source |
|---|---|---|
| `raw/Base_DPS_Ituri_Week43.xlsx` | Base IDS hebdomadaire | DPS Ituri |
| `external/mve_zones.csv` | Cas et décès confirmés de MVE par zone | Sitrep INSP-COUSP n092 |
| `external/mve_provinces.csv` | Idem par province | Sitrep INSP-COUSP n092 |
| `external/rapport_s32_reference.csv` | Tableau du rapport MSF S32, pour le rapprochement | MSF |
| `external/completude_as.csv` | Complétude par aire de santé | MSF |
| `corrections_manuelles.csv` | Corrections validées par la DPS | DPS |
| `external/controles_reels.yml` | Valeurs de contrôle des tests sur données réelles | Calculées |

Empreinte SHA-256 de la base utilisée pour l'analyse :

```
608acc91bf96e17509484d5cb40f99bc55379c5299e2db85c95a88234296504b
```

## Structure de la base

Une ligne par zone-semaine-maladie. Colonnes : `num`, `pays`, `prov`, `zs`, `pop`, `numsem`, `debutsem`, `maladie` ; cas par âge `c328tnn`, `c011mois`, `c1259mois`, `c515ans`, `cp15ans` ; décès par âge `dtnn`, `d011mois`, `d1259mois`, `d515ans`, `dp15ans` ; `totalcas`, `totaldeces`. La lecture échoue explicitement si une colonne manque.
