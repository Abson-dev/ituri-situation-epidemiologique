# Plan d'analyse statistique

**Statut :** rédigé après une première exploration des données, non enregistré. Pour renforcer la crédibilité : enregistrer ce plan (OSF) **avant** d'obtenir la base 2025, puis l'utiliser comme test de confirmation.

## Question

La morbidité courante notifiée a-t-elle diminué davantage dans les zones de santé les plus touchées par la MVE après la déclaration du 15 mai 2026 ?

## Plan d'étude

Série temporelle interrompue contrôlée, à exposition continue. Unité : zone de santé-semaine. 36 zones, semaines 1 à 43 de 2026.

| Élément | Choix | Justification |
|---|---|---|
| Rupture | S20 | Semaine de la déclaration |
| Exposition | log(1 + cas confirmés cumulés de MVE / 10 000), sitrep n092 | Évite un seuil arbitraire |
| Contraste rapporté | 10/10 000 contre 0 | Lisible et dans la plage observée |
| Résultats principaux | Paludisme suspect, IRA, pneumonie, diarrhée < 5 ans, typhoïde | Volumes élevés, rapportage régulier |
| Résultats secondaires | Paludisme confirmé, rapport confirmés/suspects, soumission des rapports | Sensibles à la rupture de TDR |
| Modèle | Poisson (pseudo-vraisemblance), effets fixes zone et semaine | Robuste à la surdispersion ; absorbe saisonnalité commune et population |
| Inférence | Erreurs groupées par zone, t(35), permutations, Holm | 36 grappes : les approximations asymptotiques seules sont insuffisantes |

## Hypothèse d'identification et contrôles

Sans MVE, les zones auraient évolué parallèlement. Elle ne peut pas être prouvée ; elle est testée par : étude d'événement, test placebo à S10, tendances par zone.

**Résultat de ces contrôles :** le paludisme suspect échoue au placebo et ne doit pas porter la conclusion. Les IRA passent ces contrôles.

## Analyses volontairement écartées

| Analyse | Raison |
|---|---|
| Rt de la MVE (EpiEstim) | Il faudrait des cas par date de début des symptômes ; la FHA hebdomadaire de l'IDS mesure surtout l'activité de notification |
| Modèle spatial, carte | Pas de limites de zones de santé |
| Prévision ou nowcasting | Une seule version de la base : il faudrait des versions hebdomadaires archivées |
| Causalité sur la mortalité | Trop peu de décès, et une notification très incomplète |

## Ce qui ferait le plus progresser l'article

Par ordre d'importance :

1. **Base IDS 2025 par zone :** placebo sur l'année précédente, contrôle de la saisonnalité propre à chaque zone. C'est la limite principale.
2. **Stocks de TDR par zone :** tranche l'hypothèse de la rupture de confirmation.
3. **Sitreps MVE hebdomadaires par zone :** exposition variable dans le temps et plus de puissance. Le biais d'adoption échelonnée devra alors être traité.
4. **Fréquentation des structures (DHIS2/SNIS) :** sépare la baisse de consultation de la baisse de notification.
5. **Limites des zones et événements de sécurité (ACLED) :** débordements spatiaux et confusion.

## Avant soumission

- Autorisation d'utilisation des données par la DPS Ituri.
- Avis du comité d'éthique de MSF et du comité compétent en RDC, ou dérogation.
- Vérification du sitrep n092 dans le document original : les chiffres viennent du rapport MSF, qui contient des incohérences.
- Accord sur les auteurs (ICMJE) et rôle de la DPS et des équipes de zone.
- Dépôt du code avec DOI (Zenodo).
- Revue cible suggérée : *BMJ Global Health* ou *International Journal of Infectious Diseases* en l'état ; *Lancet Global Health* devient réaliste une fois les points 1 et 2 obtenus.
