# Statistical analysis plan

**Status:** written after initial data exploration and not registered. Findings from this plan should be treated as hypothesis-generating until confirmed in independent data.

## Question

Did notified routine morbidity fall more in health zones with greater Ebola virus disease (EVD) burden after the outbreak declaration?

## Design

Controlled interrupted time series with a continuous exposure. The unit of analysis is the health-zone week.

| Element | Choice | Rationale |
|---|---|---|
| Break | Week containing the outbreak declaration | Common, externally defined date |
| Exposure | log(1 + cumulative confirmed EVD cases per 10,000) | Avoids an arbitrary threshold |
| Reported contrast | 10 per 10,000 versus none | Interpretable, within the observed range |
| Primary outcomes | Suspected malaria, acute respiratory infection, pneumonia, diarrhoea with dehydration under 5, typhoid fever | High volume, consistently reported |
| Secondary outcomes | Confirmed malaria, confirmed-to-suspected ratio, report submission | Sensitive to testing and reporting changes |
| Model | Poisson pseudo-likelihood with zone and week fixed effects | Robust to overdispersion; absorbs shared seasonality and time-invariant population |
| Inference | Zone-clustered errors, *t*(G−1), permutation tests, Holm adjustment | Few clusters make asymptotic inference alone insufficient |

## Identifying assumption and checks

Without the outbreak, zones would have followed parallel trends. This cannot be proved. It is probed with an event study, a joint pre-trend test, a placebo break in pre-outbreak data, and zone-specific linear trends.

## Sensitivity analyses

- Case-count and surveillance-based exposures; categorical exposure
- Excluding the two highest-burden zones, transition weeks, or low-reporting zones
- Alternative rules for conflicting records
- Leave-one-zone-out
- Negative binomial mixed model with randomised quantile residual diagnostics

## Analyses deliberately excluded

| Analysis | Reason |
|---|---|
| Reproduction number from routine haemorrhagic fever notifications | Requires cases by symptom onset; weekly notifications mainly reflect reporting activity |
| Spatial models | Requires health-zone boundaries |
| Nowcasting | Requires archived weekly versions of the database |
| Causal analysis of mortality | Too few deaths and incomplete notification |

## Data that would strengthen inference

1. Zone-level surveillance from a prior year, for a placebo-year test and zone-specific seasonality.
2. Diagnostic test stock records.
3. Weekly zone-level EVD series, for time-varying exposure.
4. Facility utilisation data, to separate care-seeking from reporting.
5. Health-zone boundaries and security-event data, for spatial spillover and confounding.
