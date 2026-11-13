# Changelog

## Unreleased

- English documentation, with a French guide for surveillance teams.
- Personal details, manuscript text and values derived from real data moved to local, unversioned files.
- Dashboard redesign: MSF colours, restyled login page, themed key figures and clearer tables. The logo stays out of the repository and only appears on real data.
- Dashboard: new Années page comparing 2023 to 2026, and a year filter on the Données page. Synthetic history is generated for the demo and is never mixed with real data.
- Input files may now be named `IDSR Week44.xlsx` as well as `Base_DPS_Ituri_Week44.xlsx`.
- Past-year bases are recognised by the year in their name, such as `IDSR 2025_WEEK52.xls`, in `data/raw/` or `data/raw/historique/`. They can never be taken for the current week. Files are read by their content, so an `.xls` that is really an `.xlsx` works.
- Reads Excel 2.x (BIFF2) workbooks, such as some `IDSR Week45.xls` exports, which readxl cannot open. Unknown cell types stop the read rather than drop values.
- Weekly report: cumulative totals for the previous year over the same weeks, computed from its base, when that base is present.
- Weekly report: the malaria trend compares only zones that reported in both weeks, and the measles section no longer lists a zone with missing weeks as having no cases.

## 0.1.0

- Weekly report, analysis, supplementary appendix, tests, and synthetic-data demo mode.
