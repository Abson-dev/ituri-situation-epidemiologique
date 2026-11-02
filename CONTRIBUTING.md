# Contributing

1. **Never commit real data, generated reports, or unpublished results.** The `.gitignore` is a safeguard, not a substitute for checking `git status` before each commit.
2. Open an issue before making a substantial change to the methods.
3. Put analytic choices in `config.yml`, not in the code.
4. Run `Rscript run_all.R --demo` before opening a pull request; it must finish without error.
5. Add a test when fixing a bug or adding a cleaning rule.
6. If you add or update a package, refresh the lockfile with `renv::snapshot(type = "explicit")`.
