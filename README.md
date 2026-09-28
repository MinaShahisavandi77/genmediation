# genmediation

**Genetic Mediation Analysis via Longitudinal SEM**

`genmediation` wraps lavaan-based structural equation models to estimate how
parental polygenic risk scores (PRS) for depression mediate the association
between genetic liability and child internalising problems, through
longitudinal observed maternal depression.

---

## Installation

```r
# Install from GitHub
# install.packages("pak")
pak::pak("<your-github-username>/genmediation")

# Install from a local source directory
install.packages("path/to/genmediation", repos = NULL, type = "source")

# Or with devtools
# install.packages("devtools")
devtools::install_local("path/to/genmediation")
```

---

## Quick start

```r
library(genmediation)

# 1. Load and Z-score PRS variables
df  <- read.csv("dataset.csv")
df  <- prepare_prs_data(df, prs_type = "MDD")

# 2. Fit the SEM (bootstrap = 200 for a quick test)
fit <- run_genetic_mediation(df, prs_type = "MDD", bootstrap = 200)

# 3. Inspect results
print_sem_summary(fit)

# 4. Extract indirect / total effects as a data frame
extract_indirect_effects(fit)

# 5. Full tidy parameter table (for export)
results <- tidy_sem_results(fit)
write.csv(results, "mdd_sem_results.csv", row.names = FALSE)
```

---

## Package structure

```
genmediation/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── genmediation-package.R   # package docs & imports
│   ├── prepare_data.R           # prepare_prs_data()
│   ├── build_model.R            # build_mdd_model(), build_ppd_model()
│   ├── run_model.R              # run_genetic_mediation()
│   └── extract_results.R        # extract_indirect_effects(), tidy_sem_results()
├── tests/testthat/
│   ├── test-prepare_data.R
│   └── test-build_model.R
└── vignettes/
    └── genmediation-intro.Rmd
```

---

## Key functions

| Function | Purpose |
|---|---|
| `prepare_prs_data()` | Z-score PRS columns |
| `build_mdd_model()` | Build lavaan syntax for MDD model |
| `build_ppd_model()` | Build lavaan syntax for PPD model |
| `run_genetic_mediation()` | Fit SEM with bootstrap SE |
| `extract_indirect_effects()` | Pull indirect/total effects |
| `tidy_sem_results()` | All parameters as a tidy data frame |
| `print_sem_summary()` | Formatted lavaan summary |

---

## Citation

If you use this package, please cite the underlying lavaan package:

> Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
> *Journal of Statistical Software*, 48(2), 1–36.
