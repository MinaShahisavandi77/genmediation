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
pak::pak("MinaShahisavandi77/genmediation")

# Install from a local source directory
install.packages("path/to/genmediation", repos = NULL, type = "source")

# Or with devtools
# install.packages("devtools")
devtools::install_local("path/to/genmediation")
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



---

## Citation

If you use this package, please cite the underlying lavaan package:

> Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling.
> *Journal of Statistical Software*, 48(2), 1–36.
