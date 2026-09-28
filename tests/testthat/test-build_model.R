# ============================================================
# Tests for build_mediation_model()
# ============================================================

# Minimal helper — no covariates
bm_bare <- function(mediator_vars, ...) {
  build_mediation_model(
    outcome       = "outcome",
    mediator_vars = mediator_vars,
    prs_child     = "prs_c",
    prs_parent1   = "prs_p1",
    prs_parent2   = "prs_p2",
    ...
  )
}

# ---- basic structure ------------------------------------------

test_that("returns a non-empty character string", {
  m <- bm_bare(c(t1 = "m1", t2 = "m2"))
  expect_type(m, "character")
  expect_length(m, 1L)
  expect_gt(nchar(m), 100L)
})

test_that("user variable names appear, no hardcoded names", {
  m <- bm_bare(c(wave1 = "dep_w1", wave2 = "dep_w2"))
  expect_true(grepl("dep_w1",  m))
  expect_true(grepl("dep_w2",  m))
  expect_true(grepl("prs_p1",  m))
  expect_true(grepl("outcome", m))
  expect_false(grepl("MDD|PPD|dep_pre|intern|h_c_|h_m_", m))
})

test_that("total_ind and total always defined", {
  for (n_tp in 1:4) {
    dv <- setNames(paste0("m", seq_len(n_tp)), paste0("t", seq_len(n_tp)))
    m  <- bm_bare(dv)
    expect_true(grepl("total_ind", m))
    expect_true(grepl("total",     m))
  }
})

# ---- autoregressive paths ------------------------------------

test_that("no AR path for single mediator", {
  m <- bm_bare(c(t1 = "m1"))
  expect_false(grepl("\\ba1\\b", m))
})

test_that("AR paths scale correctly with timepoint count", {
  m2 <- bm_bare(c(t1 = "m1", t2 = "m2"))
  expect_true(grepl("a1\\*m1", m2))
  expect_false(grepl("\\ba2\\b", m2))

  m4 <- bm_bare(c(t1="m1", t2="m2", t3="m3", t4="m4"))
  expect_true(grepl("\\ba3\\b", m4))
  expect_false(grepl("\\ba4\\b", m4))
})

# ---- indirect effect count -----------------------------------

test_that("indirect effect count equals n*(n+1)/2", {
  for (n_tp in 1:5) {
    dv    <- setNames(paste0("m", seq_len(n_tp)), paste0("t", seq_len(n_tp)))
    m     <- bm_bare(dv)
    n_ind <- length(gregexpr(":=", m)[[1]]) - 2L
    expect_equal(n_ind, n_tp * (n_tp + 1L) / 2L)
  }
})

test_that("sequential path products are correct", {
  m <- bm_bare(c(t1="m1", t2="m2", t3="m3", t4="m4"))
  expect_true(grepl("bp1_1 \\* a1 \\* g2",         m))
  expect_true(grepl("bp1_1 \\* a1 \\* a2 \\* g3",  m))
  expect_true(grepl("bp1_1 \\* a1 \\* a2 \\* a3 \\* g4", m))
})

# ---- unnamed mediator_vars -----------------------------------

test_that("unnamed mediator_vars fall back to t1/t2 labels", {
  m <- bm_bare(c("m1", "m2"))
  expect_true(grepl("ind_t1",    m))
  expect_true(grepl("ind_t2",    m))
  expect_true(grepl("ind_t1_t2", m))
})

# ---- covariates_mediator: NULL (default) ---------------------

test_that("NULL covariates_mediator: no covariate terms in mediator equations", {
  m <- bm_bare(c(t1 = "m1", t2 = "m2"))
  # mediator equations should contain only AR + PRS terms
  med_lines <- grep("^m[12] ~", strsplit(m, "\n")[[1]], value = TRUE)
  for (ln in med_lines)
    expect_false(grepl("cov_", ln))
})

# ---- covariates_mediator: character vector (same everywhere) --

test_that("character vector covariates appear in every mediator equation", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2", t3 = "m3"),
    covariates_mediator = c("age", "sex")
  )
  for (tp in c("t1", "t2", "t3")) {
    expect_true(grepl(paste0("cov_", tp, "_age"), m),
                label = paste("age in", tp))
    expect_true(grepl(paste0("cov_", tp, "_sex"), m),
                label = paste("sex in", tp))
  }
})

test_that("mediator covariates do NOT appear in outcome equation unless also in covariates_outcome", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2"),
    covariates_mediator = c("age", "sex"),
    covariates_outcome  = NULL
  )
  out_line <- grep("^outcome ~", strsplit(m, "\n")[[1]], value = TRUE)
  expect_false(grepl("cov_t", out_line))
})

# ---- covariates_mediator: named list (per-timepoint) ----------

test_that("named list: correct covariates per timepoint", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2", t3 = "m3"),
    covariates_mediator = list(
      t1 = c("age", "sex"),
      t2 = c("age", "sex"),
      t3 = c("age", "sex", "extra")
    )
  )
  # extra only at t3
  expect_true(grepl("cov_t3_extra", m))
  expect_false(grepl("cov_t1_extra", m))
  expect_false(grepl("cov_t2_extra", m))
})

test_that("named list: timepoint with NULL gets no covariates", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2", t3 = "m3"),
    covariates_mediator = list(
      t1 = c("age", "sex"),
      t2 = NULL,              # explicitly suppressed
      t3 = c("age", "sex")
    )
  )
  lines <- strsplit(m, "\n")[[1]]
  t2_line <- grep("^m2 ~", lines, value = TRUE)
  expect_false(grepl("cov_", t2_line))
})

test_that("named list: timepoint not mentioned gets no covariates", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2", t3 = "m3"),
    covariates_mediator = list(t1 = c("age"), t3 = c("age"))
    # t2 not listed at all
  )
  lines  <- strsplit(m, "\n")[[1]]
  t2_line <- grep("^m2 ~", lines, value = TRUE)
  expect_false(grepl("cov_", t2_line))
})

test_that("named list: wrong names raise an informative error", {
  expect_error(
    bm_bare(
      c(t1 = "m1", t2 = "m2"),
      covariates_mediator = list(t1 = "age", bad_name = "sex")
    ),
    "do not match"
  )
})

test_that("unnamed list raises an error", {
  expect_error(
    bm_bare(c(t1 = "m1"), covariates_mediator = list("age")),
    "fully named"
  )
})

# ---- covariates_outcome --------------------------------------

test_that("covariates_outcome appear only in outcome equation", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2"),
    covariates_outcome = c("child_age", "sex")
  )
  out_line  <- grep("^outcome ~", strsplit(m, "\n")[[1]], value = TRUE)
  expect_true(grepl("gcov_child_age", out_line))
  expect_true(grepl("gcov_sex",       out_line))

  med_lines <- grep("^m[12] ~", strsplit(m, "\n")[[1]], value = TRUE)
  for (ln in med_lines)
    expect_false(grepl("gcov_", ln))
})

# ---- covariate correlations ----------------------------------

test_that("all unique covariates across model are correlated pairwise", {
  m <- bm_bare(
    c(t1 = "m1", t2 = "m2"),
    covariates_mediator = c("age", "sex"),
    covariates_outcome  = c("age", "child_age")
  )
  # unique covariates: age, sex, child_age  -> 3 pairs
  expect_true(grepl("age ~~ sex",       m))
  expect_true(grepl("age ~~ child_age", m))
  expect_true(grepl("sex ~~ child_age", m))
})

test_that("no covariate correlation block when no covariates specified", {
  m <- bm_bare(c(t1 = "m1", t2 = "m2"))
  expect_false(grepl("Covariate correlations", m))
})

# ---- error handling ------------------------------------------

test_that("empty mediator_vars raises error", {
  expect_error(bm_bare(character(0)), "non-empty")
})
