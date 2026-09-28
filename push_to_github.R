## push_to_github.R
## ------------------------------------------------------------------
## Run this from R / RStudio with the working directory set to the
## genmediation package folder (the one containing DESCRIPTION).
##
##   setwd("path/to/genmediation")
##   source("push_to_github.R")
##
## It will: install helper packages, build docs (man/ + NAMESPACE),
## run R CMD check, initialise git, create the GitHub repo and push.
## ------------------------------------------------------------------

## ---- 0. Settings --------------------------------------------------
private_repo <- FALSE      # TRUE = private GitHub repo
run_check    <- TRUE       # FALSE to skip devtools::check() (faster)

## ---- 1. Helper packages ------------------------------------------
need <- c("usethis", "devtools", "roxygen2", "gitcreds", "gert", "gh")
new  <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(new)) install.packages(new)

stopifnot("Run this from the package folder (DESCRIPTION not found)" =
            file.exists("DESCRIPTION"))
usethis::proj_set(".")

## ---- 2. GitHub credentials ---------------------------------------
## usethis needs a GitHub personal access token stored once.
if (inherits(try(gitcreds::gitcreds_get(), silent = TRUE), "try-error")) {
  message("No GitHub token found. A browser window will open to create one.")
  message("Give it the 'repo' and 'workflow' scopes, copy it, then paste it below.")
  usethis::create_github_token()
  gitcreds::gitcreds_set()
}

## ---- 3. Build documentation & check ------------------------------
devtools::document()                       # writes man/ and NAMESPACE
if (run_check) devtools::check(vignettes = FALSE)

## ---- 4. Git init + first commit ----------------------------------
if (!dir.exists(".git")) {
  usethis::use_git(message = "Initial commit of genmediation")
} else {
  gert::git_add(".")
  gert::git_commit_all("Prepare genmediation for GitHub")
}

## ---- 5. Create the GitHub repo and push --------------------------
usethis::use_github(private = private_repo)

## ---- 6. (optional) nice-to-haves ---------------------------------
## usethis::use_pkgdown_github_pages()     # documentation website
## usethis::use_github_release()          # tag v0.1.0 as a release

message("\nDone. Install with:\n  pak::pak(\"<your-github-username>/genmediation\")")
