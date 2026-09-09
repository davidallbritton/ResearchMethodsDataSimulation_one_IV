#!/usr/bin/env Rscript
#
# Runs every test_*.R in this directory and prints a summary.
#
#   Rscript tests/run_all.R
#
# Exits non-zero if anything failed, so it can gate a deploy.

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
dir   <- if (file.exists("tests/helper.R")) "tests" else "."
files <- sort(list.files(dir, pattern = "^test_.*\\.R$", full.names = TRUE))

results <- list()
for (f in files) {
    cat("\n", basename(f), "\n", sep = "")
    reset_tally()
    err <- tryCatch({ source(f, local = new.env(parent = globalenv())); NULL },
                    error = function(e) conditionMessage(e))
    results[[basename(f)]] <- list(pass = .tally$pass, fail = .tally$fail,
                                   failed = .tally$failed, err = err)
    if (!is.null(err)) cat("  *** the file itself errored:", err, "\n")
}

cat("\n", strrep("=", 72), "\n", sep = "")
tp <- tf <- 0L; broke <- FALSE
for (nm in names(results)) {
    r <- results[[nm]]
    tp <- tp + r$pass; tf <- tf + r$fail
    if (!is.null(r$err)) broke <- TRUE
    status <- if (!is.null(r$err)) "ERRORED"
              else if (r$fail > 0)  "FAILED"
              else                  "ok"
    cat(sprintf("%-24s %3d pass  %3d fail   %s\n", nm, r$pass, r$fail, status))
    for (l in r$failed) cat(sprintf("%26s- %s\n", "", l))
}
cat(strrep("-", 72), "\n", sep = "")
cat(sprintf("%-24s %3d pass  %3d fail\n", "TOTAL", tp, tf))

quit(status = if (tf > 0 || broke) 1L else 0L)
