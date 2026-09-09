# Shared setup for the test files in this directory.
#
# Each test loads app.R WITHOUT launching it, then drives the module servers
# with shiny::testServer. Nothing here starts a server or opens a port.
#
#   Rscript tests/run_all.R          run everything, with a summary
#   Rscript tests/test_scales.R      run one file
#
# Both work from the project root or from inside tests/.

suppressMessages(library(shiny))

# Find app.R by walking up from the working directory, so the tests do not
# care whether they were started from the project root or from tests/.
app_path <- function() {
    d <- normalizePath(".", mustWork = FALSE)
    for (i in 1:5) {
        f <- file.path(d, "app.R")
        if (file.exists(f)) return(f)
        d <- dirname(d)
    }
    stop("could not find app.R at or above ", normalizePath("."))
}

# Just the constants, formatters and the Likert scale engine -- everything
# above the styling block. No Shiny involved, so these load fast.
load_engine <- function() {
    src <- readLines(app_path())
    cut <- grep("^# ---- Shared styling", src)[1]
    eval(parse(text = paste(src[1:(cut - 1)], collapse = "\n")),
         envir = globalenv())
}

# The whole app except the final shinyApp() call, so the module servers can be
# exercised without starting anything.
load_app <- function() {
    src <- readLines(app_path())
    src <- src[1:(grep("^shinyApp\\(", src)[1] - 1)]
    eval(parse(text = paste(src, collapse = "\n")), envir = globalenv())
}

# Assertion plus a tally that run_all.R reads back to summarise.
.tally <- new.env(parent = emptyenv())
reset_tally <- function() {
    .tally$pass <- 0L; .tally$fail <- 0L; .tally$failed <- character(0)
}
reset_tally()

ok <- function(label, cond) {
    good <- isTRUE(cond)
    if (good) {
        .tally$pass <- .tally$pass + 1L
    } else {
        .tally$fail <- .tally$fail + 1L
        .tally$failed <- c(.tally$failed, label)
    }
    cat(sprintf("  %-64s %s\n", label, if (good) "PASS" else "*** FAIL ***"))
    invisible(good)
}
