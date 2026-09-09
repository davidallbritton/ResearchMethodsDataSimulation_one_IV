# Every output slot a module's UI declares must be defined in that same
# module's server, and vice versa.
#
# The three modules are near-identical in shape, so an edit anchored on a line
# of code that exists in all three lands in whichever one comes first in the
# file. That has happened three times: the paired panel's slider observer went
# to the t-test module, the t-test module got a duplicate, and output$split_note
# was defined in corrServer while its UI slot sat in ttestUI. None of them
# errored -- the misplaced code was simply dead, and every behavioural test
# still passed.
#
# Run from the project root:  Rscript tests/test_module_wiring.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")

src <- readLines(app_path())
starts <- grep("^[a-zA-Z]+(UI|Server) <- function", src)
names(starts) <- sub(" <- function.*", "", src[starts])
ends <- c(starts[-1] - 1, length(src))

body_of <- function(fn) {
    i <- which(names(starts) == fn)
    if (!length(i)) stop("no such function: ", fn)
    src[starts[i]:ends[i]]
}

OUT_FNS <- paste0("(plotOutput|tableOutput|uiOutput|verbatimTextOutput|",
                  "textOutput|imageOutput|downloadButton)")

ui_slots <- function(fn) {
    b <- body_of(fn)
    m <- regmatches(b, gregexpr(paste0(OUT_FNS, '\\(ns\\("[^"]+"\\)'), b))
    sort(unique(sub('.*ns\\("([^"]+)".*', "\\1", unlist(m))))
}

server_outputs <- function(fn) {
    b <- body_of(fn)
    m <- regmatches(b, gregexpr('output\\$[A-Za-z0-9_]+', b))
    sort(unique(sub("output\\$", "", unlist(m))))
}

for (mod in list(c("corrUI", "corrServer"),
                 c("ttestUI", "ttestServer"),
                 c("pairedUI", "pairedServer"))) {
    ui  <- ui_slots(mod[1])
    srv <- server_outputs(mod[2])
    missing <- setdiff(ui, srv)      # declared in the UI, never rendered
    orphan  <- setdiff(srv, ui)      # rendered, but nothing displays it
    cat("\n", mod[1], " / ", mod[2], "\n", sep = "")
    ok(sprintf("%s: every UI slot is rendered by its own server", mod[2]),
       length(missing) == 0)
    if (length(missing))
        cat("       never rendered:", paste(missing, collapse = ", "), "\n")
    ok(sprintf("%s: no output defined that its UI never shows", mod[2]),
       length(orphan) == 0)
    if (length(orphan))
        cat("       orphaned:", paste(orphan, collapse = ", "), "\n")
}

cat("\nObservers live in the module that owns their inputs\n")
expected <- list(corrServer = c("x", "y"), ttestServer = c("iv", "dv"),
                 pairedServer = "dv")
for (m in names(expected)) {
    b <- body_of(m)
    hits <- grep('observe_scale_range\\(input, session, "', b, value = TRUE)
    got  <- sort(sub('.*, "([^"]+)".*', "\\1", hits))
    ok(sprintf("%s registers exactly %s", m,
               paste(sort(expected[[m]]), collapse = " + ")),
       identical(got, sort(expected[[m]])))
}
