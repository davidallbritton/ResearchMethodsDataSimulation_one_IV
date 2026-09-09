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

# Output-slot constructors, optionally namespaced (DT::DTOutput). Add to this
# list when introducing a new kind of output, or its slot will look orphaned.
OUT_FNS <- paste0("[A-Za-z0-9._:]*",
                  "(plotOutput|tableOutput|uiOutput|verbatimTextOutput|",
                  "textOutput|imageOutput|downloadButton|",
                  "DTOutput|dataTableOutput)")

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

# Whether a UI slot is actually filled is checked by RUNNING the module, not by
# reading it: an output can legitimately be assigned inside a helper
# (wire_data_table does exactly that), which no amount of text scanning will
# see. Shiny reports an unfilled slot as "hasn't been defined yet"; anything
# else, including a validate() message, means the output exists.
load_app()
INPUTS <- list(
    corrServer = list(mean_x = 0, sd_x = 1, mean_y = 0, slope = .5, sd_e = 1,
        n = 20, generate = 1, x_gen = 0, y_gen = 0, x_use = FALSE,
        y_use = FALSE),
    ttestServer = list(mean1 = 100, sd1 = 15, mean2 = 115, sd2 = 15, n = 20,
        generate = 1, iv_gen = 0, dv_gen = 0, iv_force = 0, iv_unforce = 0,
        iv_use = FALSE, dv_use = FALSE),
    pairedServer = list(mean_c1 = 100, sd_c1 = 15, mean_c2 = 110, sd_c2 = 15,
        rho = .5, n = 20, generate = 1, dv_gen = 0, dv_use = FALSE))

filled <- function(server_fn, slots, inputs) {
    undefined <- character(0)
    testServer(get(server_fn), args = list(id = "m"), {
        do.call(session$setInputs, inputs)
        for (o in slots) {
            e <- tryCatch({ force(output[[o]]); NULL },
                          error = function(e) conditionMessage(e))
            if (!is.null(e) && grepl("hasn't been defined", e))
                undefined <<- c(undefined, o)
        }
    })
    undefined
}

pdf(NULL)
for (mod in list(c("corrUI", "corrServer"),
                 c("ttestUI", "ttestServer"),
                 c("pairedUI", "pairedServer"))) {
    ui  <- ui_slots(mod[1])
    srv <- server_outputs(mod[2])
    cat("\n", mod[1], " / ", mod[2], "\n", sep = "")

    missing <- filled(mod[2], ui, INPUTS[[mod[2]]])
    ok(sprintf("%s: every UI slot is actually filled", mod[2]),
       length(missing) == 0)
    if (length(missing))
        cat("       never rendered:", paste(missing, collapse = ", "), "\n")

    # The other direction stays a text check: an output assigned in the module
    # body that no UI slot displays is dead code, which is how output$split_note
    # ended up defined in the wrong module.
    orphan <- setdiff(srv, ui)
    ok(sprintf("%s: no output defined that its UI never shows", mod[2]),
       length(orphan) == 0)
    if (length(orphan))
        cat("       orphaned:", paste(orphan, collapse = ", "), "\n")
}
invisible(dev.off())

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
