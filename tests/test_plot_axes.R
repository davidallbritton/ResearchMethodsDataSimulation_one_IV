# A variable shown as a scale must be plotted on its FULL response range, so a
# ceiling or floor effect appears as a pile-up against the edge of the frame
# rather than being hidden by axes that shrink to fit the data. A fixed frame
# also means redrawing the scale shows the points moving, not the axes.
#
# renderPlot draws on its own device, so par("usr") afterwards reads nothing.
# Instead plot() is shadowed to record the limits the app actually asks for.
#
# Run from the project root:  Rscript tests/test_plot_axes.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(808)
pdf(NULL)                                  # somewhere for the real plot to go

.orig_plot <- plot
cap <- new.env(parent = emptyenv())
assign("plot", function(...) {
    a <- list(...)
    cap$xlim <- a$xlim; cap$ylim <- a$ylim
    do.call(.orig_plot, a)
}, envir = globalenv())
# NB: cleanup is explicit at the bottom of this file, NOT on.exit(). At the top
# level of a sourced file on.exit() fires as soon as that one expression
# finishes, which would tear the shadow down before a single test ran.

spans <- function(v, lo, hi) !is.null(v) && isTRUE(all.equal(as.numeric(v),
                                                             c(lo, hi)))

cat("\nCORRELATION\n")
testServer(corrServer, args = list(id = "c"), {
    do.call(session$setInputs, list(
        mean_x = 0, sd_x = 1, mean_y = 0, slope = .5, sd_e = 1, n = 40,
        generate = 1, x_gen = 0, y_gen = 0,
        x_use = FALSE, x_items = 4, x_min = 1, x_max = 7, x_rel = .8,
        x_cols = "mean", x_target = 4,
        y_use = FALSE, y_items = 4, y_min = 1, y_max = 5, y_rel = .8,
        y_cols = "mean", y_target = 3))

    force(output$scatter)
    ok("no scales: neither axis is pinned to a response range",
       !spans(cap$xlim, 1, 7) && !spans(cap$ylim, 1, 5))

    session$setInputs(y_use = TRUE)
    force(output$scatter)
    ok("Y scale: y axis spans the full 1-5 response range",
       spans(cap$ylim, 1, 5))
    ok("Y scale: x axis keeps the model-based frame",
       !spans(cap$xlim, 1, 7))

    session$setInputs(x_use = TRUE)
    force(output$scatter)
    ok("both scales: x axis spans 1-7", spans(cap$xlim, 1, 7))
    ok("both scales: y axis spans 1-5", spans(cap$ylim, 1, 5))

    session$setInputs(y_target = 5, y_gen = 1)
    force(output$scatter)
    ok("a ceiling does NOT shrink the frame", spans(cap$ylim, 1, 5))

    before <- cap$ylim
    session$setInputs(y_gen = 2)
    force(output$scatter)
    ok("redrawing the scale leaves the frame identical",
       identical(before, cap$ylim))

    # Low/High are generation settings, so they are snapshotted like the rest:
    # until the scale is regenerated the displayed data is still on the OLD
    # range, and the axis must keep showing that range rather than a wider one
    # the data does not live on.
    session$setInputs(y_max = 9)
    force(output$scatter)
    ok("widening the range alone does not move the axis yet",
       spans(cap$ylim, 1, 5))
    session$setInputs(y_gen = 3)
    force(output$scatter)
    ok("after regenerating, the axis spans the new 1-9 range",
       spans(cap$ylim, 1, 9))
})

cat("\nINDEPENDENT t-TEST\n")
testServer(ttestServer, args = list(id = "t"), {
    do.call(session$setInputs, list(mean1 = 100, sd1 = 15, mean2 = 115,
        sd2 = 15, n = 30, generate = 1, iv_gen = 0, dv_gen = 0,
        iv_use = FALSE, iv_items = 4, iv_min = 1, iv_max = 7, iv_rel = .8,
        iv_cols = "mean", iv_target = 4,
        dv_use = TRUE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
        dv_cols = "mean", dv_target = 4))
    force(output$dotplot)
    ok("DV scale: y axis spans the full 1-7 range", spans(cap$ylim, 1, 7))

    session$setInputs(dv_target = 7, dv_gen = 1)
    force(output$dotplot)
    ok("ceiling keeps the full range rather than zooming in",
       spans(cap$ylim, 1, 7))

    session$setInputs(dv_use = FALSE)
    force(output$dotplot)
    ok("no scale: frame returns to the model-based limits",
       !spans(cap$ylim, 1, 7) && cap$ylim[2] > 120)
})

cat("\nPAIRED t-TEST\n")
testServer(pairedServer, args = list(id = "p"), {
    do.call(session$setInputs, list(mean_c1 = 100, sd_c1 = 15, mean_c2 = 110,
        sd_c2 = 15, rho = .5, n = 30, generate = 1, dv_gen = 0,
        dv_use = TRUE, dv_items = 5, dv_min = 1, dv_max = 5, dv_rel = .8,
        dv_cols = "mean", dv_target = 3))
    force(output$pairplot)
    ok("DV scale: y axis spans the full 1-5 range", spans(cap$ylim, 1, 5))

    session$setInputs(dv_target = 1, dv_gen = 1)
    force(output$pairplot)
    ok("floor keeps the full range", spans(cap$ylim, 1, 5))

    session$setInputs(dv_use = FALSE)
    force(output$pairplot)
    ok("no scale: frame returns to the model-based limits", cap$ylim[2] > 120)
})

cat("\nEvery module registers its slider-range observer\n")
# The observer was once registered twice in the t-test module and not at all in
# the paired one, so the paired slider silently stopped tracking its Low/High
# inputs. MockShinySession does not expose update messages, so this is checked
# structurally instead.
src <- readLines(app_path())
bounds <- c(grep("^corrServer <- function", src),
            grep("^ttestServer <- function", src),
            grep("^pairedServer <- function", src),
            grep("^instructionsUI <- function", src))
names(bounds) <- c("corrServer", "ttestServer", "pairedServer", "end")
expected <- list(corrServer = c("x", "y"), ttestServer = c("iv", "dv"),
                 pairedServer = "dv")
for (m in names(expected)) {
    body <- src[bounds[[m]]:(bounds[[which(names(bounds) == m) + 1]] - 1)]
    hits <- grep('observe_scale_range\\(input, session, "', body, value = TRUE)
    got  <- sort(sub('.*, "([^"]+)".*', "\\1", hits))
    ok(sprintf("%s registers exactly %s", m,
               paste(sort(expected[[m]]), collapse = " + ")),
       identical(got, sort(expected[[m]])))
}

# Cleanup: restore the real plot() and close the device.
if (exists("plot", envir = globalenv(), inherits = FALSE))
    rm("plot", envir = globalenv())
invisible(dev.off())
