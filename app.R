#
# Research Methods Data Simulation
#
# A teaching app: students choose population parameters and generate fake data
# for class projects, then download it as CSV to analyze in their stats
# software. Three generators, each on its own page:
#
#   * Scatterplot / correlation    (correlation module)
#   * Independent-samples t-test   (ttest module)
#   * Paired-samples t-test        (paired module)
#
# A landing "Instructions" page explains all three and links to each.
# Navigation uses a HIDDEN tabsetPanel (no visible tab bar, to save screen
# space); the buttons/links call updateTabsetPanel() to switch pages. Each
# generator is a Shiny module, so their (identical) input/output IDs never
# collide. APP_VERSION below is shown in a footer on every page, so the
# deployed build can be identified at a glance.
#
# Any variable can optionally be measured as a multi-item Likert scale instead
# of a single continuous score. See the "Likert scale engine" section below for
# how the items are generated and why the mapping uses population parameters.
#

library(shiny)

# ---- Shared constants -------------------------------------------------------

# App version, shown in the footer. Bump this whenever you deploy a change, so
# what students see on screen tells you which build is live.
APP_VERSION <- "1.1.0-dev"

# Label for the row-number column, on screen and in the downloaded CSV.
ID_LABEL <- "Participant"

# Group names for the t-test (also the levels of its IV).
G1 <- "Group 1"
G2 <- "Group 2"

# Condition names for the paired t-test (the two measurements per participant).
C1 <- "Condition 1"
C2 <- "Condition 2"

# ---- Shared formatting helpers ----------------------------------------------

# One display format everywhere on screen: 2 decimals. Undefined statistics --
# which degenerate settings really can produce -- print as an em dash rather
# than "NA", so a table can still show what IS computable next to what is not.
fmt <- function(x) ifelse(is.finite(x), sprintf("%.2f", x), "\u2014")

# ...except in the R code blocks, which must reproduce what the app did.
# as.character() keeps a typed 1.005 as 1.005 rather than rounding to 1.00.
fmt_code <- function(x) as.character(x)

# APA p-value: no leading zero, and "< .001" for very small values.
fmt_p <- function(p) {
    if (p < .001) "< .001" else sub("0\\.", ".", sprintf("= %.3f", p))
}

# APA correlation: 2 decimals, no leading zero, sign preserved (e.g. -.45).
fmt_r <- function(x) ifelse(is.finite(x),
                            sub("(-?)0\\.", "\\1.", sprintf("%.2f", x)),
                            "\u2014")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a

# ---- Likert scale engine ----------------------------------------------------
#
# Scales are built with a latent-trait / threshold model -- the way Likert data
# actually arises. The continuous score a panel already generated IS the latent
# trait; each item is a noisy reading of it, chopped into integer response
# categories by evenly spaced thresholds:
#
#   z      = (score - population mean) / population SD
#   item*  = z + e,        e ~ N(0, sigma_e^2)
#   item   = the response category item* falls into
#
# Two properties matter for teaching:
#
#   * The mapping is built from the POPULATION parameters the student typed,
#     never from the observed sample range, so the same settings always map the
#     same way and the population/sample distinction the app teaches survives.
#   * sigma_e comes from a target reliability, so the scale carries real
#     measurement error. That attenuates correlations and effect sizes exactly
#     as unreliable measures do in practice -- visible by comparing the
#     "This sample" and "This sample as scales" columns.

# How far out (in SDs) the outermost thresholds sit. Wider => more responses
# pile into the middle categories; narrower => more floor and ceiling.
SCALE_SPAN <- 2.5

# Item-level error SD implied by a target alpha for an n-item composite.
# Spearman-Brown inverted: alpha = n*p / (1 + (n-1)*p), where p is the
# per-item reliability, and p = 1 / (1 + sigma_e^2) for a unit-variance trait.
scale_error_sd <- function(n_items, reliability) {
    a <- max(0.05, min(0.99, reliability))
    n <- max(1, n_items)
    p <- a / (n - a * (n - 1))
    p <- max(1e-4, min(0.9999, p))
    sqrt(1 / p - 1)
}

# The latent shift that puts the AVERAGE RESPONSE at `target`.
#
# Thresholds are symmetric about zero, so an unshifted scale always averages the
# middle of its response range -- which means no amount of widening or narrowing
# can produce a floor or ceiling effect. Shifting the latent distribution
# against the thresholds is what does that.
#
# For items ~ N(mu, 1), E[response] = k_min + sum(pnorm(mu - cuts)), which is
# monotone in mu, so the required shift is a one-dimensional root find. At the
# very ends of the range the shift is infinite, so it is capped -- which is
# exactly the degenerate "everybody answered 7" case, deliberately reachable.
scale_shift_for <- function(target, cuts, k_min, k_max) {
    tgt <- max(k_min + 1e-3, min(k_max - 1e-3, target))
    f <- function(mu) k_min + sum(pnorm(mu - cuts)) - tgt
    if (f(-10) >= 0) return(-10)
    if (f(10)  <= 0) return(10)
    uniroot(f, c(-10, 10))$root
}

# Turn continuous scores into integer Likert items.
#
# ref_mean/ref_sd define the shared mapping. Pass the SAME reference for
# variables that must stay comparable -- the two groups of a t-test, the two
# conditions of a paired design -- otherwise each is standardized to its own
# centre and the difference between them is erased.
#
# target is where the average response should land on the response range.
# Defaulting to the midpoint reproduces an unshifted, symmetric scale.
make_scale_items <- function(score, ref_mean, ref_sd, n_items,
                             k_min, k_max, reliability, target = NULL) {
    n_items <- max(1, round(n_items))
    k_min <- round(k_min); k_max <- round(k_max)
    if (k_max <= k_min) k_max <- k_min + 1
    n_cat <- k_max - k_min + 1

    sigma_e <- scale_error_sd(n_items, reliability)
    z <- if (ref_sd > 0) (score - ref_mean) / ref_sd else rep(0, length(score))

    # Thresholds live in the standardized item metric, so they do not depend on
    # this particular sample.
    sd_item <- sqrt(1 + sigma_e^2)
    cuts <- seq(-SCALE_SPAN, SCALE_SPAN, length.out = n_cat + 1)[2:n_cat]

    if (is.null(target)) target <- (k_min + k_max) / 2
    mu <- scale_shift_for(target, cuts, k_min, k_max)

    items <- lapply(seq_len(n_items), function(j) {
        istar <- (z + rnorm(length(z), 0, sigma_e)) / sd_item + mu
        k_min + findInterval(istar, cuts)
    })
    out <- as.data.frame(items)
    names(out) <- paste0("q", seq_len(n_items))
    out
}

# The scale controls for one variable: the on/off box, then the definition
# inputs, the column picker and this variable's own redraw button, all revealed
# only when it is ticked.
#
# raw_label names the third column group. It defaults to the continuous score,
# but on a grouping IV that column is the group label, not a score.
scaleControlsUI <- function(ns, prefix, label,
                            raw_label = "Original continuous score") {
    use_id <- ns(paste0(prefix, "_use"))
    tagList(
        checkboxInput(use_id, label, value = FALSE),
        conditionalPanel(
            condition = sprintf("input['%s'] == true", use_id),
            div(
                class = "scale-box",
                fluidRow(
                    column(4, numericInput(ns(paste0(prefix, "_items")),
                                           "Items:", value = 4, min = 1, step = 1)),
                    column(4, numericInput(ns(paste0(prefix, "_min")),
                                           "Low:", value = 1, step = 1)),
                    column(4, numericInput(ns(paste0(prefix, "_max")),
                                           "High:", value = 7, step = 1))
                ),
                numericInput(ns(paste0(prefix, "_rel")),
                             "Reliability (Cronbach's \u03b1):",
                             value = 0.8, min = 0.05, max = 0.99, step = 0.05),
                tags$label("Typical response", class = "control-label"),
                div(class = "zone-labels",
                    span("Floor effects"), span("Well targeted"),
                    span("Ceiling effects")),
                sliderInput(ns(paste0(prefix, "_target")), label = NULL,
                            min = 1, max = 7, value = 4, step = 0.5,
                            width = "100%"),
                checkboxGroupInput(ns(paste0(prefix, "_cols")),
                                   "Include in the CSV:",
                                   choices = setNames(
                                       c("items", "mean", "raw"),
                                       c("Individual items", "Scale mean",
                                         raw_label)),
                                   selected = c("items", "mean")),
                actionButton(ns(paste0(prefix, "_gen")), "Generate Scale Scores",
                             class = "btn-primary btn-sm", width = "100%"),
                helpText("Redraws just this scale from the sample scores already
                          on screen.")
            )
        )
    )
}

# Scale settings come in two kinds, and the split is what lets the scale scores
# be redrawn without disturbing the sample underneath them:
#
#   * The settings that change the NUMBERS -- item count, response range,
#     reliability -- are snapshotted when a button is pressed, exactly as the
#     population parameters are. Editing them does nothing until you ask.
#   * The on/off tick and the column picker only change what is DISPLAYED, so
#     they are read live and take effect immediately, without redrawing.
scale_spec_of <- function(input, prefix) {
    list(
        items = max(1, round(input[[paste0(prefix, "_items")]] %||% 4)),
        kmin  = round(input[[paste0(prefix, "_min")]] %||% 1),
        kmax  = round(input[[paste0(prefix, "_max")]] %||% 7),
        rel   = input[[paste0(prefix, "_rel")]] %||% 0.8,
        target = input[[paste0(prefix, "_target")]] %||% NA
    )
}

# The slider's bounds come from the Low/High inputs, so it has to be rebuilt
# whenever they change. The current position is kept, clamped into the new
# range, rather than snapping back to the midpoint.
observe_scale_range <- function(input, session, prefix) {
    observeEvent(list(input[[paste0(prefix, "_min")]],
                      input[[paste0(prefix, "_max")]]), {
        lo <- round(input[[paste0(prefix, "_min")]] %||% 1)
        hi <- round(input[[paste0(prefix, "_max")]] %||% 7)
        if (hi <= lo) hi <- lo + 1
        v <- input[[paste0(prefix, "_target")]] %||% ((lo + hi) / 2)
        updateSliderInput(session, paste0(prefix, "_target"),
                          min = lo, max = hi, step = 0.5,
                          value = max(lo, min(hi, v)))
    }, ignoreInit = TRUE)
}

# The live half, merged with the snapshotted half for one variable.
scale_settings <- function(input, prefix, spec) {
    c(list(use  = isTRUE(input[[paste0(prefix, "_use")]]),
           cols = input[[paste0(prefix, "_cols")]] %||% character(0)),
      spec)
}


# ---- Degenerate-data guards -------------------------------------------------
#
# Extreme settings really can leave a variable with no variance: a population SD
# of 0, or "Typical response" dragged to the end of the scale so everyone gives
# the same answer. Those cases are worth reaching -- seeing a measure fail is
# the lesson -- but the statistics fail in three different ways, so the
# condition is detected up front rather than caught afterwards:
#
#   * t.test(var.equal = TRUE) throws "data are essentially constant"
#   * cor() and cor.test() return NA with only a warning -- which would print
#     as a result unless we stop first
#   * summary(lm())$fstatistic returns NULL, crashing the line that reads it
#
# The messages go through validate()/need(), so the explanation appears in place
# of the statistic instead of a red R error.

# A midpoint target reproduces the unshifted scale, so NA means "leave centred".
spec_target <- function(spec) if (is.na(spec$target)) NULL else spec$target

CONST_TOL <- 1e-9
is_constant <- function(v) {
    s <- suppressWarnings(sd(as.numeric(v)))
    !is.finite(s) || s < CONST_TOL
}

# Nothing varies at all.
msg_no_variance <- function(labels) {
    sprintf("Every participant has the same value for %s, so it has no
             variability at all. Statistics that describe how scores vary
             together cannot be computed from a variable that does not vary.
             Move \u201cTypical response\u201d away from the end of the scale,
             or raise the population SD.",
            paste(labels, collapse = " and "))
}

# Varies between groups but not within them: perfect separation.
msg_no_within_variance <- function() {
    "Every participant within a group gave exactly the same answer, so there is
     no variability inside either group. The groups are perfectly separated,
     which makes t infinitely large and impossible to calculate. Real data
     always has some spread inside each group."
}

# The split put nobody on one side.
msg_empty_group <- function() {
    "Every participant fell on the same side of the median, so one group is
     empty and there is nothing to compare. The scale has almost no spread \u2014
     move \u201cTypical response\u201d away from the end of the scale, or raise
     the population SD."
}

# Computable, but only just: worth a caution rather than a refusal.
caution_note <- function(v, k_min, k_max) {
    v <- as.numeric(v)
    n_distinct <- length(unique(v))
    at_end <- mean(v <= k_min + 1e-9 | v >= k_max - 1e-9)
    if (n_distinct <= 2)
        return(sprintf("Only %d different score%s appear\u2014 the measure is
                        barely discriminating between people, so these estimates
                        are very unstable.",
                       n_distinct, if (n_distinct == 1) "" else "s"))
    if (at_end > 0.6)
        return(sprintf("%.0f%% of scores sit at the very top or bottom of the
                        response range. That is a %s effect: the measure cannot
                        record how much further apart people really are.",
                       100 * at_end,
                       if (mean(v >= k_max - 1e-9) > mean(v <= k_min + 1e-9))
                           "ceiling" else "floor"))
    NULL
}

# A variable measured as a scale is always plotted on its FULL response range,
# so a ceiling or floor effect shows as a pile-up against the edge of the frame
# instead of being hidden by axes that shrink to fit whatever was drawn. It also
# keeps the frame still across redraws, so regenerating the scale shows the
# points moving inside a fixed range.
scale_range <- function(st) {
    lo <- st$kmin; hi <- st$kmax
    if (hi <= lo) hi <- lo + 1
    c(lo, hi)
}

# Tick marks at the actual response options, endpoints included.
#
# R chooses its own ticks otherwise, and for many ranges it never labels the
# anchors: a 1-9 scale comes out labelled 0, 2, 4, 6, 8, 10 -- both endpoints
# missing and two values that are not response options at all -- while a 1-4
# scale gets 1.5 and 2.5. The axis then reads as if it stops short of the scale.
scale_axis <- function(side, st) {
    r  <- scale_range(st)
    at <- seq(r[1], r[2])
    if (length(at) > 12) {                      # long scales: thin the labels
        step <- ceiling((r[2] - r[1]) / 10)
        at <- unique(c(seq(r[1], r[2], by = step), r[2]))
    }
    axis(side, at = at)
}

# ---- Median split of a measured IV ------------------------------------------
#
# When the grouping IV is measured as a Likert scale, the groups come from a
# median split of that scale -- the same split a student would perform, so the
# Group column can be reproduced from the data they are given.
#
# Ties at the median are the normal case, not an edge case: a bounded discrete
# scale has few possible means and many participants. At the defaults (30 per
# group, 4 items, 1-7) the middle two scores tie in about 81% of samples, with
# roughly 6 or 7 people sharing that value, and adding items barely helps --
# ten items on a 1-7 scale still ties 67% of the time. Everyone sharing the
# median value lands in the low group, so the groups come out unequal. That is
# how a real median split behaves and the app does not hide it.

# The split a student would perform: the low group is at or below the median.
median_split_groups <- function(m) {
    factor(ifelse(m <= median(m), G1, G2), levels = c(G1, G2))
}

# Nudge the fewest scores by the smallest step that makes the split come out
# even. Offered as an explicit choice, clearly labelled as unrealistic.
#
# Unequal groups happen precisely when the middle two scores tie, so the fix is
# to lift the excess tied participants just past that value: +1 on a single
# item, which moves the scale mean by 1/items -- the smallest change the scale
# can express. Every other score is left exactly as generated.
force_equal_split <- function(items, k_min, k_max) {
    m  <- rowMeans(items)
    nt <- length(m); half <- nt %/% 2
    if (nt < 2) return(list(items = items, moved = 0L))
    srt <- sort(m)
    if (srt[half] != srt[half + 1]) return(list(items = items, moved = 0L))

    v     <- srt[half]
    tied  <- which(m == v)
    n_up  <- sum(m < v) + length(tied) - half   # how many must rise

    moved <- 0L
    for (i in tied) {
        if (moved >= n_up) break
        row <- as.numeric(items[i, ])
        j   <- which(row < k_max)
        if (!length(j)) next                    # already at the ceiling
        j   <- j[which.min(row[j])]             # lift the lowest item
        items[i, j] <- row[j] + 1
        moved <- moved + 1L
    }
    list(items = items, moved = moved)
}

# Which version of a variable the student will actually analyze -- that is,
# whichever one lands in their CSV. The continuous score wins when it is there;
# otherwise the scale mean stands in for it.
analysis_choice <- function(st) {
    if (!st$use) return("raw")
    if ("raw" %in% st$cols) return("raw")
    if (any(c("mean", "items") %in% st$cols)) return("mean")
    "raw"
}

# Assemble one variable's columns for the table and CSV, in items/mean/raw
# order, honouring the column picker.
scale_columns <- function(st, base, items, scale_mean, raw) {
    out <- list()
    if (st$use && "items" %in% st$cols && !is.null(items)) {
        nm <- paste0(base, "_Scale_q", seq_len(ncol(items)))
        for (j in seq_len(ncol(items))) out[[nm[j]]] <- items[[j]]
    }
    if (st$use && "mean" %in% st$cols && !is.null(scale_mean)) {
        out[[paste0(base, "_Scale_Mean")]] <- round(scale_mean, 2)
    }
    if (!st$use || "raw" %in% st$cols || length(out) == 0) {
        out[[base]] <- raw
    }
    out
}

# Tick the descriptives column that everything else on the page is built from:
# the version of the data the result boxes and the plot use, and the one the CSV
# will contain. The tick alone carries that; spelling it out in the header made
# the column read as being only about the download.
csv_flag <- function(on) if (on) " \u2713" else ""

# Wrap a descriptives cell so that same column stands out.
csv_cell <- function(v, on) {
    if (on) paste0("<span class='csv-col'>", v, "</span>") else v
}

# Reproducible R for one variable's scale, with this panel's actual numbers.
# Reproducible R for one variable's scale, with this panel's actual numbers.
#
# Every name is prefixed with the variable, so two scales in one block cannot
# clobber each other -- and so the paired panel's latent z, which builds the
# correlated scores, survives. Lengths come from the data rather than from n,
# because a t-test's Score is 2n rows long while its n is per group.
scale_code_snippet <- function(st, var, ref_mean, ref_sd, source_expr = var) {
    if (!st$use) return("")
    n_cat <- st$kmax - st$kmin + 1
    sigma <- scale_error_sd(st$items, st$rel)
    cuts  <- seq(-SCALE_SPAN, SCALE_SPAN, length.out = n_cat + 1)[2:n_cat]
    tgt   <- if (is.na(st$target)) (st$kmin + st$kmax) / 2 else st$target
    mu    <- scale_shift_for(tgt, cuts, st$kmin, st$kmax)
    v     <- function(suffix) paste0(var, suffix)
    paste0(
        "\n# ", var, " as a ", st$items, "-item ", st$kmin, "-", st$kmax,
        " Likert scale\n",
        "#   target alpha = ", fmt_code(st$rel),
        ", typical response = ", fmt_code(tgt), "\n",
        v("_sigma"), " <- ", fmt_code(signif(sigma, 6)),
        "   # item noise implied by that alpha\n",
        v("_mu"), " <- ", fmt_code(signif(mu, 6)),
        "   # shift that puts the average response at ", fmt_code(tgt), "\n",
        v("_z"), " <- ",
        if (isTRUE(all.equal(ref_mean, 0)) && isTRUE(all.equal(ref_sd, 1)))
            source_expr
        else paste0("(", source_expr, " - ", fmt_code(signif(ref_mean, 6)),
                    ") / ", fmt_code(signif(ref_sd, 6))), "\n",
        v("_cuts"), " <- seq(-", fmt_code(SCALE_SPAN), ", ",
        fmt_code(SCALE_SPAN), ", length.out = ", n_cat + 1, ")[2:", n_cat, "]\n",
        v("_items"), " <- replicate(", st$items, ", ", st$kmin,
        " + findInterval(\n",
        "    (", v("_z"), " + rnorm(length(", v("_z"), "), 0, ", v("_sigma"),
        ")) / sqrt(1 + ", v("_sigma"), "^2) + ", v("_mu"), ",\n",
        "    ", v("_cuts"), "))\n",
        v("_Scale_Mean"), " <- rowMeans(", v("_items"), ")\n"
    )
}

# When the IV is measured as a scale, the groups ARE a median split of it.
iv_code_snippet <- function(st) {
    if (!st$use) return("")
    paste0(
        scale_code_snippet(st, "Group", 0, 1, source_expr = "rnorm(2 * n)"),
        "# the grouping IS a median split of that scale, ties and all\n",
        "Group <- ifelse(Group_Scale_Mean <= median(Group_Scale_Mean),\n",
        "                \"", G1, "\", \"", G2, "\")\n",
        "table(Group)   # rarely an even split, and that is the lesson\n"
    )
}

# ---- Shared styling ---------------------------------------------------------

app_css <- HTML("
    .param-box { border: 1px solid #b8c4d0; border-radius: 6px;
                 background: #f4f7fa; padding: 10px 12px 4px 12px; }
    .param-box .form-group { margin-bottom: 6px; }
    .param-box label { margin-bottom: 1px; font-weight: normal; }
    .param-box .box-title { font-weight: bold; display: block;
                            margin-bottom: 2px; }
    .param-box .var-head { font-weight: bold; display: block;
                           margin: 8px 0 2px 0; }
    .param-box .box-note { color: #5a6570; font-size: 90%;
                           margin-bottom: 8px; }
    .param-box .help-block { margin: 8px 0 4px 0; }
    .sample-controls { margin-top: 12px; }
    .sample-controls .form-group { margin-bottom: 0; }
    .panel-row { display: flex; flex-wrap: wrap; gap: 20px;
                 align-items: flex-start; }
    .stats-col { flex: 0 0 auto; }
    .plot-col  { flex: 1 1 340px; min-width: 300px; }
    .stats-col table td, .stats-col table th { white-space: nowrap; }
    /* First column is the statistic's name; every value column is numeric.
       Done in CSS because the column count varies with the scale settings. */
    .stats-col table td:not(:first-child),
    .stats-col table th:not(:first-child) { text-align: right; }
    .stats-col h4 { margin-top: 14px; }
    .data-head { display: flex; align-items: baseline; gap: 10px; }
    .data-head h4 { margin-bottom: 6px; }
    /* Results readout below a plot (used by both generators). */
    .result-box { border: 1px solid #d4d4d4; border-radius: 6px;
                  background: #fafafa; padding: 8px 12px; margin-top: 10px;
                  max-width: 460px; }
    .result-box .result-title { font-weight: bold; }
    .result-box .apa { font-size: 108%; }
    .result-box .decision { color: #5a6570; }
    /* The primary box is the one students should model their write-up on;
       make it stand out from the supplementary boxes below it. */
    .result-box.primary { border: 2px solid #2c5f8a; background: #eef4fa; }
    .result-box.primary .result-title { color: #204c6e; }
    .result-flag { display: inline-block; font-size: 78%; font-weight: bold;
                   text-transform: uppercase; letter-spacing: .04em;
                   color: #fff; background: #2c5f8a; border-radius: 3px;
                   padding: 1px 7px; margin-bottom: 6px; }
    /* Navigation and instructions. */
    .nav-back { margin: 8px 0 0 2px; font-size: 95%; }
    .instructions-wrap { max-width: 860px; }
    .gen-card { border: 1px solid #b8c4d0; border-radius: 6px;
                background: #f4f7fa; padding: 14px 18px; margin-bottom: 16px; }
    .gen-card h3 { margin-top: 4px; }
    /* Likert scale controls, revealed by each variable's checkbox. */
    .scale-box { border-left: 3px solid #2c5f8a; background: #eef4fa;
                 padding: 6px 10px 2px 10px; margin: 0 0 10px 8px; }
    .scale-box .form-group { margin-bottom: 6px; }
    .scale-box label { font-weight: normal; }
    .scale-box .shiny-input-checkboxgroup label { font-weight: bold; }
    /* Zone labels above the Typical response slider. */
    .zone-labels { display: flex; justify-content: space-between;
                   font-size: 78%; color: #5a6570; margin-bottom: -6px; }
    /* validate()/need() messages, styled as explanations rather than errors. */
    .shiny-output-error-validation {
        display: block; border: 1px solid #d9b38c; border-radius: 6px;
        background: #fdf6ec; color: #7a5320; padding: 8px 12px;
        margin-top: 10px; max-width: 460px; font-size: 95%; }
    /* Unequal groups are the lesson of the whole IV-as-scale feature, so the
       warning is deliberately loud: red, full width, above everything else,
       and it pulses a few times when it first appears. */
    .split-warn { border: 2px solid #b32020; border-left: 10px solid #b32020;
                  border-radius: 6px; background: #fdecea; color: #7a1a1a;
                  padding: 12px 16px; margin: 0 0 16px 0; font-size: 100%;
                  box-shadow: 0 2px 6px rgba(179, 32, 32, .25);
                  animation: warnpulse 1.1s ease-in-out 3; }
    .split-warn .warn-head { display: block; font-weight: bold; font-size: 116%;
                             text-transform: uppercase; letter-spacing: .04em;
                             margin-bottom: 5px; color: #a01818; }
    .split-warn .btn { margin-top: 10px; }
    @keyframes warnpulse {
        0%, 100% { box-shadow: 0 2px 6px rgba(179, 32, 32, .25); }
        50%      { box-shadow: 0 0 0 7px rgba(179, 32, 32, .30); }
    }
    .split-note { border: 1px solid #b8c4d0; border-radius: 6px;
                  background: #f4f7fa; color: #44515e; padding: 8px 12px;
                  margin-bottom: 10px; max-width: 560px; font-size: 90%; }
    .split-note a { margin-left: 6px; }
    .caution-note { border: 1px solid #d9b38c; border-radius: 6px;
                    background: #fdf6ec; color: #7a5320; padding: 6px 10px;
                    margin-top: 8px; max-width: 460px; font-size: 90%; }
    /* The descriptives column matching what will be downloaded. */
    .csv-col { background: #fff6d9; font-weight: bold; }
    /* Version footer, shown under every page. */
    .app-footer { margin: 28px 0 10px 0; padding-top: 8px;
                  border-top: 1px solid #e0e0e0; color: #7a838c;
                  font-size: 85%; }
")

# =============================================================================
#  Correlation module -- scatterplot / correlation data
# =============================================================================

corrUI <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Simulate Data: Scatterplot / Correlation"),
        sidebarLayout(

            sidebarPanel(
                width = 4,

                div(
                    class = "param-box",

                    span(class = "box-title", "Population parameters"),
                    div(class = "box-note",
                        "The true values in the whole population. Your sample is
                         drawn from a population that looks like this."),

                    span(class = "var-head", "Predictor / IV (X)"),
                    numericInput(ns("mean_x"), "Mean of X:", value = 0),
                    numericInput(ns("sd_x"),   "SD of X:",   value = 1, min = 0),

                    span(class = "var-head", "Outcome / DV (Y)"),
                    numericInput(ns("mean_y"), "Mean of Y:", value = 0),
                    numericInput(ns("slope"),
                                 "Slope: how much Y rises per 1-unit rise in X:",
                                 value = 0.5, step = 0.1),
                    numericInput(ns("sd_e"),
                                 "Noise: SD of the random error added to Y:",
                                 value = 1, min = 0, step = 0.1),

                    uiOutput(ns("r_preview"))
                ),

                # Measurement, not population: how each variable is observed.
                div(
                    class = "param-box", style = "margin-top: 12px;",
                    span(class = "box-title", "Measure as Likert scales"),
                    div(class = "box-note",
                        "Replace a continuous score with a set of Likert items,
                         the way a real questionnaire would measure it."),
                    scaleControlsUI(ns, "x", "Predictor / IV (X) as a scale"),
                    scaleControlsUI(ns, "y", "Outcome / DV (Y) as a scale")
                ),

                # Sample size and the button sit outside the box: how many cases
                # you draw is not a property of the population.
                div(
                    class = "sample-controls",
                    fluidRow(
                        column(6, numericInput(ns("n"), "Number of cases (N):",
                                               value = 30, min = 3, step = 1)),
                        column(6, div(style = "margin-top: 25px;",
                                      actionButton(ns("generate"), "Generate Data",
                                                   class = "btn-primary",
                                                   width = "100%")))
                    )
                ),

                tags$hr(),
                tags$strong("The model"),
                uiOutput(ns("sd_note")),
                uiOutput(ns("equations")),

                tags$hr(),
                tags$strong("R code"),
                verbatimTextOutput(ns("code"))
            ),

            mainPanel(
                width = 8,
                div(
                    class = "panel-row",
                    div(
                        class = "stats-col",
                        tags$h4("Descriptive Statistics"),
                        tableOutput(ns("sample_stats")),
                        div(
                            class = "data-head",
                            tags$h4("Sample Data"),
                            downloadButton(ns("download_csv"), "CSV",
                                           class = "btn-xs")
                        ),
                        div(
                            style = "max-height: 420px; overflow-y: auto;",
                            tableOutput(ns("data_table"))
                        )
                    ),
                    div(
                        class = "plot-col",
                        tags$h4("Scatterplot"),
                        plotOutput(ns("scatter"), height = "500px"),
                        uiOutput(ns("cor_result")),
                        uiOutput(ns("reg_result"))
                    )
                )
            )
        )
    )
}

corrServer <- function(id) {
    moduleServer(id, function(input, output, session) {

        # Everything implied by a set of parameter values.
        derive <- function(mean_x, sd_x, mean_y, slope, sd_e) {
            sd_x <- abs(sd_x); sd_e <- abs(sd_e)
            sd_y <- sqrt(slope^2 * sd_x^2 + sd_e^2)
            list(
                mean_x = mean_x, sd_x = sd_x, mean_y = mean_y,
                slope = slope, sd_e = sd_e, sd_y = sd_y,
                # intercept that puts the line through (mean_x, mean_y)
                b0 = mean_y - slope * mean_x,
                r  = if (sd_y > 0) slope * sd_x / sd_y else 0
            )
        }

        # Live, un-gated view of the inputs, for the "r so far" preview.
        live <- reactive({
            req(input$sd_x, input$slope, input$sd_e)
            derive(input$mean_x, input$sd_x, input$mean_y, input$slope, input$sd_e)
        })

        # Snapshot of the inputs, taken only when "Generate Data" is clicked.
        params <- eventReactive(input$generate, {
            p <- derive(input$mean_x, input$sd_x, input$mean_y,
                        input$slope, input$sd_e)
            p$n <- max(3, round(input$n))
            p
        }, ignoreNULL = FALSE)

        # Each variable's item settings are re-read when Generate Data runs or
        # when that variable's own redraw button is pressed. sim_data() does not
        # depend on these, so redrawing a scale leaves the sample alone -- and
        # each spec is separate, so redrawing X does not disturb Y.
        spec_x <- eventReactive(list(input$generate, input$x_gen),
                                scale_spec_of(input, "x"), ignoreNULL = FALSE)
        spec_y <- eventReactive(list(input$generate, input$y_gen),
                                scale_spec_of(input, "y"), ignoreNULL = FALSE)

        sx <- reactive(scale_settings(input, "x", spec_x()))
        sy <- reactive(scale_settings(input, "y", spec_y()))

        observe_scale_range(input, session, "x")
        observe_scale_range(input, session, "y")

        sim_data <- reactive({
            p <- params()
            X <- rnorm(p$n, mean = p$mean_x, sd = p$sd_x)
            Y <- p$b0 + p$slope * X + rnorm(p$n, mean = 0, sd = p$sd_e)
            data.frame(X = round(X, 2), Y = round(Y, 2))
        })

        # Likert items for whichever variables have scales switched on. X and Y
        # are different constructs, so each is standardized by its own
        # population parameters and gets its own independent mapping.
        # One reactive per variable, depending only on the sample, the item
        # settings and its own on/off tick -- never on the column picker, or
        # ticking a column would silently redraw the scores.
        mk_scale <- function(use, spec, score, ref_mean, ref_sd) {
            if (!use) return(NULL)
            it <- make_scale_items(score, ref_mean, ref_sd, spec$items,
                                   spec$kmin, spec$kmax, spec$rel,
                                   target = spec_target(spec))
            list(items = it, mean = rowMeans(it))
        }

        scaled_x <- reactive({
            p <- params()
            mk_scale(isTRUE(input$x_use), spec_x(),
                     sim_data()$X, p$mean_x, p$sd_x)
        })
        scaled_y <- reactive({
            p <- params()
            mk_scale(isTRUE(input$y_use), spec_y(),
                     sim_data()$Y, p$mean_y, p$sd_y)
        })
        scaled <- reactive(list(x = scaled_x(), y = scaled_y()))

        # The X and Y the student will actually analyze -- whichever version of
        # each ends up in their CSV. Every result box and the plot use these.
        analysis_vars <- reactive({
            d <- sim_data(); sc <- scaled()
            pick <- function(st, sv, raw) {
                if (analysis_choice(st) == "mean" && !is.null(sv)) sv$mean else raw
            }
            list(X = pick(sx(), sc$x, d$X),
                 Y = pick(sy(), sc$y, d$Y),
                 x_scaled = sx()$use && analysis_choice(sx()) == "mean",
                 y_scaled = sy()$use && analysis_choice(sy()) == "mean")
        })

        # Compact readout inside the parameter box; fuller note lives below.
        output$r_preview <- renderUI({
            p <- live()
            helpText(HTML(sprintf(
                "These settings give: total SD of Y = <b>%s</b>,
                 correlation &rho; = <b>%s</b>",
                fmt(p$sd_y), fmt(p$r)
            )))
        })

        output$sd_note <- renderUI({
            p <- live()
            helpText(HTML(sprintf(
                "Y ends up with a <b>total</b> SD of <b>%s</b> &mdash; bigger than
                 the noise you typed (%s), because Y varies for <i>two</i> reasons:
                 people differ on X (which moves them along the line), and each
                 person also has their own random error.
                 More noise &rarr; weaker r. Steeper slope &rarr; stronger r.",
                fmt(p$sd_y), fmt(p$sd_e)
            )))
        })

        output$equations <- renderUI({
            p <- params()
            withMathJax(
                helpText("Each X score is the mean of X plus random error:"),
                helpText(sprintf(
                    "$$X_i = \\bar{X} + e_i = %s + e_i, \\quad e_i \\sim N(0, %s)$$",
                    fmt(p$mean_x), fmt(p$sd_x)
                )),
                helpText("Each Y score starts at the mean of Y, is adjusted up or
                          down by how far that person's X is from average, then
                          gets its own random error:"),
                helpText(sprintf(
                    "$$Y_i = \\bar{Y} + b(X_i - \\bar{X}) + e_i
                           = %s + %s(X_i - %s) + e_i$$",
                    fmt(p$mean_y), fmt(p$slope), fmt(p$mean_x)
                )),
                helpText(sprintf("$$e_i \\sim N(0, %s)$$", fmt(p$sd_e))),
                helpText("Multiplying out gives the usual regression equation:"),
                helpText(sprintf(
                    "$$\\hat{Y}_i = b_0 + b_1 X_i = %s + %s X_i$$",
                    fmt(p$b0), fmt(p$slope)
                ))
            )
        })

        output$code <- renderText({
            p <- params()
            paste0(
                "n <- ", p$n, "\n",
                "X <- rnorm(n, mean = ", fmt_code(p$mean_x), ", sd = ",
                           fmt_code(p$sd_x), ")\n",
                "\n",
                "# start at the mean of Y, adjust for X, add noise\n",
                "Y <- ", fmt_code(p$mean_y), " + ", fmt_code(p$slope),
                " * (X - ", fmt_code(p$mean_x), ")",
                " + rnorm(n, mean = 0, sd = ", fmt_code(p$sd_e), ")\n",
                "\n",
                scale_code_snippet(sx(), "X", p$mean_x, p$sd_x),
                scale_code_snippet(sy(), "Y", p$mean_y, p$sd_y),
                "\n",
                # Analyse the same version of each variable the result boxes
                # and the plot used, so the code reproduces the reported numbers.
                {
                    xa <- if (analysis_choice(sx()) == "mean") "X_Scale_Mean" else "X"
                    ya <- if (analysis_choice(sy()) == "mean") "Y_Scale_Mean" else "Y"
                    paste0("plot(", xa, ", ", ya, ")\n",
                           "abline(lm(", ya, " ~ ", xa, "))\n",
                           "cor(", xa, ", ", ya, ")")
                }
            )
        })

        # The data as shown on screen and as downloaded: numbered rows, then
        # each variable's chosen column groups. Table and CSV stay in sync.
        labelled_data <- reactive({
            d <- sim_data(); sc <- scaled()
            cols <- c(scale_columns(sx(), "X", sc$x$items, sc$x$mean, d$X),
                      scale_columns(sy(), "Y", sc$y$items, sc$y$mean, d$Y))
            out <- cbind(seq_len(nrow(d)), data.frame(cols, check.names = FALSE))
            names(out)[1] <- ID_LABEL
            out
        })

        output$data_table <- renderTable({
            labelled_data()
        }, digits = 2, striped = TRUE)

        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("simulated_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
            },
            content = function(file) {
                write.csv(labelled_data(), file, row.names = FALSE)
            }
        )

        output$sample_stats <- renderTable({
            d <- sim_data(); p <- params(); sc <- scaled()
            fit <- lm(Y ~ X, data = d)

            # Greek letter for the population parameter, Roman for the sample
            # statistic, each shown next to its own number.
            greek <- c("μ", "σ", "μ", "σ", "σ", "β", "ρ")
            roman <- c("M", "s", "M", "s", "s", "b", "r")
            pop   <- c(p$mean_x, p$sd_x, p$mean_y, p$sd_y, p$sd_e, p$slope, p$r)
            samp  <- c(mean(d$X), sd(d$X), mean(d$Y), sd(d$Y),
                       summary(fit)$sigma, coef(fit)[2],
                       suppressWarnings(cor(d$X, d$Y)))

            tab <- data.frame(
                " "          = c("Mean of X", "SD of X", "Mean of Y",
                                 "SD of Y (total spread)",
                                 "SD of the error (noise)",
                                 "Slope", "Correlation"),
                "Population" = paste(greek, "=", fmt(pop)),
                check.names  = FALSE
            )

            # Both sample columns are always shown when a scale is on, so the
            # cost of measuring with items is visible side by side. The one the
            # CSV actually holds is highlighted.
            any_scale  <- sx()$use || sy()$use
            raw_in_csv <- !any_scale ||
                (analysis_choice(sx()) == "raw" && analysis_choice(sy()) == "raw")

            tab[[paste0("This sample", csv_flag(raw_in_csv))]] <-
                csv_cell(paste(roman, "=", fmt(samp)), raw_in_csv)

            if (any_scale) {
                Xs <- if (!is.null(sc$x)) sc$x$mean else d$X
                Ys <- if (!is.null(sc$y)) sc$y$mean else d$Y
                fs <- lm(Ys ~ Xs)
                fs_sig <- suppressWarnings(summary(fs)$sigma)
                sv <- c(mean(Xs), sd(Xs), mean(Ys), sd(Ys),
                        fs_sig, coef(fs)[2],
                        suppressWarnings(cor(Xs, Ys)))
                tab[[paste0("This sample as scales", csv_flag(!raw_in_csv))]] <-
                    csv_cell(paste(roman, "=", fmt(sv)), !raw_in_csv)
            }
            tab
        }, striped = TRUE, colnames = TRUE, rownames = FALSE,
           sanitize.text.function = identity)

        # Axis limits from the MODEL, not the sample, so the plot frame and the
        # true line stay put when students regenerate with the same parameters.
        # Choose k so that every point lands inside the frame about 90% of the
        # time (a point is 2 coordinates, hence 2n normal deviates). Using the
        # marginal SD of Y also keeps the true line in frame.
        axis_limits <- reactive({
            p <- params()
            k <- max(3, qnorm(1 - (1 - 0.9^(1 / (2 * p$n))) / 2))
            pad <- function(center, sd) {
                half <- if (sd > 0) k * sd else max(1, abs(center) * 0.1)
                c(center - half, center + half)
            }
            list(x = pad(p$mean_x, p$sd_x),
                 y = pad(p$mean_y, p$sd_y))
        })

        output$scatter <- renderPlot({
            p <- params(); a <- analysis_vars(); lim <- axis_limits()

            # On a scale metric the population line is in the wrong units, and
            # the model-based axis limits no longer apply, so both are dropped.
            # With no variability there is no line to fit, but the points are
            # still worth showing -- the flat row of identical dots IS the point.
            flat <- is_constant(a$X) || is_constant(a$Y)
            r_txt <- paste("Sample r =", fmt(suppressWarnings(cor(a$X, a$Y))))

            # Each axis is framed by its own variable: a scale gets its full
            # response range, a continuous variable keeps the model-based frame.
            xlim <- if (a$x_scaled) scale_range(sx()) else lim$x
            ylim <- if (a$y_scaled) scale_range(sy()) else lim$y

            plot(a$X, a$Y,
                 xlab = if (a$x_scaled) "X (scale mean)" else "X",
                 ylab = if (a$y_scaled) "Y (scale mean)" else "Y",
                 xlim = xlim, ylim = ylim,
                 xaxt = if (a$x_scaled) "n" else "s",
                 yaxt = if (a$y_scaled) "n" else "s",
                 pch = 19, col = "steelblue", main = r_txt)
            if (a$x_scaled) scale_axis(1, sx())
            if (a$y_scaled) scale_axis(2, sy())

            if (a$x_scaled || a$y_scaled) {
                if (!flat) {
                    abline(lm(a$Y ~ a$X), col = "firebrick", lwd = 2)
                    legend("topleft", bty = "n",
                           legend = "Sample regression line",
                           col = "firebrick", lwd = 2)
                }
            } else {
                # the line the data actually came from
                abline(a = p$b0, b = p$slope, col = "grey40", lwd = 2, lty = 2)
                if (!flat) {
                    # the line estimated from this particular sample
                    abline(lm(a$Y ~ a$X), col = "firebrick", lwd = 2)
                }
                legend("topleft", bty = "n",
                       legend = c("Population line (the true model)",
                                  if (!flat) "Sample regression line"),
                       col = c("grey40", if (!flat) "firebrick"),
                       lwd = 2, lty = if (flat) 2 else c(2, 1))
            }
            if (flat) {
                mtext("No variability \u2014 no relationship can be estimated",
                      side = 3, line = 0.2, col = "#7a5320", cex = 0.95)
            }
        })

        output$cor_result <- renderUI({
            d <- analysis_vars()              # whatever is in the student's CSV
            bad <- c(if (is_constant(d$X)) "X", if (is_constant(d$Y)) "Y")
            validate(need(length(bad) == 0, msg_no_variance(bad)))

            ct <- cor.test(d$X, d$Y)          # Pearson; df = N - 2
            sig <- ct$p.value < .05

            # Fisher-z CI needs N > 3; guard the small-N case.
            ci <- if (length(ct$conf.int) == 2)
                sprintf("95%% CI [%s, %s]",
                        fmt_r(ct$conf.int[1]), fmt_r(ct$conf.int[2]))
            else NULL

            div(class = "result-box primary",
                div(class = "result-flag", "Model write-up"),
                div(class = "result-title", "Pearson correlation"),
                div(class = "apa", HTML(sprintf(
                    "<i>r</i>(%d) = %s, <i>p</i> %s",
                    round(ct$parameter), fmt_r(ct$estimate), fmt_p(ct$p.value)
                ))),
                if (!is.null(ci)) div(HTML(ci)),
                div(class = "decision", sprintf(
                    "The correlation is %sstatistically significant at α = .05.",
                    if (sig) "" else "not "
                )),
                # Computable, but the measure may be barely discriminating.
                lapply(c(if (d$x_scaled) caution_note(d$X, sx()$kmin, sx()$kmax),
                         if (d$y_scaled) caution_note(d$Y, sy()$kmin, sy()$kmax)),
                       function(m) div(class = "caution-note", m))
            )
        })

        output$reg_result <- renderUI({
            d <- analysis_vars()
            bad <- c(if (is_constant(d$X)) "X", if (is_constant(d$Y)) "Y")
            validate(need(length(bad) == 0, msg_no_variance(bad)))

            fit <- lm(d$Y ~ d$X)
            sm  <- summary(fit)
            b0  <- coef(fit)[1]; b1 <- coef(fit)[2]
            se_b1 <- sm$coefficients[2, 2]
            t_b1  <- sm$coefficients[2, 3]
            p_b1  <- sm$coefficients[2, 4]
            r2    <- sm$r.squared
            Fs    <- sm$fstatistic            # value, numdf, dendf
            # NULL whenever the predictor carries no information at all.
            validate(need(!is.null(Fs), msg_no_variance("X")))
            Fp    <- pf(Fs[1], Fs[2], Fs[3], lower.tail = FALSE)

            # Prediction equation, with the slope's sign read aloud.
            eq <- sprintf("&#374; = %s %s %sX",
                          fmt(b0), if (b1 < 0) "&minus;" else "+", fmt(abs(b1)))

            div(class = "result-box",
                div(class = "result-title", "Linear regression"),
                div(class = "apa", HTML(eq)),
                div(HTML(sprintf(
                    "<i>R</i>&sup2; = %s, <i>F</i>(%d, %d) = %s, <i>p</i> %s",
                    fmt_r(r2), round(Fs[2]), round(Fs[3]), fmt(Fs[1]), fmt_p(Fp)
                ))),
                div(HTML(sprintf(
                    "Slope: <i>b</i> = %s, <i>SE</i> = %s, <i>t</i>(%d) = %s, <i>p</i> %s",
                    fmt(b1), fmt(se_b1), round(sm$df[2]), fmt(t_b1), fmt_p(p_b1)
                )))
            )
        })
    })
}

# =============================================================================
#  t-test module -- independent-samples t-test data
# =============================================================================

ttestUI <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Simulate Data: Independent-samples t-test"),
        sidebarLayout(

            sidebarPanel(
                width = 4,

                div(
                    class = "param-box",

                    span(class = "box-title", "Population parameters"),
                    div(class = "box-note",
                        "The true values in each group in the whole population.
                         Your sample is drawn from populations that look like
                         this."),

                    span(class = "var-head", paste0(G1, "  (IV level 1)")),
                    numericInput(ns("mean1"), "Mean of Group 1:", value = 100),
                    numericInput(ns("sd1"),   "SD of Group 1:",   value = 15,
                                 min = 0),

                    span(class = "var-head", paste0(G2, "  (IV level 2)")),
                    numericInput(ns("mean2"), "Mean of Group 2:", value = 110),
                    numericInput(ns("sd2"),   "SD of Group 2:",   value = 15,
                                 min = 0),

                    uiOutput(ns("d_preview"))
                ),

                # Measurement, not population: how each variable is observed.
                div(
                    class = "param-box", style = "margin-top: 12px;",
                    span(class = "box-title", "Measure as Likert scales"),
                    div(class = "box-note",
                        "The IV scale is built so that a median split of its
                         scale mean reproduces the two groups exactly."),
                    scaleControlsUI(ns, "iv", "Grouping IV as a scale",
                                    raw_label = "Group membership"),
                    scaleControlsUI(ns, "dv", "Outcome / DV as a scale")
                ),

                # Sample size per group and the button sit outside the box.
                div(
                    class = "sample-controls",
                    fluidRow(
                        column(6, numericInput(ns("n"), "Cases per group (n):",
                                               value = 30, min = 2, step = 1)),
                        column(6, div(style = "margin-top: 25px;",
                                      actionButton(ns("generate"), "Generate Data",
                                                   class = "btn-primary",
                                                   width = "100%")))
                    )
                ),

                tags$hr(),
                tags$strong("The model"),
                uiOutput(ns("equations")),

                tags$hr(),
                tags$strong("R code"),
                verbatimTextOutput(ns("code"))
            ),

            mainPanel(
                width = 8,
                # Full width and above everything: the unequal-groups warning
                # is the point of measuring the IV as a scale.
                uiOutput(ns("split_note")),
                div(
                    class = "panel-row",
                    div(
                        class = "stats-col",
                        tags$h4("Descriptive Statistics"),
                        tableOutput(ns("sample_stats")),
                        div(
                            class = "data-head",
                            tags$h4("Sample Data"),
                            downloadButton(ns("download_csv"), "CSV",
                                           class = "btn-xs")
                        ),
                        uiOutput(ns("sort_note")),
                        div(
                            style = "max-height: 420px; overflow-y: auto;",
                            tableOutput(ns("data_table"))
                        )
                    ),
                    div(
                        class = "plot-col",
                        tags$h4("Independent Groups Comparison"),
                        plotOutput(ns("dotplot"), height = "500px"),
                        uiOutput(ns("ttest_result")),
                        uiOutput(ns("anova_result")),
                        uiOutput(ns("dummy_result"))
                    )
                )
            )
        )
    )
}

ttestServer <- function(id) {
    moduleServer(id, function(input, output, session) {

        # Everything implied by a set of parameter values.
        derive <- function(mean1, sd1, mean2, sd2) {
            sd1 <- abs(sd1); sd2 <- abs(sd2)
            sd_pooled <- sqrt((sd1^2 + sd2^2) / 2)   # equal-n pooled SD
            list(
                mean1 = mean1, sd1 = sd1, mean2 = mean2, sd2 = sd2,
                diff = mean2 - mean1, sd_pooled = sd_pooled,
                d = if (sd_pooled > 0) (mean2 - mean1) / sd_pooled else 0
            )
        }

        live <- reactive({
            req(input$sd1, input$sd2)
            derive(input$mean1, input$sd1, input$mean2, input$sd2)
        })

        params <- eventReactive(input$generate, {
            p <- derive(input$mean1, input$sd1, input$mean2, input$sd2)
            p$n <- max(2, round(input$n))
            p
        }, ignoreNULL = FALSE)

        spec_iv <- eventReactive(list(input$generate, input$iv_gen),
                                 scale_spec_of(input, "iv"), ignoreNULL = FALSE)
        spec_dv <- eventReactive(list(input$generate, input$dv_gen),
                                 scale_spec_of(input, "dv"), ignoreNULL = FALSE)

        siv <- reactive(scale_settings(input, "iv", spec_iv()))
        sdv <- reactive(scale_settings(input, "dv", spec_dv()))

        observe_scale_range(input, session, "iv")
        observe_scale_range(input, session, "dv")

        # Whether the current draw has been nudged to give equal groups. Reset
        # by anything that produces a new draw, so the realistic behaviour is
        # what a student meets first every time.
        force_equal <- reactiveVal(FALSE)
        observeEvent(input$iv_force, force_equal(TRUE))
        observeEvent(input$iv_unforce, force_equal(FALSE))
        observeEvent(list(input$generate, input$iv_gen), force_equal(FALSE))

        # One sample's raw material: the latent construct the IV scale measures,
        # and each person's standardized DV error. Drawn once per Generate Data,
        # so nothing downstream can quietly redraw the sample.
        raw_draw <- reactive({
            p <- params()
            list(latent = rnorm(2 * p$n), e = rnorm(2 * p$n))
        })

        # The IV items exactly as generated, before any nudging.
        iv_items_raw <- reactive({
            spec <- spec_iv()
            make_scale_items(raw_draw()$latent, 0, 1, spec$items,
                             spec$kmin, spec$kmax, spec$rel,
                             target = spec_target(spec))
        })

        # Nudging is a pure transform of that draw, so asking for equal groups
        # never re-randomizes the scores -- it only moves the few it must.
        scaled_iv <- reactive({
            if (!isTRUE(input$iv_use)) return(NULL)
            spec <- spec_iv()
            it <- iv_items_raw(); moved <- 0L
            if (force_equal()) {
                f <- force_equal_split(it, spec$kmin, spec$kmax)
                it <- f$items; moved <- f$moved
            }
            rownames(it) <- NULL
            list(items = it, mean = rowMeans(it), moved = moved)
        })

        sim_data <- reactive({
            p <- params(); d <- raw_draw()
            # With the IV measured as a scale, group membership IS the median
            # split of that scale. Otherwise the groups are fixed and equal.
            grp <- if (isTRUE(input$iv_use)) median_split_groups(scaled_iv()$mean)
                   else factor(rep(c(G1, G2), each = p$n), levels = c(G1, G2))
            mu <- ifelse(grp == G1, p$mean1, p$mean2)
            sg <- ifelse(grp == G1, p$sd1,   p$sd2)
            data.frame(Group = grp, Score = round(mu + sg * d$e, 2))
        })

        # DV: both groups must share ONE mapping, or standardizing each to its
        # own centre would erase the very difference being tested. The reference
        # spread is the total (between + within) population SD.
        scaled_dv <- reactive({
            if (!isTRUE(input$dv_use)) return(NULL)
            p <- params(); d <- sim_data(); spec <- spec_dv()
            ref_m  <- (p$mean1 + p$mean2) / 2
            ref_sd <- sqrt(p$sd_pooled^2 + (p$diff / 2)^2)
            it <- make_scale_items(d$Score, ref_m, ref_sd, spec$items,
                                   spec$kmin, spec$kmax, spec$rel,
                                   target = spec_target(spec))
            list(items = it, mean = rowMeans(it))
        })

        scaled <- reactive(list(iv = scaled_iv(), dv = scaled_dv()))

        # The scores the student will actually analyze. The grouping variable is
        # always Group -- a median split of the IV scale reproduces it exactly.
        analysis_vars <- reactive({
            d <- sim_data(); sc <- scaled()
            score <- if (analysis_choice(sdv()) == "mean" && !is.null(sc$dv))
                sc$dv$mean else d$Score
            list(Group = d$Group, Score = score,
                 dv_scaled = sdv()$use && analysis_choice(sdv()) == "mean")
        })

        output$d_preview <- renderUI({
            p <- live()
            helpText(HTML(sprintf(
                "These settings give: mean difference = <b>%s</b>,
                 effect size Cohen's <i>d</i> = <b>%s</b>",
                fmt(p$diff), fmt(p$d)
            )))
        })

        output$equations <- renderUI({
            p <- params()
            withMathJax(
                helpText("Each score is its group's mean plus random error:"),
                helpText(sprintf(
                    "$$Y_{i,1} = \\mu_1 + e_i = %s + e_i, \\quad e_i \\sim N(0, %s)$$",
                    fmt(p$mean1), fmt(p$sd1)
                )),
                helpText(sprintf(
                    "$$Y_{i,2} = \\mu_2 + e_i = %s + e_i, \\quad e_i \\sim N(0, %s)$$",
                    fmt(p$mean2), fmt(p$sd2)
                )),
                helpText("The effect size is the mean difference in SD units:"),
                helpText(sprintf(
                    "$$d = \\frac{\\mu_2 - \\mu_1}{s_{pooled}}
                         = \\frac{%s - %s}{%s} = %s$$",
                    fmt(p$mean2), fmt(p$mean1), fmt(p$sd_pooled), fmt(p$d)
                ))
            )
        })

        output$code <- renderText({
            p <- params()
            dv_ref_m  <- (p$mean1 + p$mean2) / 2
            dv_ref_sd <- sqrt(p$sd_pooled^2 + (p$diff / 2)^2)
            av <- if (analysis_choice(sdv()) == "mean") "Score_Scale_Mean"
                  else "Score"

            if (siv()$use) {
                # The IV is measured, so the design falls out of the split: the
                # scale comes first, the groups come from it, and only then does
                # each person get a DV from whichever group they landed in.
                paste0(
                    "n <- ", p$n, "   # per group BEFORE the split; ",
                        2 * p$n, " participants in total\n",
                    iv_code_snippet(siv()),
                    "\n# each person's DV comes from the group the split put them in\n",
                    "Score <- ifelse(Group == \"", G1, "\", ",
                        fmt_code(p$mean1), ", ", fmt_code(p$mean2), ") +\n",
                    "         ifelse(Group == \"", G1, "\", ",
                        fmt_code(p$sd1), ", ", fmt_code(p$sd2),
                        ") * rnorm(2 * n)\n",
                    scale_code_snippet(sdv(), "Score", dv_ref_m, dv_ref_sd),
                    "\n# var.equal = TRUE gives Student's t (R defaults to Welch)\n",
                    "g1 <- ", av, "[Group == \"", G1, "\"]\n",
                    "g2 <- ", av, "[Group == \"", G2, "\"]\n",
                    "t.test(g2, g1, var.equal = TRUE)\n",
                    "boxplot(g1, g2)")
            } else {
                paste0(
                    "n <- ", p$n, "\n",
                    "group1 <- rnorm(n, mean = ", fmt_code(p$mean1),
                        ", sd = ", fmt_code(p$sd1), ")\n",
                    "group2 <- rnorm(n, mean = ", fmt_code(p$mean2),
                        ", sd = ", fmt_code(p$sd2), ")\n",
                    if (sdv()$use)
                        paste0("\n# the two groups as one data set\n",
                               "Score <- c(group1, group2)\n",
                               "Group <- rep(c(\"", G1, "\", \"", G2,
                               "\"), each = n)\n") else "",
                    scale_code_snippet(sdv(), "Score", dv_ref_m, dv_ref_sd),
                    "\n",
                    "# var.equal = TRUE gives Student's t (R defaults to Welch)\n",
                    if (analysis_choice(sdv()) == "mean")
                        paste0("g1 <- Score_Scale_Mean[Group == \"", G1, "\"]\n",
                               "g2 <- Score_Scale_Mean[Group == \"", G2, "\"]\n",
                               "t.test(g2, g1, var.equal = TRUE)\n",
                               "boxplot(g1, g2)")
                    else paste0("t.test(group2, group1, var.equal = TRUE)\n",
                                "boxplot(group1, group2)"))
            }
        })

        labelled_data <- reactive({
            d <- sim_data(); sc <- scaled()
            cols <- c(scale_columns(siv(), "Group", sc$iv$items, sc$iv$mean,
                                    d$Group),
                      scale_columns(sdv(), "Score", sc$dv$items, sc$dv$mean,
                                    d$Score))
            out <- cbind(seq_len(nrow(d)), data.frame(cols, check.names = FALSE))
            names(out)[1] <- ID_LABEL
            out
        })

        # Ordered so the median split is visible: with the IV measured as a
        # scale, reading down the scale mean shows exactly where the cut falls
        # and which tied participants ended up on which side. The CSV keeps
        # participant order -- row order does not matter to a stats package,
        # and an ID-ordered file is what students expect to open.
        display_data <- reactive({
            d <- labelled_data()
            if (isTRUE(input$iv_use)) d <- d[order(scaled_iv()$mean), ,
                                             drop = FALSE]
            d
        })

        output$sort_note <- renderUI({
            if (!isTRUE(input$iv_use)) return(NULL)
            helpText(HTML("Sorted by the IV scale mean so you can see where the
                           median split falls. The downloaded CSV keeps
                           participant order."))
        })

        output$data_table <- renderTable({
            display_data()
        }, digits = 2, striped = TRUE)

        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("simulated_ttest_data_",
                       format(Sys.Date(), "%Y-%m-%d"), ".csv")
            },
            content = function(file) {
                write.csv(labelled_data(), file, row.names = FALSE)
            }
        )

        output$split_note <- renderUI({
            if (!isTRUE(input$iv_use)) return(NULL)
            ns <- session$ns
            d  <- sim_data()
            n1 <- sum(d$Group == G1); n2 <- sum(d$Group == G2)

            if (force_equal()) {
                moved <- scaled_iv()$moved
                return(div(
                    class = "split-note",
                    HTML(sprintf(
                        "Group sizes forced to %d and %d by nudging <b>%d</b>
                         scale score%s up one point on a single item. Real data
                         does not oblige like this \u2014 the nudge is here so
                         you can see what insisting on equal groups costs.",
                        n1, n2, moved, if (moved == 1) "" else "s")),
                    actionLink(ns("iv_unforce"), "Undo, show the real split")))
            }
            if (n1 == n2) return(NULL)
            div(
                class = "split-warn",
                span(class = "warn-head",
                     sprintf("\u26a0 Unequal groups: %d vs %d", n1, n2)),
                HTML("Several participants share the same scale mean, and a
                      median split must put all of them on the same side. This
                      is what splitting a real measured variable does \u2014 the
                      scale is too coarse to divide people evenly, and which
                      side a tied participant lands on is decided by the
                      cut-off rather than by anything about that person.
                      <b>Sort the data table by the scale mean to see exactly
                      where the split falls.</b>"),
                br(),
                actionButton(ns("iv_force"),
                             "Force equal group sizes (not how real data works)",
                             class = "btn-sm btn-danger"))
        })

        output$sample_stats <- renderTable({
            d <- sim_data(); p <- params(); sc <- scaled()

            # The six statistics, computed on whichever version of the score
            # we are given.
            stats_for <- function(v) {
                a <- v[d$Group == G1]; b <- v[d$Group == G2]
                sp <- sqrt((var(a) + var(b)) / 2)
                c(mean(a), sd(a), mean(b), sd(b), mean(b) - mean(a),
                  if (sp > 0) (mean(b) - mean(a)) / sp else 0)
            }

            greek <- c("μ₁", "σ₁", "μ₂", "σ₂", "μ₂−μ₁", "δ")
            roman <- c("M₁", "s₁", "M₂", "s₂", "M₂−M₁", "d")
            pop   <- c(p$mean1, p$sd1, p$mean2, p$sd2, p$diff, p$d)

            tab <- data.frame(
                " "          = c("Mean of Group 1", "SD of Group 1",
                                 "Mean of Group 2", "SD of Group 2",
                                 "Mean difference", "Cohen's d"),
                "Population" = paste(greek, "=", fmt(pop)),
                check.names  = FALSE
            )

            raw_in_csv <- !sdv()$use || analysis_choice(sdv()) == "raw"
            tab[[paste0("This sample", csv_flag(raw_in_csv))]] <-
                csv_cell(paste(roman, "=", fmt(stats_for(d$Score))), raw_in_csv)

            if (sdv()$use) {
                tab[[paste0("This sample as scales", csv_flag(!raw_in_csv))]] <-
                    csv_cell(paste(roman, "=", fmt(stats_for(sc$dv$mean))),
                             !raw_in_csv)
            }

            # With the IV measured as a scale the group sizes are an outcome of
            # the split rather than something the student set, so show them.
            if (isTRUE(input$iv_use)) {
                n1 <- sum(d$Group == G1); n2 <- sum(d$Group == G2)
                extra <- tab[1, ]
                extra[[1]] <- "Group sizes"
                extra[[2]] <- paste0("N = ", nrow(d))
                for (k in seq(3, ncol(tab)))
                    extra[[k]] <- sprintf("n\u2081 = %d, n\u2082 = %d", n1, n2)
                tab <- rbind(tab, extra)
            }
            tab
        }, striped = TRUE, colnames = TRUE, rownames = FALSE,
           sanitize.text.function = identity)

        output$ttest_result <- renderUI({
            d <- analysis_vars()              # whatever is in the student's CSV
            s1 <- d$Score[d$Group == G1]; s2 <- d$Score[d$Group == G2]

            validate(need(!is_constant(d$Score),
                          msg_no_variance("the outcome")))
            validate(need(sum(d$Group == G1) > 0 && sum(d$Group == G2) > 0,
                          msg_empty_group()))
            # Separately reachable: the groups differ but nobody inside a group
            # does, which makes t infinite rather than merely undefined.
            validate(need(!is_constant(s1) && !is_constant(s2),
                          msg_no_within_variance()))

            # Student's t (pooled variance), to match the pooled-SD Cohen's d.
            tt <- t.test(s2, s1, var.equal = TRUE)
            samp_d <- (mean(s2) - mean(s1)) / sqrt((var(s1) + var(s2)) / 2)
            sig <- tt$p.value < .05

            div(class = "result-box primary",
                div(class = "result-flag", "Model write-up"),
                div(class = "result-title", "Independent-samples t-test"),
                div(class = "apa", HTML(sprintf(
                    "<i>t</i>(%d) = %s, <i>p</i> %s, <i>d</i> = %s",
                    round(tt$parameter), fmt(tt$statistic), fmt_p(tt$p.value),
                    fmt(samp_d)
                ))),
                div(HTML(sprintf(
                    "Mean difference = %s, 95%% CI [%s, %s]",
                    fmt(mean(s2) - mean(s1)),
                    fmt(tt$conf.int[1]), fmt(tt$conf.int[2])
                ))),
                div(class = "decision", sprintf(
                    "The difference is %sstatistically significant at α = .05.",
                    if (sig) "" else "not "
                )),
                lapply(if (d$dv_scaled)
                           caution_note(d$Score, sdv()$kmin, sdv()$kmax),
                       function(m) div(class = "caution-note", m))
            )
        })

        # Just for fun: the same comparison as a one-way ANOVA. With two groups
        # F = t^2 and the p-value is identical to the t-test above.
        output$anova_result <- renderUI({
            a  <- analysis_vars()
            validate(need(!is_constant(a$Score), msg_no_variance("the outcome")))
            validate(need(sum(a$Group == G1) > 0 && sum(a$Group == G2) > 0,
                          msg_empty_group()))
            # Same condition as the t-test above: this box claims F = t^2, so it
            # must not report F = Inf while the t-test declines to compute.
            validate(need(!is_constant(a$Score[a$Group == G1]) &&
                          !is_constant(a$Score[a$Group == G2]),
                          msg_no_within_variance()))
            d  <- data.frame(Group = a$Group, Score = a$Score)
            s  <- summary(aov(Score ~ Group, data = d))[[1]]
            Fv <- s[["F value"]][1]; Fp <- s[["Pr(>F)"]][1]
            ss <- s[["Sum Sq"]]; eta2 <- ss[1] / sum(ss)

            div(class = "result-box",
                div(class = "result-title", "One-way ANOVA"),
                div(class = "apa", HTML(sprintf(
                    "<i>F</i>(%d, %d) = %s, <i>p</i> %s",
                    round(s[["Df"]][1]), round(s[["Df"]][2]),
                    fmt(Fv), fmt_p(Fp)
                ))),
                div(HTML(sprintf("&eta;&sup2; = %s", fmt_r(eta2)))),
                div(class = "decision",
                    "Same p as the t-test — with two groups, F = t².")
            )
        })

        # And again as a correlation: code the groups 0/1 and correlate with the
        # score. This point-biserial r has the same p; the t-test is a
        # correlation with a two-value predictor.
        output$dummy_result <- renderUI({
            d <- analysis_vars()
            validate(need(!is_constant(d$Score), msg_no_variance("the outcome")))
            validate(need(sum(d$Group == G1) > 0 && sum(d$Group == G2) > 0,
                          msg_empty_group()))
            # This box's punchline is that r-squared equals the ANOVA's eta-squared,
            # so it stands or falls with the ANOVA.
            validate(need(!is_constant(d$Score[d$Group == G1]) &&
                          !is_constant(d$Score[d$Group == G2]),
                          msg_no_within_variance()))
            x  <- as.integer(d$Group) - 1L      # Group 1 = 0, Group 2 = 1
            ct <- cor.test(x, d$Score)

            div(class = "result-box",
                div(class = "result-title",
                    "Point-biserial correlation (groups coded 0/1)"),
                div(class = "apa", HTML(sprintf(
                    "<i>r</i>(%d) = %s, <i>p</i> %s",
                    round(ct$parameter), fmt_r(ct$estimate), fmt_p(ct$p.value)
                ))),
                div(class = "decision",
                    "Same p once more — and r² equals the ANOVA's η².")
            )
        })

        # Y-axis range from the MODEL, so the frame and the population-mean lines
        # stay put when students regenerate with the same parameters.
        y_limits <- reactive({
            p <- params()
            k <- max(3, qnorm(1 - (1 - 0.9^(1 / (2 * p$n))) / 2))
            lo <- min(p$mean1 - k * p$sd1, p$mean2 - k * p$sd2)
            hi <- max(p$mean1 + k * p$sd1, p$mean2 + k * p$sd2)
            if (lo == hi) c(lo - 1, hi + 1) else c(lo, hi)
        })

        output$dotplot <- renderPlot({
            d <- analysis_vars(); p <- params()
            s1 <- d$Score[d$Group == G1]; s2 <- d$Score[d$Group == G2]
            samp_sd_pooled <- sqrt((var(s1) + var(s2)) / 2)
            samp_d <- if (samp_sd_pooled > 0)
                (mean(s2) - mean(s1)) / samp_sd_pooled else 0

            # On a scale metric the population-mean lines are in the wrong
            # units, so the frame is the scale's own full response range --
            # which is also what makes a ceiling or floor effect visible.
            ylim <- if (d$dv_scaled) scale_range(sdv()) else y_limits()

            xpos <- c(1, 2)
            plot(NA, xlim = c(0.5, 2.5), ylim = ylim,
                 xaxt = "n", xlab = "",
                 yaxt = if (d$dv_scaled) "n" else "s",
                 ylab = if (d$dv_scaled) "DV scale mean" else "Score (DV)",
                 main = paste("Sample d =", fmt(samp_d)))
            axis(1, at = xpos, labels = c(G1, G2))
            if (d$dv_scaled) scale_axis(2, sdv())

            # jittered raw scores
            jit1 <- xpos[1] + runif(length(s1), -0.12, 0.12)
            jit2 <- xpos[2] + runif(length(s2), -0.12, 0.12)
            points(jit1, s1, pch = 19, col = "steelblue")
            points(jit2, s2, pch = 19, col = "steelblue")

            seg <- 0.28
            if (!d$dv_scaled) {
                # population means: grey dashed
                segments(xpos - seg, c(p$mean1, p$mean2),
                         xpos + seg, c(p$mean1, p$mean2),
                         col = "grey40", lwd = 2, lty = 2)
            }
            # sample means: red solid
            segments(xpos - seg, c(mean(s1), mean(s2)),
                     xpos + seg, c(mean(s1), mean(s2)),
                     col = "firebrick", lwd = 2)

            legend("topleft", bty = "n",
                   legend = if (d$dv_scaled) "Sample means"
                            else c("Population means (the true model)",
                                   "Sample means"),
                   col = if (d$dv_scaled) "firebrick" else c("grey40", "firebrick"),
                   lwd = 2, lty = if (d$dv_scaled) 1 else c(2, 1))

            if (is_constant(d$Score)) {
                mtext("No variability \u2014 the groups cannot be compared",
                      side = 3, line = 0.2, col = "#7a5320", cex = 0.95)
            }
        })
    })
}

# =============================================================================
#  Paired-samples (dependent) t-test module
#
#  Like the independent module, but each participant is measured twice and the
#  two measurements are CORRELATED. That correlation is a third population
#  parameter: the stronger it is, the smaller the SD of the difference scores
#  (sd_D = sqrt(s1^2 + s2^2 - 2*rho*s1*s2)), and the more powerful the test.
# =============================================================================

pairedUI <- function(id) {
    ns <- NS(id)
    tagList(
        titlePanel("Simulate Data: Paired-samples t-test"),
        sidebarLayout(

            sidebarPanel(
                width = 4,

                div(
                    class = "param-box",

                    span(class = "box-title", "Population parameters"),
                    div(class = "box-note",
                        "Each participant is measured twice. Set the true mean and
                         SD of each measurement, plus how strongly the two
                         measurements are correlated."),

                    span(class = "var-head", paste0(C1, "  (measurement 1)")),
                    numericInput(ns("mean_c1"), "Mean of Condition 1:", value = 100),
                    numericInput(ns("sd_c1"),   "SD of Condition 1:",   value = 15,
                                 min = 0),

                    span(class = "var-head", paste0(C2, "  (measurement 2)")),
                    numericInput(ns("mean_c2"), "Mean of Condition 2:", value = 110),
                    numericInput(ns("sd_c2"),   "SD of Condition 2:",   value = 15,
                                 min = 0),

                    numericInput(ns("rho"),
                                 "Correlation between the two measurements (r):",
                                 value = 0.5, min = -1, max = 1, step = 0.05),

                    uiOutput(ns("dz_preview"))
                ),

                # Measurement, not population: how the outcome is observed. The
                # IV here is the repeated measure itself, so only the DV gets a
                # scale -- one definition, applied to both measurements.
                div(
                    class = "param-box", style = "margin-top: 12px;",
                    span(class = "box-title", "Measure as Likert scales"),
                    div(class = "box-note",
                        "Both measurements share one scale definition and one
                         mapping, so the difference between them survives."),
                    scaleControlsUI(ns, "dv", "Outcome / DV as a scale")
                ),

                # Sample size and the button sit outside the box.
                div(
                    class = "sample-controls",
                    fluidRow(
                        column(6, numericInput(ns("n"),
                                               "Number of participants (n):",
                                               value = 30, min = 2, step = 1)),
                        column(6, div(style = "margin-top: 25px;",
                                      actionButton(ns("generate"), "Generate Data",
                                                   class = "btn-primary",
                                                   width = "100%")))
                    )
                ),

                tags$hr(),
                tags$strong("The model"),
                uiOutput(ns("equations")),

                tags$hr(),
                tags$strong("R code"),
                verbatimTextOutput(ns("code"))
            ),

            mainPanel(
                width = 8,
                div(
                    class = "panel-row",
                    div(
                        class = "stats-col",
                        tags$h4("Descriptive Statistics"),
                        tableOutput(ns("sample_stats")),
                        div(
                            class = "data-head",
                            tags$h4("Sample Data"),
                            downloadButton(ns("download_csv"), "CSV",
                                           class = "btn-xs")
                        ),
                        helpText(sprintf("Score1 = %s, Score2 = %s.", C1, C2)),
                        div(
                            style = "max-height: 420px; overflow-y: auto;",
                            tableOutput(ns("data_table"))
                        )
                    ),
                    div(
                        class = "plot-col",
                        tags$h4("Paired Scores Comparison"),
                        plotOutput(ns("pairplot"), height = "500px"),
                        uiOutput(ns("ttest_result")),
                        uiOutput(ns("anova_result")),
                        uiOutput(ns("cor_result"))
                    )
                )
            )
        )
    )
}

pairedServer <- function(id) {
    moduleServer(id, function(input, output, session) {

        # Everything implied by a set of parameter values.
        derive <- function(mean_c1, sd_c1, mean_c2, sd_c2, rho) {
            sd_c1 <- abs(sd_c1); sd_c2 <- abs(sd_c2)
            rho   <- max(-0.99, min(0.99, rho))   # keep sd_D > 0 and generation valid
            sd_D  <- sqrt(sd_c1^2 + sd_c2^2 - 2 * rho * sd_c1 * sd_c2)
            list(
                mean_c1 = mean_c1, sd_c1 = sd_c1, mean_c2 = mean_c2, sd_c2 = sd_c2,
                rho = rho, sd_D = sd_D, diff = mean_c2 - mean_c1,
                dz = if (sd_D > 0) (mean_c2 - mean_c1) / sd_D else 0
            )
        }

        live <- reactive({
            req(input$sd_c1, input$sd_c2, input$rho)
            derive(input$mean_c1, input$sd_c1, input$mean_c2, input$sd_c2, input$rho)
        })

        params <- eventReactive(input$generate, {
            p <- derive(input$mean_c1, input$sd_c1, input$mean_c2, input$sd_c2,
                        input$rho)
            p$n <- max(2, round(input$n))
            p
        }, ignoreNULL = FALSE)

        spec_dv <- eventReactive(list(input$generate, input$dv_gen),
                                 scale_spec_of(input, "dv"), ignoreNULL = FALSE)

        sdv <- reactive(scale_settings(input, "dv", spec_dv()))

        observe_scale_range(input, session, "dv")

        # Two correlated measurements per participant (same construction as the
        # correlation module: measurement 2 is measurement 1's z-score, blended
        # with fresh noise in proportion rho).
        sim_data <- reactive({
            p <- params()
            z  <- rnorm(p$n)
            s1 <- p$mean_c1 + p$sd_c1 * z
            s2 <- p$mean_c2 + p$sd_c2 * (p$rho * z + sqrt(1 - p$rho^2) * rnorm(p$n))
            data.frame(Score1 = round(s1, 2), Score2 = round(s2, 2))
        })

        # One shared mapping for both measurements. Standardizing each condition
        # by its own mean would centre them both on zero and erase the very
        # difference the paired test is about, so the reference is common.
        #
        # Item noise is drawn independently for each condition, so the observed
        # correlation between the two scale means comes out BELOW the population
        # rho the student typed. That attenuation is real -- unreliable measures
        # correlate less -- and the two descriptives columns make it visible.
        scaled <- reactive({
            if (!isTRUE(input$dv_use)) return(NULL)
            p <- params(); d <- sim_data(); spec <- spec_dv()
            ref_m  <- (p$mean_c1 + p$mean_c2) / 2
            ref_sd <- sqrt((p$sd_c1^2 + p$sd_c2^2) / 2 + (p$diff / 2)^2)
            mk <- function(score) {
                it <- make_scale_items(score, ref_m, ref_sd, spec$items,
                                       spec$kmin, spec$kmax, spec$rel,
                                       target = spec_target(spec))
                list(items = it, mean = rowMeans(it))
            }
            list(c1 = mk(d$Score1), c2 = mk(d$Score2),
                 ref_mean = ref_m, ref_sd = ref_sd)
        })

        # The two measurements the student will actually analyze.
        analysis_vars <- reactive({
            d <- sim_data(); sc <- scaled()
            use_scale <- analysis_choice(sdv()) == "mean" && !is.null(sc)
            list(Score1 = if (use_scale) sc$c1$mean else d$Score1,
                 Score2 = if (use_scale) sc$c2$mean else d$Score2,
                 dv_scaled = use_scale)
        })

        output$dz_preview <- renderUI({
            p <- live()
            helpText(HTML(sprintf(
                "These settings give: SD of the differences = <b>%s</b>,
                 Cohen's <i>d<sub>z</sub></i> = <b>%s</b>.<br/>A higher correlation
                 shrinks the difference SD and strengthens the effect.",
                fmt(p$sd_D), fmt(p$dz)
            )))
        })

        output$equations <- renderUI({
            p <- params()
            withMathJax(
                helpText("Each participant is measured twice; the two scores are
                          correlated:"),
                helpText(sprintf(
                    "$$\\text{Score}_{i,1} = \\mu_1 + e_{i,1}
                       \\quad (\\mu_1 = %s,\\ \\sigma_1 = %s)$$",
                    fmt(p$mean_c1), fmt(p$sd_c1)
                )),
                helpText(sprintf(
                    "$$\\text{Score}_{i,2} = \\mu_2 + e_{i,2}
                       \\quad (\\mu_2 = %s,\\ \\sigma_2 = %s),
                       \\quad \\text{cor} = %s$$",
                    fmt(p$mean_c2), fmt(p$sd_c2), fmt(p$rho)
                )),
                helpText("The test uses each participant's difference score,
                          \\(D_i = \\text{Score}_{i,2} - \\text{Score}_{i,1}\\):"),
                helpText(sprintf(
                    "$$d_z = \\frac{\\mu_2 - \\mu_1}{\\sigma_D}, \\quad
                       \\sigma_D = \\sqrt{\\sigma_1^2 + \\sigma_2^2
                                          - 2\\rho\\sigma_1\\sigma_2} = %s$$",
                    fmt(p$sd_D)
                ))
            )
        })

        output$code <- renderText({
            p <- params()
            paste0(
                "n <- ", p$n, "\n",
                "# two correlated measurements per participant (r = ",
                    fmt_code(p$rho), ")\n",
                "z <- rnorm(n)\n",
                "Score1 <- ", fmt_code(p$mean_c1), " + ", fmt_code(p$sd_c1),
                    " * z\n",
                "Score2 <- ", fmt_code(p$mean_c2), " + ", fmt_code(p$sd_c2),
                    " * (", fmt_code(p$rho), " * z + sqrt(1 - ", fmt_code(p$rho),
                    "^2) * rnorm(n))\n",
                "\n",
                # Both measurements go through the SAME mapping, or the
                # difference between them would be standardized away.
                scale_code_snippet(sdv(), "Score1",
                                   (p$mean_c1 + p$mean_c2) / 2,
                                   sqrt((p$sd_c1^2 + p$sd_c2^2) / 2 +
                                        (p$diff / 2)^2)),
                scale_code_snippet(sdv(), "Score2",
                                   (p$mean_c1 + p$mean_c2) / 2,
                                   sqrt((p$sd_c1^2 + p$sd_c2^2) / 2 +
                                        (p$diff / 2)^2)),
                "\n",
                {
                    a <- if (analysis_choice(sdv()) == "mean")
                        c("Score1_Scale_Mean", "Score2_Scale_Mean")
                    else c("Score1", "Score2")
                    paste0("t.test(", a[2], ", ", a[1], ", paired = TRUE)\n",
                           "cor(", a[1], ", ", a[2], ")")
                }
            )
        })

        labelled_data <- reactive({
            d <- sim_data(); sc <- scaled()
            cols <- c(scale_columns(sdv(), "Score1", sc$c1$items, sc$c1$mean,
                                    d$Score1),
                      scale_columns(sdv(), "Score2", sc$c2$items, sc$c2$mean,
                                    d$Score2))
            out <- cbind(seq_len(nrow(d)), data.frame(cols, check.names = FALSE))
            names(out)[1] <- ID_LABEL
            out
        })

        output$data_table <- renderTable({
            labelled_data()
        }, digits = 2, striped = TRUE)

        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("simulated_paired_data_",
                       format(Sys.Date(), "%Y-%m-%d"), ".csv")
            },
            content = function(file) {
                write.csv(labelled_data(), file, row.names = FALSE)
            }
        )

        output$sample_stats <- renderTable({
            d <- sim_data(); p <- params()
            s1 <- d$Score1; s2 <- d$Score2; D <- s2 - s1
            samp_dz <- if (sd(D) > 0) mean(D) / sd(D) else 0

            greek <- c("μ₁", "σ₁", "μ₂", "σ₂", "ρ", "μ₂−μ₁", "δ")
            roman <- c("M₁", "s₁", "M₂", "s₂", "r", "M₂−M₁", "d")
            pop   <- c(p$mean_c1, p$sd_c1, p$mean_c2, p$sd_c2, p$rho, p$diff, p$dz)

            stats_for <- function(a, b) {
                dd <- b - a
                c(mean(a), sd(a), mean(b), sd(b),
                  suppressWarnings(cor(a, b)), mean(dd),
                  if (sd(dd) > 0) mean(dd) / sd(dd) else 0)
            }

            tab <- data.frame(
                " "          = c("Mean of Condition 1", "SD of Condition 1",
                                 "Mean of Condition 2", "SD of Condition 2",
                                 "Correlation of C1 & C2",
                                 "Mean difference", "Cohen's d (dz)"),
                "Population" = paste(greek, "=", fmt(pop)),
                check.names  = FALSE
            )

            raw_in_csv <- !sdv()$use || analysis_choice(sdv()) == "raw"
            tab[[paste0("This sample", csv_flag(raw_in_csv))]] <-
                csv_cell(paste(roman, "=", fmt(stats_for(s1, s2))), raw_in_csv)

            if (sdv()$use) {
                sc <- scaled()
                tab[[paste0("This sample as scales", csv_flag(!raw_in_csv))]] <-
                    csv_cell(paste(roman, "=",
                                   fmt(stats_for(sc$c1$mean, sc$c2$mean))),
                             !raw_in_csv)
            }
            tab
        }, striped = TRUE, colnames = TRUE, rownames = FALSE,
           sanitize.text.function = identity)

        output$ttest_result <- renderUI({
            d <- analysis_vars()              # whatever is in the student's CSV
            s1 <- d$Score1; s2 <- d$Score2; D <- s2 - s1

            validate(need(!is_constant(D), msg_no_variance("the difference
                          scores (every participant changed by exactly the same
                          amount)")))

            tt <- t.test(s2, s1, paired = TRUE)
            samp_dz <- if (sd(D) > 0) mean(D) / sd(D) else 0
            sig <- tt$p.value < .05

            div(class = "result-box primary",
                div(class = "result-flag", "Model write-up"),
                div(class = "result-title", "Paired-samples t-test"),
                div(class = "apa", HTML(sprintf(
                    "<i>t</i>(%d) = %s, <i>p</i> %s, <i>d<sub>z</sub></i> = %s",
                    round(tt$parameter), fmt(tt$statistic), fmt_p(tt$p.value),
                    fmt(samp_dz)
                ))),
                div(HTML(sprintf(
                    "Mean difference = %s, 95%% CI [%s, %s]",
                    fmt(mean(D)), fmt(tt$conf.int[1]), fmt(tt$conf.int[2])
                ))),
                div(class = "decision", sprintf(
                    "The difference is %sstatistically significant at α = .05.",
                    if (sig) "" else "not "
                )),
                lapply(if (d$dv_scaled)
                           c(caution_note(s1, sdv()$kmin, sdv()$kmax),
                             caution_note(s2, sdv()$kmin, sdv()$kmax))[1],
                       function(m) div(class = "caution-note", m))
            )
        })

        # Just for fun: the same comparison as a repeated-measures ANOVA. With
        # two conditions F = t^2 and the p-value is identical to the paired
        # t-test above.
        output$anova_result <- renderUI({
            d <- analysis_vars()
            s1 <- d$Score1; s2 <- d$Score2
            validate(need(!is_constant(s2 - s1),
                          msg_no_variance("the difference scores")))
            tt <- t.test(s2, s1, paired = TRUE)
            Fv <- tt$statistic^2
            df2 <- round(tt$parameter)               # n - 1
            eta2 <- Fv / (Fv + df2)                  # partial eta^2

            div(class = "result-box",
                div(class = "result-title", "Repeated-measures ANOVA"),
                div(class = "apa", HTML(sprintf(
                    "<i>F</i>(1, %d) = %s, <i>p</i> %s",
                    df2, fmt(Fv), fmt_p(tt$p.value)
                ))),
                div(HTML(sprintf("partial &eta;&sup2; = %s", fmt_r(eta2)))),
                div(class = "decision",
                    "Same p as the paired t-test — with two conditions, F = t².")
            )
        })

        # The score1-score2 correlation: the paired-specific relationship. The
        # stronger it is, the smaller the SD of the differences and the more
        # powerful the test.
        output$cor_result <- renderUI({
            d <- analysis_vars()
            bad <- c(if (is_constant(d$Score1)) "Condition 1",
                     if (is_constant(d$Score2)) "Condition 2")
            validate(need(length(bad) == 0, msg_no_variance(bad)))
            ct <- cor.test(d$Score1, d$Score2)

            ci <- if (length(ct$conf.int) == 2)
                sprintf("95%% CI [%s, %s]",
                        fmt_r(ct$conf.int[1]), fmt_r(ct$conf.int[2]))
            else NULL

            div(class = "result-box",
                div(class = "result-title",
                    "Correlation between the two measurements"),
                div(class = "apa", HTML(sprintf(
                    "<i>r</i>(%d) = %s, <i>p</i> %s",
                    round(ct$parameter), fmt_r(ct$estimate), fmt_p(ct$p.value)
                ))),
                if (!is.null(ci)) div(HTML(ci)),
                div(class = "decision",
                    "Higher r → smaller SD of the differences → a more powerful
                     paired test.")
            )
        })

        # Y-axis range from the MODEL, so the frame and the population-mean lines
        # stay put when students regenerate with the same parameters.
        y_limits <- reactive({
            p <- params()
            k <- max(3, qnorm(1 - (1 - 0.9^(1 / (2 * p$n))) / 2))
            lo <- min(p$mean_c1 - k * p$sd_c1, p$mean_c2 - k * p$sd_c2)
            hi <- max(p$mean_c1 + k * p$sd_c1, p$mean_c2 + k * p$sd_c2)
            if (lo == hi) c(lo - 1, hi + 1) else c(lo, hi)
        })

        output$pairplot <- renderPlot({
            d <- analysis_vars(); p <- params()
            s1 <- d$Score1; s2 <- d$Score2; D <- s2 - s1
            samp_dz <- if (sd(D) > 0) mean(D) / sd(D) else 0

            # On a scale metric the population-mean lines are in the wrong
            # units, so the frame is the scale's own full response range --
            # which is also what makes a ceiling or floor effect visible.
            ylim <- if (d$dv_scaled) scale_range(sdv()) else y_limits()

            xpos <- c(1, 2)
            plot(NA, xlim = c(0.5, 2.5), ylim = ylim,
                 xaxt = "n", xlab = "",
                 yaxt = if (d$dv_scaled) "n" else "s",
                 ylab = if (d$dv_scaled) "DV scale mean" else "Score (DV)",
                 main = paste("Sample d_z =", fmt(samp_dz)))
            axis(1, at = xpos, labels = c(C1, C2))
            if (d$dv_scaled) scale_axis(2, sdv())

            # faint line linking each participant's two scores (the pairing)
            segments(xpos[1], s1, xpos[2], s2,
                     col = adjustcolor("grey30", alpha.f = 0.35), lwd = 1)
            points(rep(xpos[1], length(s1)), s1, pch = 19, col = "steelblue")
            points(rep(xpos[2], length(s2)), s2, pch = 19, col = "steelblue")

            seg <- 0.28
            if (!d$dv_scaled) {
                # population means: grey dashed
                segments(xpos - seg, c(p$mean_c1, p$mean_c2),
                         xpos + seg, c(p$mean_c1, p$mean_c2),
                         col = "grey40", lwd = 2, lty = 2)
            }
            # sample means: red solid
            segments(xpos - seg, c(mean(s1), mean(s2)),
                     xpos + seg, c(mean(s1), mean(s2)),
                     col = "firebrick", lwd = 2)

            legend("topleft", bty = "n",
                   legend = if (d$dv_scaled)
                                c("Each participant (paired scores)",
                                  "Sample means")
                            else c("Each participant (paired scores)",
                                   "Population means (the true model)",
                                   "Sample means"),
                   col = if (d$dv_scaled)
                             c(adjustcolor("grey30", alpha.f = 0.5), "firebrick")
                         else c(adjustcolor("grey30", alpha.f = 0.5),
                                "grey40", "firebrick"),
                   lwd = if (d$dv_scaled) c(1, 2) else c(1, 2, 2),
                   lty = if (d$dv_scaled) c(1, 1) else c(1, 2, 1))

            if (is_constant(D)) {
                mtext("No variability in the difference scores",
                      side = 3, line = 0.2, col = "#7a5320", cex = 0.95)
            }
        })
    })
}

# =============================================================================
#  Instructions (landing) page
# =============================================================================

instructionsUI <- function() {
    div(
        class = "instructions-wrap",
        titlePanel("Research Methods Data Simulator"),

        p("Generate realistic fake data for your class projects. Pick the",
          strong("population parameters"), "for the situation you want to
          study, draw a sample, and download it as a CSV file to analyze in
          your statistics software. Choose the kind of data you need:"),

        div(
            class = "gen-card",
            h3("Scatterplot / Correlation"),
            p("For studying the relationship between two continuous variables:
               a predictor (IV) and an outcome (DV). You set the population
               means and SDs, how strongly the outcome depends on the predictor
               (the slope), and how much random noise to add. The app draws a
               sample, plots it with the true and sample regression lines, and
               reports the correlation."),
            actionButton("to_corr", "Generate correlation data →",
                         class = "btn-primary btn-lg")
        ),

        div(
            class = "gen-card",
            h3("Independent-samples t-test"),
            p("For comparing the means of two independent groups on a continuous
               outcome (DV). You set each group's population mean and SD. The app
               draws a sample from each group, shows the group comparison, and
               reports the t-test result and Cohen's d."),
            actionButton("to_ttest", "Generate t-test data →",
                         class = "btn-primary btn-lg")
        ),

        div(
            class = "gen-card",
            h3("Paired-samples t-test"),
            p("For comparing two measurements taken on the ", em("same"),
              " participants (e.g. before vs. after) on a continuous outcome
               (DV). You set each measurement's population mean and SD, plus how
               strongly the two measurements are correlated. The app draws a
               sample, shows each participant's paired scores, and reports the
               paired t-test and Cohen's d."),
            actionButton("to_paired", "Generate paired t-test data →",
                         class = "btn-primary btn-lg")
        ),

        div(
            class = "gen-card",
            h3("Likert scales"),
            p("Any generator can measure a variable as a ", strong("multi-item
               Likert scale"), " instead of a single continuous score. Tick the
               variable under ", em("Measure as Likert scales"), ", set how many
               items it has, its low and high response values, and how reliable
               it is. Each item is a noisy reading of the underlying score, so
               the scale carries real measurement error \u2014 which is why the
               descriptive statistics show ", em("This sample"), " and ",
              em("This sample as scales"), " side by side."),
            p("Checkboxes control what lands in the CSV: the individual items,
               the scale mean, the original continuous score, or any
               combination. Ask for items only if you want students to compute
               the scale score themselves.")
        ),

        h4("How to use any generator"),
        tags$ol(
            tags$li("Type the population parameters on the left."),
            tags$li("Optionally tick a variable under ",
                    strong("Measure as Likert scales"), " and define it."),
            tags$li("Set how many cases to draw, then click ",
                    strong("Generate Data"), "."),
            tags$li("Review the descriptive statistics and plot on the right."),
            tags$li("Click the ", strong("CSV"), " button above the data table
                     to download your dataset."),
            tags$li("Use the ", strong("← Instructions"), " link to come back
                     to this page.")
        )
    )
}

# =============================================================================
#  Main app: hidden tabset ties the three pages together
# =============================================================================

ui <- fluidPage(
    withMathJax(),
    tags$head(tags$style(app_css)),

    # type = "hidden" => no tab bar is drawn; we switch pages with the buttons
    # and links below via updateTabsetPanel().
    tabsetPanel(
        id = "nav", type = "hidden",

        tabPanelBody("instructions", instructionsUI()),

        tabPanelBody(
            "corr",
            div(class = "nav-back",
                actionLink("home_from_corr", "← Instructions")),
            corrUI("corr")
        ),

        tabPanelBody(
            "ttest",
            div(class = "nav-back",
                actionLink("home_from_ttest", "← Instructions")),
            ttestUI("ttest")
        ),

        tabPanelBody(
            "paired",
            div(class = "nav-back",
                actionLink("home_from_paired", "← Instructions")),
            pairedUI("paired")
        )
    ),

    # Outside the tabset, so it appears on every page.
    div(class = "app-footer",
        paste0("Research Methods Data Simulator \u00b7 version ", APP_VERSION))
)

server <- function(input, output, session) {

    # Page navigation.
    observeEvent(input$to_corr,         updateTabsetPanel(session, "nav", "corr"))
    observeEvent(input$to_ttest,        updateTabsetPanel(session, "nav", "ttest"))
    observeEvent(input$to_paired,       updateTabsetPanel(session, "nav", "paired"))
    observeEvent(input$home_from_corr,  updateTabsetPanel(session, "nav", "instructions"))
    observeEvent(input$home_from_ttest, updateTabsetPanel(session, "nav", "instructions"))
    observeEvent(input$home_from_paired,updateTabsetPanel(session, "nav", "instructions"))

    # Generators.
    corrServer("corr")
    ttestServer("ttest")
    pairedServer("paired")
}

shinyApp(ui = ui, server = server)
