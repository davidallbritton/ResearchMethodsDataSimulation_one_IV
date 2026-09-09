# Every output renders without error under a range of scale configurations.
# Catches wiring mistakes that the data-level tests would not see, because a
# reactive can be correct while the renderer that consumes it is not.
#
# Run from the project root:  Rscript tests/test_outputs.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

png(tempfile())                    # sink for renderPlot
on.exit(invisible(dev.off()), add = TRUE)
set.seed(202)                      # pin the draws so runs are comparable

# Force each named output and report any that raise. A validation message is
# still an error here -- these configurations should all be computable.
render_all <- function(label, mod, id, inputs, outs) {
    testServer(mod, args = list(id = id), {
        do.call(session$setInputs, inputs)
        errs <- character(0)
        for (o in outs) {
            e <- tryCatch({ force(output[[o]]); NULL },
                          error = function(e) conditionMessage(e))
            if (!is.null(e)) errs <- c(errs, paste0(o, ": ", e))
        }
        ok(label, !length(errs))
        if (length(errs)) for (e in errs) cat("      ", e, "\n")
    })
}

CORR_OUT <- c("sample_stats", "data_table", "scatter", "cor_result",
              "reg_result", "code", "equations", "r_preview", "sd_note")
TT_OUT   <- c("sample_stats", "data_table", "dotplot", "ttest_result",
              "anova_result", "dummy_result", "code", "equations", "d_preview")
PR_OUT   <- c("sample_stats", "data_table", "pairplot", "ttest_result",
              "anova_result", "cor_result", "code", "equations", "dz_preview")

cat("\nCORRELATION outputs\n")
for (cfg in list(list(lab = "no scales",             x = FALSE, y = FALSE, cols = NULL),
                 list(lab = "Y scale, items+mean",   x = FALSE, y = TRUE,  cols = c("items","mean")),
                 list(lab = "both scales, mean only",x = TRUE,  y = TRUE,  cols = "mean"),
                 list(lab = "both scales, raw too",  x = TRUE,  y = TRUE,  cols = c("items","mean","raw")))) {
    render_all(cfg$lab, corrServer, "c", list(
        mean_x = 0, sd_x = 1, mean_y = 0, slope = .5, sd_e = 1, n = 30,
        generate = 1, x_gen = 0, y_gen = 0,
        x_use = cfg$x, x_items = 3, x_min = 1, x_max = 5, x_rel = .8,
        x_cols = cfg$cols, x_target = 3,
        y_use = cfg$y, y_items = 4, y_min = 1, y_max = 7, y_rel = .8,
        y_cols = cfg$cols, y_target = 4), CORR_OUT)
}

cat("\nINDEPENDENT t-TEST outputs\n")
for (cfg in list(list(lab = "no scales",                iv = FALSE, dv = FALSE, cols = NULL),
                 list(lab = "DV scale, mean only",      iv = FALSE, dv = TRUE,  cols = "mean"),
                 list(lab = "IV+DV scales, items+mean", iv = TRUE,  dv = TRUE,  cols = c("items","mean")),
                 list(lab = "IV+DV scales, raw too",    iv = TRUE,  dv = TRUE,  cols = c("items","mean","raw")))) {
    render_all(cfg$lab, ttestServer, "t", list(
        mean1 = 100, sd1 = 15, mean2 = 115, sd2 = 15, n = 25,
        generate = 1, iv_gen = 0, dv_gen = 0,
        iv_use = cfg$iv, iv_items = 4, iv_min = 1, iv_max = 7, iv_rel = .8,
        iv_cols = cfg$cols, iv_target = 4,
        dv_use = cfg$dv, dv_items = 5, dv_min = 1, dv_max = 5, dv_rel = .8,
        dv_cols = cfg$cols, dv_target = 3), TT_OUT)
}

cat("\nPAIRED t-TEST outputs\n")
for (cfg in list(list(lab = "no scales",           dv = FALSE, cols = NULL),
                 list(lab = "DV scale, items only",dv = TRUE,  cols = "items"),
                 list(lab = "DV scale, mean only", dv = TRUE,  cols = "mean"),
                 list(lab = "DV scale, raw too",   dv = TRUE,  cols = c("items","mean","raw")))) {
    render_all(cfg$lab, pairedServer, "p", list(
        mean_c1 = 100, sd_c1 = 15, mean_c2 = 110, sd_c2 = 15, rho = .5, n = 25,
        generate = 1, dv_gen = 0,
        dv_use = cfg$dv, dv_items = 6, dv_min = 1, dv_max = 7, dv_rel = .8,
        dv_cols = cfg$cols, dv_target = 4), PR_OUT)
}
