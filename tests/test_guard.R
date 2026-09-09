# Degenerate settings are reachable on purpose -- a population SD of 0, or
# Typical response dragged to the end of the scale. They must produce an
# explanation, never a raw R error.
#
# The three failure modes being guarded against:
#   t.test(var.equal = TRUE)   throws "data are essentially constant"
#   cor() / cor.test()         return NA with only a warning
#   summary(lm())$fstatistic   returns NULL, crashing the line that reads it
#
# Run from the project root:  Rscript tests/test_guard.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

png(tempfile())
on.exit(invisible(dev.off()), add = TRUE)
set.seed(404)          # these scenarios are random; pin them so runs compare

# "ok" = rendered normally, "guarded" = validate() explained itself,
# "raw" = an unhandled R error reached the user.
classify <- function(expr) {
    tryCatch({ force(expr); "ok" },
             error = function(e)
                 if (inherits(e, "validation") ||
                     grepl("validation", paste(class(e), collapse = " ")))
                     "guarded" else paste("raw:", conditionMessage(e)))
}

# expect: "guarded" for outputs that cannot be computed, "any" where either a
# clean render or an explanation is acceptable.
scenario <- function(label, mod, id, inputs, expect) {
    testServer(mod, args = list(id = id), {
        do.call(session$setInputs, inputs)
        raw <- character(0); missing_guard <- character(0)
        for (o in names(expect)) {
            got <- classify(output[[o]])
            if (startsWith(got, "raw:")) raw <- c(raw, paste(o, got))
            else if (expect[[o]] == "guarded" && got != "guarded")
                missing_guard <- c(missing_guard, o)
        }
        ok(paste(label, "- no raw R errors"), !length(raw))
        if (length(raw)) for (r in raw) cat("      ", r, "\n")
        if (length(missing_guard) || any(unlist(expect) == "guarded"))
            ok(paste(label, "- expected explanations appear"),
               !length(missing_guard))
        if (length(missing_guard))
            cat("       not guarded:", paste(missing_guard, collapse = ", "), "\n")
    })
}

base_c <- list(mean_x = 0, sd_x = 1, mean_y = 0, slope = .5, sd_e = 1, n = 30,
    generate = 1, x_gen = 0, y_gen = 0,
    x_use = FALSE, x_items = 4, x_min = 1, x_max = 7, x_rel = .8,
    x_cols = c("items","mean"), x_target = 4,
    y_use = FALSE, y_items = 4, y_min = 1, y_max = 7, y_rel = .8,
    y_cols = c("items","mean"), y_target = 4)

cat("\nCORRELATION\n")
scenario("population SD of X = 0", corrServer, "c",
    modifyList(base_c, list(sd_x = 0)),
    list(sample_stats = "any", scatter = "any",
         cor_result = "guarded", reg_result = "guarded"))
# Pinning to an endpoint makes constancy likely but not certain -- with four
# items at target 7 about 99.6% of people max out, so a sample of 30 still has
# some variance around a tenth of the time. Either outcome is correct here; the
# guarantee being tested is only that neither path reaches a raw R error. The
# SD = 0 scenarios below are the deterministically degenerate ones.
scenario("Y scale pinned to the ceiling", corrServer, "c",
    modifyList(base_c, list(y_use = TRUE, y_target = 7, y_cols = "mean")),
    list(sample_stats = "any", scatter = "any",
         cor_result = "any", reg_result = "any"))

# Deterministic version of the same idea: no variance at all, whatever the draw.
scenario("Y scale with zero population spread", corrServer, "c",
    modifyList(base_c, list(sd_x = 0, sd_e = 0, y_use = TRUE, y_cols = "mean")),
    list(sample_stats = "any", scatter = "any",
         cor_result = "guarded", reg_result = "guarded"))
scenario("healthy ceiling (target 6) still computes", corrServer, "c",
    modifyList(base_c, list(y_use = TRUE, y_target = 6, y_cols = "mean")),
    list(sample_stats = "any", scatter = "any",
         cor_result = "any", reg_result = "any"))

base_t <- list(mean1 = 100, sd1 = 15, mean2 = 115, sd2 = 15, n = 20,
    generate = 1, iv_gen = 0, dv_gen = 0,
    iv_use = FALSE, iv_items = 4, iv_min = 1, iv_max = 7, iv_rel = .8,
    iv_cols = c("items","mean"), iv_target = 4,
    dv_use = FALSE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = c("items","mean"), dv_target = 4)

cat("\nINDEPENDENT t-TEST\n")
scenario("both population SDs = 0", ttestServer, "t",
    modifyList(base_t, list(sd1 = 0, sd2 = 0)),
    list(sample_stats = "any", dotplot = "any", ttest_result = "guarded",
         anova_result = "guarded", dummy_result = "guarded"))
scenario("perfect separation (no variance within groups)", ttestServer, "t",
    modifyList(base_t, list(sd1 = 0, sd2 = 0, mean1 = 100, mean2 = 200)),
    list(sample_stats = "any", dotplot = "any", ttest_result = "guarded",
         anova_result = "guarded", dummy_result = "guarded"))
scenario("DV scale pinned to the ceiling", ttestServer, "t",
    modifyList(base_t, list(dv_use = TRUE, dv_target = 7, dv_cols = "mean")),
    list(sample_stats = "any", dotplot = "any", ttest_result = "any",
         anova_result = "any", dummy_result = "any"))

base_p <- list(mean_c1 = 100, sd_c1 = 15, mean_c2 = 110, sd_c2 = 15,
    rho = .5, n = 20, generate = 1, dv_gen = 0,
    dv_use = FALSE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = c("items","mean"), dv_target = 4)

cat("\nPAIRED t-TEST\n")
scenario("both population SDs = 0", pairedServer, "p",
    modifyList(base_p, list(sd_c1 = 0, sd_c2 = 0)),
    list(sample_stats = "any", pairplot = "any", ttest_result = "guarded",
         anova_result = "guarded", cor_result = "guarded"))
scenario("DV scale pinned to the floor", pairedServer, "p",
    modifyList(base_p, list(dv_use = TRUE, dv_target = 1, dv_cols = "mean")),
    list(sample_stats = "any", pairplot = "any", ttest_result = "any",
         anova_result = "any", cor_result = "any"))

cat("\nThe ANOVA and point-biserial boxes must refuse on the SAME condition as\n")
cat("the t-test, since both claim to mirror it.\n")
testServer(ttestServer, args = list(id = "t"), {
    do.call(session$setInputs, modifyList(base_t, list(sd1 = 0, sd2 = 0,
                                                       mean1 = 100, mean2 = 200)))
    ok("t-test, ANOVA and point-biserial all decline together",
       all(vapply(c("ttest_result", "anova_result", "dummy_result"),
                  function(o) classify(output[[o]]) == "guarded", logical(1))))
})
