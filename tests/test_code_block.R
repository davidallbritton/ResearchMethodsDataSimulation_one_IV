# The "R code" box claims to reproduce what the app just did. So it has to
# actually run, and produce the same thing.
#
# It has been wrong three ways at once: it referenced a variable the block never
# created (t-test Score), it silently omitted the Typical response shift so an
# off-centre scale came out centred, and on the paired panel it emitted only one
# of the two measurements while reusing the name z that built them.
#
# Run from the project root:  Rscript tests/test_code_block.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(515)

# Run a generated block in a clean environment and hand back what it defined.
run_block <- function(txt) {
    env <- new.env(parent = globalenv())
    pdf(NULL); on.exit(invisible(dev.off()), add = TRUE)
    eval(parse(text = txt), envir = env)
    env
}

emit <- function(mod, id, inputs) {
    out <- NULL
    testServer(mod, args = list(id = id), {
        do.call(session$setInputs, inputs)
        out <<- output$code
    })
    out
}

BIG <- 4000   # enough rows that a realized mean is a fair test of the shift

cat("\nCORRELATION\n")
txt <- emit(corrServer, "c", list(
    mean_x = 0, sd_x = 1, mean_y = 0, slope = .5, sd_e = 1, n = BIG,
    generate = 1, x_gen = 0, y_gen = 0,
    x_use = TRUE, x_items = 4, x_min = 1, x_max = 7, x_rel = .8,
    x_cols = "mean", x_target = 4,
    y_use = TRUE, y_items = 4, y_min = 1, y_max = 7, y_rel = .8,
    y_cols = "mean", y_target = 6))
e <- run_block(txt)
ok("runs without error", TRUE)
ok("defines both scale means",
   all(c("X_Scale_Mean", "Y_Scale_Mean") %in% ls(e)))
ok("X (typical response 4) averages about 4",
   abs(mean(e$X_Scale_Mean) - 4) < 0.25)
ok("Y (typical response 6) averages about 6 -- the shift IS applied",
   abs(mean(e$Y_Scale_Mean) - 6) < 0.25)
ok("items stay inside the response range",
   min(e$Y_items) >= 1 && max(e$Y_items) <= 7)
ok("the two scales do not clobber each other's names",
   !identical(e$X_Scale_Mean, e$Y_Scale_Mean))
ok("X and Y scale means are correlated, as the model says",
   cor(e$X_Scale_Mean, e$Y_Scale_Mean) > 0.1)

cat("\nINDEPENDENT t-TEST\n")
txt <- emit(ttestServer, "t", list(
    mean1 = 100, sd1 = 15, mean2 = 115, sd2 = 15, n = BIG,
    generate = 1, iv_gen = 0, dv_gen = 0,
    iv_use = TRUE, iv_items = 4, iv_min = 1, iv_max = 7, iv_rel = .8,
    iv_cols = "mean", iv_target = 4,
    dv_use = TRUE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = "mean", dv_target = 5))
e <- run_block(txt)
ok("runs without error", TRUE)
ok("defines Score, which the block used to reference without creating",
   "Score" %in% ls(e) && length(e$Score) == 2 * BIG)
ok("defines the DV scale mean", "Score_Scale_Mean" %in% ls(e))
ok("DV scale has one value per row, not per group",
   length(e$Score_Scale_Mean) == 2 * BIG)
ok("DV (typical response 5) averages about 5",
   abs(mean(e$Score_Scale_Mean) - 5) < 0.25)
ok("group 2 scores higher on the DV scale", {
   g <- e$Group
   mean(e$Score_Scale_Mean[g == G2]) > mean(e$Score_Scale_Mean[g == G1]) })
ok("defines the IV scale mean", "Group_Scale_Mean" %in% ls(e))
ok("IV median split reproduces the groups exactly", {
   m <- e$Group_Scale_Mean; g <- e$Group
   max(m[g == G1]) <= min(m[g == G2]) })

cat("\nPAIRED t-TEST\n")
txt <- emit(pairedServer, "p", list(
    mean_c1 = 100, sd_c1 = 15, mean_c2 = 110, sd_c2 = 15, rho = .6, n = BIG,
    generate = 1, dv_gen = 0,
    dv_use = TRUE, dv_items = 5, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = "mean", dv_target = 4))
e <- run_block(txt)
ok("runs without error", TRUE)
ok("emits BOTH measurements, not just the first",
   all(c("Score1_Scale_Mean", "Score2_Scale_Mean") %in% ls(e)))
ok("the latent z that built the correlated scores is not clobbered",
   length(e$z) == BIG)
ok("condition 2 still scores higher after scaling",
   mean(e$Score2_Scale_Mean) > mean(e$Score1_Scale_Mean))
ok("the pairing survives: the two scale means correlate",
   cor(e$Score1_Scale_Mean, e$Score2_Scale_Mean) > 0.2)

cat("\nNo scales: the blocks are unchanged\n")
txt <- emit(corrServer, "c", list(mean_x = 0, sd_x = 1, mean_y = 0, slope = .5,
    sd_e = 1, n = 30, generate = 1, x_gen = 0, y_gen = 0,
    x_use = FALSE, y_use = FALSE))
ok("correlation block mentions no scale", !grepl("Scale_Mean", txt))
ok("correlation block still runs", { run_block(txt); TRUE })
txt <- emit(ttestServer, "t", list(mean1 = 100, sd1 = 15, mean2 = 115,
    sd2 = 15, n = 30, generate = 1, iv_gen = 0, dv_gen = 0,
    iv_use = FALSE, dv_use = FALSE))
ok("t-test block does not define Score when no scale is used",
   !grepl("Score <- c\\(group1", txt))
ok("t-test block still runs", { run_block(txt); TRUE })

cat("\nThe analysis lines match what the result boxes reported\n")
# The block used to end by testing the continuous scores even when the app had
# reported a test of the scale means.
txt <- emit(ttestServer, "t", list(mean1 = 100, sd1 = 15, mean2 = 115,
    sd2 = 15, n = 60, generate = 1, iv_gen = 0, dv_gen = 0, iv_use = FALSE,
    dv_use = TRUE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = "mean", dv_target = 4))
ok("t-test: analyses the scale mean when that is what is in the CSV",
   grepl("Score_Scale_Mean[Group", txt, fixed = TRUE))
ok("t-test: does NOT still test the raw group vectors",
   !grepl("t.test(group2, group1", txt, fixed = TRUE))
ok("t-test: the tidied z line has no redundant - 0 or / 1",
   !grepl("- 0) / 1", txt, fixed = TRUE))

txt <- emit(ttestServer, "t", list(mean1 = 100, sd1 = 15, mean2 = 115,
    sd2 = 15, n = 60, generate = 1, iv_gen = 0, dv_gen = 0, iv_use = FALSE,
    dv_use = TRUE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = c("mean", "raw"), dv_target = 4))
ok("t-test: reverts to the raw scores when they are in the CSV",
   grepl("t.test(group2, group1", txt, fixed = TRUE))

txt <- emit(corrServer, "c", list(mean_x = 0, sd_x = 1, mean_y = 0, slope = .5,
    sd_e = 1, n = 60, generate = 1, x_gen = 0, y_gen = 0,
    x_use = FALSE, x_items = 4, x_min = 1, x_max = 7, x_rel = .8,
    x_cols = "mean", x_target = 4,
    y_use = TRUE, y_items = 4, y_min = 1, y_max = 7, y_rel = .8,
    y_cols = "mean", y_target = 4))
ok("correlation: correlates X with the Y scale mean",
   grepl("cor(X, Y_Scale_Mean)", txt, fixed = TRUE))
ok("correlation: block still runs", { run_block(txt); TRUE })

txt <- emit(pairedServer, "p", list(mean_c1 = 100, sd_c1 = 15, mean_c2 = 110,
    sd_c2 = 15, rho = .5, n = 60, generate = 1, dv_gen = 0,
    dv_use = TRUE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = "mean", dv_target = 4))
ok("paired: tests the two scale means",
   grepl("t.test(Score2_Scale_Mean, Score1_Scale_Mean, paired = TRUE)",
         txt, fixed = TRUE))
ok("paired: block still runs", { run_block(txt); TRUE })
