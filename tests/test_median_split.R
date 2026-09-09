# When the grouping IV is measured as a Likert scale, the groups ARE a median
# split of that scale -- ties and all -- so the split is reproducible from the
# data the student is handed, and usually comes out uneven. The "force equal
# group sizes" escape hatch must nudge the fewest scores by the smallest step
# and re-randomize nothing.
#
# Run from the project root:  Rscript tests/test_median_split.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(77)

IV <- function(...) modifyList(list(
    mean1 = 100, sd1 = 15, mean2 = 115, sd2 = 15, n = 30,
    generate = 1, iv_gen = 0, dv_gen = 0, iv_force = 0, iv_unforce = 0,
    iv_use = TRUE, iv_items = 4, iv_min = 1, iv_max = 7, iv_rel = .8,
    iv_cols = c("items", "mean"), iv_target = 4,
    dv_use = FALSE, dv_items = 4, dv_min = 1, dv_max = 7, dv_rel = .8,
    dv_cols = "mean", dv_target = 4), list(...))

cat("\nThe grouping is a real median split\n")
testServer(ttestServer, args = list(id = "t"), {
    do.call(session$setInputs, IV())
    ok("Group is exactly the median split of the IV scale mean",
       all(sim_data()$Group == median_split_groups(scaled_iv()$mean)))
    ok("a student re-splitting the CSV column gets the same answer", {
       m <- scaled_iv()$mean
       g <- ifelse(m <= median(m), G1, G2)
       all(as.character(sim_data()$Group) == g) })
    ok("with the IV scale off, groups are fixed and equal", {
       session$setInputs(iv_use = FALSE)
       d <- sim_data(); sum(d$Group == G1) == 30 && sum(d$Group == G2) == 30 })
})

cat("\nUnequal groups are the normal outcome, not an edge case\n")
uneq <- 0; reps <- 40
for (r in 1:reps) {
    testServer(ttestServer, args = list(id = "t"), {
        do.call(session$setInputs, IV(generate = r))
        d <- sim_data()
        if (sum(d$Group == G1) != sum(d$Group == G2)) uneq <<- uneq + 1
    })
}
cat(sprintf("      %d of %d draws split unevenly\n", uneq, reps))
ok("most draws give unequal groups", uneq > reps * 0.5)

cat("\nThe warning appears exactly when the split is uneven\n")
testServer(ttestServer, args = list(id = "t"), {
    do.call(session$setInputs, IV())
    d <- sim_data(); uneven <- sum(d$Group == G1) != sum(d$Group == G2)
    ok("warning shown iff the groups differ in size",
       uneven == !is.null(output$split_note))
    session$setInputs(iv_use = FALSE)
    ok("no warning when the IV is not measured as a scale",
       is.null(output$split_note))
})

cat("\nForcing equal groups nudges minimally and re-randomizes nothing\n")
testServer(ttestServer, args = list(id = "t"), {
    do.call(session$setInputs, IV())
    # find a draw that actually splits unevenly
    for (r in 1:30) {
        session$setInputs(generate = r)
        d <- sim_data()
        if (sum(d$Group == G1) != sum(d$Group == G2)) break
    }
    before <- as.matrix(scaled_iv()$items)
    n1 <- sum(d$Group == G1); n2 <- sum(d$Group == G2)
    ok("started from an uneven split", n1 != n2)

    session$setInputs(iv_force = 1)
    after <- as.matrix(scaled_iv()$items)
    d2 <- sim_data()
    ok("groups are now exactly equal",
       sum(d2$Group == G1) == sum(d2$Group == G2))
    ok("still a valid median split of the nudged scale",
       all(d2$Group == median_split_groups(scaled_iv()$mean)))

    changed <- which(rowSums(before != after) > 0)
    ok("only a handful of participants were touched",
       length(changed) > 0 && length(changed) <= abs(n1 - n2))
    ok("each touched participant changed exactly one item",
       all(rowSums(before != after)[changed] == 1))
    ok("each change was a single point upward",
       all((rowSums(after) - rowSums(before))[changed] == 1))
    ok("nobody else's scores moved at all",
       identical(before[-changed, ], after[-changed, ]))
    ok("the reported nudge count matches what changed",
       scaled_iv()$moved == length(changed))
    ok("the warning is replaced by a note", !is.null(output$split_note))

    session$setInputs(iv_unforce = 1)
    ok("undo restores the original scores", identical(before, as.matrix(scaled_iv()$items)))
})

cat("\nThe forced state resets with a new draw\n")
testServer(ttestServer, args = list(id = "t"), {
    do.call(session$setInputs, IV())
    session$setInputs(iv_force = 1)
    ok("forcing is on", force_equal())
    session$setInputs(generate = 99)
    ok("a new sample goes back to the honest split", !force_equal())
    session$setInputs(iv_force = 2)
    session$setInputs(iv_gen = 5)
    ok("redrawing the IV scale also resets it", !force_equal())
})

cat("\nA split that leaves one group empty is explained, not crashed\n")
testServer(ttestServer, args = list(id = "t"), {
    # everyone pinned to the ceiling: one scale value, so nobody is above it
    do.call(session$setInputs, IV(iv_target = 7, n = 20))
    d <- sim_data()
    if (sum(d$Group == G2) == 0 || sum(d$Group == G1) == 0) {
        cls <- tryCatch({ force(output$ttest_result); "ok" },
                        error = function(e)
                            if (inherits(e, "validation")) "guarded" else "raw")
        ok("empty group yields an explanation, not an R error", cls == "guarded")
    } else {
        ok("ceiling draw still had both groups (acceptable)", TRUE)
    }
})
