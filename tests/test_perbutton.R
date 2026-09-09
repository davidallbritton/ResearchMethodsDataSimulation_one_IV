# Each variable's redraw button is independent of every other, and the
# t-test IV names its third column group correctly.
#
# Run from the project root:  Rscript tests/test_perbutton.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(11)

cat("\n== per-variable buttons are independent ==\n")
testServer(corrServer, args=list(id="c"), {
  session$setInputs(mean_x=0, sd_x=1, mean_y=0, slope=.5, sd_e=1, n=50,
    x_use=TRUE, x_items=4, x_min=1, x_max=7, x_rel=.8, x_cols=c("items","mean"),
    y_use=TRUE, y_items=4, y_min=1, y_max=7, y_rel=.8, y_cols=c("items","mean"),
    generate=1, x_gen=0, y_gen=0)
  base <- sim_data(); x1 <- scaled_x()$mean; y1 <- scaled_y()$mean

  session$setInputs(x_gen=1)
  ok("X button redrew X", !identical(x1, scaled_x()$mean))
  ok("X button left Y alone", identical(y1, scaled_y()$mean))
  ok("X button left the sample alone", identical(base, sim_data()))

  y2 <- scaled_y()$mean; x2 <- scaled_x()$mean
  session$setInputs(y_gen=1)
  ok("Y button redrew Y", !identical(y2, scaled_y()$mean))
  ok("Y button left X alone", identical(x2, scaled_x()$mean))
  ok("Y button left the sample alone", identical(base, sim_data()))

  session$setInputs(x_items=6)
  ok("editing X items alone changes nothing", ncol(scaled_x()$items) == 4)
  session$setInputs(x_gen=2)
  ok("X button applies X's new item count", ncol(scaled_x()$items) == 6)
  ok("...and Y keeps its own item count", ncol(scaled_y()$items) == 4)

  session$setInputs(generate=2)
  ok("Generate Data draws a new sample", !identical(base, sim_data()))
})

cat("\n== t-test IV column label ==\n")
ui <- ttestUI("t")
h <- paste(as.character(ui), collapse="")
ok("IV offers 'Group membership', not a continuous score",
   grepl("Group membership", h, fixed=TRUE))
ok("DV still says 'Original continuous score'",
   grepl("Original continuous score", h, fixed=TRUE))
ok("t-test page has two redraw buttons",
   lengths(regmatches(h, gregexpr("Generate Scale Scores", h))) == 2)
uc <- paste(as.character(corrUI("c")), collapse="")
ok("correlation page has two redraw buttons",
   lengths(regmatches(uc, gregexpr("Generate Scale Scores", uc))) == 2)
ok("correlation page has no 'Group membership' label",
   !grepl("Group membership", uc, fixed=TRUE))
up <- paste(as.character(pairedUI("p")), collapse="")
ok("paired page has one redraw button",
   lengths(regmatches(up, gregexpr("Generate Scale Scores", up))) == 1)

cat("\n== t-test IV scale still valid after its own redraw ==\n")
testServer(ttestServer, args=list(id="t"), {
  session$setInputs(mean1=100, sd1=15, mean2=115, sd2=15, n=30,
    iv_use=TRUE, iv_items=4, iv_min=1, iv_max=7, iv_rel=.8, iv_cols=c("items","mean"),
    dv_use=TRUE, dv_items=5, dv_min=1, dv_max=5, dv_rel=.8, dv_cols="mean",
    generate=1, iv_gen=0, dv_gen=0)
  dv1 <- scaled_dv()$mean; iv1 <- scaled_iv()$mean
  session$setInputs(dv_gen=1)
  ok("DV button leaves the IV alone", identical(iv1, scaled_iv()$mean))

  # The groups come from a median split of the IV scale, so redrawing the IV
  # scale is redrawing the design: people change groups and the DV follows.
  session$setInputs(iv_gen=1)
  ok("IV button redraws the design, so the DV changes with it",
     !identical(dv1, scaled_dv()$mean))
  ok("the grouping is still reproducible from the IV scale",
     all(sim_data()$Group == median_split_groups(scaled_iv()$mean)))
  ok("'raw' for the IV yields the Group column", {
     session$setInputs(iv_cols=c("items","mean","raw"))
     "Group" %in% names(labelled_data()) })
})
