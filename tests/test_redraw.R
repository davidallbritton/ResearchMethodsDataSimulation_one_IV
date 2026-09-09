# Redrawing scale scores must leave the underlying sample untouched.
#
# Run from the project root:  Rscript tests/test_redraw.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(7)

cat("\n== CORRELATION: redraw scales, keep the sample ==\n")
testServer(corrServer, args=list(id="c"), {
  session$setInputs(mean_x=0, sd_x=1, mean_y=0, slope=.5, sd_e=1, n=50,
                    x_use=FALSE, y_use=TRUE, y_items=4, y_min=1, y_max=7,
                    y_rel=.7, y_cols=c("items","mean"), generate=1, x_gen=0, y_gen=0, iv_gen=0, dv_gen=0)
  samp1  <- sim_data(); sc1 <- scaled_y()$mean

  session$setInputs(y_gen=1)
  samp2  <- sim_data(); sc2 <- scaled_y()$mean
  ok("sample scores UNCHANGED after Generate Scale Scores", identical(samp1, samp2))
  ok("scale scores DID change", !identical(sc1, sc2))
  ok("scale still tracks the same sample",
     cor(samp2$Y, sc2) > 0.6)

  session$setInputs(y_gen=2)
  ok("second redraw still leaves the sample alone", identical(samp1, sim_data()))

  # column picker must not silently redraw
  sc3 <- scaled_y()$mean
  session$setInputs(y_cols=c("items","mean","raw"))
  ok("ticking a CSV column does not redraw scale scores",
     identical(sc3, scaled_y()$mean))
  ok("ticking a CSV column does not redraw the sample",
     identical(samp1, sim_data()))
  ok("but the CSV column did appear", "Y" %in% names(labelled_data()))

  # editing item settings should do nothing until the button is pressed
  session$setInputs(y_items=6)
  ok("editing item count alone changes nothing yet",
     ncol(scaled_y()$items) == 4)
  session$setInputs(y_gen=3)
  ok("after the button, the new item count applies",
     ncol(scaled_y()$items) == 6)
  ok("and the sample is STILL the same", identical(samp1, sim_data()))

  # Generate Data must refresh both
  session$setInputs(generate=2)
  ok("Generate Data draws a new sample", !identical(samp1, sim_data()))
})

cat("\n== t-TEST: same guarantees ==\n")
testServer(ttestServer, args=list(id="t"), {
  session$setInputs(mean1=100, sd1=15, mean2=115, sd2=15, n=30,
                    iv_use=TRUE, iv_items=4, iv_min=1, iv_max=7, iv_rel=.8,
                    iv_cols=c("items","mean"),
                    dv_use=TRUE, dv_items=5, dv_min=1, dv_max=5, dv_rel=.8,
                    dv_cols="mean", generate=1, x_gen=0, y_gen=0, iv_gen=0, dv_gen=0)
  s1 <- sim_data(); iv1 <- scaled_iv()$mean; dv1 <- scaled_dv()$mean
  session$setInputs(iv_gen=1, dv_gen=1)
  ok("sample unchanged", identical(s1, sim_data()))
  ok("DV scale redrawn", !identical(dv1, scaled_dv()$mean))
  ok("IV scale redrawn", !identical(iv1, scaled_iv()$mean))
  ok("IV median split STILL reproduces the groups", {
     m <- scaled_iv()$mean; g <- sim_data()$Group
     max(m[g==G1]) <= min(m[g==G2]) })
})

cat("\n== PAIRED: same guarantees ==\n")
testServer(pairedServer, args=list(id="p"), {
  session$setInputs(mean_c1=100, sd_c1=15, mean_c2=110, sd_c2=15, rho=.6, n=100,
                    dv_use=TRUE, dv_items=6, dv_min=1, dv_max=7, dv_rel=.8,
                    dv_cols=c("items","mean"), generate=1, x_gen=0, y_gen=0, iv_gen=0, dv_gen=0)
  s1 <- sim_data(); a1 <- scaled()$c1$mean
  session$setInputs(dv_gen=1)
  ok("sample unchanged", identical(s1, sim_data()))
  ok("scale redrawn", !identical(a1, scaled()$c1$mean))
  ok("condition 2 still higher after redraw",
     mean(scaled()$c2$mean) > mean(scaled()$c1$mean))
  ok("pairing still correlated after redraw",
     cor(scaled()$c1$mean, scaled()$c2$mean) > .2)
})
