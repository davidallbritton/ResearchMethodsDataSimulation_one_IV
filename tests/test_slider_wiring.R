# Every Typical response slider actually reaches its generator.
# This is the seam that broke once: the engine was right, the wiring absent.
#
# Run from the project root:  Rscript tests/test_slider_wiring.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(21)

cat("\n== every slider actually reaches its generator ==\n")
testServer(corrServer, args=list(id="c"), {
  base <- list(mean_x=0, sd_x=1, mean_y=0, slope=.5, sd_e=1, n=400,
    x_use=TRUE, x_items=4,x_min=1,x_max=7,x_rel=.8,x_cols="mean",x_target=4,
    y_use=TRUE, y_items=4,y_min=1,y_max=7,y_rel=.8,y_cols="mean",y_target=4,
    generate=1, x_gen=0, y_gen=0)
  do.call(session$setInputs, base)
  mid_x <- mean(scaled_x()$mean); mid_y <- mean(scaled_y()$mean)
  session$setInputs(x_target=6, x_gen=1)
  ok("corr X slider moves the X scale up", mean(scaled_x()$mean) - mid_x > 1.2)
  ok("corr X slider leaves Y alone", abs(mean(scaled_y()$mean) - mid_y) < 0.2)
  session$setInputs(y_target=2, y_gen=1)
  ok("corr Y slider moves the Y scale down", mid_y - mean(scaled_y()$mean) > 1.2)
})

testServer(ttestServer, args=list(id="t"), {
  base <- list(mean1=100, sd1=15, mean2=115, sd2=15, n=200,
    iv_use=TRUE, iv_items=4,iv_min=1,iv_max=7,iv_rel=.8,iv_cols="mean",iv_target=4,
    dv_use=TRUE, dv_items=4,dv_min=1,dv_max=7,dv_rel=.8,dv_cols="mean",dv_target=4,
    generate=1, iv_gen=0, dv_gen=0)
  do.call(session$setInputs, base)
  mid_dv <- mean(scaled_dv()$mean); mid_iv <- mean(scaled_iv()$mean)
  session$setInputs(dv_target=6, dv_gen=1)
  ok("t-test DV slider moves the DV scale up",
     mean(scaled_dv()$mean) - mid_dv > 1.2)
  ok("t-test DV slider leaves the IV alone",
     abs(mean(scaled_iv()$mean) - mid_iv) < 0.2)
  session$setInputs(iv_target=2, iv_gen=1)
  ok("t-test IV slider moves the IV scale down",
     mid_iv - mean(scaled_iv()$mean) > 1.2)
  ok("t-test IV median split SURVIVES a shifted target", {
     m <- scaled_iv()$mean; g <- sim_data()$Group
     max(m[g==G1]) <= min(m[g==G2]) })
  session$setInputs(dv_target=7, dv_gen=2)
  ok("t-test DV slider reaches the ceiling", mean(scaled_dv()$mean) > 6.8)
})

testServer(pairedServer, args=list(id="p"), {
  base <- list(mean_c1=100, sd_c1=15, mean_c2=110, sd_c2=15, rho=.6, n=300,
    dv_use=TRUE, dv_items=4,dv_min=1,dv_max=7,dv_rel=.8,dv_cols="mean",dv_target=4,
    generate=1, dv_gen=0)
  do.call(session$setInputs, base)
  mid1 <- mean(scaled()$c1$mean)
  session$setInputs(dv_target=6, dv_gen=1)
  ok("paired slider moves BOTH conditions up",
     mean(scaled()$c1$mean) - mid1 > 1.0 &&
     mean(scaled()$c2$mean) > mean(scaled()$c1$mean) - 0.01)
  ok("paired conditions still share one mapping (C2 stays higher)",
     mean(scaled()$c2$mean) >= mean(scaled()$c1$mean))
  session$setInputs(dv_target=1, dv_gen=2)
  ok("paired slider reaches the floor", mean(scaled()$c1$mean) < 1.3)
})

cat("\n== a non-default target is not silently ignored anywhere ==\n")
# Direct guard against the class of bug: every call site must pass target.
body_txt <- paste(readLines(app_path()), collapse = "\n")
n_calls  <- length(gregexpr("make_scale_items\\(", body_txt)[[1]])
n_target <- length(gregexpr("target = spec_target\\(spec\\)", body_txt)[[1]])
# The definition reads "make_scale_items <- function", so this regex matches
# call sites only; every one of them must pass a target.
ok(sprintf("all %d generator call sites pass a target (%d do)", n_calls, n_target),
   n_target == n_calls)
