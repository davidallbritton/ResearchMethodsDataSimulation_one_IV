# Ticking a scale box generates immediately; unticking restores the
# continuous column. Neither disturbs the sample.
#
# Run from the project root:  Rscript tests/test_tick.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(3)
cat("\n== ticking the box with no button press ==\n")
testServer(corrServer, args=list(id="c"), {
  session$setInputs(mean_x=0, sd_x=1, mean_y=0, slope=.5, sd_e=1, n=20,
                    x_use=FALSE, y_use=FALSE, generate=1, gen_scales=0,
                    y_items=4, y_min=1, y_max=7, y_rel=.8, y_cols=c("items","mean"),
                    x_items=4, x_min=1, x_max=7, x_rel=.8, x_cols=c("items","mean"))
  base <- sim_data()
  ok("no scale yet", is.null(scaled_y()))
  session$setInputs(y_use=TRUE)
  ok("ticking the box alone produces a scale immediately", !is.null(scaled_y()))
  ok("...without redrawing the sample", identical(base, sim_data()))
  ok("...and it shows up in the CSV", "Y_Scale_Mean" %in% names(labelled_data()))
  yv <- scaled_y()$mean
  session$setInputs(x_use=TRUE)
  ok("ticking the OTHER variable does not redraw this one",
     identical(yv, scaled_y()$mean))
  session$setInputs(y_use=FALSE)
  ok("unticking removes it immediately", is.null(scaled_y()))
  ok("...and the continuous column returns", "Y" %in% names(labelled_data()))
})
