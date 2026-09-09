# Column selection and CSV assembly across all three modules.
#
# Run from the project root:  Rscript tests/test_server.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_app()

set.seed(1)

cat("\n== CORRELATION ==\n")
testServer(corrServer, args = list(id = "corr"), {
  session$setInputs(mean_x=0, sd_x=1, mean_y=0, slope=.5, sd_e=1, n=40,
                    x_use=FALSE, y_use=FALSE, generate=1)
  ok("no scales: CSV is Participant, X, Y",
     identical(names(labelled_data()), c(ID_LABEL, "X", "Y")))
  ok("no scales: analysis uses raw X", identical(analysis_vars()$X, sim_data()$X))

  session$setInputs(y_use=TRUE, y_items=4, y_min=1, y_max=7, y_rel=.8,
                    y_cols=c("items","mean"), generate=2)
  nm <- names(labelled_data())
  ok("Y scale on: item columns present",
     all(paste0("Y_Scale_q", 1:4) %in% nm))
  ok("Y scale on: scale mean present", "Y_Scale_Mean" %in% nm)
  ok("Y scale on, raw unticked: continuous Y absent", !("Y" %in% nm))
  ok("X untouched: continuous X present", "X" %in% nm)
  ok("analysis Y is the scale mean", isTRUE(analysis_vars()$y_scaled))

  session$setInputs(y_cols=c("items","mean","raw"), generate=3)
  ok("raw ticked: continuous Y back", "Y" %in% names(labelled_data()))
  ok("raw ticked: analysis reverts to continuous",
     isFALSE(analysis_vars()$y_scaled))

  session$setInputs(y_cols=character(0), generate=4)
  ok("nothing ticked: falls back to continuous Y",
     "Y" %in% names(labelled_data()))
})

cat("\n== INDEPENDENT t-TEST ==\n")
testServer(ttestServer, args = list(id = "tt"), {
  session$setInputs(mean1=100, sd1=15, mean2=115, sd2=15, n=40,
                    iv_use=FALSE, dv_use=FALSE, generate=1)
  ok("no scales: CSV is Participant, Group, Score",
     identical(names(labelled_data()), c(ID_LABEL, "Group", "Score")))

  session$setInputs(dv_use=TRUE, dv_items=5, dv_min=1, dv_max=5, dv_rel=.8,
                    dv_cols=c("mean"), generate=2)
  d <- labelled_data()
  ok("DV scale mean present", "Score_Scale_Mean" %in% names(d))
  ok("DV items not requested, so absent",
     !any(grepl("Score_Scale_q", names(d))))
  ok("group difference survives the shared mapping", {
     sc <- scaled()$dv$mean; g <- sim_data()$Group
     mean(sc[g == G2]) - mean(sc[g == G1]) > 0.2 })

  session$setInputs(iv_use=TRUE, iv_items=4, iv_min=1, iv_max=7, iv_rel=.8,
                    iv_cols=c("items","mean"), generate=3)
  sc <- scaled()$iv$mean; g <- sim_data()$Group
  ok("IV median split reproduces the groups exactly",
     max(sc[g == G1]) <= min(sc[g == G2]))
  ok("IV item columns present",
     all(paste0("Group_Scale_q", 1:4) %in% names(labelled_data())))
})

cat("\n== PAIRED t-TEST ==\n")
testServer(pairedServer, args = list(id = "pr"), {
  session$setInputs(mean_c1=100, sd_c1=15, mean_c2=110, sd_c2=15, rho=.6, n=200,
                    dv_use=FALSE, generate=1)
  ok("no scales: CSV is Participant, Score1, Score2",
     identical(names(labelled_data()), c(ID_LABEL, "Score1", "Score2")))

  session$setInputs(dv_use=TRUE, dv_items=6, dv_min=1, dv_max=7, dv_rel=.85,
                    dv_cols=c("items","mean"), generate=2)
  nm <- names(labelled_data())
  ok("both conditions get item columns",
     all(c("Score1_Scale_q1","Score2_Scale_q1") %in% nm))
  ok("both conditions get scale means",
     all(c("Score1_Scale_Mean","Score2_Scale_Mean") %in% nm))
  sc <- scaled()
  ok("condition 2 mean still higher on the scale",
     mean(sc$c2$mean) > mean(sc$c1$mean))
  r_obs <- cor(sc$c1$mean, sc$c2$mean)
  cat(sprintf("      rho = 0.60 ; observed r on scale means = %.3f\n", r_obs))
  ok("paired correlation preserved, attenuated", r_obs > .25 && r_obs < .60)
  ok("analysis vars are the scale means", isTRUE(analysis_vars()$dv_scaled))
})
