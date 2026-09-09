# The Typical response control: where the average response lands,
# floor/ceiling reachability, and the guard helpers.
#
# Run from the project root:  Rscript tests/test_target.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_engine()

set.seed(9)
th <- rnorm(20000)

cat("Realized mean response vs requested target (1-7, 4 items, alpha .8)\n")
cat(sprintf("%-10s %-14s %-12s %s\n","target","realized mean","% at 7","% at 1"))
for (tg in c(1, 2, 3, 4, 5, 6, 7)) {
  it <- make_scale_items(th, 0, 1, 4, 1, 7, .8, target = tg)
  m  <- rowMeans(it); raw <- as.matrix(it)
  cat(sprintf("%-10.1f %-14.2f %-12.1f %.1f\n", tg, mean(m),
              100*mean(raw==7), 100*mean(raw==1)))
}
cat("\n")
mid <- rowMeans(make_scale_items(th, 0, 1, 4, 1, 7, .8, target = 4))
ok("midpoint target centres the scale", abs(mean(mid) - 4) < 0.05)
noarg <- rowMeans(make_scale_items(th, 0, 1, 4, 1, 7, .8))
ok("omitting target matches the midpoint (back-compatible)",
   abs(mean(noarg) - mean(mid)) < 0.05)
hi <- rowMeans(make_scale_items(th, 0, 1, 4, 1, 7, .8, target = 6))
ok("target 6 shifts the mean up", mean(hi) > 5.4 && mean(hi) < 6.1)
lo <- rowMeans(make_scale_items(th, 0, 1, 4, 1, 7, .8, target = 2))
ok("target 2 shifts the mean down", mean(lo) < 2.6 && mean(lo) > 1.9)
ok("target 6 creates a ceiling (SD shrinks vs centred)", sd(hi) < sd(mid))
end <- rowMeans(make_scale_items(th, 0, 1, 4, 1, 7, .8, target = 7))
ok("endpoint target is reachable and degenerate",
   mean(end) > 6.9 && sd(end) < 0.15)
ok("endpoint does not hang or error", is.finite(mean(end)))
ok("endpoint target on a 1-5 scale also works", {
   e5 <- rowMeans(make_scale_items(th, 0, 1, 3, 1, 5, .8, target = 1))
   mean(e5) < 1.1 })

cat("\nGuard helpers\n")
ok("is_constant on a constant vector", is_constant(rep(7, 50)))
ok("is_constant FALSE on a varying vector", !is_constant(rnorm(50)))
ok("is_constant on a factor-ish integer", is_constant(rep(3L, 20)))
ok("caution fires on 2 distinct values",
   !is.null(caution_note(rep(c(6,7), 25), 1, 7)))
ok("caution fires on heavy ceiling",
   !is.null(caution_note(c(rep(7,80), 4:8/2), 1, 7)))
ok("no caution on healthy spread",
   is.null(caution_note(round(rnorm(200, 4, 1.2)), 1, 7)))
ok("fmt renders NA as an em dash", fmt(NA_real_) == "—")
ok("fmt still vectorises", identical(fmt(c(1, NA)), c("1.00", "—")))
ok("fmt_r renders NaN as an em dash", fmt_r(NaN) == "—")
