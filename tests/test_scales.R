# The Likert scale engine: item range, reliability targeting,
# sample-independence of the mapping, and degenerate inputs.
#
# Run from the project root:  Rscript tests/test_scales.R
# Or run everything:          Rscript tests/run_all.R

source(if (file.exists("tests/helper.R")) "tests/helper.R" else "helper.R")
load_engine()

# Load only the engine (everything above "Shared styling").

set.seed(42)

# --- 1. range and integrality ---
x  <- rnorm(2000, 100, 15)
it <- make_scale_items(x, 100, 15, n_items = 4, k_min = 1, k_max = 7, reliability = .8)
ok("items are integers", all(it == round(it)))
ok("items within [1,7]", min(as.matrix(it)) >= 1 && max(as.matrix(it)) <= 7)
ok("uses full range", min(as.matrix(it)) == 1 && max(as.matrix(it)) == 7)
ok("4 item columns", ncol(it) == 4)

# --- 2. reliability lands near target ---
cronbach <- function(m) {
    m <- as.matrix(m); k <- ncol(m)
    (k/(k-1)) * (1 - sum(apply(m, 2, var)) / var(rowSums(m)))
}
for (target in c(.6, .7, .8, .9)) {
    a <- cronbach(make_scale_items(rnorm(5000), 0, 1, 6, 1, 7, target))
    ok(sprintf("alpha ~ %.2f (got %.3f)", target, a), abs(a - target) < .05)
}

# --- 3. mapping is sample-independent (same params -> same thresholds) ---
# High reliability => almost no item noise, so the map is near-deterministic.
a1 <- make_scale_items(c(70, 100, 130), 100, 15, 1, 1, 7, .99)[[1]]
a2 <- make_scale_items(c(70, 100, 130), 100, 15, 1, 1, 7, .99)[[1]]
ok("same inputs -> same categories (no sample rescale)", identical(a1, a2))
# and a different sample range does not shift the mapping
b <- make_scale_items(c(70, 100, 130, 400), 100, 15, 1, 1, 7, .99)[[1]]
ok("extra extreme case does not move earlier ones", identical(b[1:3], a1))

# --- 4. group difference survives a COMMON mapping ---
g1 <- rnorm(3000, 100, 15); g2 <- rnorm(3000, 115, 15)
ref_m <- 107.5; ref_sd <- sqrt(15^2 + 7.5^2)
m1 <- rowMeans(make_scale_items(g1, ref_m, ref_sd, 4, 1, 7, .8))
m2 <- rowMeans(make_scale_items(g2, ref_m, ref_sd, 4, 1, 7, .8))
ok("group 2 scale mean > group 1", mean(m2) - mean(m1) > 0.3)
cat(sprintf("      continuous d = %.3f ; scale d = %.3f (attenuated)\n",
    (mean(g2)-mean(g1))/15,
    (mean(m2)-mean(m1))/sqrt((var(m1)+var(m2))/2)))

# --- 5. per-condition mapping WOULD erase it (why we use a common ref) ---
w1 <- rowMeans(make_scale_items(g1, mean(g1), sd(g1), 4, 1, 7, .8))
w2 <- rowMeans(make_scale_items(g2, mean(g2), sd(g2), 4, 1, 7, .8))
ok("per-group mapping erases the difference (expected)",
   abs(mean(w2) - mean(w1)) < 0.1)

# --- 6. paired correlation is preserved but attenuated ---
n <- 5000; rho <- .6
z <- rnorm(n)
s1 <- 100 + 15*z
s2 <- 110 + 15*(rho*z + sqrt(1-rho^2)*rnorm(n))
rm_ <- (100+110)/2; rs <- sqrt((15^2+15^2)/2 + 5^2)
p1 <- rowMeans(make_scale_items(s1, rm_, rs, 6, 1, 7, .8))
p2 <- rowMeans(make_scale_items(s2, rm_, rs, 6, 1, 7, .8))
obs <- cor(p1, p2)
cat(sprintf("      population rho = %.2f ; observed r on scales = %.3f\n", rho, obs))
ok("paired correlation preserved (positive, attenuated)", obs > .3 && obs < rho)
ok("condition 2 mean still higher", mean(p2) > mean(p1))

# --- 7. degenerate inputs do not hang or error ---
ok("k_max <= k_min is repaired", {
    z <- make_scale_items(rnorm(20), 0, 1, 3, 5, 5, .8); all(z >= 5 & z <= 6) })
ok("ref_sd = 0 does not error", {
    z <- make_scale_items(rep(3, 20), 3, 0, 3, 1, 7, .8); nrow(z) == 20 })
ok("1 item works", ncol(make_scale_items(rnorm(20), 0, 1, 1, 1, 5, .8)) == 1)
ok("reliability 0.99 works", is.finite(scale_error_sd(4, .99)))
ok("reliability out of range is clamped", is.finite(scale_error_sd(4, 5)))
