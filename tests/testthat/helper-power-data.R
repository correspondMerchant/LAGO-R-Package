# Shared power-goal test data (used by test-icc-power.R and
# test-power-missing-outcome.R). The ncp constraint only binds on a small
# sample, so these direct-function tests use a small clustered dataset.

make_small_clustered <- function() {
  # deterministic small two-arm clustered dataset. J centers per label, m each.
  J <- 8
  m <- 10
  rows <- list()
  # fixed 0/1 pattern per center to avoid any RNG dependence across R versions
  for (j in seq_len(J)) {
    grp <- if (j <= J / 2) "control" else "treatment"
    rate <- if (grp == "treatment") 0.45 else 0.30
    y <- as.integer(seq_len(m) / m <= rate)
    rows[[j]] <- data.frame(
      center = paste0("c", j), group = grp, y = y
    )
  }
  do.call(rbind, rows)
}

small_coeff <- function(d) {
  c(
    "(Intercept)" = stats::qlogis(mean(d$y[d$group == "control"])),
    "dose" = 0.6
  )
}
