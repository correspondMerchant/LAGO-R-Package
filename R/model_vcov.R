# The variance of the coefficients of a continuous outcome model, without
# clustering, as the confidence set uses it. X is the design matrix of the
# fitted rows, with one column per coefficient.
continuous_vcov <- function(X, model, outcome_data, link) {
  # a family other than gaussian weights its rows by its variance function,
  # so on the identity link its model based variance is glm()'s own
  if (link == "identity" && !identical(model$family$family, "gaussian")) {
    v <- tryCatch(stats::vcov(model), error = function(e) NULL)
    idx <- if (!is.null(v)) match(gsub("`", "", colnames(X)), gsub("`", "", rownames(v)))
    if (!is.null(v) && !anyNA(idx)) {
      vcov_matrix <- v[idx, idx, drop = FALSE]
      dimnames(vcov_matrix) <- list(colnames(X), colnames(X))
      return(vcov_matrix)
    }
  }
  # computed on a centered and scaled copy of X and turned back, see
  # conditioned_design()
  conditioned <- conditioned_design(X)
  v <- continuous_vcov_raw(conditioned$Z, model, outcome_data, link)
  out <- conditioned$M %*% v %*% t(conditioned$M)
  dimnames(out) <- list(colnames(X), colnames(X))
  out
}

# A centered and scaled copy Z = X M of a design matrix, with M. Each column
# that is not constant is scaled to sd 1, and centered when X has an intercept
# column. A coefficient vector b of X is M g for the coefficients g of Z, so a
# variance V of g is M V t(M) for b. The forms of the variance below are all
# products of X, so they are the same up to round off, which is now small: a
# component far from 0, or in very large or small units, otherwise makes X'X
# nearly singular.
conditioned_design <- function(X) {
  p <- ncol(X)
  M <- diag(p)
  intercept <- which(apply(X, 2, function(col) all(col == 1)))[1]
  for (j in seq_len(p)) {
    col <- X[, j]
    if (!is.na(intercept) && j == intercept) next
    center <- if (!is.na(intercept)) mean(col) else 0
    spread <- stats::sd(col)
    if (!is.finite(spread) || spread == 0) spread <- max(abs(col))
    if (!is.finite(spread) || spread == 0) spread <- 1
    M[j, j] <- 1 / spread
    if (!is.na(intercept)) M[intercept, j] <- -center / spread
  }
  Z <- X %*% M
  colnames(Z) <- colnames(X)
  list(Z = Z, M = M)
}

continuous_vcov_raw <- function(X, model, outcome_data, link) {
  n <- nrow(X)
  n_params <- ncol(X)
  fitted_values <- model$fitted.values
  prior_weights <- model$prior.weights
  if (is.null(prior_weights)) prior_weights <- rep(1, n)
  unit_weights <- all(prior_weights == 1)

  # a family other than gaussian weights its rows by its variance function,
  # so on the identity link its model based variance is glm()'s own
  if (link == "identity" && !identical(model$family$family, "gaussian")) {
    v <- tryCatch(stats::vcov(model), error = function(e) NULL)
    idx <- if (!is.null(v)) match(gsub("`", "", colnames(X)), gsub("`", "", rownames(v)))
    if (!is.null(v) && !anyNA(idx)) {
      vcov_matrix <- v[idx, idx, drop = FALSE]
      dimnames(vcov_matrix) <- list(colnames(X), colnames(X))
      return(vcov_matrix)
    }
  }
  if (link == "identity") {
    positive <- prior_weights > 0
    residual_df <- sum(positive) - n_params
    if (residual_df <= 0) {
      stop(paste0(
        "Continuous identity covariance requires positive residual ",
        "degrees of freedom among positive weight rows."
      ))
    }
    if (unit_weights) {
      residuals <- outcome_data - fitted_values
      sigma2 <- sum(residuals^2) / (n - n_params)
      bread <- xtx_inverse(X)
    } else {
      X_positive <- X[positive, , drop = FALSE]
      residuals <- outcome_data[positive] - fitted_values[positive]
      sigma2 <- sum(prior_weights[positive] * residuals^2) / residual_df
      bread <- xtx_inverse(X_positive, prior_weights[positive])
    }
    vcov_matrix <- bread * sigma2
  } else {
    # The compiled kernel does ONLY the per-observation accumulation of the
    # HC0 form: J = sum_i (ddb_i ddb_i^T)/n and
    # V = sum_i (ddb_i (y_i - p_i)^2 ddb_i^T)/n. solve() and the final
    # bread %*% V %*% t(bread) stay in R, so a singular J errors here.
    acc <- sandwich_hc0_logit_accumulate(
      X, fitted_values, outcome_data,
      prior_weights = if (unit_weights) NULL else prior_weights
    )
    bread <- solve(acc$J)
    vcov_matrix <- (bread %*% acc$V %*% t(bread)) / n
  }
  rownames(vcov_matrix) <- colnames(X)
  colnames(vcov_matrix) <- colnames(X)
  vcov_matrix
}

# The variance of the fitted coefficients that the intervals and the
# confidence set use: glm()'s own for a binary outcome, and
# continuous_vcov() for a continuous one.
lago_model_vcov <- function(model, outcome_type, link) {
  if (identical(outcome_type, "binary")) {
    return(stats::vcov(model))
  }
  X <- stats::model.matrix(model)
  # a coefficient glm() could not estimate has no variance
  X <- X[, !is.na(stats::coef(model)), drop = FALSE]
  continuous_vcov(X, model, model$y, link)
}

# The coefficient table of a result: each estimate with the standard error
# the intervals and the confidence set use.
lago_coef_table <- function(x) {
  estimates <- stats::coef(x$model)
  se <- rep(NA_real_, length(estimates))
  vcov_matrix <- tryCatch(
    lago_model_vcov(x$model, x$outcome_type, x$model$family$link),
    error = function(e) NULL
  )
  if (!is.null(vcov_matrix)) {
    se[match(rownames(vcov_matrix), names(estimates))] <- sqrt(diag(vcov_matrix))
  }
  data.frame(Estimate = unname(estimates), `Std. Error` = se,
    row.names = names(estimates), check.names = FALSE)
}

# The sentence that says which standard errors the table shows.
lago_se_note <- function(x) {
  if (identical(x$outcome_type, "binary")) {
    "Standard errors are those of the fitted glm()."
  } else if (identical(x$model$family$link, "logit")) {
    "Standard errors are robust (sandwich, HC0), as the confidence set uses them."
  } else {
    "Standard errors are model based, as the confidence set uses them."
  }
}


# The inverse of X'WX, from a QR decomposition of sqrt(W) X. Forming X'X and
# solving it squares the condition number of X, so a component far from 0,
# such as values near 100000, made solve() call a well fitted model singular.
# glm() fits by QR too. A rank deficient X is refused as solve() refused it.
xtx_inverse <- function(X, w = NULL) {
  Xw <- if (is.null(w)) X else sqrt(w) * X
  q <- qr(Xw)
  if (q$rank < ncol(Xw)) {
    stop("system is computationally singular: the design matrix is not of full rank")
  }
  inv <- chol2inv(qr.R(q))
  back <- order(q$pivot)
  inv <- inv[back, back, drop = FALSE]
  dimnames(inv) <- list(colnames(X), colnames(X))
  inv
}
