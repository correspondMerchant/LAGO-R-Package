# The variance of the coefficients of a continuous outcome model, without
# clustering, as the confidence set uses it. X is the design matrix of the
# fitted rows, with one column per coefficient.
continuous_vcov <- function(X, model, outcome_data, link) {
  n <- nrow(X)
  n_params <- ncol(X)
  fitted_values <- model$fitted.values
  prior_weights <- model$prior.weights
  if (is.null(prior_weights)) prior_weights <- rep(1, n)
  unit_weights <- all(prior_weights == 1)

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
      bread <- solve(t(X) %*% X)
    } else {
      X_positive <- X[positive, , drop = FALSE]
      residuals <- outcome_data[positive] - fitted_values[positive]
      sigma2 <- sum(prior_weights[positive] * residuals^2) / residual_df
      bread <- solve(t(X_positive) %*% (prior_weights[positive] * X_positive))
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
