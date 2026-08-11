// Sandwich (robust/clustered) variance accumulation for the LOGIT link.
//
// These kernels replace the two per-observation R for-loops in get_vcov()
// (R/get_confidence_set.R): the clustered logit estimator and the
// non-clustered HC0 logit estimator. They do ONLY the per-observation
// accumulation into the "bread" matrix J and the "meat" matrix V, and return
// those matrices to R. The matrix inversion (solve()) and the final
// bread %*% V %*% t(bread) are deliberately left in R so the numerics of the
// inversion are byte-for-byte identical to the original R path, and so a
// singular J still errors in R exactly as it did before (no regularization).
//
// For observation i the score contribution uses the working weight
//   w_i = p_i * (1 - p_i)
// and the vector
//   ddb_i = w_i * x_i        (x_i is row i of the design matrix X)
// which is d p_i / d beta, the gradient of the fitted probability.

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

// Clustered logit sandwich accumulation (Cameron-Gelbach-Miller building block).
//
// Per cluster c:
//   cluster_hessian_c = sum_{i in c} ddb_i ddb_i^T
//   cluster_score_c   = sum_{i in c} ddb_i * (y_i - p_i)
// then, matching the R loop's arithmetic exactly (each cluster's contribution
// is divided by n_clusters before being summed):
//   J = sum_c cluster_hessian_c / n_clusters
//   V = sum_c (cluster_score_c cluster_score_c^T) / n_clusters
//
// cluster_index must be 0-based and enumerate clusters in the SAME order R's
// unique(cluster_id) produces (first appearance), so the summation order over
// clusters matches the R original. Observations are visited in increasing
// index order, so within a cluster the accumulation order matches R too.
//
// [[Rcpp::export]]
List sandwich_cluster_logit_accumulate(NumericMatrix X,
                                       IntegerVector cluster_index,
                                       int n_clusters,
                                       NumericVector fitted_values,
                                       NumericVector outcome) {
  int n = X.nrow();
  int p = X.ncol();

  // Input guards (undefined-behavior prevention). The loop below reads
  // fitted_values[i], outcome[i] and cluster_index[i] for i in [0, n) and uses
  // cluster_index[i] to index the accumulators, all WITHOUT bounds checks, so
  // malformed input would corrupt memory silently rather than erroring. It is
  // refused here up front instead.
  //
  // Length mismatch is reachable: a predictor column with sporadic NAs makes
  // glm() na.omit those rows, so fitted_values comes back shorter than X. A
  // vector shorter than n would then be read past its end (out-of-bounds READ).
  if (fitted_values.size() != n || outcome.size() != n) {
    stop("sandwich kernel: fitted_values and outcome must have length "
         "nrow(X) (%d); got %d and %d.",
         n, (int) fitted_values.size(), (int) outcome.size());
  }
  if (cluster_index.size() != n) {
    stop("sandwich kernel: cluster_index must have length nrow(X) (%d); "
         "got %d.",
         n, (int) cluster_index.size());
  }
  // n_clusters sizes the accumulators and cluster_index[i] indexes into them,
  // so n_clusters < 1 sizes them empty and any cluster_index[i] outside
  // [0, n_clusters) is an out-of-bounds WRITE into hess/score below (memory
  // corruption). match(cluster_id, unique(cluster_id)) - 1L always lands in
  // range, so these only fire on a malformed caller.
  if (n_clusters < 1) {
    stop("sandwich kernel: n_clusters must be >= 1; got %d.", n_clusters);
  }
  for (int i = 0; i < n; ++i) {
    int c = cluster_index[i];
    if (c < 0 || c >= n_clusters) {
      stop("sandwich kernel: cluster_index[%d] = %d is out of range "
           "[0, n_clusters) = [0, %d).",
           i, c, n_clusters);
    }
  }

  // per-cluster hessian (flattened p*p) and score (p), zero-initialized
  std::vector<double> hess(static_cast<std::size_t>(n_clusters) * p * p, 0.0);
  std::vector<double> score(static_cast<std::size_t>(n_clusters) * p, 0.0);

  std::vector<double> ddb(p);

  for (int i = 0; i < n; ++i) {
    double p_i = fitted_values[i];
    double w = p_i * (1.0 - p_i);
    double resid = outcome[i] - p_i;
    for (int a = 0; a < p; ++a) {
      ddb[a] = w * X(i, a);
    }
    int c = cluster_index[i];
    std::size_t hbase = static_cast<std::size_t>(c) * p * p;
    std::size_t sbase = static_cast<std::size_t>(c) * p;
    for (int a = 0; a < p; ++a) {
      score[sbase + a] += ddb[a] * resid;
      double da = ddb[a];
      std::size_t row = hbase + static_cast<std::size_t>(a) * p;
      for (int b = 0; b < p; ++b) {
        hess[row + b] += da * ddb[b];
      }
    }
  }

  NumericMatrix J(p, p);
  NumericMatrix V(p, p);
  double nc = static_cast<double>(n_clusters);
  for (int c = 0; c < n_clusters; ++c) {
    std::size_t hbase = static_cast<std::size_t>(c) * p * p;
    std::size_t sbase = static_cast<std::size_t>(c) * p;
    for (int a = 0; a < p; ++a) {
      double sa = score[sbase + a];
      std::size_t row = hbase + static_cast<std::size_t>(a) * p;
      for (int b = 0; b < p; ++b) {
        J(a, b) += hess[row + b] / nc;
        V(a, b) += (sa * score[sbase + b]) / nc;
      }
    }
  }

  return List::create(Named("J") = J, Named("V") = V);
}

// Non-clustered HC0 logit sandwich accumulation.
//
// Per observation i (the HC0 per-observation meat form (y_i - p_i)^2, NOT a
// clustered score):
//   J += (ddb_i ddb_i^T) / n
//   V += (ddb_i (y_i - p_i)^2 ddb_i^T) / n
// The division by n happens per observation, matching the R loop.
//
// [[Rcpp::export]]
List sandwich_hc0_logit_accumulate(NumericMatrix X,
                                    NumericVector fitted_values,
                                    NumericVector outcome) {
  int n = X.nrow();
  int p = X.ncol();
  double nn = static_cast<double>(n);

  // Input guard (undefined-behavior prevention). The loop reads
  // fitted_values[i] and outcome[i] for i in [0, n) with no bounds check, so a
  // vector shorter than n is read past its end (out-of-bounds READ). This is
  // reachable when a predictor column with sporadic NAs makes glm() na.omit
  // rows, leaving fitted_values shorter than X. Refuse it here.
  if (fitted_values.size() != n || outcome.size() != n) {
    stop("sandwich kernel: fitted_values and outcome must have length "
         "nrow(X) (%d); got %d and %d.",
         n, (int) fitted_values.size(), (int) outcome.size());
  }

  NumericMatrix J(p, p);
  NumericMatrix V(p, p);
  std::vector<double> ddb(p);

  for (int i = 0; i < n; ++i) {
    double p_i = fitted_values[i];
    double w = p_i * (1.0 - p_i);
    double resid = outcome[i] - p_i;
    double r2 = resid * resid;
    for (int a = 0; a < p; ++a) {
      ddb[a] = w * X(i, a);
    }
    for (int a = 0; a < p; ++a) {
      double da = ddb[a];
      for (int b = 0; b < p; ++b) {
        double jab = da * ddb[b];
        J(a, b) += jab / nn;
        // matches R: v_i = ddb_i %*% (r2) %*% t(ddb_i), i.e. ddb_a * r2 * ddb_b
        V(a, b) += (da * r2 * ddb[b]) / nn;
      }
    }
  }

  return List::create(Named("J") = J, Named("V") = V);
}
