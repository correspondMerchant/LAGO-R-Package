get_int_vector <- function(
    include_interaction_terms,
    intervention_components,
    main_components,
    x) {
  if (include_interaction_terms) {
    int_vector <- numeric(length(intervention_components))
    for (i in seq_along(intervention_components)) {
      components <- strsplit(
        gsub("`", "", intervention_components[i]), ":"
      )[[1]]

      if (length(components) == 1) {
        # For single components, use the value directly
        idx <- which(main_components == components[1])
        int_vector[i] <- x[idx]
      } else {
        # For interaction terms, multiply the corresponding values
        prod_result <- 1
        for (comp in components) {
          # identify which component in the main_components corresponds
          # to "comp".
          idx <- which(main_components == comp)
          prod_result <- prod_result * x[idx]
        }
        int_vector[i] <- prod_result
      }
    }
    int_vector <- c(1, int_vector)
  } else {
    int_vector <- c(1, x)
  }

  return(int_vector)
}
