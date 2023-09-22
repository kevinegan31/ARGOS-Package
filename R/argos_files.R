#' Lasso
#'
#' This function performs lasso regression using the cv.glmnet function,
#' then refits the model using ordinary least squares.
#'
#' @param data A data frame or matrix containing the predictors and response. The response must be in the first column.
#' @param index A numeric vector of indices indicating the rows of 'data' to use for the lasso regression.
#' @param ols_ps A logical scalar. If TRUE (default), the function returns the coefficients from the OLS fit. If FALSE, it returns the coefficients from the lasso fit.
#'
#' @return A numeric vector of coefficients. If 'ols_ps' is TRUE, these are the coefficients from the OLS fit. If 'ols_ps' is FALSE, these are the coefficients from the lasso fit. If an error occurs during the lasso or OLS fit, the function returns a vector of NAs.
#'
#' @export
#' @import Matrix
#' @import glmnet
#' @import tidyverse
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats BIC
#' @importFrom glmnet cv.glmnet
lasso <- function(data, index, ols_ps = TRUE) {
  tryCatch({
    x <- as.matrix(data[index,-1])
    y <- as.matrix(data[index, 1, drop = FALSE])
    lasso_init <-
      cv.glmnet(x, y, alpha = 1, intercept = TRUE) #alpha=1, lasso
    lasso_init_lambda_min <- lasso_init$lambda.min
    lambda_init_grid <- lasso_init$lambda
    coef <- as.numeric(coef(lasso_init,
                            lasso_init_lambda_min))
    if (lasso_init$lambda.min ==
        lasso_init$lambda[length(lasso_init$lambda)]) {
      lower_bound_grid <- lasso_init$lambda.min / 10
      upper_bound_grid <-
        min(lasso_init$lambda[1], 1.1 * lasso_init$lambda.min)
      lambda_grid <-
        seq(upper_bound_grid, lower_bound_grid, length = 100)
      lasso_new <- cv.glmnet(x,
                             y,
                             alpha = 1,
                             lambda = lambda_grid,
                             intercept = TRUE)
      lasso_second_grid <- lasso_new$lambda
      coef <-
        as.numeric(coef(lasso_new,
                        lasso_new$lambda.min))
    }
    threshold_sequence <- 10 ^ (-8:1)
    lasso_final_coefficients_list <-
      lapply(threshold_sequence, function(x) {
        ifelse(abs(coef) <= x,
               0,
               coef)
      })
    coef_logical_list <-
      sapply(lasso_final_coefficients_list, function(e) {
        !all(e == 0)
      })
    lasso_final_coefficients_list <-
      lasso_final_coefficients_list[which(coef_logical_list)]
    ols_list <- lapply(lasso_final_coefficients_list, function(e) {
      coef_nonzero <- e != 0
      if (sum(coef_nonzero) > 0) {
        if (coef_nonzero[1] & any(coef_nonzero[-1])) {
          selected_x <- x[, coef_nonzero[-1], drop = FALSE]
          ols <- lm(y ~ as.matrix(selected_x))
        } else if (coef_nonzero[1]) {
          ols <- lm(y ~ 1)
        } else {
          selected_x <- x[, coef_nonzero[-1], drop = FALSE]
          ols <- lm(y ~ 0 + as.matrix(selected_x))
        }
      }
    })
    bic_min_list <- lapply(ols_list, function(e) {
      BIC(e)
    })
    lasso_ols_coefficients_list <-
      lapply(seq_along(lasso_final_coefficients_list), function(e) {
        coef_nonzero <- lasso_final_coefficients_list[[e]] != 0
        vect_coef <- rep(0, ncol(data))
        vect_coef[coef_nonzero] <- ols_list[[e]]$coefficients
        return(vect_coef)
      })
    lasso_final_coefficients <-
      lasso_final_coefficients_list[[which.min(bic_min_list)]]
    lasso_ols_coefficients <-
      lasso_ols_coefficients_list[[which.min(bic_min_list)]]
    if (ols_ps) {
      coef <- lasso_ols_coefficients
    } else {
      coef <- lasso_final_coefficients
    }
    return(coef)
  }, error = function(e) {
    rep(NA, ncol(data))
  })
}
#' Adaptive Lasso
#'
#' This function performs adaptive lasso regression using the cv.glmnet function, then refits the model using ordinary least squares.
#'
#' @param data A data frame or matrix containing the predictors and response. The response must be in the first column.
#' @param index A numeric vector of indices indicating the rows of 'data' to use for the adaptive lasso regression.
#' @param weights_method A character string specifying the method to calculate the weights. Can be either "ols" or "ridge". Default is "ols".
#' @param ols_ps A logical scalar. If TRUE (default), the function returns the coefficients from the OLS fit. If FALSE, it returns the coefficients from the lasso fit.
#'
#' @return A numeric vector of coefficients. If 'ols_ps' is TRUE, these are the coefficients from the OLS fit. If 'ols_ps' is FALSE, these are the coefficients from the lasso fit. If an error occurs during the lasso or OLS fit, the function returns a vector of NAs.
#'
#' @export
#' @import Matrix
#' @import glmnet
#' @importFrom stats lm
#' @importFrom stats lsfit
#' @importFrom stats coef
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom stats BIC
#' @importFrom glmnet cv.glmnet
alasso <-
  function(data,
           index,
           weights_method = c("ols", "ridge"),
           ols_ps = TRUE) {
    tryCatch({
      x <- as.matrix(data[index,-1])
      y <- as.matrix(data[index, 1, drop = FALSE])
      if (weights_method == "ols") {
        ols <-
          lsfit(
            x = x,
            y = y,
            intercept = FALSE,
            tolerance = 1e-20
          )[[1]]
        ols_coef <- ols
        ols_coef[is.na(ols_coef)] <- 0
        weight <- ols_coef
      }
      if (weights_method == "ridge") {
        ridge_init <- cv.glmnet(x, y, alpha = 0, intercept = TRUE)
        ridge_coef <- as.numeric(coef(ridge_init,
                                      s = ridge_init$lambda.min))
        if (ridge_init$lambda.min ==
            ridge_init$lambda[length(ridge_init$lambda)]) {
          lower_bound_grid <- ridge_init$lambda.min / 10
          upper_bound_grid <-
            min(ridge_init$lambda[1], 1.1 * ridge_init$lambda.min)
          lambda_grid <-
            seq(upper_bound_grid, lower_bound_grid, length = 100)
          ridge_new <-
            cv.glmnet(x,
                      y,
                      alpha = 0,
                      lambda = lambda_grid,
                      intercept = TRUE)
          ridge_coef <- as.numeric(coef(ridge_new,
                                        ridge_new$lambda.min))
        }
        ridge_coef[is.na(ridge_coef)] <- 0
        weight <- ridge_coef[-1]
      }
      # alpha=1, lasso
      alasso_init <- cv.glmnet(
        x,
        y,
        alpha = 1,
        penalty.factor = 1 / abs(weight),
        intercept = TRUE
      )
      alasso_init_lambda_min <- alasso_init$lambda.min
      alasso_init_lambda_grid <- alasso_init$lambda
      coef <- as.numeric(coef(alasso_init, alasso_init$lambda.min))
      if (alasso_init$lambda.min ==
          alasso_init$lambda[length(alasso_init$lambda)]) {
        lower_bound_grid <- alasso_init$lambda.min / 10
        upper_bound_grid <-
          min(alasso_init$lambda[1], 1.1 * alasso_init$lambda.min)
        lambda_grid <-
          seq(upper_bound_grid, lower_bound_grid, length = 100)
        alasso_new <- cv.glmnet(
          x,
          y,
          alpha = 1,
          penalty.factor = 1 / abs(weight),
          lambda = lambda_grid,
          intercept = TRUE
        )
        alasso_second_lambda_grid <- alasso_new$lambda
        coef <- as.numeric(coef(alasso_new, alasso_new$lambda.min))
      }
      threshold_sequence <- 10 ^ (-8:1)
      alasso_final_coefficients_list <- lapply(threshold_sequence, function(x) {
        ifelse(abs(coef) <= x,
               0,
               coef)
      })
      coef_logical_list <-
        sapply(alasso_final_coefficients_list, function(e) {
          !all(e == 0)
        })
      alasso_final_coefficients_list <-
        alasso_final_coefficients_list[which(coef_logical_list)]
      ols_list <- lapply(alasso_final_coefficients_list, function(e) {
        coef_nonzero <- e != 0
        if (sum(coef_nonzero) > 0) {
          if (coef_nonzero[1] & any(coef_nonzero[-1])) {
            selected_x <- x[, coef_nonzero[-1], drop = FALSE]
            ols <- lm(y ~ as.matrix(selected_x))
          } else if (coef_nonzero[1]) {
            ols <- lm(y ~ 1)
          } else {
            selected_x <- x[, coef_nonzero[-1], drop = FALSE]
            ols <- lm(y ~ 0 + as.matrix(selected_x))
          }
        }
      })
      bic_min_list <- lapply(ols_list, function(e) {
        BIC(e)
      })
      alasso_ols_coefficients_list <-
        lapply(seq_along(alasso_final_coefficients_list), function(e) {
          coef_nonzero <- alasso_final_coefficients_list[[e]] != 0
          vect_coef <- rep(0, ncol(data))
          vect_coef[coef_nonzero] <- ols_list[[e]]$coefficients
          return(vect_coef)
        })
      alasso_final_coefficients <-
        alasso_final_coefficients_list[[which.min(bic_min_list)]]
      alasso_ols_coefficients <-
        alasso_ols_coefficients_list[[which.min(bic_min_list)]]
      if (ols_ps) {
        coef <- alasso_ols_coefficients
      } else {
        coef <- alasso_final_coefficients
      }
      return(coef)
    }, error = function(e) {
      rep(NA, ncol(data))
    })
  }
#' Optimal Savitzky-Golay Filter Parameters Finder
#'
#' This function finds the optimal parameters for the Savitzky-Golay filter by evaluating combinations of polynomial orders and window lengths.
#'
#' @param x_t A numeric vector or one-column matrix. The data to be smoothed.
#' @param dt A numeric scalar. The time-step interval of the data. Default is 1.
#' @param polyorder A numeric scalar. The order of the polynomial to be used in the Savitzky-Golay filter. If not specified, 4 will be used by default.
#'
#' @return A list with three elements:
#'   - sg_combinations: a matrix where each row represents a combination of polynomial order and window length tried.
#'   - sg_order_wl: a vector of length 2 with the optimal polynomial order and window length.
#'   - f_dist: a data frame with the mean squared error of the differences between the original data and the smoothed data for each combination.
#'
#' @export
#' @import signal
#' @import tidyverse
#' @importFrom tidyr expand_grid
sg_optimal_combination <- function(x_t, dt = 1, polyorder) {
  ### Create Combinations
  wl_max <- round((nrow(as.matrix(x_t)) * 0.05), 0)
  wl_max <- ifelse(wl_max %% 2 == 0, wl_max + 1, wl_max)

  ### Polynomial Order
  polyorder <- if(missing(polyorder)) 4 else polyorder
  ### If the Window length calculation is less than 11
  ### we will just try the two minimum values.
  if (wl_max < 13) {
    ### Combinations
    sg_combinations <- cbind(4, 13)
  } else {
    ### Combinations
    if (wl_max > 101) {
      window_length <- seq(5, 101, by = 2)
    } else {
      if (wl_max %% 2 == 0) {
        wl_max <- wl_max + 1
        window_length <- seq(5, wl_max, by = 2)
      } else {
        window_length <- seq(5, wl_max, by = 2)
      }
    }
    sg_combinations <- expand_grid(polyorder, window_length) %>%
      subset(window_length > polyorder + 7 - polyorder %% 2) %>%
      as.matrix()
    if (nrow(sg_combinations) == 1) {
      sg_combinations <- cbind(4, 13)
    }
  }
  ### Determine MSE for Combinations
  mse_xt <- sapply(seq_len(nrow(sg_combinations)), function(i) {
    x_t_smoothed <- x_t %>% sgolayfilt(p = sg_combinations[i, 1],
                                       n = sg_combinations[i, 2],
                                       m = 0,
                                       ts = dt)
    Metrics::mse(x_t, x_t_smoothed)
  })

  mse_df <- data.frame(mse_xt = unlist(mse_xt))

  sg_best_combination <- which.min(mse_df$mse_xt)
  sg_order_wl <- cbind(sg_combinations[sg_best_combination, 1],
                       sg_combinations[sg_best_combination, 2])
  return(
    list(
      sg_combinations = sg_combinations,
      sg_order_wl = sg_order_wl,
      mse_df = mse_df
    )
  )
}
#' Automatic Regression for Governing Equations (ARGOS)
#'
#' This function performs sparse regression on a dataset to identify the governing equations
#' of the system. It uses the Savitzky-Golay filter to preprocess the data and then applies
#' the Lasso or Adaptive Lasso for feature selection.
#'
#' @param x_t A matrix. The input data.
#' @param monomial_degree An integer. The maximum degree of the monomials in the design matrix. Default is 5.
#' @param sg_poly_order A numeric scalar. The order of the polynomial to be used in the Savitzky-Golay filter. Default is 4.
#' @param dt A numeric scalar. The time-step interval of the data. Default is 1.
#' @param state_var_deriv An integer. The index of the state variable for which the derivative is calculated. Default is 1.
#' @param alpha_level A numeric scalar. The level of significance for confidence intervals. Default is 0.05.
#' @param num_samples An integer. The number of bootstrap samples. Default is 2000.
#' @param sr_method A character string. The sparse regression method to be used, either "lasso" or "alasso". Default is "lasso".
#' @param weights_method A string or NULL. The method for calculating weights in the Adaptive Lasso. If NULL, ridge regression pilot estimates are used. Default is NULL.
#' @param ols_ps A logical. If TRUE, post-selection OLS is performed after the Lasso or Adaptive Lasso. Default is TRUE.
#' @param parallel A character string. The type of parallel computation to be used, either "no", "multicore" or "snow". Default is "no".
#' @param ncpus An integer or NULL. The number of cores to be used in parallel computation. If NULL, the function will try to detect the number of cores. Default is NULL.
#'
#' @return A list with three elements:
#'   - point_estimates: a vector of point estimates for the coefficients.
#'   - ci: a matrix where each column represents the lower and upper bounds of the confidence interval for a coefficient.
#'   - identified_model: a matrix of coefficients of the identified model.
#'
#' @export
#' @import boot
#' @import tidyverse
#' @importFrom stats polym
#' @importFrom magrittr `%>%`
argos <- function(x_t,
                  monomial_degree = 5,
                  dt = 1,
                  sg_poly_order = 4,
                  state_var_deriv = 1,
                  alpha_level = 0.05,
                  num_samples = 2000,
                  sr_method = c("lasso", "alasso"),
                  weights_method = NULL,
                  ols_ps = TRUE,
                  parallel = c("no", "multicore", "snow"),
                  ncpus = NULL) {
  parallel <- match.arg(parallel)  # add this line
  sr_method <- match.arg(sr_method)  # add this line
  # Check if parallel processing is requested
  if (parallel != "no") {
    # Check if ncpus is NULL
    if (is.null(ncpus)) {
      # Detect number of cores and assign it to ncpus
      ncpus <- parallel::detectCores()
    }
  }
  monomial_degree <- monomial_degree
  dt <- dt
  # Filter x_t
  num_columns <- ncol(x_t)
  x_filtered <- list()
  xdot_filtered <- list()
  # Filter x_t
  for (i in 1:num_columns) {
    if (x_t[1, i]) {
      sg_combinations <- sg_optimal_combination(x_t[, i],
                                                dt,
                                                polyorder = sg_poly_order)[[2]]
      x_filtered[[i]] <- sgolayfilt(
        x_t[, i],
        p = sg_combinations[1, 1],
        n = sg_combinations[1, 2],
        m = 0,
        ts = dt
      )
      xdot_filtered[[i]] <- sgolayfilt(
        x_t[, i],
        p = sg_combinations[1, 1],
        n = sg_combinations[1, 2],
        m = 1,
        ts = dt
      )
    }
  }
  # Combine filtered data and derivatives
  x_t <- do.call(cbind, x_filtered)
  sg_dx <- do.call(cbind, xdot_filtered)
  ### Sort state variables for expansion
  ### x_t needs to be in reverse order because of how poly function expands
  ### We do this here so that we can use it for the for loop to determine
  ### optimal SG parameters
  out_sorted <- x_t %>%
    data.frame() %>%
    rev()
  # Polynomial Expansion
  expanded_theta <- polym(as.matrix(out_sorted),
                          degree = monomial_degree, raw = TRUE)
  # Order by degree using as.numeric_version numeric_version allows to
  # convert names of variables and expand without limit
  ordered_results <- order(attr(expanded_theta, "degree"),
                           as.numeric_version(colnames(expanded_theta)))
  # Sort Theta Matrix
  sorted_theta <- expanded_theta[, ordered_results]
  sorted_theta <- data.frame(sorted_theta)
  # Change Variable Names
  s <- strsplit(substring(colnames(sorted_theta), 2), "\\.")
  colnames(sorted_theta) <- sapply(s, function(x) {
    vec <- c("x", "y", "z")[seq_along(x)]
    x <- as.integer(x)
    y <- rep(vec, rev(x))
    paste(y, collapse = "")
  })
  # That lost the attributes, so put them back
  attr(sorted_theta, "degree") <-
    attr(expanded_theta, "degree")[ordered_results]
  sorted_theta <-
    sorted_theta[, order(attr(sorted_theta, "degree"), colnames(sorted_theta))]
  # That lost the attributes again, so put them back
  attr(sorted_theta, "degree") <-
    attr(expanded_theta, "degree")[ordered_results]
  monomial_orders <- attr(expanded_theta, 'degree')[ordered_results]
  # Create derivative and combine with theta matrix with SG Golay
  derivative_data <- list()
  for (i in 1:num_columns) {
    deriv_col <- sg_dx[, i]
    dot_df <- data.frame(cbind(deriv_col, sorted_theta))
    derivative_data[[i]] <- dot_df
  }
  # Access the desired data frame using the derivative variable
  data <- derivative_data[[state_var_deriv]]
  # Perform initial sparse regression to determine polynomial order of design matrix
  sr_method <- if(missing(sr_method)) "lasso" else sr_method
  weights_method <- if(missing(weights_method)) "ridge" else weights_method
  if (sr_method == "alasso") {
    initial_estimate <-
      alasso(data, weights_method = weights_method, ols_ps = ols_ps)
  } else {
    initial_estimate <- lasso(data, ols_ps = ols_ps)
  }
  # max nonzero value from sparse regression
  init_nz_max <- max(which(initial_estimate != 0))
  # Determine new theta order based on max nonzero value.
  # Include all monomials in max value
  new_theta_order <- sum(monomial_orders <=
                           monomial_orders[init_nz_max])
  # Rerun Bootstrap with Truncated Matrix
  if (is.na(new_theta_order) |
      new_theta_order == length(monomial_orders)) {
    post_lasso_matrix <- data
  } else {
    post_lasso_matrix <- data[-1][, 1:(new_theta_order)]
    post_lasso_matrix <-
      cbind.data.frame(data[1], post_lasso_matrix)
  }
  # Create list to compile necessary information for bootstrap.
  # Add updated matrix
  if (sr_method == "alasso") {
    boot_info <-
      c(
        list(data = post_lasso_matrix, R = num_samples),
        statistic = match.fun("alasso"),
        weights_method = weights_method,
        ols_ps = ols_ps,
        parallel = parallel,
        ncpus = ncpus
      )
  } else {
    boot_info <-
      c(
        list(data = post_lasso_matrix, R = num_samples),
        statistic = match.fun("lasso"),
        ols_ps = ols_ps,
        parallel = parallel,
        ncpus = ncpus
      )
  }

  # boot Function on Original Dataframe
  boot_s <- do.call(boot, boot_info)
  # Matrix of coefficients from bootstrap samples
  boot_t0 <- boot_s$t0 # point estimates
  boot_t <- boot_s$t # sample estimates
  ### In case of string/character, change to numeric
  boot_t <-
    matrix(as.numeric(boot_t),
           nrow = num_samples,
           ncol = ncol(boot_t))
  # subset any NAs
  num_nas_boot <- sum(apply(boot_t, 1, function(x)
    any(is.na(x))))
  boot_t <-
    subset(boot_t, apply(boot_t, 1, function(x)
      any(!is.na(x))))
  # ordered polynomial degree of variables alpha
  # typically equal to 0.05, 0.01, or 0.10
  b <- nrow(boot_t)
  q_normal <- alpha_level
  # Lower bound
  q1_normal <- (b * q_normal) / 2
  # Upper bound
  q2_normal <- b - q1_normal + 1
  if (round(q1_normal) <= 0) {
    q1_normal <- 1
  }
  if (q2_normal > b) {
    q2_normal <- b
  }
  # Sort and determine value of lower and upper bound
  ci <- apply(boot_t, 2, function(u) {
    sort(u)[c(round(q1_normal, 0), round(q2_normal, 0))]
  })
  ci[is.na(ci)] <- 0
  count_zero <- apply(boot_t, 2, function(x) {
    length(which(x == 0))
  })
  percent_zero <- apply(boot_t, 2, function(x) {
    length(which(x == 0)) / length(x)
  })
  df_columns <- c("(Intercept)", colnames(post_lasso_matrix)[-1])
  identified_model <- matrix(data = NA, nrow = length(boot_t0))
  rownames(identified_model) <- c("(Intercept)", df_columns[-1])
  ### Check if confidence intervals contain variable and do not cross zero
  for (i in seq_along(identified_model)) {
    if (ci[1, i] <= boot_t0[i] & ci[2, i] >= boot_t0[i] &
        ((ci[1, i] <= 0 && ci[2, i] >= 0)) == FALSE) {
      identified_model[i,] <- boot_t0[i]
    } else {
      identified_model[i,] <- 0
    }
  }
  return(
    list(
      point_estimates = boot_t0,
      ci = ci,
      identified_model = identified_model
    )
  )
}
