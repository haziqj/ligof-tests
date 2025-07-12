## -----------------------------------------------------------------------------
#| message: false
library(tidyverse)    
library(lavaan)
library(lavaan.bingof)  
options(width = 80)


## -----------------------------------------------------------------------------
simulate_data <- function(n = 1000) {
  popmod <- "
    f1 =~ 0.8*y1 + 0.7*y2 + 0.5*y3 + 0.4*y4 + 0.3*y5
    f1 ~~ 1*f1
  
    y1| -1.43*t1
    y2| -0.55*t2
    y3| -0.13*t3
    y4| -0.72*t4
    y5| -1.13*t5
  " 
  
  lavaan::simulateData(popmod, sample.nobs = n) |>
    lapply(ordered) |>
    as_tibble()
}

dat <- simulate_data(n = 1000)
glimpse(dat)


## -----------------------------------------------------------------------------
fit <- cfa("f1 =~ y1 + y2 + y3 + y4 + y5", dat, std.lv = TRUE)
summary(fit)


## -----------------------------------------------------------------------------
lavaan.bingof::get_uni_bi_moments
get_uni_bi_moments(fit)
e2_hat <- with(get_uni_bi_moments(fit), {
    p2_hat <- c(pdot1, pdot2)
    pi2_hat <- c(pidot1, pidot2)
    p2_hat - pi2_hat
  })
str(e2_hat)


## -----------------------------------------------------------------------------
lavaan.bingof:::create_Sigma2_matrix
lavaan.bingof:::create_Sigma2_matrix(fit) |>
  Matrix::Matrix() |>
  round(3)


## -----------------------------------------------------------------------------
create_Delta2_matrix <- function(lavobject) {
  p <- lavobject@Model@nvar
  all_thresh <- inspect(lavobject, "est")$tau

  if(ncol(all_thresh) != 1L) {
    stop("This simplified function only handles purely binary indicators (1 threshold per variable).")
  }
  tau <- as.numeric(all_thresh)
  Sigma_hat <- inspect(lavobject, "implied")$cov
  rho_ij <- Sigma_hat[lower.tri(Sigma_hat)]

  Delta_full <- lavaan:::computeDelta(lavobject@Model)[[1]]
  derTauToTheta <- Delta_full[1:p, , drop = FALSE]
  derRhoToTheta <- Delta_full[-(1:p), , drop = FALSE]

  pair_idx <- which(lower.tri(Sigma_hat), arr.ind = TRUE)
  npairs <- nrow(pair_idx)

  # Precompute all necessary values
  dnorm_tau <- dnorm(tau)
  tau_i <- tau[pair_idx[, 2]]
  tau_j <- tau[pair_idx[, 1]]
  dnorm_tau_i <- dnorm_tau[pair_idx[, 2]]
  dnorm_tau_j <- dnorm_tau[pair_idx[, 1]]
  rho_values <- rho_ij

  # Common terms for vectorized calculations
  denominator_sq <- 1 - rho_values^2
  sqrt_denominator_sq <- sqrt(denominator_sq)
  z1 <- (rho_values * tau_i - tau_j) / sqrt_denominator_sq
  z2 <- (rho_values * tau_j - tau_i) / sqrt_denominator_sq

  # Vectorized derivatives calculations
  dP.dTaui <- -dnorm_tau_i * pnorm(z1)
  dP.dTauj <- -dnorm_tau_j * pnorm(z2)
  exponent <- -0.5 * (tau_i^2 - 2 * rho_values * tau_i * tau_j + tau_j^2) / denominator_sq
  dP.dRho <- exp(exponent) / (2 * pi * sqrt_denominator_sq)

  # Vectorized matrix operations
  i_indices <- pair_idx[, 2]
  j_indices <- pair_idx[, 1]

  dP_taui_theta <- dP.dTaui * derTauToTheta[i_indices, , drop = FALSE]
  dP_tauj_theta <- dP.dTauj * derTauToTheta[j_indices, , drop = FALSE]
  dP_rho_theta <- dP.dRho * derRhoToTheta

  derBiv_11_wrtTheta <- dP_taui_theta + dP_tauj_theta + dP_rho_theta

  # Combine results
  rbind(
    -dnorm_tau * derTauToTheta,
    derBiv_11_wrtTheta
  )
}


## -----------------------------------------------------------------------------
create_Delta2_matrix(fit) |> 
  Matrix::Matrix(sparse = TRUE) |>
  round(3)


## -----------------------------------------------------------------------------
# knitr::purl("ligof.qmd", output = "ligof.R", quiet = TRUE)

