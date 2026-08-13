library(dplyr)
library(nleqslv)

data <- read.csv(
  "data/ftsp.csv",
  stringsAsFactors = FALSE
)

trait_columns <- c(
  "ft_la",
  "ft_sla",
  "ft_ldmc",
  "ft_stomata",
  "ft_dpph",
  "ft_proline",
  "ft_totalchl"
)

data <- data %>%
  filter(
    complete.cases(across(all_of(trait_columns)))
  )

Tmat <- scale(
  as.matrix(data[, trait_columns])
)

S <- nrow(Tmat)
K <- ncol(Tmat)

cwm_target_raw <- c(
  cwm1,
  cwm2,
  cwm3,
  cwm4,
  cwm5,
  cwm6,
  cwm7
)

cwm_target <- (
  cwm_target_raw - attr(Tmat, "scaled:center")
) / attr(Tmat, "scaled:scale")

cats_abundance <- function(lambda, Tmat) {
  eta <- as.vector(Tmat %*% lambda)
  eta <- eta - max(eta)
  p <- exp(eta)
  p / sum(p)
}

cwm_constraint <- function(lambda, Tmat, cwm_target) {
  p <- cats_abundance(lambda, Tmat)
  cwm_pred <- colSums(
    sweep(Tmat, 1, p, "*")
  )
  cwm_pred - cwm_target
}

lambda_start <- rep(0, K)

fit <- nleqslv(
  x = lambda_start,
  fn = cwm_constraint,
  Tmat = Tmat,
  cwm_target = cwm_target,
  method = "Broyden",
  control = list(
    ftol = 1e-10,
    xtol = 1e-10,
    maxit = 1000
  )
)

lambda_est <- fit$x

p_pred <- cats_abundance(
  lambda_est,
  Tmat
)

result_composite <- data.frame(
  species = data$species,
  relative_abundance = p_pred
) %>%
  arrange(desc(relative_abundance))

predicted_cwm_scaled <- colSums(
  sweep(Tmat, 1, p_pred, "*")
)

predicted_cwm <- (
  predicted_cwm_scaled *
    attr(Tmat, "scaled:scale")
) +
  attr(Tmat, "scaled:center")

cwm_validation <- data.frame(
  trait = trait_columns,
  predicted = predicted_cwm,
  observed = cwm_target_raw,
  difference = predicted_cwm - cwm_target_raw
)

write.csv(
  result_composite,
  "results/rank_open.csv",
  row.names = FALSE
)

write.csv(
  cwm_validation,
  "results/cwm_validation_open.csv",
  row.names = FALSE
)

write.csv(
  data.frame(
    parameter = lambda_est
  ),
  "results/lambda_open.csv",
  row.names = FALSE
)