#gitcreds::gitcreds_set()

# Test 1: 2 covariates, 3 strata (00, 01, 11), ER for 00 and 11.
# Apr 23, 2022
# Models:
# P(S = 00) = 0.3
# P(S = 01) = 0.5
# P(S = 11) = 0.2
# Y | S = 00 ~ Bernoulli(expit(X1 - X2))
# Y | S = 01, Z ~ Bernoulli(expit(2X1 - 0.5X2 + Z))
# Y | S = 11 ~ Bernoulli(exp(X2))

set.seed(0)

n <- 1000
data <- list()
data$X1 <- rnorm(n)
data$X2 <- rnorm(n)

get_one <- function(log_p1, log_p2, log_p3) {
  m <- max(log_p1, log_p2, log_p3, na.rm = T)
  f <- function(x) if (is.na(x)) 0 else exp(x - m)
  return (which.max(rmultinom(1, 1, c(f(log_p1), f(log_p2), f(log_p3)))))
}

## S
data$S <- sapply(
  1:n,
  function(i) get_one(
    log(0.3), 
    log(0.5),
    log(0.2)
  )
)
data$Z <- rbinom(n, 1, 0.5)
data$D <- ifelse(data$Z == 1, 
                 dplyr::case_when(data$S == 1 ~ 0,
                                  data$S == 2 ~ 1,
                                  data$S == 3 ~ 1),
                 dplyr::case_when(data$S == 1 ~ 0,
                                  data$S == 2 ~ 0,
                                  data$S == 3 ~ 1))

expit <- function(x) exp(x) / (1 + exp(x))

data$Y <- dplyr::case_when(data$S == 1 ~ rbinom(n, 1, expit(data$X1 - data$X2)),
                           data$S == 2 ~ rbinom(n, 1, expit(2 * data$X1 - 0.5 * data$X2 + data$Z)),
                           data$S == 3 ~ rbinom(n, 1, expit(data$X2))
                           )

write.csv(data, "test/no_covariates/data5.csv", row.names = F)
write.csv(data, "data5.csv", row.names = F)

Y.formula <- Y ~ X1 + X2
S.formula <- Z + D ~ 1
data <- read.csv("test/no_covariates/data5.csv")
stan_data <- get.stan.data(S.formula = S.formula, Y.formula = Y.formula, data = data)

test_p1s3c2_new <- rstan::stan(file = "test/new/1_test_p1s3c3.stan", data = stan_data,
                           iter = 1000, chains = 6, cores = 6)

test_p1s3c2_new
saveRDS(test_p2s5c0_new, file = "test/new/1_test_p2s5c0.Rds")

exp(c(0, 0.64, -0.38))/sum(exp(c(0, 0.64, -0.38)))

# Test 2: No covariates, 3 strata (n - 00, c - 01, a - 11), ER for 00 and 11.
# Apr 24, 2022
# Models:
# P(S = 00) = 0.3
# P(S = 01) = 0.2
# P(S = 11) = 0.5
# Y | S = 00 ~ N(3, 1)
# Y | S = 01, Z ~ N(-1 - Z, 0.5)
# Y | S = 11 ~ N(1, 2)

set.seed(0)

data <- read.csv("test/data.csv")
n <- nrow(data)

get_one <- function(log_p1, log_p2, log_p3, log_p4) {
  m <- max(log_p1, log_p2, log_p3, log_p4, na.rm = T)
  f <- function(x) if (is.na(x)) 0 else exp(x - m)
  return (which.max(rmultinom(1, 1, c(f(log_p1), f(log_p2), f(log_p3), f(log_p4)))))
}

## S
data$S <- sapply(
  1:n,
  function(i) get_one(
    log(0.3), 
    log(0.2),
    NA,
    log(0.5)
  )
)
data$Z <- rbinom(n, 1, 0.5)
data$D <- ifelse(data$Z == 1, 
                 ifelse(data$S %in% c(2, 4), 1, 0), 
                 ifelse(data$S %in% c(3, 4), 1, 0))
data$Y <- ifelse(data$S == 1,
                 rnorm(n, 3, 1), 
                 ifelse(data$S == 2,
                        rnorm(n, -1 - data$Z, 0.5),
                        rnorm(n, 1, 2))
)

write.csv(data, "test/no_covariates/data2.csv", row.names = F)

Y.formula <- Y ~ 1
S.formula <- Z + D ~ 1
data <- read.csv("test/no_covariates/data2.csv")
stan_data <- get.stan.data(S.formula = S.formula, Y.formula = Y.formula, data = data)
