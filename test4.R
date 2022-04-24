#gitcreds::gitcreds_set()

# Test 1: No covariates, 5 strata (0000, 0001, 0011, 0101, 1111), ER for 0000, 0101, and 1111.
# Apr 23, 2022
# Models:
# P(S = 0000) = 0.15
# P(S = 0001) = 0.2
# P(S = 0011) = 0.1
# P(S = 0101) = 0.4
# P(S = 1111) = 0.15
# Y | S = 0000 ~ N(3, 1)
# Y | S = 0001, Z ~ N(-1 - Z, 0.5)
# Y | S = 0011, Z ~ N(1 + 3Z, 0.5)
# Y | S = 0101 ~ N(-1, 3)
# Y | S = 1111 ~ N(1, 2)

set.seed(0)

data <- read.csv("test/data.csv")
n <- nrow(data)

get_one <- function(log_p1, log_p2, log_p3, log_p4, log_p5) {
  m <- max(log_p1, log_p2, log_p3, log_p4, log_p5, na.rm = T)
  f <- function(x) if (is.na(x)) 0 else exp(x - m)
  return (which.max(rmultinom(1, 1, c(f(log_p1), f(log_p2), f(log_p3), f(log_p4), f(log_p5)))))
}

## S
data$S <- sapply(
  1:n,
  function(i) get_one(
    log(0.15), 
    log(0.2),
    log(0.1),
    log(0.4),
    log(0.15)
  )
)
data$Z <- rbinom(n, 1, 0.5)
data$D1 <- ifelse(data$Z == 1, 
                 dplyr::case_when(data$S == 1 ~ 0,
                                  data$S == 2 ~ 0,
                                  data$S == 3 ~ 1,
                                  data$S == 4 ~ 0,
                                  data$S == 5 ~ 1),
                 dplyr::case_when(data$S == 1 ~ 0,
                                  data$S == 2 ~ 0,
                                  data$S == 3 ~ 0,
                                  data$S == 4 ~ 0,
                                  data$S == 5 ~ 1))

data$D2 <- ifelse(data$Z == 1, 
                  dplyr::case_when(data$S == 1 ~ 0,
                                   data$S == 2 ~ 1,
                                   data$S == 3 ~ 1,
                                   data$S == 4 ~ 1,
                                   data$S == 5 ~ 1),
                  dplyr::case_when(data$S == 1 ~ 0,
                                   data$S == 2 ~ 0,
                                   data$S == 3 ~ 0,
                                   data$S == 4 ~ 1,
                                   data$S == 5 ~ 1))

data$Y <- dplyr::case_when(data$S == 1 ~ rnorm(n, 3, 1),
                           data$S == 2 ~ rnorm(n, -1 - data$Z, 0.5),
                           data$S == 3 ~ rnorm(n, 1 + 3 * data$Z, 0.5),
                           data$S == 4 ~ rnorm(n, -1, 3),
                           data$S == 5 ~ rnorm(n, 1, 2)
                           )

write.csv(data, "test/no_covariates/data4.csv", row.names = F)
write.csv(data, "data4.csv", row.names = F)

Y.formula <- Y ~ 1
S.formula <- Z + D1 + D2 ~ 1
data <- read.csv("test/no_covariates/data4.csv")
stan_data <- get.stan.data(S.formula = S.formula, Y.formula = Y.formula, data = data)

test_p2s5c0 <- rstan::stan(file = "test/new/1_test_p2s5c0.stan", data = stan_data,
                           iter = 1000, chains = 6, cores = 6)

test_p2s5c0

exp(c(0, 0.24, -0.38, 0.93, -0.11))/sum(exp(c(0, 0.24, -0.38, 0.93, -0.11)))

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
