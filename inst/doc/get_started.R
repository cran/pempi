## ---- echo=FALSE--------------------------------------------------------------
library(pempi)

## -----------------------------------------------------------------------------
# Load pempi
library(pempi)

# Austrian data (November 2020)
pi0 = 93914/7166167

# Load data
data("covid19_austria")

# Random sampling
n = nrow(covid19_austria)
R1 = sum(covid19_austria$Y == 1 & covid19_austria$Z == 1)
R2 = sum(covid19_austria$Y == 0 & covid19_austria$Z == 1)
R3 = sum(covid19_austria$Y == 1 & covid19_austria$Z == 0)
R4 = sum(covid19_austria$Y == 0 & covid19_austria$Z == 0)

# Compute CMLE
conditional_mle(R1 = R1, R2 = R2, R3 = R3, R4 = R4, pi0 = pi0)

## -----------------------------------------------------------------------------
# Assumed measurement errors
alpha0 = 0
alpha  = 1/100
beta   = 10/100

# Compute CMLE with measurement error
conditional_mle(R1 = R1, R2 = R2, R3 = R3, R4 = R4, pi0 = pi0,
                alpha = alpha, alpha0 = alpha0, beta = beta)

## -----------------------------------------------------------------------------
# Without measurement error
moment_estimator(R3 = R3, n = n, pi0 = pi0)

# With measurement error
moment_estimator(R3 = R3, n = n, pi0 = pi0, alpha = alpha,
                 alpha0 = alpha0, beta = beta)

## -----------------------------------------------------------------------------
# Without measurement error
marginal_mle(R1 = R1, R3 = R3, n = n, pi0 = pi0)

# With measurement error
marginal_mle(R1 = R1, R3 = R3, n = n, pi0 = pi0, 
             alpha = alpha, beta = beta, alpha0 = alpha0)

## -----------------------------------------------------------------------------
# Without measurement error
survey_mle(R = R1 + R3, n = n)

# With measurement error
survey_mle(R = R1 + R3, n = n, alpha = alpha, beta = beta)

## -----------------------------------------------------------------------------
# Load pempi
library(pempi)

# Austrian data (November 2020)
pi0 = 93914/7166167

# Weighted sampling
R1w = sum(covid19_austria$weights[covid19_austria$Y == 1 & covid19_austria$Z == 1])
R2w = sum(covid19_austria$weights[covid19_austria$Y == 0 & covid19_austria$Z == 1])
R3w = sum(covid19_austria$weights[covid19_austria$Y == 1 & covid19_austria$Z == 0])
R4w = sum(covid19_austria$weights[covid19_austria$Y == 0 & covid19_austria$Z == 0])

# Average of squared weights 
V = mean(covid19_austria$weights^2)

# Compute CMLE
conditional_mle(R1 = R1w, R2 = R2w, R3 = R3w, R4 = R4w, 
                pi0 = pi0, V = V)

# Compute MME
moment_estimator(R3 = R3w, pi0 = pi0, n = n, V = V)

# Survey MLE
survey_mle(R = R1w + R3w, pi0 = pi0, n = n, V = V)

## -----------------------------------------------------------------------------
# Compute MME
moment_estimator(R3 = R3w, pi0 = pi0, n = n, V = V, alpha = alpha,
                 alpha0 = alpha0, beta = beta)

