## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

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

# Weighted sampling
R1w = sum(covid19_austria$weights[covid19_austria$Y == 1 & covid19_austria$Z == 1])
R2w = sum(covid19_austria$weights[covid19_austria$Y == 0 & covid19_austria$Z == 1])
R3w = sum(covid19_austria$weights[covid19_austria$Y == 1 & covid19_austria$Z == 0])
R4w = sum(covid19_austria$weights[covid19_austria$Y == 0 & covid19_austria$Z == 0])

# Print table
data_mat = matrix(c(R1w, R2w, R3w, R4w, R1, R2, R3, R4), 2, 4, byrow = TRUE)
rownames(data_mat) = c("Weighted sampling", "Unweighted sampling")
colnames(data_mat) = c("R1 (R11)", "R2 (R10)", "R3 (R01)", "R4 (R00)")
knitr::kable(round(data_mat, 4))

## -----------------------------------------------------------------------------
R1 + R2 + R3 + R4

## -----------------------------------------------------------------------------
R1w + R2w + R3w + R4w

## -----------------------------------------------------------------------------
# Measurement error
alpha0 = 0
alpha = 1/100
beta = 10/100

## -----------------------------------------------------------------------------
# Survey MLE without measurement error
(smle_no_meas_error_random = survey_mle(R = R1 + R3, n = n))

# Survey MLE with measurement error (as defined above)
(smle_with_meas_error_random = survey_mle(R = R1 + R3, n = n, 
                              alpha = alpha, beta = beta))

## -----------------------------------------------------------------------------
# Survey (weighted) MLE without measurement error
(smle_no_meas_error_strat = survey_mle(R = R1w + R3w, n = n, 
                            V = mean(covid19_austria$weights^2)))

# Survey MLE with measurement error (as defined above)
(smle_with_meas_error_strat = survey_mle(R = R1w + R3w, n = n,
                             alpha = alpha, beta = beta, 
                             V = mean(covid19_austria$weights^2)))

## -----------------------------------------------------------------------------
# MME without measurement error
(mme_no_meas_error_random = moment_estimator(R3 = R3, n = n, 
                            pi0 = pi0))

# MME with measurement error (as defined above)
(mme_with_meas_error_random = moment_estimator(R3 = R3, n = n,
                              pi0 = pi0, alpha = alpha, 
                              beta = beta, alpha0 = alpha0))

## -----------------------------------------------------------------------------
# MME without measurement error
(mme_no_meas_error_strat = moment_estimator(R3 = R3w, n = n,
                          pi0 = pi0,
                          V = mean(covid19_austria$weights^2)))

# MME with measurement error (as defined above)
(mme_with_meas_error_strat = moment_estimator(R3 = R3w, n = n,
                             pi0 = pi0, alpha = alpha, beta = beta,
                             alpha0 = alpha0, 
                             V = mean(covid19_austria$weights^2)))

## -----------------------------------------------------------------------------
# CMLE without measurement error
(cmle_no_meas_error_random = conditional_mle(R1 = R1, R2 = R2, 
                            R3 = R3, R4 = R4, pi0 = pi0))

# CMLE with measurement error (as defined above)
(cmle_with_meas_error_random = conditional_mle(R1 = R1, R2 = R2, 
                              R3 = R3, R4 = R4, pi0 = pi0, 
                              alpha = alpha, beta = beta, 
                              alpha0 = alpha0))

## -----------------------------------------------------------------------------
# CMLE without measurement error
(cmle_no_meas_error_strat = conditional_mle(R1 = R1w, R2 = R2w, 
                            R3 = R3w, R4 = R4w, pi0 = pi0, 
                            V = mean(covid19_austria$weights^2)))

# CMLE with measurement error (as defined above)
(cmle_with_meas_error_strat = conditional_mle(R1 = R1w, R2 = R2w, 
                              R3 = R3w, R4 = R4w, n = n, pi0 = pi0,
                              alpha = alpha, beta = beta, 
                              alpha0 = alpha0, 
                              V = mean(covid19_austria$weights^2)))

## -----------------------------------------------------------------------------
# MMLE without measurement error
(mmle_no_meas_error_random = marginal_mle(R1 = R1, R3 = R3, n = n, pi0 = pi0))

# MMLE with measurement error (as defined above)
(mmle_with_meas_error_random  = marginal_mle(R1 = R1, R3 = R3, n = n, pi0 = pi0, alpha = alpha, beta = beta, alpha0 = alpha0))

## -----------------------------------------------------------------------------
table2 = matrix(NA, 6, 6)
rownames(table2) = c("SMLE (stratified)", "MME (stratified)", "Estimated beta0 (stratified)", "SMLE (random)", "MME (random)", "Estimated beta0 (random)")
colnames(table2) = c("Estimates (no meas. err.)", "95% CI (low)", "95% CI (high)", "Estimates (with meas. err.)", "95% CI (low)", "95% CI (high)")

table2[1, ] = 100*c(smle_no_meas_error_strat$estimate,
                smle_no_meas_error_strat$ci_asym,
                smle_with_meas_error_strat$estimate,
                smle_with_meas_error_strat$ci_asym) 

table2[2, ] = 100*c(mme_no_meas_error_strat$estimate,
                mme_no_meas_error_strat$ci_asym,
                mme_with_meas_error_strat$estimate,
                mme_with_meas_error_strat$ci_asym) 

table2[3, ] = 100*c(mme_no_meas_error_strat$beta0,
                mme_no_meas_error_strat$ci_beta0,
                mme_with_meas_error_strat$beta0,
                mme_with_meas_error_strat$ci_beta0)

table2[4, ] = 100*c(smle_no_meas_error_random$estimate,
                smle_no_meas_error_random$ci_asym,
                smle_with_meas_error_random$estimate,
                smle_with_meas_error_random$ci_asym)

table2[5, ] = 100*c(mme_no_meas_error_random$estimate,
                mme_no_meas_error_random$ci_asym,
                mme_with_meas_error_random$estimate,
                mme_with_meas_error_random$ci_asym) 
table2[6, ] = 100*c(mme_no_meas_error_random$beta0,
                mme_no_meas_error_random$ci_beta0,
                mme_with_meas_error_random$beta0,
                mme_with_meas_error_random$ci_beta0)

knitr::kable(round(table2, 3))

## ---- fig.align='center', fig.height=5, fig.width=6---------------------------
pi0 = 93914/7166167
cols = c("#F8766DFF", "#00BFC4FF")
delta = 0.1
cex_pt = 1.5
lwd_ci = 2
pch_mme = 16
pch_smle = 15

plot(NA, xlim = c(0.75, 4.25), ylim = c(1, 4), axes = FALSE, ann = FALSE)
grid()
box()
col_text = "grey60"
cex_text = 0.85
cex_text2 = 0.65
abline(v = c(1.5, 2.5, 3.5), col = col_text)

axis(2)

mtext("Stratified sampling", side = 3, line = 1.75, cex = cex_text, at = 1, col = col_text)
mtext("no measurment error", side = 3, line = 0.75, cex = cex_text2, at = 1, col = col_text)

mtext("Random sampling", side = 3, line = 1.75, cex = cex_text, at = 2, col = col_text)
mtext("no measurment error", side = 3, line = 0.75, cex = cex_text2, at = 2, col = col_text)

mtext("Stratified sampling", side = 3, line = 1.75, cex = cex_text, at = 3, col = col_text)
mtext("with measurment error", side = 3, line = 0.75, cex = cex_text2, at = 3, col = col_text)

mtext("Random sampling", side = 3, line = 1.75, cex = cex_text, at = 4, col = col_text)
mtext("with measurment error", side = 3, line = 0.75, cex = cex_text2, at = 4, col = col_text)

mtext("Prevalence (%)", side = 2, line = 3, cex = 1.15)
abline(h = 100*pi0, lwd = 2, lty = 2)

text(1, 1.18, expression(pi[0]), cex = 1.15)

legend("topright", c("MME", "95% CI",
                    "Survey MLE", "95% CI"),
       bty = "n", col = c(cols[1], cols[1], cols[2], cols[2]),
       lwd = c(NA, lwd_ci, NA, lwd_ci), pch = c(pch_mme, NA, pch_smle, NA),
       pt.cex = 1.5, cex = 0.7)

# 1) Stratified sampling, without ME
points(1 - delta, 100*mme_no_meas_error_strat$estimate, col = cols[1], pch = pch_mme, cex = cex_pt)
lines(c(1, 1) - delta, 100*mme_no_meas_error_strat$ci_asym, col = cols[1], lwd = lwd_ci)

points(1 + delta, 100*smle_no_meas_error_strat$estimate, col = cols[2], pch = pch_smle, cex = cex_pt)
lines(c(1, 1) + delta, 100*smle_no_meas_error_strat$ci_asym, col = cols[2], lwd = lwd_ci)

# 2) Random sampling, without ME
points(2 - delta, 100*mme_no_meas_error_random$estimate, col = cols[1], pch = pch_mme, cex = cex_pt)
lines(c(2, 2) - delta, 100*mme_no_meas_error_random$ci_asym, col = cols[1], lwd = lwd_ci)

points(2 + delta, 100*smle_no_meas_error_random$estimate, col = cols[2], pch = pch_smle, cex = cex_pt)
lines(c(2, 2) + delta, 100*smle_no_meas_error_random$ci_asym, col = cols[2], lwd = lwd_ci)

# 3) Stratified sampling, with ME
points(3 - delta, 100*mme_with_meas_error_strat$estimate, col = cols[1], pch = pch_mme, cex = cex_pt)
lines(c(3, 3) - delta, 100*mme_with_meas_error_strat$ci_asym, col = cols[1], lwd = lwd_ci)

points(3 + delta, 100*smle_with_meas_error_strat$estimate, col = cols[2], pch = pch_smle, cex = cex_pt)
lines(c(3, 3) + delta, 100*smle_with_meas_error_strat$ci_asym, col = cols[2], lwd = lwd_ci)

# 4) Random sampling, with ME
points(4 - delta, 100*mme_with_meas_error_random$estimate, col = cols[1], pch = pch_mme, cex = cex_pt)
lines(c(4, 4) - delta, 100*mme_with_meas_error_random$ci_asym, col = cols[1], lwd = lwd_ci)

points(4 + delta, 100*smle_with_meas_error_random$estimate, col = cols[2], pch = pch_smle, cex = cex_pt)
lines(c(4, 4) + delta, 100*smle_with_meas_error_random$ci_asym, col = cols[2], lwd = lwd_ci)

## ---- fig.align='center', fig.height=5, fig.width=6---------------------------
# Assumptions
pi0 = 93914/7166167
alpha = 1/100
alpha0 = 0
m = 300
beta = seq(from = 0, to = 30, length.out = m)/100
res_moment = res_smle = matrix(NA, m, 3)
V = mean(covid19_austria$weights^2)

for (i in 1:m){
  # Moment estimator
  inter = moment_estimator(R3 = R3w, n = n, pi0 = pi0,
                           alpha = alpha, alpha0 = alpha0,
                           beta = beta[i], V = V)

  res_moment[i,] = c(inter$estimate, inter$ci_asym)

  # Survey MLE
  inter = survey_mle(R = R1w + R3w, n = n, pi0 = pi0,
                     alpha = alpha, alpha0 = alpha0,
                     beta = beta[i], V = V)

  res_smle[i,] = c(inter$estimate, inter$ci_asym)
}

cols = c("#F8766DFF", "#00BFC4FF")
cols2 = c("#F8766D1F", "#00BFC41F")

plot(NA, xlim = 100*range(beta), ylim = c(1, 4.25), axes = FALSE, ann = FALSE)
grid()
box()
axis(1)
axis(2)
mtext(expression(paste(beta, " (%)")), side = 1, line = 3, cex = 1.15)
mtext("Prevalence (%)", side = 2, line = 3, cex = 1.15)
abline(h = 100*pi0, lwd = 2, lty = 2)
abline(h = 100*pi0, lwd = 2, lty = 2)

text(2.5, 1.18, expression(pi[0]), cex = 1.15)

legend("topleft", c("MME", "95% CI",
                    "Survey MLE", "95% CI"),
       bty = "n", col = c(cols[1], cols2[1],cols[2], cols2[2]),
       lwd = c(3, NA, 3, NA), pch = c(NA, 15, NA, 15),
       pt.cex = 2.5)
lines(100*beta, 100*res_moment[,1], lwd = 3, col = cols[1])
polygon(100*c(beta, rev(beta)),
        100*c(res_moment[,2], rev(res_moment[,3])),
        col = cols2[1], border = NA)

lines(100*beta, 100*res_smle[,1], lwd = 3, col = cols[2])
polygon(100*c(beta, rev(beta)),
        100*c(res_smle[,2], rev(res_smle[,3])),
        col = cols2[2], border = NA)

