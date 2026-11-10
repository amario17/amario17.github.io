#==============================================================================#
#   Multi-environment analysis of segregating populations across generations   #
#         Genetic (G) and residual (R) (co)variance structures in ASReml-R     #
#==============================================================================#
#
# Sousa JMA, Vieira EFT, Fronza RTL, Vieira Netto JAF, Lima GW, Dias KOG,
# Nardino M (2025). Statistical genetics models with residual and genetic
# structures enhance the accuracy of selecting wheat populations.
# Euphytica 221:55. https://doi.org/10.1007/s10681-025-03500-z
#
# 25 tropical wheat populations (5 x 5 half diallel) and their parents were
# evaluated in F2, F3 and F4, in rainfed and irrigated trials at Viçosa and
# São Gotardo (MG, Brazil). An environment is a combination of generation,
# location and water regime (6 environments).
#
# The same population is evaluated in different generations. The lines inside
# a population are not true replicates across generations, but they share the
# population's genetic background, so the population effect in each generation
# is correlated. The joint models below differ only in how this covariance
# between generations (matrix G) and the residual variance (matrix R) are
# modeled:
#
#   Model  R     G     (co)variance of the population effects across generations
#   M1     ID    ID    one genetic variance, no covariance structure
#   M2     ID    DIAG  one genetic variance per generation, zero covariance
#   M3     ID    CS    common variance and common covariance (compound symmetry)
#   M4     ID    CSH   variance per generation, common correlation
#   M5     ID    UN    variance per generation, one correlation per pair
#   M6     ID    FA1   factor analytic of order 1
#   M7     DIAG  CSH   M4 + one residual variance per generation  (selected)
#
# Data columns: Year, Location, Environment, Generation, Genotype, Block, GY
# (kg/ha, decimal comma). Populations are named "PARENT A X PARENT B".
#==============================================================================#

rm(list = ls())

library(asreml)
library(dplyr)

data <- read.table("my_data.txt", header = TRUE, sep = "\t", dec = ",", na.strings = "")

data <- transform(data,
                  GY  = GY / 1000,                                 # t/ha
                  G   = factor(Genotype),
                  Ger = factor(Generation),
                  L   = factor(Location),
                  E   = factor(Environment),
                  B   = factor(Block))
data <- data %>% arrange(Ger, E)
data$type <- ifelse(grepl(" X ", data$Genotype), "population", "cultivar")

table(data$E, data$Ger)                                            # environments x generations
tapply(data$GY, data$E, mean, na.rm = TRUE)


#==============================================================================#
#  1. SINGLE-ENVIRONMENT ANALYSES
#==============================================================================#
# y = mu + b + g + e,  g ~ N(0, s2g I),  e ~ N(0, s2e I)
# LRT for the genotype effect:  LRT = 2 [ ln L(full) - ln L(reduced) ]
# Broad-sense heritability:     H2 = s2g / (s2g + s2e)
# Selective accuracy:           r  = sqrt(1 - PEV / s2g)

single <- NULL
for (e in levels(data$E)) {

  de <- droplevels(subset(data, E == e))

  full <- asreml(fixed = GY ~ B, random = ~ G,
                 na.action = na.method(y = "include", x = "include"),
                 trace = FALSE, data = de)
  red  <- asreml(fixed = GY ~ B,
                 na.action = na.method(y = "include", x = "include"),
                 trace = FALSE, data = de)

  vc  <- summary(full)$varcomp
  s2g <- vc["G", "component"]
  s2e <- vc["units!R", "component"]
  lrt <- 2 * (full$loglik - red$loglik)
  pev <- mean(predict(full, classify = "G", trace = FALSE)$pvals$std.error^2)

  single <- rbind(single, data.frame(
    env = e, generation = unique(de$Ger), location = unique(de$L),
    s2g = s2g, s2e = s2e,
    LRT = lrt, p = 0.5 * pchisq(lrt, df = 1, lower.tail = FALSE),   # boundary test (mixture of chi2)
    H2  = s2g / (s2g + s2e),
    r   = sqrt(max(0, 1 - pev / s2g)),
    mean_GY = mean(de$GY, na.rm = TRUE)))
}
print(single, digits = 3)


#==============================================================================#
#  2. JOINT MODELS: STRUCTURES FOR THE GENERATION EFFECT
#==============================================================================#
# y = mu + X1 r + X2 b + Z g + e
#   r: environment (location) and generation effects (fixed)
#   b: block within location (random)
#   g: population effect in each generation, g ~ N(0, Sigma_g)
#   G x L x generation interaction (random, identity)
#   e ~ N(0, R)

# M1 - ID: Sigma_g = s2g I + s2gg I  (main genotype effect + genotype within generation)
M1 <- asreml(fixed = GY ~ L + Ger,
             random = ~ G + Ger:G + G:Ger:L + B:L,
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

# M2 - DIAG: Sigma_g = diag(s2g_F2, s2g_F3, s2g_F4) (x) I
#   each generation has its own genetic variance; no covariance between them
M2 <- asreml(fixed = GY ~ L + Ger,
             random = ~ at(Ger):G + G:Ger:L + B:L,
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

# M3 - CS: Sigma_g = [ s2g I_a + s2c J_a ] (x) I
#   same variance in all generations and the same covariance between any pair
M3 <- asreml(fixed = GY ~ L + Ger,
             random = ~ corv(Ger):G + G:Ger:L + B:L,
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

# M4 - CSH: Sigma_g = D [ rho J_a + (1 - rho) I_a ] D (x) I,  D = diag(s_F2, s_F3, s_F4)
#   one variance per generation and a single genetic correlation rho between generations
M4 <- asreml(fixed = GY ~ L + Ger,
             random = ~ corh(Ger):G + G:Ger:L + B:L,
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

# M5 - UN: Sigma_g = Sigma (x) I,  Sigma fully unstructured (a x a)
#   one variance per generation and one correlation for each pair of generations
M5 <- asreml(fixed = GY ~ L + Ger,
             random = ~ corgh(Ger):G + G:Ger:L + B:L,
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

# M6 - FA1: Sigma_g = [ Lambda Lambda' + Psi ] (x) I
#   one latent factor captures the covariance between generations; Psi = specific variances
M6 <- asreml(fixed = GY ~ L + Ger,
             random = ~ fa(Ger, 1):G + G:Ger:L + B:L,
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

# M7 - DIAG (R) + CSH (G): as M4, with one residual variance per generation
#   R = (+)_j s2e_j I_nj   (direct sum over generations)
M7 <- asreml(fixed = GY ~ L + Ger,
             random = ~ corh(Ger):G + G:Ger:L + B:L,
             residual = ~ dsum(~ units | Ger),
             na.action = na.method(x = "include", y = "include"),
             maxit = 100, trace = FALSE, data = data)

modelos <- list(M1 = M1, M2 = M2, M3 = M3, M4 = M4, M5 = M5, M6 = M6, M7 = M7)
for (m in names(modelos)) for (k in 1:5) {                         # update until convergence
  if (modelos[[m]]$converge) break
  modelos[[m]] <- update(modelos[[m]])
}


#==============================================================================#
#  3. MODEL COMPARISON (same fixed part: AIC and BIC from REML)
#==============================================================================#

comparison <- NULL
for (m in names(modelos)) {
  s  <- summary(modelos[[m]])
  vc <- s$varcomp
  comparison <- rbind(comparison, data.frame(
    model    = m,
    R        = c("ID", "ID", "ID", "ID", "ID", "ID", "DIAG")[match(m, names(modelos))],
    G        = c("ID", "DIAG", "CS", "CSH", "UN", "FA1", "CSH")[match(m, names(modelos))],
    logL     = modelos[[m]]$loglik,
    npar     = sum(vc$bound != "F"),
    AIC      = s$aic,
    BIC      = s$bic,
    converge = modelos[[m]]$converge))
}
comparison <- comparison[order(comparison$AIC), ]
print(comparison, digits = 4, row.names = FALSE)

best <- comparison$model[1]
fit  <- modelos[[best]]
cat("\nSelected model:", best, "\n")
print(summary(fit)$varcomp)


#==============================================================================#
#  4. POPULATION SELECTION WITH THE SELECTED MODEL
#==============================================================================#

# BLUPs of each genotype across generations and in each generation
blup_overall <- predict(fit, classify = "G", trace = FALSE)$pvals
blup_ger     <- predict(fit, classify = "G:Ger", trace = FALSE)$pvals

blup_overall$type <- ifelse(grepl(" X ", blup_overall$G), "population", "cultivar")
blup_overall <- blup_overall[order(blup_overall$predicted.value, decreasing = TRUE), ]
print(blup_overall[, c("G", "type", "predicted.value", "std.error")], digits = 3, row.names = FALSE)

# Accuracy of the population BLUPs (mean over genotypes): r = sqrt(1 - PEV / s2g)
vc  <- summary(fit)$varcomp
s2g <- mean(vc[grepl("Ger:G!Ger_.*:F[0-9]$|Ger:G!Ger_F[0-9]$", rownames(vc)), "component"])
pev <- mean(blup_overall$std.error^2)
cat("Mean genetic variance across generations:", round(s2g, 3), "| accuracy r:", round(sqrt(max(0, 1 - pev / s2g)), 3), "\n")

# Selection gain (%) when selecting the top 10 populations
pops <- subset(blup_overall, type == "population")
gain <- 100 * (mean(head(pops$predicted.value, 10)) - mean(pops$predicted.value)) / mean(pops$predicted.value)
cat("Selection gain (top 10 populations):", round(gain, 2), "%\n")

write.csv(single,       "single_environment.csv", row.names = FALSE)
write.csv(comparison,   "model_comparison.csv",   row.names = FALSE)
write.csv(blup_overall, "blups_overall.csv",      row.names = FALSE)
write.csv(blup_ger,     "blups_by_generation.csv", row.names = FALSE)
