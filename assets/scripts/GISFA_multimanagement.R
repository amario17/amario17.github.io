#------------------------------------------------------------------------------#
############################## GIS FA Wheat ####################################
#------------------------------------------------------------------------------#
#
# João Marcos Amario de Sousa
#
# Sousa JMA, Blasques GM, Condé AT, Silva AF, Eides JR, Fronza RTL,
# Fernandes SB, Carneiro PCS, Bhering LL, Nardino M, Dias KOG (2026).
# Modeling genotype-by-environment interaction for variety recommendation in
# tropical wheat. Crop Science 66:e70309. https://doi.org/10.1002/csc2.70309
#
# What you need
#   1. pheno.txt         : one row per plot -> Env, Harvest (irr / seq),
#                          Genotype, Block, GY
#   2. covamb_exp.csv    : environmental covariates (ECs) of the TESTED
#                          environments, one row per environment, in the order
#                          E1, E2, ..., with LAT and LON in the first columns
#   3. matcovamb_<harvest>.csv : the SAME ECs (same names, units and growing
#                          window) for every pixel of the target population of
#                          environments (TPE), with LAT and LON in the first
#                          columns, one file per management
#
# Steps
#   1. Factor analytic models (FA1 to FA5) with one intercept per management
#   2. Rotation and choice of the FA model (ASR)
#   3. GIS-FA: PLS of the rotated loadings on the ECs and prediction in the TPE
#------------------------------------------------------------------------------#

rm(list = ls())

library(asreml)
library(dplyr)
library(pls)

#------------------------------------------------------------------------------#
# fa.outs2: rotation of loadings and scores (SVD), eBLUPs, genetic
# (co)variance and correlation matrices and model diagnostics (ASVR)
#------------------------------------------------------------------------------#

fa.outs2 <- function(model, name.env, name.gen, name.harvest){

  require(asreml)

  # Data used in the model
  data <- as.data.frame(model$mf)
  num.env <- nlevels(data[, name.env])
  num.gen <- nlevels(data[, name.gen])

  # Variance components
  sum_mod <- summary(model)
  load <- sum_mod$varcomp[grep('fa\\d+', rownames(sum_mod$varcomp)), ]
  var  <- sum_mod$varcomp[grep('var', rownames(sum_mod$varcomp)), 1]

  ## Loadings' rotation via SVD
  load$fa <- regmatches(rownames(load), regexpr("fa\\d+", rownames(load)))
  mat.loadings <- do.call(cbind, lapply(split(load[, c('fa', 'component')], f = load$fa),
                                        function(x) x[,'component']))
  rownames(mat.loadings) <- levels(factor(data[, name.env]))

  svd.lambda <- svd(mat.loadings)
  D <- diag(svd.lambda$d^2, nrow = length(svd.lambda$d))
  if(sum(svd.lambda$u[,1] < 0) / nrow(svd.lambda$u) > 0.5){
    svd.lambda$u <- -1 * svd.lambda$u
    svd.lambda$v <- -1 * svd.lambda$v
  }
  mat.loadings.star <- svd.lambda$u
  colnames(mat.loadings.star) <- colnames(mat.loadings)
  rownames(mat.loadings.star) <- rownames(mat.loadings)

  ## Scores rotation via SVD
  scores <- data.frame(summary(model, coef = TRUE)$coef.random)
  scores <- scores[grep('Comp', rownames(scores)), ]
  scores$fa <- regmatches(rownames(scores), regexpr("_Comp\\d+", rownames(scores)))
  scor.vec <- do.call(c, lapply(split(scores, f = scores$fa), function(x) x[,'solution']))
  scor.vec.star <- kronecker(sqrt(D) %*% t(svd.lambda$v), diag(num.gen)) %*% scor.vec
  scor.mat.star <- matrix(scor.vec.star, nrow = num.gen, ncol = length(unique(scores$fa)),
                          dimnames = list(levels(data[, name.gen]), unique(load$fa)))

  ## eBLUPs
  class_str <- paste(name.harvest, name.gen, name.env, sep = ":")
  modpred <- predict(model, classify = class_str, sed = TRUE)
  blups <- modpred$pvals
  blups <- na.omit(blups)

  temp <- data.frame(
    temp_harvest = rep(unique(data[, name.harvest]), each = num.env * num.gen),
    temp_env     = rep(rep(levels(data[, name.env]), each = num.gen), times = length(unique(data[, name.harvest]))),
    temp_gen     = rep(levels(data[, name.gen]), times = num.env * length(unique(data[, name.harvest]))),
    marginal     = as.vector(kronecker(mat.loadings.star, diag(num.gen)) %*% scor.vec.star)
  )
  colnames(temp)[1:3] <- c(name.harvest, name.env, name.gen)
  blups <- merge(blups, temp, by = c(name.harvest, name.env, name.gen))
  colnames(blups)[which(colnames(blups) == 'predicted.value')] <- 'conditional'

  ## Genetic (co)variance and correlation matrices
  Gvcov <- mat.loadings.star %*% tcrossprod(D, mat.loadings.star) + diag(var)
  Gcor  <- cov2cor(Gvcov)

  ## Proportion of the genetic variance explained by the factors
  expvar <- sum(diag(mat.loadings.star %*% tcrossprod(D, mat.loadings.star))) / sum(diag(Gvcov))

  ## Average semivariance ratio (ASVR)
  lambdacross <- mat.loadings.star %*% tcrossprod(D, mat.loadings.star)
  nE <- nrow(lambdacross)
  rowi <- row(lambdacross)
  coli <- col(lambdacross)
  upper <- rowi < coli
  lambda_ASV <- 2/(nE * (nE - 1)) * sum(0.5 * (diag(lambdacross)[rowi[upper]] + diag(lambdacross)[coli[upper]]) - lambdacross[upper])

  nE2 <- nrow(Gvcov)
  rowi2 <- row(Gvcov)
  coli2 <- col(Gvcov)
  upper2 <- rowi2 < coli2
  G_ASV <- 2/(nE2 * (nE2 - 1)) * sum(0.5 * (diag(Gvcov)[rowi2[upper2]] + diag(Gvcov)[coli2[upper2]]) - Gvcov[upper2])
  ASVR <- lambda_ASV / G_ASV

  ## Diagnostics
  diagnostics <- c(
    expvar = round(expvar * 100, 3),
    ASVR   = round(ASVR * 100, 3),
    aic    = round(sum_mod$aic, 3),
    bic    = round(sum_mod$bic, 3),
    logl   = round(sum_mod$loglik, 3)
  )

  results <- list(
    'rot.loads'   = mat.loadings.star,
    'rot.scores'  = scor.mat.star,
    'blups'       = blups,
    'Gvcov'       = Gvcov,
    'Gcor'        = Gcor,
    'diagnostics' = diagnostics
  )

  return(results)
}


#------------------------------------------------------------------------------#
#                                MET analysis
#------------------------------------------------------------------------------#

# Data
data <- read.table("pheno.txt",
                   na.strings = "NA", header = TRUE, sep = "\t")

data <- transform(data,
                  gen = factor(Genotype),
                  rep = factor(Block),
                  env = factor(Env),
                  Harvest = factor(Harvest))
str(data)

num.env = nlevels(data$env)
num.gen = nlevels(data$gen)
name.env = levels(data$env)
name.gen = levels(data$gen)

# Factor analytic - FA
#   y = mu_m + X beta + Z g + e
#   mu_m: one intercept per management (Harvest: irrigated / rainfed)
#   g = (Lambda (x) I) f + delta,  e ~ N(0, (+) s2e_j I)

asreml.options(maxit = 100, workspace = '4gb', pworkspace = '2gb')

Mod1 <- asreml(fixed = GY ~ - 1 + Harvest + rep:env + env,
               random = ~ gen:fa(env, 1),
               residual = ~ dsum(~id(units) | env),
               na.action = na.method(x = "include", y = "include"), maxit = 100,
               data = data)

Mod2 <- asreml(fixed = GY ~ - 1 + Harvest + rep:env + env,
               random = ~ gen:fa(env, 2),
               residual = ~ dsum(~id(units) | env),
               na.action = na.method(x = "include", y = "include"), maxit = 100,
               data = data)

Mod3 <- asreml(fixed = GY ~ - 1 + Harvest + rep:env + env,
               random = ~ gen:fa(env, 3),
               residual = ~ dsum(~id(units) | env),
               na.action = na.method(x = "include", y = "include"), maxit = 100,
               data = data)

Mod4 <- asreml(fixed = GY ~ - 1 + Harvest + rep:env + env,
               random = ~ gen:fa(env, 4),
               residual = ~ dsum(~id(units) | env),
               na.action = na.method(x = "include", y = "include"), maxit = 100,
               data = data)

Mod5 <- asreml(fixed = GY ~ - 1 + Harvest + rep:env + env,
               random = ~ gen:fa(env, 5),
               residual = ~ dsum(~id(units) | env),
               na.action = na.method(x = "include", y = "include"), maxit = 100,
               data = data)

# Rotation and diagnostics of each model
fa.models <- list(Mod1, Mod2, Mod3, Mod4, Mod5)
fa.res <- lapply(fa.models, function(x) fa.outs2(x, name.env = "env",
                                                 name.gen = "gen",
                                                 name.harvest = "Harvest"))
diagnos <- do.call(rbind, lapply(fa.res, function(x) x$diagnostics))
rownames(diagnos) <- c("mod1", "mod2", "mod3", "mod4", "mod5")
diagnos

# Best model
# CHOOSE the model: the first one whose ASVR reaches 75% (the factors capture
# at least 75% of the genetic (co)variance between environments). In the
# paper this was FA4 (mod4); change the index below for your data.

fa4 = fa.res[[4]]
save(fa4, file = "FA4.RDA")
rm(Mod1, Mod2, Mod3, Mod4, Mod5, fa.models, fa.res); gc()

head(fa4$rot.loads)
head(fa4$rot.scores)
fa4$Gcor[1:5, 1:5]


#------------------------------------------------------------------------------#
#                   Prediction with PLS for Harvest
#------------------------------------------------------------------------------#
# Lambda* = W B + E            (PLS: ECs of the tested environments -> loadings)
# Lambda*_U = Omega B          (loadings of every pixel of the TPE)
# g_U = (Lambda*_U (x) I) f*   (eBLUP of every genotype in every pixel)
# Run once for each management.

harvest <- "seq"                                   # CHOOSE the management: "seq" (rainfed) or "irr" (irrigated)

# Arquivo Data
load('FA4.RDA')
matcovamb <- read.table("covamb_exp.csv", header = T, sep = ",")
rownames(matcovamb) <- paste0("E", seq_len(nrow(matcovamb)))
matcovamb <- matcovamb[,-c(1,2)]                   # drop LAT and LON

amb_seq <- unique(subset(data, Harvest == harvest)$env)

fa4_seq <- fa4$rot.loads[amb_seq, , drop = FALSE]

matcovamb_seq <- matcovamb[amb_seq, , drop = FALSE]

data.pls.lamb_all <- data.frame(
  lambda = I(fa4_seq),
  CovAmb = I(scale(as.matrix(matcovamb_seq))))

# CHOOSE the number of PLS components (ncomp). In the paper it was 7, the
# number with the highest mean correlation between predicted and observed
# eBLUPs in leave-one-environment-out cross-validation.
pls2.lamb = pls::plsr(lambda ~ CovAmb, validation = 'none', data = data.pls.lamb_all,
                      ncomp = 7)
pls::explvar(pls2.lamb)

pls_model_all <- plsr(lambda ~ CovAmb,
                      validation = "none",
                      data = data.pls.lamb_all,
                      ncomp = 7)
coef_pls_all  <- coef(pls_model_all, intercept = TRUE)[,,1]

# ECs of every pixel of the TPE (same columns as covamb_exp.csv)
mat_full <- read.csv(paste0("matcovamb_", harvest, ".csv"), header = T)
mat_full <- mat_full[,-c(1:2)]                     # drop LAT and LON

# one line per factor of the chosen model (here fa1 to fa4)
pred_lambda = scale(mat_full) %*% coef_pls_all[-1, ]
pred_lambda = cbind(
  fa1 = pred_lambda[,'fa1'] + coef_pls_all["(Intercept)", "fa1"],
  fa2 = pred_lambda[,'fa2'] + coef_pls_all["(Intercept)", "fa2"],
  fa3 = pred_lambda[,'fa3'] + coef_pls_all["(Intercept)", "fa3"],
  fa4 = pred_lambda[,'fa4'] + coef_pls_all["(Intercept)", "fa4"])

EBLUPs_pred = as.matrix(pred_lambda[,'fa1']) %*% fa4$rot.scores[,'fa1'] +
  as.matrix(pred_lambda[,'fa2']) %*% fa4$rot.scores[,'fa2'] +
  as.matrix(pred_lambda[,'fa3']) %*% fa4$rot.scores[,'fa3'] +
  as.matrix(pred_lambda[,'fa4']) %*% fa4$rot.scores[,'fa4'] +
  mean(data$GY, na.rm = T)
colnames(EBLUPs_pred) = levels(data$gen)

# eBLUPs of each genotype in each pixel, with the pixel coordinates
covamb_pred <- read.csv(paste0("matcovamb_", harvest, ".csv"), header = T)
covamb_pred$index <- as.character(seq_len(nrow(covamb_pred)))
pred.list = lapply(apply(EBLUPs_pred, 2, function(x) reshape2::melt(x)),
                   function(x) x |>
                     dplyr::rename(blup = value) |>
                     tibble::rownames_to_column('point') |>
                     dplyr::left_join(
                       unique(covamb_pred[,c('index', 'LON', 'LAT')]),
                       by = c('point' = 'index')))
save(pred.list, file = paste0("eblups_", harvest, ".RData"))
