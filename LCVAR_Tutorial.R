# jonashaslbeck@protonmail.com; Feb 3rd, 2024

# --------------------------------------------------------
# ---------- What is happening here? ---------------------
# --------------------------------------------------------

# This is the code to reproduce all analyses, results and
# figures as shown in the paper

# --------------------------------------------------------
# ---------- Load Packages -------------------------------
# --------------------------------------------------------

library(plyr)
library(ClusterVAR)
library(RColorBrewer)
library(xtable)
library(brms)

source("Helpers.R")

# --------------------------------------------------------
# ---------- Load Processed Data -------------------------
# --------------------------------------------------------

data <- readRDS("Files/Data_Grommisch2020_clean.RDS")
head(data)
dim(data)

# --------------------------------------------------------
# ---------- Get Overview --------------------------------
# --------------------------------------------------------

vars_em <- c("Happy", "Relaxed", "Sad", "Angry")

# ----- Number of persons -----
Nt_ptp <- ddply(data, .(SEMA_ID), function(x) nrow(x))
mean(Nt_ptp$V1)
# hist(Nt_ptp$V1)
u_pers <- Nt_ptp$SEMA_ID
n_pers <- length(u_pers)

# ----- Compute Between-person measures; DASS-21 (Depression, Anxiety, Stress) -----
# This is later used for external validation
DASS <- ddply(data, .(SEMA_ID), function(x) x[1, c("DASS_D_agg", "DASS_A_agg", "DASS_S_agg")])
head(DASS)
MEANS <- ddply(data, .(SEMA_ID), function(x) colMeans(x[, vars_em], na.rm=TRUE))


# --------------------------------------------------------
# ---------- Visualize Time Series -----------------------
# --------------------------------------------------------

# Draw three random subjects
set.seed(17)
rnd_sub <- u_pers[sample(1:n_pers, size=3)]

# ----- Figure for Paper -----
sc <- 0.95
pdf("Figures/Fig_DataViz_3Subj_Grommisch2020.pdf", width = 8*sc, height = 8*sc)

# Layout
lmat <- matrix(4:12, 3, 3, byrow = TRUE)
lmat <- cbind(1:3, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1))
# layout.show(lo)

# Plot Labels
for(j in 1:3)   plotLabel(paste0("     Person ", LETTERS[j]), srt = 90)

# Plot Data
for(j in 1:3) {
  PlotTS_Flex(data = data,
              IDcol = "SEMA_ID",
              ID = rnd_sub[j], # Subject number, here fixed
              variable = "Happy", # Variable
              layout = FALSE,
              title = FALSE,
              ylab = TRUE,
              xlim = c(1,200))

  plotBV_flex(data = data,
              IDcol = "SEMA_ID",
              ID = rnd_sub[j], # Subject number, here fixed
              variable1 = "Happy",
              variable2 = "Happy", # Variable
              lag = TRUE,
              title = FALSE,
              para = TRUE)
}

dev.off()


# ----- Get Time Series Plots for All Variables and Persons -----
# (This is not shown in the paper)

pdf("Figures/TSPlots_All_Persons_Grommisch2020.pdf", width=8, height=4)

n_pers <- length(u_pers)

# Loop over persons
for(j in 1:n_pers) {
  # Make Layout
  lmat <- matrix(1:8, 2, 4, byrow = TRUE)
  lo <- layout(lmat, widths = c(1, .25, 1, .25), heights = c(1,1))
  # Make plot for person j
  for(i in 1:4)   PlotTS_Flex(data = data,
                              IDcol = "SEMA_ID",
                              ID = u_pers[j], # Subject number, here fixed
                              variable = vars_em[i], # Variable
                              layout = FALSE,
                              title = TRUE,
                              ylab = TRUE,
                              xlim = c(1,200))
  print(j)
}

dev.off()



# --------------------------------------------------------
# ---------- Number Predictable Time Points --------------
# --------------------------------------------------------

# This allows us to check the number of predictable time points
# with different lag specified

numberPredictableObservations(Data = data,
                              yVars = 5:8,
                              xContinuous = 2,
                              Beep = 3,
                              Day = 4,
                              ID = 1,
                              Lags = 1)


# --------------------------------------------------------
# ---------- Fit LCVAR Sequence with linear trend --------
# --------------------------------------------------------

out_1to10 <- LCVAR(Data = data,
                   yVars = 5:8,
                   xContinuous = 2,
                   Beep = 3,
                   Day = 4,
                   ID = 1,
                   Lags = 1,
                   Clusters = 1:10,
                   Cores = 10,
                   Rand = 75,
                   RndSeed = 1) # Reproducibility

# saveRDS(out_1to10, "Files/Results_Gromisch2020_1to10.RDS")
out_1to10 <- readRDS("Files/Results_Gromisch2020_1to10.RDS")
# NOTE: here we load this instead of fitting the model


# --------------------------------------------------------
# ---------- LogLikelihood & BIC -------------------------
# --------------------------------------------------------

# ----- Get negLL / BIC -----
sum_out <- summary(out_1to10, show="GNL")
sum_out
sum_out$FunctionOutput$Proportions

# Convergence?
sum_out$FunctionOutput$Converged # all

# BIC plot
plot(out_1to10, show = "GNL")

# Get negLL
negLL <- -sum_out$FunctionOutput$`log-likelihood`
negLL_scaled <- negLL/max(negLL)
# Get BIC
BIC <- sum_out$FunctionOutput$BIC
BIC_scaled <- BIC/max(BIC)


sc <- 1
pdf("Figures/Results_Gromisch2020_LL_BIC.pdf", width=5*sc, height=3.5*sc)

par(mar=c(4,3.5,1,1))

## Canvas
K <- 10
plot.new()
ymax <- 1
ymin <- 0.965
plot.window(xlim=c(1,K), ylim=c(ymin, ymax))
axis(1, 1:K)
axis(2, las=2)
grid()
title(xlab="Number of Clusters K", line=2.5)

## Plot Data
shift <- 0.1
# Plot negLL
points((1:K)-shift, negLL_scaled, pch=16, cex=1.25)
lines((1:K)-shift, negLL_scaled, lwd=2)
# Plot BIC
points((1:K)+shift, BIC_scaled, col="tomato", pch=16, cex=1.25)
lines((1:K)+shift, BIC_scaled, col="tomato", lwd=2)

# Legend
legend("top", legend=c("Negative LL (scaled)", "BIC (scaled)"),
       text.col=c("black", "tomato"), bty="n", cex=1)

dev.off()


# --------------------------------------------------------
# ---------- Estimated Mixing Proportions -------
# --------------------------------------------------------

sum_out <- summary(out_1to10, show="GNL")
v_prop <- sum_out$FunctionOutput$Proportions

m_float <- matrix(NA, 10, 10)
for(i in 1:10) m_float[i,1:i] <- as.numeric(unlist(strsplit(v_prop[i], " ")))
rownames(m_float) <- paste0(1:10, " Clusters")
colnames(m_float) <- paste0("C ", 1:10)

# Use xtable to get LaTeX table
xtable(m_float)


# --------------------------------------------------------
# ---------- Parameter Estimates -------------------------
# --------------------------------------------------------

# Get coefficient objects for all K
l_coefs <- list()
K <- 10
for(k in 1:K) l_coefs[[k]] <- coef.ClusterVAR(out_1to10, Model = rep(1, k))

# Get proportions for K=4 model for plotting below
K4_prop <- m_float[4, 1:4]

# --------------------------
# ----- Means / Trends -----
# --------------------------

k <- 4
m_ints <- l_coefs[[k]]$Exogenous_coefficients[, 1, ] # Note: since trend is included, those are means at t=0
m_ltr <- l_coefs[[k]]$Exogenous_coefficients[, 2, ] # Linear trends

# Colors
cols_k4 <- RColorBrewer::brewer.pal(k, "Set2")

sc <- 1
pdf("Figures/Results_Gromisch2020_Fig_Means_and_LT_k4.pdf", width=8*sc, height=4*sc)

# ----- Layout
par(mar=c(3,3,2,1))
par(mfrow=c(1,2))

# ----- Means at t=0 -----
plot.new()
plot.window(xlim=c(1,4), ylim=c(0,70))
axis(1, labels=vars_em, at=1:4)
axis(2, las=2)
grid()
# title(ylab="Estimated Mean")
title(main=expression("Estimated Mean at " ~ X[t] == 0), font.main=1)
for(i in 1:4) {
  lines(m_ints[, i], lwd=3, col=cols_k4[i])
  points(m_ints[, i], pch=20, cex=2, col=cols_k4[i])
}
legend("topright", paste0("Cluster ", 1:k, " (", K4_prop*100, "%)"), text.col=cols_k4, bty="n")

# ----- Linear Trends -----
plot.new()
plot.window(xlim=c(1,4), ylim=c(-.04,.04))
axis(1, labels=vars_em, at=1:4)
axis(2, las=2)
grid()
abline(h=0, col="grey", lty=2)
# title(ylab="Estimated Mean")
title(main="Slope of Linear Trend", font.main=1)
for(i in 1:4) {
  lines(m_ltr[, i], lwd=3, col=cols_k4[i])
  points(m_ltr[, i], pch=20, cex=2, col=cols_k4[i])
}

dev.off()


# --------------------------
# ----- Lag Eff Matrices ---
# --------------------------

k <- 4
sc <- 1
pdf(paste0("Figures/Results_Gromisch2020_LR_Fig_Estimates_Phi_K=", k, "_v2.pdf"), width=6*sc, height=6*sc)

plot(out_1to10, show = "specific",
     Model = rep(1,k),
     labels = vars_em,
     mar = c(2.5,3.75,2,0.5))

dev.off()


# --------------------------------------------------------
# ---------- External Validation -------------------------
# --------------------------------------------------------

# ----- Get classification -----
l_coefs <- list()
K <- 10
for(k in 1:K) l_coefs[[k]] <- coef.ClusterVAR(out_1to10, Model = rep(1, k))

k <- 4
cmemb <- as.numeric(l_coefs[[k]]$Classification)
table(cmemb) # distribution across clusters

# ----- Combine everything into 1 dataframe -----
data_ev <- cbind(DASS, MEANS, as.factor(cmemb))[, -5]
head(data_ev)
colnames(data_ev) <- c("SEMA_ID", "D", "A", "S", "Happy", "Relaxed", "Sad", "Angry", "Cluster")
head(data_ev)

# ----- Test whether DASS-scales are differet across clusters -----

# --- DASS: Depression ---
set.seed(1)
## Baseline
# Fit models
null_model_D <- brm(D ~ 1, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
alt_model_D <- brm(D ~ Cluster, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
# Get BF
bf_D <- bayes_factor(alt_model_D, null_model_D)
round(bf_D$bf)
## With Covariates
null_model_D_cov <- brm(D ~ 1 + Happy + Relaxed + Sad + Angry, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
alt_model_D_cov <- brm(D ~ Cluster + Happy + Relaxed + Sad + Angry, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
# Get BF
bf_D_cov <- bayes_factor(alt_model_D_cov, null_model_D_cov)
round(bf_D_cov$bf)


# --- DASS: Anxiety ---
set.seed(1)
## Baseline
# Fit models
null_model_A <- brm(A ~ 1, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
alt_model_A <- brm(A ~ Cluster, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
# Get BF
bf_A <- bayes_factor(alt_model_A, null_model_A)
round(bf_A$bf)
## With Covariates
null_model_A_cov <- brm(A ~ 1 + Happy + Relaxed + Sad + Angry, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
alt_model_A_cov <- brm(A ~ Cluster + Happy + Relaxed + Sad + Angry, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
# Get BF
bf_A_cov <- bayes_factor(alt_model_A_cov, null_model_A_cov)
round(bf_A_cov$bf)

# --- DASS: Stress ---
set.seed(1)
## Baseline
# Fit models
null_model_S <- brm(S ~ 1, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
alt_model_S <- brm(S ~ Cluster, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
# Get BF
bf_S <- bayes_factor(alt_model_S, null_model_S)
round(bf_S$bf)
## With Covariates
null_model_S_cov <- brm(S ~ 1 + Happy + Relaxed + Sad + Angry, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
alt_model_S_cov <- brm(S ~ Cluster + Happy + Relaxed + Sad + Angry, data = data_ev, family = gaussian(), chains = 4, iter = 2000)
# Get BF
bf_S_cov <- bayes_factor(alt_model_S_cov, null_model_S_cov)
round(bf_S_cov$bf)


# --------------------------------------------------------
# ---------- Residual Analysis ---------------------------
# --------------------------------------------------------

# -------- Compute Predictions --------

l_pred <- LCVARPred(object = out_1to10,
                    data = data,
                    k = 4)


# ---------- Compute R2 for each person and variable ----------

l_R2s <- lapply(l_pred$Predictions, function(x) {
  R2s <- c(cor(x$Happy_hat, x$Happy, use="complete.obs")^2,
           cor(x$Relaxed_hat, x$Relaxed, use="complete.obs")^2,
           cor(x$Sad_hat, x$Sad, use="complete.obs")^2,
           cor(x$Angry_hat, x$Angry, use="complete.obs")^2)
  return(R2s)
})
m_R2s <- do.call(rbind, l_R2s)
colnames(m_R2s) <- vars_em

# Plotting
df_R2 <- data.frame(values = as.vector(m_R2s),
                    Variables = rep(colnames(m_R2s), each = nrow(m_R2s)),
                    Clusters = rep(l_pred$Classification, times = ncol(m_R2s)))

mean(df_R2[df_R2$Clusters==3 & df_R2$Variables=="Happy",]$values)

# For plotting replace Variables with integers, to avoid sorting issue
df_R2_plot <- df_R2
df_R2_plot$Variables[df_R2_plot$Variables=="Happy"] <- 1
df_R2_plot$Variables[df_R2_plot$Variables=="Relaxed"] <- 2
df_R2_plot$Variables[df_R2_plot$Variables=="Sad"] <- 3
df_R2_plot$Variables[df_R2_plot$Variables=="Angry"] <- 4

pdf("Figures/Fig_ResAnalysis_R2.pdf", width = 7, height=5)

boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df_R2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k4, each=4), ylim=c(0, .7))
grid()
boxplot(values ~ Variables + Clusters,  # Specify groups and subgroups
        data = df_R2_plot, las=2, axes=FALSE, xlab="", ylab="", col=rep(cols_k4, each=4), ylim=c(0, .7), add=TRUE)
axis(1, 1:16, rep(vars_em, times=4), las=2)
axis(2, las=2)
title(ylab="Proportion of Explained Variance")
mp <- seq(1, 16, length=9)
mps <- mp[c(2, 4, 6, 8)]
for(k in 1:4) text(mps[k], 0.7, paste0("Cluster ", k), col=cols_k4[k])

dev.off()



# ---------- Plotting Residuals for 3 Time Series in Paper ----------

# Draw same three random subjects as above
set.seed(17)
ind_rnd_sub <- sample(1:n_pers, size=3)
rnd_sub <- u_pers[ind_rnd_sub]

sc <- 0.95
pdf("Figures/Fig_DataViz_3Subj_Grommisch2020_WithPreds.pdf", width=8*sc, height = 8*sc)

# Layout
lmat <- matrix(4:12, 3, 3, byrow = TRUE)
lmat <- cbind(1:3, lmat)
lo <- layout(lmat, widths = c(0.15, 1, .25, 1), heights = c(1,1,1))
# layout.show(lo)

# Plot Labels
for(j in 1:3)   plotLabel(paste0("     Person ", LETTERS[j], " (Cluster ", l_pred$Classification[ind_rnd_sub[j]], ")"), srt = 90)

# Plot Data
for(j in 1:3) {

  data_res_j <- l_pred$Predictions[[ind_rnd_sub[j]]]

  PlotTS_Flex(data = data_res_j,
              IDcol = "SEMA_ID",
              ID = rnd_sub[j], # Subject number, here fixed
              variable = "Happy", # Variable
              variable2 = "Happy_hat",
              layout = FALSE,
              title = FALSE,
              ylab = TRUE,
              xlim = c(1,200),
              trend=FALSE,
              resLegend = c(FALSE, TRUE, FALSE)[j])
  plotBV_flex(data=data_res_j,
              IDcol = "SEMA_ID",
              ID = rnd_sub[j], # Subject number, here fixed
              variable1 = "Happy_hat",
              variable2 = "Happy", # Variable
              lag=FALSE,
              title=FALSE,
              para=FALSE,
              fit= FALSE,
              diag = TRUE,
              R2=TRUE)
}

dev.off()




# --------------------------------------------------------
# ---------- Time Series Plots with Cluster Membership ---
# --------------------------------------------------------

# This is the same figure as above, but with the cluster assignment
# and a dataset in which people are ordered by cluster assignment

# cmemb
ord <- order(cmemb)
u_pers_ord <- u_pers[ord]
cmemb_ord <- cmemb[ord]

pdf("Figures/TSPlots_All_Persons_Grommisch2020_wCl.pdf", width=8, height=4)

n_pers <- length(u_pers)


# Loop over persons
for(j in 1:n_pers) {
  # Make Layout
  lmat <- matrix(1:8, 2, 4, byrow = TRUE)
  lo <- layout(lmat, widths = c(1, .25, 1, .25), heights = c(1,1))

  # Make title for person


  # Make plot for person j
  for(i in 1:4) {

    title <- paste0(vars_em[i], " (person = ", u_pers[j], "; Cluster =", cmemb_ord[j], ")")

    PlotTS_Flex(data = data,
                IDcol = "SEMA_ID",
                ID = u_pers_ord[j], # Subject number, here fixed
                variable = vars_em[i], # Variable
                layout = FALSE,
                title = title,
                ylab = TRUE,
                xlim = c(1,200))

  }
  print(j)
}

dev.off()

























