# jonashaslbeck@protonmail.com; Nov 12th, 2024

# --------------------------------------------------------
# ---------- What is happening here? ---------------------
# --------------------------------------------------------

# Helper functions for the ClusterVAR Tutorial


# --------------------------------------------------------
# ---------- Get # of Predictable Time Points per Subj ---
# --------------------------------------------------------

# But: I think this is already in the package by now ...

FPred <- function(day, beep, maxlag) {

  N <- length(day)
  check1 <- check2 <- check3 <- rep(FALSE, N)
  for(i in (maxlag+1):N) {
    # check1[i] <- (day[i-1] == day[i]) & ((beep[i] - beep[i-maxlag]) == maxlag)
    check1[i] <- (day[i-1] == day[i]) & ((beep[i] - beep[i-1]) == 1)
    if(maxlag > 1) check2[i] <- (day[i-1] == day[i]) & ((beep[i] - beep[i-2]) == 2)
    if(maxlag > 2) check3[i] <- (day[i-1] == day[i]) & ((beep[i] - beep[i-3]) == 3)
  }

  if(maxlag==1) return(sum(check1))
  if(maxlag==2) return(sum(check1 & check2))
  if(maxlag==3) return(sum(check1 & check2 & check3))

} # eoF


# ------------------------------------------
# -------- Plot Single Time Series + Marg | Make Flexible to work with any dataset --
# ------------------------------------------

PlotTS_Flex <- function(data,
                        IDcol, # column with subject ID
                        ID, # subject ID
                        variable, # variabe colname
                        variable2 = NULL,
                        layout = FALSE,
                        title = TRUE,
                        ylab = FALSE,
                        xlim = NULL,
                        trend = TRUE,
                        resLegend = FALSE) {

  # Subset data
  data_ss <- data[data[['SEMA_ID']] == ID, ]
  Nt_ss <- nrow(data_ss)
  if(is.null(xlim)) xlim <- c(1, Nt_ss)

  # Layout
  if(layout) layout(matrix(1:2, ncol=2), widths = c(1, .35))

  # LinePlot
  par(mar=c(4,3,2,1))
  plot.new()
  plot.window(xlim=xlim, ylim=c(0, 100))
  if(ylab) title(ylab=variable, line=2.25)
  axis(1)
  axis(2, las=2)
  grid()
  # Plot Data
  lines(data_ss[, variable])
  # abline(h=c(0,100))

  # Second variable (I will use that for predictions later)
  if(!is.null(variable2)) lines(data_ss[, variable2], col="orange")

  if(trend) {
    time <- 1:nrow(data_ss)
    lm_obj <- lm(data_ss[, variable]~time)
    abline(lm_obj, lwd=1, col="black", lty=2)
  }

  if(resLegend) legend("topright", legend=c("Data", "Predictions"), bty="n", text.col=c("black", "orange"))

  if(title) title(main=paste0(variable, " (person = ", u_pers[j], ")"), font.main=1)
  # Marginal
  par(mar=c(4,0,2,1))
  hist_data <- hist(data_ss[, variable], plot = FALSE, breaks=seq(0, 100, length=20))
  barplot(hist_data$counts,
          horiz = TRUE,  # Horizontal bars
          names.arg = NULL,
          axes=FALSE)
  x_seq <- seq(0, 100, length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(data_ss[, variable], na.rm = TRUE),
                     sd = sd(data_ss[, variable], na.rm = TRUE))
  scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
  lines(scaled_den, seq(0, 24, length=1000), col="grey") # Not entiresure where those 22 come from

} # eoF



# ------------------------------------------
# -------- Bivariate Plot | Make Flexible for any dataset ------------------
# ------------------------------------------

plotBV_flex <- function(data,
                        IDcol,
                        ID,
                        variable1,
                        variable2,
                        lag = FALSE,
                        title = TRUE,
                        fit = TRUE,
                        para = FALSE,
                        diag = FALSE,
                        xlim = NULL,
                        R2=FALSE) {

  # Subset data
  data_ss <- data[data[['SEMA_ID']] == ID, ]
  Nt_ss <- nrow(data_ss)
  if(is.null(xlim)) xlim <- c(1, Nt_ss)

  # Canvas
  par(mar=c(4,4,2,1))
  plot.new()
  plot.window(xlim=c(0, 100), ylim=c(0, 100))
  axis(1)
  axis(2, las=2)
  grid()
  if(diag) abline(0, 1, col="grey")
  if(lag) {
    title(xlab = bquote(.(variable1)[t-1]),
          ylab = bquote(.(variable2)[t]),
          line = 2.5)
  } else {
    title(xlab=variable1, ylab=variable2, line=2.5)
  }
  if(title) title(main=paste0("Person ", ID), font.main=1)

  # Plot Data
  if(lag) {
    x1 <- data_ss[-Nt_ss, variable1]
    x2 <- data_ss[-1, variable2]
  } else {
    x1 <- data_ss[, variable1]
    x2 <- data_ss[, variable2]
  }
  points(x1, x2, pch=20, cex=1.2)
  lm_obj <- lm(x2 ~ x1)
  if(fit) abline(lm_obj, lwd=2, col="orange")

  # Add regression results
  if(para) text(80, 7, paste0("a = ",
                              round(coef(lm_obj)[1], 2),
                              ", b = ",
                              round(coef(lm_obj)[2], 2)),
                col="orange")

  # ADD R2
  if(R2) {
    r2 <- cor(x1, x2, use="complete.obs")^2
    r2 <- round(r2, 2)
    text(20, 80, bquote(R^2 == .(r2)))

  }

} # eoF


# ------------------------------------------
# -------- Plot Single Time Series + Marg --
# ------------------------------------------

PlotTS <- function(data,
                   j, # subjects
                   i, # variables
                   layout=FALSE,
                   title=TRUE,
                   ylab=FALSE) {

  cols <- brewer.pal(3, "Set1")
  vars <- c("happy", "stressed", "anxious", "sad")

  # Subset data
  u_pers <- unique(data$subj_id)
  data_ss <- data[data$subj_id == u_pers[j], ]

  # Layout
  if(layout) layout(matrix(1:2, ncol=2), widths = c(1, .35))
  # LinePlot
  par(mar=c(4,3,2,1))
  plot.new()
  plot.window(xlim=c(1, 215), ylim=c(0, 105))
  if(ylab) title(ylab=vars[i], line=2.25)
  axis(1)
  axis(2, las=2)
  # plot Waves
  for(w in 1:3) {
    w_range <- range(which(data_ss$wave==w))
    rect(xleft=w_range[1],
         ybottom = 0,
         xright = w_range[2],
         ytop = 100,
         col=alpha(cols[w], alpha=0.45), border=FALSE)
    text(mean(w_range), 105, paste0("Wave ", w), col=cols[w])
  }
  # Plot Data
  lines(data_ss[, vars[i]])


  if(title) title(main=paste0(vars[i], " (person = ", u_pers[j], ")"), font.main=1)
  # Marginal
  par(mar=c(4,0,2,1))
  hist_data <- hist(data_ss[, vars[i]], plot = FALSE, breaks=seq(0, 100, length=20))
  barplot(hist_data$counts,
          horiz = TRUE,  # Horizontal bars
          names.arg = NULL,
          axes=FALSE)
  x_seq <- seq(0, 100, length=1000)
  gauss_den <- dnorm(x_seq,
                     mean = mean(data_ss[, vars[i]], na.rm = TRUE),
                     sd = sd(data_ss[, vars[i]], na.rm = TRUE))
  scaled_den <- (gauss_den/max(gauss_den) ) * max(hist_data$counts)
  lines(scaled_den, seq(0, 24, length=1000), col="grey") # Not entiresure where those 22 come from

} # eoF


# ------------------------------------------
# -------- Bivariate Plot ------------------
# ------------------------------------------

plotBV <- function(data, j, i1, i2, lag=FALSE,
                   wave=FALSE, title=TRUE, para=FALSE) {

  vars <- c("happy", "stressed", "anxious", "sad")
  if(wave) {
    cols <- brewer.pal(3, "Set1")
    col_b <- "black"
  } else {
    cols <- rep("black", 3)
    col_b <- "orange"
  }

  # Subset data
  u_pers <- unique(data$subj_id)
  data_ss <- data[data$subj_id == u_pers[j], ]
  N_ss <- nrow(data_ss)

  # Canvas
  par(mar=c(4,4,2,1))
  plot.new()
  plot.window(xlim=c(0, 100), ylim=c(0, 100))
  axis(1)
  axis(2, las=2)
  if(lag) {
    title(xlab=paste0(vars[i1], "(t-1)"), ylab=paste0(vars[i2], "(t)"), line=2.5)
  } else {
    title(xlab=vars[i1], ylab=vars[i2], line=2.5)
  }
  if(title) title(main=paste0("Person ", u_pers[j]), font.main=1)

  # Plot Data
  if(lag) {
    x1 <- data_ss[-N_ss, vars[i1]]
    x2 <- data_ss[-1, vars[i2]]
  } else {
    x1 <- data_ss[, vars[i1]]
    x2 <- data_ss[, vars[i2]]
  }
  points(x1, x2, col=cols[data_ss$wave], pch=20, cex=1.2)
  lm_obj <- lm(x2 ~ x1)
  abline(lm_obj, lwd=2, col=col_b)

  # Add regression results
  if(para) text(80, 7, paste0("a = ",
                              round(coef(lm_obj)[1], 2),
                              ", b = ",
                              round(coef(lm_obj)[2], 2)),
                col="orange")

  if(wave) for(w in 1:3) {
    if(length(data_ss[data_ss$wave==w, vars[i2]] > 5)) {
      lm_obj_w <- lm(data_ss[data_ss$wave==w, vars[i2]] ~ data_ss[data_ss$wave==w, vars[i1]])
      abline(lm_obj_w, col=cols[w])
    }
  }

} # eoF


# --------------------------------------------------------
# ---------- Plotting Labels in Canvas -------------------
# --------------------------------------------------------

plotLabel <- function(text, cex=1.4, srt=0) {
  par(mar=rep(0,4))
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(0,1))
  text(0.5, 0.5, text, cex=cex, srt=srt)
}


# --------------------------------------------------------
# ---------- Functions for Residual Analysis [adapted] ---
# --------------------------------------------------------

# Note that the following is hardcoded for our dataset
# (e.g., 4 variables in columns 5:8, 1 lag, etc.)

# Function 1: Make predictions with given VAR
# Input:
# VAR matrix, mu, linear trend, timing info (days, beep), data
# Output:
# Predictions for predictable time points for all variables

VARPred <- function(mu, lin, phi, data_s, method="matrix") {

  # Identify predictable time points
  N <- nrow(data_s)
  p <- 4
  p_loc <- 5:8
  ind_pred <- (data_s$DayNr[-1] == data_s$DayNr[-N]) & ((data_s$Beep[-1] - data_s$Beep[-N]) == 1)
  ind_pred <- c(NA, ind_pred) # The first is always unpredictable

  m_pred <- matrix(NA, N, p)
  # Predict
  for(j in 1:4) {

    # ----- Version 1: Element Wise -----
    if(method=="elementwise") {
      for(t in 2:N) {
        xtm1 <- data_s[t-1, p_loc] - mu - lin * data_s$Time[t-1]
        xt <- sum(phi[j, ] * xtm1) + mu[j] + lin[j] * data_s$Time[t]
        m_pred[t, j] <- xt
      }
    }
    # ----- Version 2: Vectorized -----
    if(method=="matrix") {
      xtm1 <- sweep(data_s[-N, p_loc], 2, mu, "-") - matrix(rep(lin, N-1), ncol=4, byrow=T) * data_s$Time[-N]
      xt <- as.matrix(xtm1) %*% matrix(phi[j, ], nrow=4) + mu[j] + lin[j] * data_s$Time[-1]
      m_pred[2:N, j] <- as.numeric(xt)
    }
  }
  # Delete inadmissible rows

  # data_s$Happy[207]
  # is.na(data_s$Happy)[207]

  # browser()

  m_pred[!ind_pred, ] <- NA
  NAs <- apply(data_s, 1, function(x) any(is.na(x[p_loc])))
  m_pred[NAs, ] <- NA


  # Make dataframe
  out_df <- data.frame(cbind(data_s[, 1:8], m_pred))
  colnames(out_df)[1:8] <- colnames(data_s)[1:8]
  colnames(out_df)[9:12] <- paste0(colnames(data_s)[5:8], "_hat")

  return(out_df)

} # eoF


# Function 2: Get Individual Models
# Input:
# LCVAR output object
# Output:
# Predictions for all predicable time points of all persons, Using Function 1

LCVARPred <- function(object, data, k) {

  # Loop over people
  u_pers <- unique(data$SEMA_ID)
  n_pers <- length(u_pers)

  l_pred <- list()
  for(i in 1:n_pers) {

    ### Get model for person i
    # Get classification for person i
    model_k4 <- coef.ClusterVAR(object, Model = rep(1, k))
    c_i <- as.numeric(model_k4$Classification)[i]
    # Get model of cluster c for person i
    mu <- model_k4$Exogenous_coefficients[, 1, c_i]
    lin <- model_k4$Exogenous_coefficients[, 2, c_i]
    phi <- model_k4$VAR_coefficients[, , c_i]


    ### Subset Data
    data_s <- data[data$SEMA_ID==u_pers[i], ]

    ### Make Predictions
    l_pred[[i]] <- VARPred(mu=mu, lin=lin, phi=phi, data_s=data_s)

  } # end for

  ## Return
  outlist <- list("Predictions"=l_pred,
                  "Classification"=as.numeric(model_k4$Classification))
  return(outlist)

} # eof



# # Make predictions
# head(data)
# test1 <- LCVARPred(object, data, k=4)
#
# plot(test1$Predictions[[1]]$Happy, type="o")
# lines(test1$Predictions[[1]]$Happy_hat, col="red", type="o")
# abline(v=69, lty=2)
# cbind(test1$Predictions[[1]]$Happy, test1$Predictions[[1]]$Happy_hat)






