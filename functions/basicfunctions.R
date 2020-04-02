# Master Thesis
# Manuel Neumann

# Basic functions and wrappers

proptable <- function(x, useNA = "no", round = 2){
  table(x, useNA = useNA) %>% prop.table() %>% {.*100} %>% round(digits = round)
}


# ggpreview
# Taken from https://twitter.com/tjmahr/status/1083094031826124800?s=09
ggpreview <- function(..., device = "png"){
  fname <- tempfile(fileext = paste0(".", device))
  ggsave(filename = fname, device = device, ...)
  system2("open", fname)
  invisible(NULL)
}


# Loglikelihood multinomial logit model
ll_mnl <- function(theta, X, Z) {
  
  # Store some numbers that are helpful later:
  k <- ncol(X) # Number of estimates
  J <- ncol(Z) # Number of choices
  N <- nrow(X) # Number of observations
  
  # Create matrix of betas and set the first category to 0 (baseline category)
  beta <- matrix(0,  nrow = J, ncol = k)
  # Each row are the estimators, related to the choice
  
  # Fill up the matrix with the rest of the betas:
  beta[-1, ] <- matrix(theta[1:(k * (J - 1))], ncol = k, byrow = T)
  
  # utilities, systematic part
  V <- apply(beta, 1, function(x) X %*% x)
  # Dimension: observations x categories
  
  # probabilities
  Sexp <- apply(V, 1, function(x) sum(exp(x))) # Denominator
  P <- apply(V, 2, function(x) exp(x)/Sexp) # Dividing the utility through the sum of all others
  # Dimension: [NxJ]: For every observation we get a probability to choose one of the categories
  
  # log-likelihood
  loglik <- sum(log(P[Z]))
  return(loglik)
  
}


# Simulation
scenfu <- function(scenario, xvariable,
                   S,
                   probs = c(0.025, 0.975)){
  
  returnlist <- list()
  
  # Simulate the data
  simdat <- apply(scenario, 1, function(x) 1/ (1 + exp(-(S %*% x)))) %>% tibble()
  
  returnlist[[1]] <- simdat
  
  # Summary for plots
  simplotdat <- cbind(tibble(mean = apply(simdat, 2, mean),
                             qlower = apply(simdat, 2, quantile, probs = probs[1]),
                             qupper = apply(simdat, 2, quantile, probs = probs[2])),
                      scenario)
  
  returnlist[[2]] <- simplotdat

  indi <- which(colnames(simplotdat) == xvariable)
  colnames(simplotdat)[indi] <- "x"
  
  returnlist[[3]] <- ggplot(simplotdat, aes(x = x, y = mean, 
                                            ymin = qlower, ymax = qupper))
  
  names(returnlist) <- c("Simulated Data",
                         "Plot Data",
                         "Plot")
  return(returnlist)
}


# Simulating Logit moderl with Observed Value Approach

log_pred_ova <- function(model,
                         xvari, by = NULL,
                         scenname = NULL, scenvalue = NULL,
                         nsim = 1000,
                         seed = "random",
                         probs = c(0.025, 0.975)) {
  
  # Create list that is returned in the end.
  output <- list()
  
  # Data
  data <- model$data
  
  # Get matrix of coefficients out of the model
  mu <- coef(model)
  
  # Number of coefficients
  ncoef <- length(mu)
  
  # Variance-covariance matrix of estimates
  varcov <- vcov(model)
  
  # Set seed if needed:
  if (seed != "random") {
    set.seed(seed = seed)
  }
  
  # Simulate a sampling distribution
  S <- mvrnorm(nsim, mu, varcov)
  output[["S"]] <- S
  
  # Artificial variation ov independent variable of interest
  if (is.null(by) == TRUE) {
    by <- abs(min(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE) -
                max(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE))
  }
  
  variation <- seq(from = min(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE),
                   to = max(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE),
                   by = by)
  
  
  output[["Variation"]] <- variation
  
  # Length of sequence
  nseq <- length(variation)
  output[["nVariation"]] <- nseq
  
  variables <- as.character(attr(model$terms, "variables"))[-1]
  
  # Name of independent variables
  iv <- variables[2:length(variables)]
  output[["IV"]] <- iv
  
  # Name of dependent variable
  dv <- variables[1]
  output[["DV"]] <- dv
  
  # Full observations (listwise deletion):
  data_redux <- na.omit(data[, variables])
  
  obs <- nrow(data_redux)
  
  # Numbers of interactions
  ninteraction <- sum(grepl(":", names(model$coefficients)))
  
  # Matrix of observations
  X <- matrix(NA, ncol = ncoef, nrow = obs)
  colnames(X) <- names(model$coefficients)
  # 1 for the Intercept
  X[, 1] <- 1
  # Values of the independent variables
  X[, 2:(length(iv)+1)] <- as.matrix(data_redux[, iv])
  
  
  # Prepare array to fill in the matrix with the observed values
  ovacases <- array(NA, c(dim(X), nseq))
  # Fill in the matrices:
  ovacases[,,] <- X
  
  # Select the position of the variable which should vary:
  varidim <- which(colnames(X) == xvari)
  
  
  # Artificially alter the variable in each dimension according to
  # the preferred sequence:
  for (i in 1:nseq) {
    ovacases[, varidim, i] <- variation[i]
  }
  
  # Hold a second variable steady (if need be)
  if(is.null(scenname) == FALSE) {
    scendim <- which(colnames(X) == scenname)
    
    for (i in 1:nseq) {
      ovacases[, scendim, i] <- scenvalue
    }
  }
  
  
  # Compute interactions:
  if (ninteraction != 0) {
    
    # Get position of interaction terms
    interactionterms <- which(grepl(":", names(model$coefficients)) == TRUE)
    
    # Compute the terms:
    for (i in c(interactionterms)) {
      # First variable name of the interaction:
      firstint <- gsub(":.*", "", names(model$coefficients[i]))
      # Second variable name of the interaction:
      secondint <- gsub(".*:", "", names(model$coefficients[i]))
      
      # Get position in matrix:
      intdim1 <- which(colnames(X) == firstint)
      intdim2 <- which(colnames(X) == secondint)
      
      # Compute interaction term:
      for(j in 1:nseq) {
        ovacases[, i, j] <- ovacases[, intdim1, j]*ovacases[, intdim2, j]
      }
    }
  }
  
  P <- apply(ovacases, 3, function(x) {
    apply(S, 1, function(s) mean(1 / (1+exp(-x %*% s))))
    })
  
  output[["P"]] <- P
  
  # Aggregate the simulations
  # Create tibble for plot
  if (is.null(scenvalue) == TRUE) {
    plotdat <- data.frame(x = variation,
                          mean = rep(NA, nseq),
                          lower = rep(NA, nseq),
                          upper = rep(NA, nseq))
  } else {
    plotdat <- tibble::tibble(x = variation,
                              scen = rep(scenvalue, each = nseq),
                              mean = rep(NA, nseq),
                              lower = rep(NA, nseq),
                              upper = rep(NA, nseq))
  }
  
  plotdat$mean <- apply(P, 2, mean)
  plotdat$lower <- apply(P, 2, quantile, probs = probs[1])
  plotdat$upper <- apply(P, 2, quantile, probs = probs[2])
  
  
  # Rename the variables in the plot data
  if (is.null(scenname) == TRUE) {
    colnames(plotdat)[1] <- xvari
  } else {
    colnames(plotdat)[1:2] <- c(xvari, scenname)
  }
  
  
  # Put the data in the output
  output[["plotdata"]] <- plotdat
  
  return(output)
}


log_pred_sce <- function(model,
                         data = NULL,
                         xvari, by = NULL,
                         nsim = 1000,
                         seed = "random",
                         probs = c(0.025, 0.975)) {
  
  # Create list that is returned in the end.
  output <- list()
  
  # Data
  # data
  
  # Get matrix of coefficients out of the model
  mu <- coef(model)
  
  # Number of coefficients
  ncoef <- length(mu)
  
  # Variance-covariance matrix of estimates
  varcov <- vcov(model)
  
  # Set seed if needed:
  if (seed != "random") {
    set.seed(seed = seed)
  }
  
  # Simulate a sampling distribution
  S <- mvrnorm(nsim, mu, varcov)
  output[["S"]] <- S
  
  # Artificial variation ov independent variable of interest
  if (is.null(by) == TRUE) {
    by <- abs(min(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE) -
                max(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE))
  }
  
  variation <- seq(from = min(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE),
                   to = max(eval(parse(text = paste0("data$", xvari))), na.rm = TRUE),
                   by = by)
  
  
  output[["Variation"]] <- variation
  
  # Length of sequence
  nseq <- length(variation)
  output[["nVariation"]] <- nseq
  
  variables <- as.character(attr(model$terms, "variables"))[-1]
  
  # Name of independent variables
  iv <- variables[2:length(variables)]
  output[["IV"]] <- iv
  
  # Name of dependent variable
  dv <- variables[1]
  output[["DV"]] <- dv
  
  # Full observations (listwise deletion):
  data_redux <- na.omit(data[, variables])
  
  obs <- nrow(data_redux)
  
  # Numbers of interactions
  ninteraction <- sum(grepl(":", names(model$coefficients)))
  
  # Matrix of observations
  X <- matrix(NA, ncol = ncoef, nrow = obs)
  colnames(X) <- names(model$coefficients)
  # 1 for the Intercept
  X[, 1] <- 1
  # Values of the independent variables
  X[, 2:(length(iv)+1)] <- as.matrix(data_redux[, iv])
  
  
  # Prepare array to fill in the matrix with the observed values
  ovacases <- array(NA, c(dim(X), nseq))
  # Fill in the matrices:
  ovacases[,,] <- X
  
  # Select the position of the variable which should vary:
  varidim <- which(colnames(X) == xvari)
  
  
  # Artificially alter the variable in each dimension according to
  # the preferred sequence:
  for (i in 1:nseq) {
    ovacases[, varidim, i] <- variation[i]
  }
  
  
  
  # Compute interactions:
  if (ninteraction != 0) {
    
    # Get position of interaction terms
    interactionterms <- which(grepl(":", names(model$coefficients)) == TRUE)
    
    # Compute the terms:
    for (i in c(interactionterms)) {
      # First variable name of the interaction:
      firstint <- gsub(":.*", "", names(model$coefficients[i]))
      # Second variable name of the interaction:
      secondint <- gsub(".*:", "", names(model$coefficients[i]))
      
      # Get position in matrix:
      intdim1 <- which(colnames(X) == firstint)
      intdim2 <- which(colnames(X) == secondint)
      
      # Compute interaction term:
      for(j in 1:nseq) {
        ovacases[, i, j] <- ovacases[, intdim1, j]*ovacases[, intdim2, j]
      }
    }
  }
  
  P <- apply(ovacases, 3, function(x) {
    apply(S, 1, function(s) mean(1 / (1+exp(-x %*% s))))
  })
  
  output[["P"]] <- P
  
  plotdat <- data.frame(x = variation,
                        mean = rep(NA, nseq),
                        lower = rep(NA, nseq),
                        upper = rep(NA, nseq))
  
  plotdat$mean <- apply(P, 2, mean)
  plotdat$lower <- apply(P, 2, quantile, probs = probs[1])
  plotdat$upper <- apply(P, 2, quantile, probs = probs[2])
  
  
  # Rename the variables in the plot data
  colnames(plotdat)[1] <- xvari
  
  
  # Put the data in the output
  output[["plotdata"]] <- plotdat
  
  return(output)
}


log_fd_ova <- function(model,
                       xvari,
                       scenname,
                       scenvalues,
                       by = NULL,
                       nsim = 1000,
                       seed = "random",
                       probs = c(0.025, 0.975)){
  # Prepare output:
  output <- list()
  
  if (seed == "random") {
    seed <- sample(1:10000, 1)
  }
  
  # Predictions for first scenario
  pred1 <- log_pred_ova(model = model,
                        xvari = xvari,
                        scenname = scenname,
                        scenvalue = scenvalues[1],
                        by = by, nsim = nsim, seed = seed,
                        probs = probs)
  output[["Prediction1"]] <- pred1
  
  # Predictions for second scenario
  pred2 <- log_pred_ova(model = model,
                        xvari = xvari,
                        scenname = scenname,
                        scenvalue = scenvalues[2],
                        by = by, nsim = nsim, seed = seed,
                        probs = probs)
  
  output[["Prediction2"]] <- pred2
  
  plotdat <- rbind(pred1$plotdata,
                   pred2$plotdata)
  
  output[["plotdata"]] <- plotdat
  
  
  # First differences
  P_fd <- array(NA, dim = c(nsim, pred1$nVariation))
  
  P_fd <- pred2$P- pred1$P
  
  output[["P"]] <- P_fd
  
  # Plotdata
  plotdata_fd <- pred1$plotdata[, c(1,3:5)]
  plotdata_fd[, c("mean", "lower", "upper")] <- NA
  
  plotdata_fd$mean <- apply(P_fd, 2, mean)
  plotdata_fd$lower <- apply(P_fd, 2, quantile, probs = probs[1])
  plotdata_fd$upper <- apply(P_fd, 2, quantile, probs = probs[2])
  
  output[["plotdata_fd"]] <- plotdata_fd
  
  return(output)
}



# Linear link
scenfulin <- function(scenario, xvariable,
                      S,
                      probs = c(0.025, 0.975)){
  
  returnlist <- list()
  
  # Simulate the data
  simdat <- apply(scenario, 1, function(x) S %*% x) %>% tibble()
  
  returnlist[[1]] <- simdat
  
  # Summary for plots
  simplotdat <- cbind(tibble(mean = apply(simdat, 2, mean),
                             qlower = apply(simdat, 2, quantile, probs = probs[1]),
                             qupper = apply(simdat, 2, quantile, probs = probs[2])),
                      scenario)
  
  returnlist[[2]] <- simplotdat
  
  indi <- which(colnames(simplotdat) == xvariable)
  colnames(simplotdat)[indi] <- "x"
  
  returnlist[[3]] <- ggplot(simplotdat, aes(x = x, y = mean, 
                                            ymin = qlower, ymax = qupper))# +
    # geom_pointrange() +
    # labs(x = xvariable)
  
  names(returnlist) <- c("Simulated Data",
                         "Plot Data",
                         "Plot")
  return(returnlist)
}



# First Differences
fd <- function(scenario1, scenario2, xvariable,
               S,
               factor = c("Factor1", "Factor2"),
               probs = c(0.025, 0.975),
               dista = 0.025){
  
  returnlist <- list()
  
  # Simulate
  list1 <- scenfu(scenario1, xvariable,
                  S, probs = probs)
  list2 <- scenfu(scenario2, xvariable,
                  S, probs = probs)
  
  # Get the data out
  data1 <- list1[[2]]
  data2 <- list2[[2]]
  
  returnlist[[1]] <- data1
  returnlist[[2]] <- data2
  
  data1$diff <- factor[1]
  data2$diff <- factor[2]
  
  difplotdat <- rbind(data1, data2)
  
  returnlist[[3]] <- difplotdat
  
  indi1 <- which(colnames(difplotdat) == xvariable)
  colnames(difplotdat)[indi1] <- "x" 
  
  difplotdat %<>% mutate(x = if_else(diff == factor[1],
                                     x - dista,
                                     x + dista))
  
  returnlist[[4]] <- ggplot(difplotdat, aes(x = x, y = mean,
                                            ymin = qlower, ymax = qupper)) # + 
    # geom_pointrange() +
    # labs(x = xvariable)
  
  fddat <- list1[[1]] - list2[[1]]
  
  returnlist[[5]] <- fddat
  
  fdplotdat <- cbind(tibble(mean = apply(fddat, 2, mean),
                            qlower = apply(fddat, 2, quantile, probs = probs[1]),
                            qupper = apply(fddat, 2, quantile, probs = probs[2])),
                     scenario1[, xvariable])
  
  returnlist[[6]] <- fdplotdat
  
  indi2 <- which(colnames(fdplotdat) == xvariable)
  colnames(fdplotdat)[indi2] <- "x"
  
  
  returnlist[[7]] <- ggplot(fdplotdat, aes(x = x, y = mean,
                                           ymin = qlower, ymax = qupper))# +
    # geom_pointrange() +
    # geom_hline(yintercept = 0) +
    # labs(x = xvariable)
  
  names(returnlist) <- c("Simulated Predicted Outcomes (Scenario1)",
                         "Simulated Predicted Outcomes (Scenario2)",
                         "Difference Plot Data",
                         "Difference Plot",
                         "First Difference Data",
                         "First Difference Plot Data",
                         "First Difference Plot")
  
  return(returnlist)
}


# First Differences
fdlin <- function(scenario1, scenario2, xvariable,
                  S,
                  factor = c("Factor1", "Factor2"),
                  probs = c(0.025, 0.975),
                  dista = 0.025){
  
  returnlist <- list()
  
  # Simulate
  list1 <- scenfulin(scenario1, xvariable,
                     S, probs = probs)
  list2 <- scenfulin(scenario2, xvariable,
                     S, probs = probs)
  
  # Get the data out
  data1 <- list1[[2]]
  data2 <- list2[[2]]
  
  returnlist[[1]] <- data1
  returnlist[[2]] <- data2
  
  data1$diff <- factor[1]
  data2$diff <- factor[2]
  
  difplotdat <- rbind(data1, data2)
  
  returnlist[[3]] <- difplotdat
  
  indi1 <- which(colnames(difplotdat) == xvariable)
  colnames(difplotdat)[indi1] <- "x" 
  
  difplotdat %<>% mutate(x = if_else(diff == factor[1],
                                     x - dista,
                                     x + dista))
  
  returnlist[[4]] <- ggplot(difplotdat, aes(x = x, y = mean,
                                            ymin = qlower, ymax = qupper,
                                            color = diff, fill = diff))
  
  fddat <- list1[[1]] - list2[[1]]
  
  returnlist[[5]] <- fddat
  
  fdplotdat <- cbind(tibble(mean = apply(fddat, 2, mean),
                            qlower = apply(fddat, 2, quantile, probs = probs[1]),
                            qupper = apply(fddat, 2, quantile, probs = probs[2])),
                     scenario1[, xvariable])
  
  returnlist[[6]] <- fdplotdat
  
  indi2 <- which(colnames(fdplotdat) == xvariable)
  colnames(fdplotdat)[indi2] <- "x"
  
  
  returnlist[[7]] <- ggplot(fdplotdat, aes(x = x, y = mean,
                                           ymin = qlower, ymax = qupper))
  
  names(returnlist) <- c("Simulated Predicted Outcomes (Scenario1)",
                         "Simulated Predicted Outcomes (Scenario2)",
                         "Difference Plot Data",
                         "Difference Plot",
                         "First Difference Data",
                         "First Difference Plot Data",
                         "First Difference Plot")
  
  return(returnlist)
}


# Logit fit-function

# This function needs a data set including:
# - the dependent variable
# - the predicted probabilty
# - the predicted outcome
# - the names of these variables

bin.fitm.fun <- function(data, 
                         dv.name, 
                         predprob.name, 
                         pred.name){
  
  dv <- data[, dv.name] %>% as_vector()
  pred <- data[, pred.name] %>% as_vector()
  pred.prob <- data[, predprob.name] %>% as_vector()
  
  if(sum(dv == 1) > sum(dv == 0)){
    pmc <- sum(dv)/length(dv)
  } else pmc <- 1-(sum(dv)/length(dv))
  
  crtab <- table(dv, pred)
  pcp <- {(crtab[1,1] + crtab[2,2])/sum(crtab)}
  
  pre <- (pcp-pmc)/(1-pmc)
  
  positive <- which(dv == 1)
  
  ePCP <- (sum(pred.prob[positive])+sum(1-pred.prob[-positive]))/nrow(data)
  
  cat(rep("-", 12),"\n",
      "Percentage of the Modal Category\n",
      pmc, "\n",
      rep("-", 12),"\n",
      "Percent Correctly Predicted\n",
      pcp, "\n",
      rep("-", 12),"\n",
      "Percent Reduction Error:\n",
      pre, "\n",
      rep("-", 12),"\n",
      "Expected Percent Correctly Predicted:\n",
      ePCP, "\n",
      rep("-", 12), "\n",
      "Confusion matrix:", "\n")
  print(crtab)
  cat(rep("-", 12))
}



fitstat <- function(object) {
  #thanks Ripley, B. D. for telling how to get the LogLik and when is invalid.
  {if (!is.null(object$call$summ) && !identical(object$call$summ,0))
    stop("when 'summ' argument is not zero,can NOT get Loglik") }
  object.base <- update(object,.~1,traceúLSE)
  dev.base <- deviance(object.base) ; L.base <- - dev.base/2
  dev.full <- deviance(object) ; L.full <- - dev.full/2
  G2 <- dev.base - dev.full
  df <- object$edf - object.base$edf
  LR.test.p <- pchisq(G2,df,lower=F)
  
  aic <- object$AIC
  
  n<-dim(object$residuals)[1]
  
  #get the predict value to cal count R2
  pre <- predict(object,type="class")
  y <- eval.parent(object$call$data)[,as.character(object$call$formula[[2]])]
  if (!identical(length(y),length(pre))) stop("Length not matched.")
  tab <- table(y,pre)
  if (!identical(dim(tab)[1],dim(tab)[2])) stop("pred and y have diff nlevels")
  ad <- max(rowSums(tab))#max of row sum
  
  #cal R2
  ML.R2 <- 1-exp(-G2/n)
  McFadden.R2 <- 1-(L.full/L.base)
  McFadden.Adj.R2 <- 1-((L.full-mod$edf)/L.base)
  Cragg.Uhler.R2 <- ML.R2/(1-exp(2*L.base/n))
  Count.R2 <- sum(diag(tab))/sum(tab)
  Count.adj.R2 <- (sum(diag(tab))-ad)/(sum(tab)-ad)
  
  #get the result
  res<-list(LR=G2,dfß,LR.test.p =LR.test.p
            ,aic=aic,ML.R2=ML.R2,Cragg.Uhler.R2=Cragg.Uhler.R2,McFadden.R2
            =McFadden.R2 ,McFadden.Adj.R2=McFadden.Adj.R2,Count.R2=Count.R2,Count.adj.R2=Count.adj.R2)
  
  #print the result
  cat("\n",
      paste(rep("-",21)),
      "\n The Fitstats are : \n",
      sprintf("G2(%d) = %f",df,G2),
      " ,Prob ",format.pval(LR.test.p),
      "\n",sprintf("AIC = %f",aic),
      sprintf(",ML.R2 = %f \n",ML.R2),
      paste(rep("-",21)),"\n",
      sprintf("Cragg.Uhler.R2 = %f \n",Cragg.Uhler.R2),
      sprintf("McFadden.R2 = %f \n",McFadden.R2),
      sprintf("McFadden.Adj.R2 = %f \n",McFadden.Adj.R2),
      sprintf("Count.R2 = %f \n",Count.R2),
      sprintf("Count.adj.R2 = %f \n",Count.adj.R2),
      "\n Note:The maxinum of ML R2 is less than 1 \n",
      paste(rep("-",21)),"\n")
  invisible(res)
}
