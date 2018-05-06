#' Fit a cell-means model with JAGS for a Gamma distributed response variable
#'
#' @param .data Model data with columns for response, cell identifier and
#'   random effect identifier.
#'
#' @param yvar Name or column index for response variable.
#'
#' @param cellvar Name or column index for integer cell identifier. Values are
#'   assumed to be consecutive integers from 1.
#'
#' @param re.idvar Name or column index for integer random effect identifier.
#'   Values are assumed to be consecutive integers from 1.
#'
#' @param n.chains Number of Markov chain Monte Carlo chains (default 2).
#'
#' @param n.update Number of updating iterations to run (default 1000).
#'
#' @param n.sims Number of sampling iterations to run (default 5000).
#'
#' @param variable.names Names of model variables to monitor. Defaults to
#'   c("mu.cell", "re.sigma", "shape").
#'
#' @param gelman.threshold Value of the Gelman-Rubin convergence diagnostic to
#'   use when assessing convergence of sampled variables (default 1.1).
#'
#' @param gelman.action Action if one or more variables do not pass the Gelman-Rubin
#'   convergence check. Options are 'stop' (default) or 'warning'.
#'
#' @return An MCMC object of sample values as a single chain.
#'
#' @export
#'
jags_cell_means_gamma <- function(.data,
                                  yvar,
                                  cellvar = "cell",
                                  re.idvar = "re.id",
                                  n.chains = 2,
                                  n.update = 1000,
                                  n.sims = 5000,
                                  variable.names = c("mu.cell", "re.sigma", "shape"),
                                  gelman.threshold = 1.1,
                                  gelman.action = c("stop", "warning")) {

  gelman.action <- match.arg(gelman.action)

  modelTxt <- "model {
    for (i in 1:N) {
      y[i] ~ dgamma(shape, shape / mu[i])
      mu[i] <- mu.cell[cell[i]] + z[re.id[i]]
    }

    for (i in 1:Nre.id) { z[i] ~ dnorm(0, pow(re.sigma, -2)) }
    re.sigma ~ dunif(0, 30)

    for (i in 1:Ncell) {
      logmu.cell[i] ~ dnorm(0, 1.0e-4)
      mu.cell[i] <- exp(logmu.cell[i])
    }

    shape ~ dunif(0, 100)
  }"

  .fit_jags_cell_means(modelTxt, .data, yvar, cellvar, re.idvar,
                       n.chains, n.update, n.sims, variable.names,
                       gelman.threshold)
}



#' Fit a cell-means model with JAGS for a Beta distributed response variable
#'
#' @param .data Model data with columns for response, cell identifier and
#'   random effect identifier.
#'
#' @param yvar Name or column index for response variable. All response values must
#'   be between 0 and 1.
#'
#' @param cellvar Name or column index for integer cell identifier. Values are
#'   assumed to be consecutive integers from 1.
#'
#' @param re.idvar Name or column index for integer random effect identifier.
#'   Values are assumed to be consecutive integers from 1.
#'
#' @param n.chains Number of Markov chain Monte Carlo chains (default 2).
#'
#' @param n.update Number of updating iterations to run (default 1000).
#'
#' @param n.sims Number of sampling iterations to run (default 5000).
#'
#' @param variable.names Names of model variables to monitor. Defaults to
#'   c("pcell", "phi", "re.sigma").
#'
#' @param gelman.threshold Value of the Gelman-Rubin convergence diagnostic to
#'   use when assessing convergence of sampled variables (default 1.1).
#'
#' @param gelman.action Action if one or more variables do not pass the Gelman-Rubin
#'   convergence check. Options are 'stop' (default) or 'warning'.
#'
#' @return An MCMC object of sample values as a single chain.
#'
#' @export
#'
jags_cell_means_beta <- function(.data,
                                 yvar,
                                 cellvar = "cell",
                                 re.idvar = "re.id",
                                 n.chains = 2,
                                 n.update = 1000,
                                 n.sims = 5000,
                                 variable.names = c("pcell", "phi", "re.sigma"),
                                 gelman.threshold = 1.1,
                                 gelman.action = c("stop", "warning")) {

  gelman.action <- match.arg(gelman.action)

  modelTxt <- "model {
    for(i in 1:N) {
      y[i] ~ dbeta(alpha[i], beta[i])

      alpha[i] <- mu[i] * phi
      beta[i]  <- (1-mu[i]) * phi
      logit(mu[i]) <- a0[cell[i]] + z[re.id[i]]
    }

    for (i in 1:Nre.id) { z[i] ~ dnorm(0, pow(re.sigma, -2)) }
    re.sigma ~ dunif(0, 1)

    for (i in 1:Ncell) {
      a0[i] ~ dnorm(0, 1.0e-3)
      logit(pcell[i]) <- a0[i]
    }

    phi ~ dgamma(0.1, 0.1)
  }"


  # Check response variable lies within the unit interval
  ii <- !is.na(.data[[yvar]])
  y <- pmin( pmax(.data[[yvar]], 0.0001), 0.9999)
  if (any(abs(.data[[yvar]][ii] - y) > 0.0001)) stop("Response values must be between 0 and 1")

  .data[[yvar]] <- y

  .fit_jags_cell_means(modelTxt, .data, yvar, cellvar, re.idvar,
                       n.chains, n.update, n.sims, variable.names,
                       gelman.threshold)
}




# Non-exported function to compile and run JAGS model and
# check samples for convergence.
#
.fit_jags_cell_means <- function(modelTxt,
                                 .data, yvar, cellvar, re.idvar,
                                 n.chains, n.update, n.sims, variable.names,
                                 gelman.threshold, gelman.action) {

  rjags::load.module("glm", quiet = TRUE)

  jags.data <- list(
    N = nrow(.data),
    Nre.id = max(.data[[re.idvar]]),
    Ncell = max(.data[[cellvar]]),

    y = .data[[yvar]],
    cell = .data[[cellvar]],
    re.id = .data[[re.idvar]]
  )

  zz <- textConnection(modelTxt)

  model <- rjags::jags.model(zz,
                             data = jags.data,

                             n.chains = n.chains)

  rjags:::update.jags(model, n.update)

  sims <- rjags::coda.samples(model,
                              variable.names = variable.names,
                              n.iter = n.sims)

  if( !all(coda::gelman.diag(sims)$psrf[,1] < gelman.threshold) ) {
    msg <- "Not all variables converged"
    if (gelman.action == "stop") stop(msg)
    else warning(msg)
  }

  coda::as.mcmc( do.call(rbind, sims) )
}

