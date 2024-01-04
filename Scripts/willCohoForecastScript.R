# call packages -----------------------------------------------------------
packages <- c("openxlsx",
              "ggplot2",
              "tidymv",
              "car",
              "ggfortify",
              "gridExtra",
              "grid",
              "ggpubr",
              "openair",
              "cvTools",
              "extrafont",
              "remotes",
              "R2jags",
              "dplyr",
              "broom.mixed",
              "boot",
              "ggmcmc",
              "scales",
              "postpack",
              "MCMCvis",
              "HDInterval")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

source("Functions\\round_fun.R")

# data steps --------------------------------------------------------------
## read-in raw data
willCohoRaw.dat <- read.xlsx("Data\\cohoDat_inp.xlsx",
                             sheet = 1,
                             colNames = TRUE)

## create data frame to store output
willCohoOut.dat <- data.frame(
  ret_year = numeric(),
  obs_cnt = numeric(),
  post_pred = numeric(),
  lwr_hdi = numeric(),
  upr_hdi = numeric()
)

fitNum <- 23

# model function ----------------------------------------------------------
kf_fun <- function(fitNum){

  ## generate a data frame to fit model
  willCohoFit.dat <- willCohoRaw.dat %>%
    drop_na() %>%
    select(
      ret_year,
      adult_cnt,
      jack_cnt
    ) %>%
    head(fitNum)

  ## create a vector to generate prediction
  willCohoPred.dat <- willCohoRaw.dat %>%
    # drop_na() %>%
    select(
      jack_cnt
    ) %>%
    head(
      fitNum + 2
    ) %>%
    tail(1)

  ## fit OLS to define initial MCMC values
  ### fit OLS
  willCohoInit.mod <- lm(
    log(
      adult_cnt
    ) ~ log(
      jack_cnt
    ),
    data = willCohoFit.dat
  )

  ### extract coefficient estimates
  willCohoInit.beta <- as.numeric(
    willCohoInit.mod$coef[2]
  )

  willCohoInit.betaSE <- coef(
    summary(
      willCohoInit.mod
    )
  )[2,2]

  ## create data list to fit model and generate prediction
  willCohoMod.dat <- list(
    adult_cnt =
      as.numeric(
        c(
          log(willCohoFit.dat$adult_cnt),

          "NA"
        )
      ),
    jack_cnt = log(
      c(
        willCohoFit.dat$jack_cnt,
        as.numeric(
          willCohoPred.dat
        )
      )
    ),
    nObs = nrow(
      willCohoFit.dat
    ) + 1
  )

  ## define initial values (user defined initial values are not accepted by
  ## jags.parallel()-default is to let jags estimate initial values; otherwise
  ## use jags() and specify initial values as below)
  inits.willCoho <- function()
  {
    list(
      beta.willCoho = runif(
        1,
        willCohoInit.beta - (5 * willCohoInit.betaSE),
        willCohoInit.beta + (5 * willCohoInit.betaSE)
      )
    )
  }

  ## specify model
  cat('
  model {

    ## observation model
    for (i in 1:nObs){

      ### liklihood
      adult_cnt[i] ~ dnorm(muAdult_cnt[i],tau.e)

      ### predictions in log space
      muAdult_cnt[i] <- alpha[i] +
        beta * (jack_cnt[i] - mean(jack_cnt[]))

      ### prediction on the arithmetic scale
      pred[i] <- exp(muAdult_cnt[i])
    }

    ## process model for intercept
    for (i in 2:nObs){
      ### define time-varying alpha parameter
      alpha[i] <- alpha[i-1] + Walpha[i]

      ### prior for annual deviation among alphas
      Walpha[i] ~ dnorm(0,tau.Walpha)
    }

    # priors and definitions
    alpha[1]~dnorm(0,0.001)     #define alpha at t=1
    beta~dnorm(0,0.001)        #define beta prior
    sig.e~dunif(0,1)            #observation variance
    sig.Walpha~dunif(0,1)       #process variance for intercept

    tau.e<-1/pow(sig.e,2)                 #observation precision
    tau.Walpha<-1/pow(sig.Walpha,2)       #process precision for intercept

  }',
      file={willCoho.mod <- tempfile()})

  ## define parameters to monitor
  willCoho.params <- c(
    "pred"
  )

  ## call jags
  start <- Sys.time()

  fit.willCoho <- jags.parallel(
    data = willCohoMod.dat,
    # inits = inits.willComb,  # see above
    parameters.to.save = willCoho.params,
    model.file = willCoho.mod,
    n.chains = 3,
    n.iter = 1000000,
    n.burnin = 75000,
    n.thin = 10,
    n.cluster = 3,
    jags.seed = 1234,
    DIC = F
  )

  stop <- Sys.time()
  duration <- stop-start
  print(duration)

# review and summarize output --------------------------------------------------
  ## review bugs object
  fit.willCoho

  ## extract simulations matrix
  mcmc.willCoho <- fit.willCoho$BUGSoutput$sims.matrix

  ## extract simulations for current predictions
  pred.mcmc.willCoho <- mcmc.willCoho[, paste(
    "pred[",nObs,"]",
    sep = ""
  )]


  ## output predictions
  willCohoOut.dat[nrow(
    willCohoOut.dat
  ) + 1,] <<- c(
    as.numeric(
      willCohoRaw.dat %>%
      # drop_na() %>%
        select(
          ret_year
        ) %>%
        head(
          fitNum + 2
        ) %>%
        tail(1)
    ),
    as.numeric(
      willCohoRaw.dat %>%
        # drop_na() %>%
        select(
          adult_cnt
        ) %>%
        head(
          fitNum + 2
        ) %>%
        tail(1)
    ),
    round(
      mean(
        pred.mcmc.willCoho
      ),
      0
    ),

    round(
      hdi(
        pred.mcmc.willCoho,
        credMass = 0.95
      )[1],
      0
    ),
    round(
      hdi(
        pred.mcmc.willCoho,
        credMass = 0.95
      )[2],
      0
    )
  )
}

for(fitNum in seq(10,23,1)){
  kf_fun(fitNum)
}
