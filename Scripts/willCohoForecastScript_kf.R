# # call packages -----------------------------------------------------------
# packages <- c("openxlsx",
#               "ggplot2",
#               "tidymv",
#               "car",
#               "ggfortify",
#               "gridExtra",
#               "grid",
#               "ggpubr",
#               "openair",
#               "cvTools",
#               "extrafont",
#               "remotes",
#               "R2jags",
#               "dplyr",
#               "broom.mixed",
#               "boot",
#               "ggmcmc",
#               "scales",
#               "postpack",
#               "MCMCvis",
#               "HDInterval")
# 
# if (!require(install.load)) {
#   install.packages("install.load")
# }
# 
# install.load::install_load(packages)
# 
# source("Functions\\round_fun.R")

# data steps --------------------------------------------------------------
# ## read-in raw data
# willCohoRaw.dat <- read.xlsx("Data\\cohoDat_inp.xlsx",
#                              sheet = 1,
#                              colNames = TRUE)

## create data frame to store output
willCohoOut_kf.dat <- data.frame(
  ret_year = numeric(),
  obs_cnt = numeric(),
  post_pred = numeric(),
  lwr_hdi = numeric(),
  upr_hdi = numeric()
)

# fitNum <- 23

# model function ----------------------------------------------------------
kf_fun <- function(fitNum){

  ## generate a data frame to fit model
  willCohoFit.dat <- willCohoInp.dat %>%
    drop_na() %>%
    select(
      ret_year,
      adult_cnt,
      jack_cnt
    ) %>%
    head(fitNum)

  ## create a vector to generate prediction
  willCohoPred.dat <- willCohoInp.dat %>%
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
      beta = runif(
        1,
        willCohoInit.beta - (5 * willCohoInit.betaSE),
        willCohoInit.beta + (5 * willCohoInit.betaSE)
      )
    )
  }

  ## specify model
  cat('
    model {
      
      # observation model
      for (i in 1:nObs){
        
        ### liklihood
        adult_cnt[i] ~ dnorm(muAdult_cnt[i],tau.e)
        
        ### predictions in log space
        muAdult_cnt[i] <- alpha[i] + beta * (jack_cnt[i] - mean(jack_cnt[]))
        
        ### prediction on the arithmetic scale
        pred[i] <- exp(muAdult_cnt[i])
      }
      
      # process model for intercept
      for (i in 2:nObs){
        ## define time-varying alpha parameter
        alpha[i] <- alpha[i-1] + Walpha[i]
        
        ## prior for annual deviation among alphas
        Walpha[i] ~ dnorm(0,tau.Walpha)
      }
      
      # priors and definitions
      
      ## define alpha at t=1
      alpha[1] ~ dnorm(0,0.001)
      
      ## define beta prior
      beta ~ dnorm(0,0.001)
      
      ## observation variance
      sig.e ~ dunif(0,1)
      
      ## process variance for intercept
      sig.Walpha ~ dunif(0,1)       
      
      ##observation precision
      tau.e<-1/pow(sig.e,2)
      
      ## process precision for intercept
      tau.Walpha<-1/pow(sig.Walpha,2)                 
      
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
  willCohoOut_kf.dat[nrow(
    willCohoOut_kf.dat
  ) + 1,] <<- c(
    as.numeric(
      willCohoInp.dat %>%
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
      willCohoInp.dat %>%
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

post_kf <- willCohoOut_kf.dat %>% 
  drop_na()

mape.est <- MAPE(
  post_kf$post_pred,
  post_kf$obs_cnt
)*100

willCoho.verifPlot_kf<-ggplot() +
  theme_bw()+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.title.y = element_text(face = "bold", size = 16,vjust = 1,margin = margin(t = 0, r = 10, b = 0, l = 0),family = "Times New Roman"),
        axis.title.x = element_text(face = "bold", size = 16,vjust = -1,margin = margin(t = 10, r = 0, b = 0, l = 0),family = "Times New Roman"),
        axis.text.x = element_text(face = "bold",size = 16,color="black", vjust=0.5,family = "Times New Roman"),
        axis.text.y = element_text(face = "bold",size = 16,color="black",family = "Times New Roman"),
        legend.title = element_blank(),
        plot.margin = margin(0.5, 1, 0.5, 0.5, "cm"),
        legend.text=element_text(size=12),
        axis.ticks.length = unit(0.15, "cm"))+
  labs(title ="State-space Model", y = "Observed", x = "Mean posterior predictions") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "Times New Roman")) +
  geom_abline(intercept = 0, slope = 1)+
  geom_point(data = willCohoOut_kf.dat,aes(post_pred,obs_cnt),shape = 19,size = 3.5,stroke=0.5)+
  scale_y_continuous(limits=c(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE))),breaks = seq(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE)),4000),expand = c(0,0))+
  scale_x_continuous(limits=c(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE))),breaks = seq(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE)),4000),expand = c(0,0))+
  annotate(geom = "text",x = 1500, y = 24000, label = paste("MAPE = ",round(mape.est,2),"%",sep = ""),hjust=0,size = 5.1,family = "Times New Roman")


png(filename=paste("Output\\Figures\\willCoho.verifPlot_kf",Sys.Date(),".png",sep=""),
    type="cairo",
    units="in",
    width=8,
    height=6,
    res=300)

print(willCoho.verifPlot_kf)
dev.off() #  turn device off
