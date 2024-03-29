# create data frame to store output ---------------------------------------
willCohoOut_kf.dat <- data.frame(
  ret_year = numeric(),
  obs_cnt = numeric(),
  post_pred = numeric(),
  lwr_hdi = numeric(),
  upr_hdi = numeric()
)


# fitNum <- 10
# model function ----------------------------------------------------------
willCohoKF_fun <- function(fitNum){
  
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
          willCohoFit.dat$adult_cnt,
          "NA"
        )
      ),
    jack_cnt =
      c(
        willCohoFit.dat$jack_cnt,
        as.numeric(
          willCohoPred.dat
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
  
  # specify model
  # cat('
  
  # model_string <- "
  #   model {
  # 
  #     # observation model
  #     for (i in 1:nObs){
  # 
  #       ### liklihood
  #       adult_cnt[i] ~ dlnorm(muAdult_cnt[i],tau.e)
  # 
  #       ### predictions in log space
  #       muAdult_cnt[i] <- alpha[i] + beta * jack_cnt_t[i]
  # 
  #       jack_cnt_t[i] <- log(jack_cnt[i])
  # 
  #       pred[i] <- exp(muAdult_cnt[i])
  # 
  #       # ### prediction on the arithmetic scale
  #       # pred[i] <- exp(muAdult_cnt[i] + sigma^2/2)
  #     }
  # 
  #     # process model for intercept
  #     for (i in 2:nObs){
  #       ## define time-varying alpha parameter
  #       alpha[i] <- alpha[i-1] + Walpha[i]
  # 
  #       ## prior for annual deviation among alphas
  #       Walpha[i] ~ dnorm(0,tau.Walpha)
  #     }
  # 
  #     # priors and definitions
  # 
  #     ## define alpha at t=1
  #     alpha[1] ~ dnorm(0,0.001)
  # 
  #     ## define beta prior
  #     beta ~ dnorm(0,0.001)
  # 
  #     ## observation variance
  #     sig.e ~ dunif(0,1)
  # 
  #     ## process variance for intercept
  #     sig.Walpha ~ dunif(0,1)
  # 
  #     ##observation precision
  #     tau.e<-1/pow(sig.e,2)
  # 
  #     ## process precision for intercept
  #     tau.Walpha<-1/pow(sig.Walpha,2)
  # 
  #   }"
  #     file={willCoho.mod <- tempfile()})
  
  ## define parameters to monitor
  willCoho.params <- c(
    "pred"
  )
  
  ## call jags
  start <- Sys.time()
  
  fit.willCoho <- jags(
    data = willCohoMod.dat,
    # inits = inits.willComb,  # see above
    parameters.to.save = willCoho.params,
    model.file = willCohoKF.logmod,
    n.chains = 3,
    n.iter = 1000000,
    n.burnin = 75000,
    n.thin = 10,
    # n.cluster = 3,
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
  ) + 1,] <<- c(#### NEED TO ADD < TO OUTPUT
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

for(fitNum in seq(
  round_fun(
    nrow(
      willCohoInp.dat
    )*0.40  # 40% of the data will be used to fit the model
  ),
  nrow(
    tail(
      head(
        willCohoInp.dat,
        -1
      ),
      -1
    )
  ),
  1
)
){
  willCohoKF_fun(fitNum)
}

# post_kf <- willCohoOut_kf.dat %>% 
#   drop_na()

mape_est_kf <- MAPE(
  head(willCohoOut_kf.dat$post_pred,-1),
  na.omit(willCohoOut_kf.dat$obs_cnt)
)*100



###### NEED TO ADD HEAD TO GEOM_POINT
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
  labs(title ="Kalman Filter", y = "Observed", x = "Mean posterior predictions") +
  theme(plot.title = element_text(hjust = 0.5,size = 16,face = "bold",family = "Times New Roman")) +
  geom_abline(intercept = 0, slope = 1)+
  geom_point(data = willCohoOut_kf.dat,aes(post_pred,obs_cnt),shape = 19,size = 3.5,stroke=0.5)+
  scale_y_continuous(limits=c(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE))),breaks = seq(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE)),4000),expand = c(0,0))+
  scale_x_continuous(limits=c(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE))),breaks = seq(0,max(max(willCohoOut_kf.dat$obs_cnt,na.rm = TRUE),max(willCohoOut_kf.dat$post_pred, na.rm = TRUE)),4000),expand = c(0,0))+
  annotate(geom = "text",x = 1500, y = 24000, label = paste("MAPE = ",round(MAPE(head(willCohoOut_kf.dat$post_pred,-1),na.omit(willCohoOut_kf.dat$obs_cnt))*100,2),"%",sep = ""),hjust=0,size = 5.1,family = "Times New Roman")


png(filename=paste("Output\\Figures\\willCoho.verifPlot_kf",Sys.Date(),".png",sep=""),
    type="cairo",
    units="in",
    width=8,
    height=6,
    res=300)

print(willCoho.verifPlot_kf)
dev.off()



# Load the R2jags package
library(R2jags)

# Define the model in BUGS language
model_code <- "
model {
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], tau) # Normal error
    mu[i] <- beta0 + beta1 * x[i] # Linear predictor
    x[i] <- log(X[i]) # Log-transformed independent variable
  }
  
  # Priors
  beta0 ~ dnorm(0, 0.001) # Vague prior for intercept
  beta1 ~ dnorm(0, 0.001) # Vague prior for slope
  tau ~ dgamma(0.01, 0.01) # Vague prior for precision
  sigma <- 1 / sqrt(tau) # Standard deviation
  
  # Back transformation
  for (i in 1:N) {
    yhat[i] <- exp(mu[i]) # Retransformed but unadjusted prediction
  }
  # gamma <- lm(y ~ yhat - 1)$coef # Regression coefficient for adjustment
  # for (i in 1:N) {
  #   yadj[i] <- gamma * yhat[i] # Adjusted retransformed prediction
  # }
}
"

# Define the data
model_data <- list(
  N = 100, # Sample size
  X = c(1:100), # Independent variable
  y = c(1:100) # Dependent variable
)

# Define the parameters to save
model_parameters <- c("beta0", "beta1", "sigma","yhat")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code),
  n.chains = 4,
  n.iter = 1000,
  n.burnin = 200,
  n.thin = 2
)

model_run

source("Models\\Kalman Filter.R")
model.file = textConnection(willCohoKF.mod)

