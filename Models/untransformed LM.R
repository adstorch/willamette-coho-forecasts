# untransformed Linear Model ---------------------------------------------------
cat('
    model {

      # observation model
      for (i in 1:nObs){

        ### liklihood
        adult_cnt[i] ~ dnorm(muAdult_cnt[i],tau.e)

        ### predictions in log space
        muAdult_cnt[i] <- alpha + beta * jack_cnt[i]

        # jack_cnt_t[i] <- log(jack_cnt[i])

        pred[i] <- muAdult_cnt[i]

        # ### prediction on the arithmetic scale
        # pred[i] <- exp(muAdult_cnt[i])
      }

      # # process model for intercept
      # for (i in 2:nObs){
      #   ## define time-varying alpha parameter
      #   alpha[i] <- alpha[i-1] + Walpha[i]
      # 
      #   ## prior for annual deviation among alphas
      #   Walpha[i] ~ dnorm(0,tau.Walpha)
      # }

      # priors and definitions

      ## define alpha at t=1
      alpha ~ dnorm(0,0.001)

      ## define beta prior
      beta ~ dnorm(0,0.001)

      ## observation variance
      sig.e ~ dunif(0,1)

      ## process variance for intercept
      # sig.Walpha ~ dunif(0,1)

      ##observation precision
      tau.e<-1/pow(sig.e,2)

      ## process precision for intercept
      # tau.Walpha<-1/pow(sig.Walpha,2)

    }',
    file={willCohoLM.mod <- tempfile()})
