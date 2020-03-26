// Project Name: KUSKOKWIM RIVER CHINOOK SALMON FORECAST MODEL - Current autoregressive model
// Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
// Date: 3.17.20
// Purpose: Predict total salmon run size 1-year in advance using current AR-1 model used by ADF&G (but in log space)
// NOTES:
//  a) _lpdf signals it's the log probability density function
//  b) When left side of a ~ is not a data object, we need to update the posterior density.

data {
  int N; // Number of observations
  int year[N]; // Year references
  real ln_est[N];  // Estimated Run Size
  real ln_sd[N];  // Estimated stdev for Run Size
  real ln_cv[N];  // Estimated CV for run size in log space
  
}

parameters {
  // AR-1 Coefficient
  real<lower=-1, upper=1> phi;
  // Intercept 
  real alpha;
  
}

transformed parameters {
  // Phi transformation
  // real phi_trans;
  // phi_trans = 2*exp(phi)/(1+exp(phi)) - 1;
  
  // PRedictions
  real ln_pred[N-1];
  real pred[N-1];
  real residual[N-1];
  real mean_ln_cv;
  
  for(y in 2:N) {
    ln_pred[y-1] =  alpha + phi*ln_est[y-1];
    residual[y-1] = ln_est[y]-ln_pred[y-1];
  }
  
  // Transform predictions
  pred = exp(ln_pred);
  
  // Mean ln_cv
  mean_ln_cv = mean(ln_cv);
  
}

model {
  // PRIORS
  phi ~ normal(0,1);
  alpha ~ uniform(-25, 25);
  
  // LIKELIHOODS OF DATA
  for(y in 2:N) {
    // ln_est[y] ~ normal(ln_pred[y-1], ln_sd[y]);
    ln_est[y] ~ normal(ln_pred[y-1] - (ln_sd[y]^2)/2, ln_sd[y]);  //Lognormal Correction
  }
  
}

generated quantities {
  // Forecast for next year
  real ln_fcst;
  real fcst;
  
  real post_ln_pred[N-1];
  real post_pred[N-1];
  
  real post_ln_fcst;
  real post_fcst;

  real logLike[N-1];
   
  real post_ln_fcst_unc;
  real post_fcst_unc;
  
  // Expected sd for forecast year
  real ln_sd_fcst;
  ln_sd_fcst = mean(ln_sd);
  // ln_sd_fcst = mean_ln_cv*ln_fcst;
  // ln_sd_fcst = sd(residual);
  
  // Forecast Prediction Interval
  ln_fcst = alpha + phi*ln_est[N];
  fcst = exp(ln_fcst);
  
  // Posterior Predictive Distribution
  // post_ln_fcst = normal_rng(ln_fcst-(mean(ln_sd)^2)/2, mean(ln_sd));
  post_ln_fcst = normal_rng(ln_fcst-(ln_sd_fcst^2)/2, ln_sd_fcst); // Using Mean cv
  post_fcst = exp(post_ln_fcst);
  
  post_ln_fcst_unc = normal_rng(ln_fcst, ln_sd_fcst);
  post_fcst_unc = exp(post_ln_fcst_unc);

  for(y in 2:N) {
    // Calculate log likelihood for WAIC calcs
    logLike[y-1] = normal_lpdf(ln_est[y] | ln_pred[y-1] - (ln_sd[y]^2)/2, ln_sd[y]);
    // Posterior Predictive Distribution for Estimate
    // post_ln_pred[y-1] = normal_rng(ln_pred[y-1], ln_sd[y]);
    post_ln_pred[y-1] = normal_rng(ln_pred[y-1] - (ln_sd[y]^2)/2, ln_sd[y]);
    post_pred[y-1] = exp(post_ln_pred[y-1]);
  }

}



