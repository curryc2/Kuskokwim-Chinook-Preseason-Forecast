// Project Name: KUSKOKWIM RIVER CHINOOK SALMON FORECAST MODEL - Current autoregressive model
// Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
// Date: 3.17.20
// Purpose: Predict total salmon run size 1-year in advance using current AR-1 model used by ADF&G (but in log space)
// NOTES:
//  a) _lpdf signals it's the log probability density function
//  b) When left side of a ~ is not a data object, we need to update the posterior density.

data {
  // int<lower=0> n_dists; //Number of Districts
  // int dists[n_dists];  //Names of Districts
  int N; // Number of observations
  int year[N]; // Year references
  real ln_est[N];  // Estimated Run Size
  real ln_sd[N];  // Estimated stdev for Run Size
}

parameters {
  // real<lower=0> pred;
  // real<lower=0, upper=1> prop_ce;
  // real<lower=0> sigma_pm_age[n_ages];
  
  // real<lower=0> sigma_proc;
  // real init_proc;
  // 
  // read proc_dev[N-1]; // Devs in state process
  
  // AR-1 Coefficient
  real phi;
  
  // Intercept 
  real alpha;
  
}

transformed parameters {
  // vector[nYearCE] hist_pred_ce;
  // for(y in 1:nYearCE) {
  //   hist_pred_ce[y] = hist_ce[y]/prop_ce;
  // }
  
  // Phi transformation
  phi_trans = 2*exp(phi)/(1+exp(phi)) - 1
  
  // PRedictions
  real ln_pred[N-1];
  real pred[N-1];
  
  for(y in 1:(N-1)) {
    ln_pred[y-1] =  alpha + phi_trans*ln_est[y-1];
  }
  
  // Transform predictions
  pred = exp(ln_pred);
 
}

model {
  // PRIORS
  // sigma_pm ~ normal(0,5);
  phi ~ normal(0,5);
  alpha ~ uniform(-25, 25);
  
  // LIKELIHOODS OF DATA
  // for(y in 1:nYearPM) { // Likelihood for total (across ages) PM prediction
  //   log(Robs_pm[y]) ~ normal(log(hist_pred_pm[y]+1e-6)-((sigma_pm^2)/2) , sigma_pm);
  // }
  // for(y in 2:N) {
  //   ln_est[y] ~ normal(ln_pred[y-1], ln_sd[y])
  // }
  
  //LIKELIHOOD OF DERIVED PARAMETERS
  // target += normal_lpdf(log(pred_pm+1e-6) | log(pred+1e-6)-((sigma_pm^2)/2), sigma_pm); // Likelihood for current forecast - ERROR NOT HERE
  
}

generated quantities {
  // real post_pred;
  // for(y in 2:N) {
  //   post_pred[N] = exp(normal_rng(ln_pred, ln_sd[y]));
  // }
  // 
  // // Forecast for next year
  // real ln_fcst;
  // real fcst;
  // 
  // ln_fcst = alpha + phi_trans*ln_est[N];
  // fcst = exp(ln_fcst);
  
}

