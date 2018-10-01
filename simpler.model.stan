//===================================================================
//Customized (one way) ANCOVA model with varying sigmas

data
{
    int <lower = 1> N; //total number of observations
    int <lower = 1> M; //number of categories (species)
    
    real y[N];
    real x[N];
    int cat[N];//numerical category for each individual
    
}

parameters
{
  vector [M] A; //intercepts
  vector [M] B; //slopes

  vector  <lower = 0> [M] sigmas; //standard errors
  
}

model
{
  vector[N] mu; //vector of expected mean values
  
  for(i in 1:N)
  {
    mu[i] = A[cat[i]] + B[cat[i]] * x[i];
    
    y[i] ~ normal(mu[i], sigmas[cat[i]]);
  }
}
//===================================================================
