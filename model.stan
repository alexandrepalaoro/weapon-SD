//===================================================================
//Customized ANCOVA model with separate intercepts, slopes and sigmas
//for each category 

data
{
    int <lower = 1> N; //total number of observations
    int <lower = 1> M; //number of species
    
    real y[N]; //response variable
    real x[N]; //continuous predictor
    int sp[N]; //species number for each individual
    int sex[N];//sex number for each individual (male = 2, female = 1)
}


parameters
{
  //each parameter is a 2x2 matrix
  //rows represent species, columns represent sex
  matrix [M,2] A; //intercepts 
  matrix [M,2] B;//slopes

  matrix[M, 2] sigmas; //standard errors of the model
  
}

model
{
  vector[N] mu; //vector of expected mean values
  
  for(i in 1:N)
  {
    mu[i] = A[sp[i],sex[i]] + B[sp[i],sex[i]] * x[i];
    
    y[i] ~ normal(mu[i], sigmas[sp[i], sex[i]]);
  }
}

generated quantities
{
  //dimorphism measures
  vector[M] SDA;
  vector[M] SDB;
  
  SDA = A[,2] - A[,1]; //differences in the intercepts
  SDB = B[,2] - B[,1]; //differences in the slopes
}
//===================================================================
