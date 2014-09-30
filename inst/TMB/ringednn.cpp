#include <TMB.hpp>

using namespace density;

template<class Type>
Type objective_function<Type>::operator() ()
{

  DATA_VECTOR(lon);
  DATA_VECTOR(lat);
  DATA_VECTOR(dt);
  DATA_FACTOR(qual); //Integers
  DATA_VECTOR(include);
  PARAMETER_VECTOR(logbeta); //Length 2 (first lat then lon)
  PARAMETER_VECTOR(logSdState);
  PARAMETER_VECTOR(logSdObs); //length 2
  //DATA_MATRIX(logCorrection); //Dim 2 x number of quality classes (first should be log(1)
  PARAMETER_MATRIX(logCorrection);
  PARAMETER_VECTOR(gamma); //Length 2 (first lat then lon)
  
  PARAMETER_MATRIX(mu); // Dim 2 x lon.size()
  PARAMETER_MATRIX(vel); // Dim 2 x lon.size()

  PARAMETER_VECTOR(df);  //Length 2 - process,error

  vector<Type> beta = exp(logbeta);
  vector<Type> varState = exp(Type(2.0)*logSdState);
  matrix<Type> varObs(logCorrection.rows(),logCorrection.cols()+1);
  matrix<Type> correction = logCorrection.array().exp().matrix();
  for(int i = 0; i < varObs.rows(); ++i){
    varObs(i,0) = exp(2.0*(logSdObs(i)));
    for(int j = 1; j < varObs.cols(); ++j){
      varObs(i,j) = exp(2.0*(logSdObs(i)+logCorrection(i,j-1)));
    }
  }

  matrix<Type> sdObs = varObs.array().sqrt().matrix();

  Type nll = 0.0;

  MVNORM_t<Type> nll_dist;//(df(0));
  MVNORM_t<Type> nll_dist_obs;//(df(1));
  matrix<Type> cov(4,4);
  vector<Type> state(4);
  matrix<Type> covObs(2,2);
  vector<Type> obs(2);

  int c = 0;
  
  

  //
  int stateNum = 0; 
  
  for(int i = 0; i < dt.size(); ++i){

    if(dt(i) > 0 && i > 0){stateNum += 1;}
    if(stateNum == 0){//Distribution for first state
      /*
      state.setZero();
      state(0) = mu(0,0)-lat(0);
      state(1) = vel(0,1);
      state(2) = mu(1,0)-lon(0);
      state(3) = vel(1,1);

      cov.setZero();
      cov(0,0) = 0.1;
      cov(1,1) = 0.1;
      cov(2,2) = 0.1;
      cov(3,3) = 0.1;

      nll_dist.setSigma(cov);
      nll += nll_dist(state);
      */
    }else if(dt(i)>0){ //Only at first time step
      //First states

      //Set up state vector
      state.setZero();
      c = 0;
      state(0) = mu(c,stateNum)-(mu(c,stateNum-1)+vel(c,stateNum-1)*(1.0-exp(-beta(c)*dt(i))/beta(c)));
      state(1) = vel(c,stateNum) - (gamma(c)+exp(-beta(c)*dt(i))*(vel(c,stateNum-1)-gamma(c)));    

      c = 1;
      state(2) = mu(c,stateNum)-(mu(c,stateNum-1)+vel(c,stateNum-1)*(1.0-exp(-beta(c)*dt(i))/beta(c)));
      state(3) = vel(c,stateNum) - (gamma(c)+exp(-beta(c)*dt(i))*(vel(c,stateNum-1)-gamma(c)));    
 
      //Set up covariance matrix
      cov.setZero();

      c = 0;
      cov(0,0) = varState(c)/pow(beta(c),2)*(dt(i)-2.0*(1.0-exp(-beta(c)*dt(i)))/beta(c)+(1.0-exp(-2.0*beta(c)*dt(i)))/(2.0*beta(c)));
      cov(1,1) = varState(c)*(1.0-exp(-2.0*beta(c)*dt(i)))/(2.0*beta(c));
      cov(1,0) = varState(c)*(1.0-2.0*exp(-beta(c)*dt(i))+exp(-2.0*beta(c)*dt(i)))/(2.0*pow(beta(c),2));
      cov(0,1) = cov(1,0);
      
      c = 1;
      cov(2,2) = varState(c)/pow(beta(c),2)*(dt(i)-2.0*(1.0-exp(-beta(c)*dt(i)))/beta(c)+(1.0-exp(-2.0*beta(c)*dt(i)))/(2.0*beta(c)));
      cov(3,3) = varState(c)*(1.0-exp(-2.0*beta(c)*dt(i)))/(2.0*beta(c));
      cov(2,3) = varState(c)*(1.0-2.0*exp(-beta(c)*dt(i))+exp(-2.0*beta(c)*dt(i)))/(2.0*pow(beta(c),2));
      cov(3,2) = cov(2,3);


	
      nll_dist.setSigma(cov);
      nll += nll_dist(state);

    }else{ //Or nothing else happens
    }

    //Then observations
    //if(include(i)==1){
      //Set up observation vector 
      obs.setZero();
      obs(0) = lat(i)-mu(0,stateNum);
      obs(1) = lon(i)-mu(1,stateNum);
    
      //Set up covariance matrix
      covObs.setZero();
      covObs(0,0) = varObs(0,qual(i));
      covObs(1,1) = varObs(1,qual(i));
      covObs(1,0) = 0.0; 
      covObs(0,1) = covObs(1,0);


      nll_dist_obs.setSigma(covObs);

      nll += nll_dist_obs(obs)*include(i);
      //}
  }

  ADREPORT(correction);
  ADREPORT(sdObs);
  return nll;
  
}