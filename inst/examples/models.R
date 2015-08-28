
## devtools::install_local("~/argosTrack")

## devtools::install_github("calbertsen/argosTrack",ref="newmove")

library(argosTrack)

dat <- adult_ringed_seal
dat <- dat[!(dat$lc=="Z"),]
dat$lc <- factor(dat$lc)
table(dat$lc)

## Random walk
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="rw",
             timeunit="hours"
             )
fitrw <- do.call(argosTrack,args)

fitrw; summary(fitrw)
plot(fitrw)
plot(summary(fitrw),bearings=TRUE)

## Random walk; Nielsen et al 2006 (ca)
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             nauticalStates=TRUE,
             errordistribution="n",
             movementmodel="rw",
             timeunit="hours"
             )
fitrwns <- do.call(argosTrack,args)

fitrwns; summary(fitrwns)
plot(fitrwns)
plot(summary(fitrwns))


## Continuous time correlated random walk
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fitctcrw <- do.call(argosTrack,args)

fitctcrw; summary(fitctcrw)
plot(fitctcrw)
plot(summary(fitctcrw))


## Continuous time correlated random walk; time varying beta
tvb <- 50
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             timevarybeta=tvb,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="ctcrw",
             timeunit="hours"
             )
fittv <- do.call(argosTrack,args)

fittv; summary(fittv)
plot(fittv)
plot(summary(fittv))


## Multiple persistence continuous time correlated random walk
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="mpctcrw",
             timeunit="hours"
             )
fitmpctcrw <- do.call(argosTrack,args)

fitmpctcrw; summary(fitmpctcrw)
plot(fitmpctcrw)
plot(summary(fitmpctcrw))


## Discrete time correlated random walk
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="dtcrw",
             nStates = 4000,
             timeunit="hours"
             )
fitdtcrw <- do.call(argosTrack,args)

fitdtcrw; summary(fitdtcrw)
plot(fitdtcrw)
plot(summary(fitdtcrw))


## Discrete time Step length and bearings
args <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="dsb",
             nauticalStates = TRUE,
             nauticalObs = TRUE,
             nStates = round(length(dat$lon)/10),
             timeunit="hours"
             )
rm(fitdsb)
fitdsb <- do.call(argosTrack,args)

fitdsb; summary(fitdsb)
plot(fitdsb)
plot(summary(fitdsb),col="grey")


library(Matrix)

ihe <- function(obj) obj$tmb_object$env$spHess(obj$tmb_object$env$last.par.best,random=TRUE)
ihe_dsb <- ihe(fitdsb)

image(a <- solve(ihe_dsb))

plot(as.POSIXct(fitdsb$dates),fitdsb$tmb_object$env$report()$yobs,col="grey",pch=16)
lines(fitdsb$state_dates,fitdsb$positions[1,],type="b")
lines(fitdsb$state_dates,fitdsb$positions[1,] + 2*fitdsb$positions_sd[1,],lty=2)
lines(fitdsb$state_dates,fitdsb$positions[1,] - 2*fitdsb$positions_sd[1,],lty=2)



## Test
library(CircStats)
library(MASS)
n2ll <- function(x){
  c(x[1] / ((60.0) * cos(x[2] * pi / (180.0) / (60.0))),
  x[2] / (60.0))
}

set.seed(3)
n <- 1000
shape <- 6
scale <- 10
scale * gamma(1+1/shape)
vs <- exp(2*c(1.1734175,1.0988463))
vs[1] * gamma(1+1/vs[2])
sd <- 0.5
gamma <- 0.7
mu <- pi/2
phi <- numeric(n)
for(i in 2:n)
    phi[i] <- rwrpcauchy(1,phi[i-1],gamma)
s <- rweibull(n,shape,scale)
x <- cbind(cumsum(s*cos(phi)),
           cumsum(s*sin(phi)))
y <- x+rnorm(2*n,0,sd)
yobs <- t(apply(y,1,n2ll))

plot(y);lines(x)

args <-  list(lon = yobs[,1],
              lat = yobs[,2],
              dates = 1:n,
              locationclass = rep("3",n),
              verbose=FALSE,
              fixcorrection=FALSE,
              errordistribution="n",
              movementmodel="dsb",
              nauticalStates = TRUE,
              nauticalObs = TRUE,
              nStates = n,
              timeunit="hours",
              include = rep(TRUE,length(y[,1])),
              equalbetas = TRUE,
              timevarybeta = 1,
              fixgammas = TRUE,
              dfVals = NULL,
              dfMap = NULL,
              minDf = 3.0,
              nlminb.control = list(eval.max=2000,
                  iter.max=1500,
                  rel.tol=1e-3,
                  x.tol=1.5e-2)
              )


## rm(list=ls())

## attach(args)
rm(fitdsbs)
fitdsbs <- do.call(argosTrack,args)
fitdsbs
plot(summary(fitdsbs))

if(exists("fitdsbs")){
par(mfrow=c(3,2))
xlim <- range(1:n)
## xlim <- range(15:30)
plot(fitdsbs$state_dates,fitdsbs$positions[1,],type="b",xlim=xlim)
points(fitdsbs$dates,fitdsbs$tmb_object$env$report()$yobs,col="grey",pch=16)
lines(fitdsbs$state_dates,fitdsbs$positions[1,] + 2*fitdsbs$positions_sd[1,],lty=2)
lines(fitdsbs$state_dates,fitdsbs$positions[1,] - 2*fitdsbs$positions_sd[1,],lty=2)
## text(fitdsbs$state_dates,fitdsbs$positions[1,],1:length(fitdsbs$state_dates),pos=3)
plot(fitdsbs$state_dates,fitdsbs$positions[2,],type="b",xlim=xlim)
points(fitdsbs$dates,fitdsbs$tmb_object$env$report()$xobs,col="grey",pch=16)
lines(fitdsbs$state_dates,fitdsbs$positions[2,] + 2*fitdsbs$positions_sd[2,],lty=2)
lines(fitdsbs$state_dates,fitdsbs$positions[2,] - 2*fitdsbs$positions_sd[2,],lty=2)
## text(fitdsbs$state_dates,fitdsbs$positions[2,],1:length(fitdsbs$state_dates),pos=3)

ihe <- function(obj) obj$tmb_object$env$spHess(obj$tmb_object$env$last.par.best,random=TRUE)
ihe_dsb <- ihe(fitdsbs)
sd <- sqrt(diag(solve(ihe_dsb)))[matrix(1:(2*args$nStates),nrow=2)[1,]]

rp <- fitdsbs$tmb_object$report()
dfs <- cbind(diff(rp$y),diff(rp$x))
## dfs
phis <- apply(dfs,1,function(x)atan2(x[2],x[1]))
ss <- apply(dfs,1,function(x)sqrt(sum(x^2)))
## cbind(apply(dfs,1,function(x)atan2(x[2],x[1])),sd[-1])
## cbind(apply(dfs,1,function(x)sqrt(sum(x^2))),sd[-1])



dates <- c(1,diff(args$dates))
dateExtremes <- range(cumsum(dates)-1)
dtStates <- rep(c(1,(ceiling(dateExtremes[2])-dateExtremes[1])/(args$nStates-1)),times=c(1,args$nStates-1))
dateObs <- cumsum(dates)
dateStates <- cumsum(dtStates)
prevState <- sapply(1:length(args$lon),
                    function(i) max((1:length(dateStates))[dateStates <= dateObs[i]]))
stateFrac <- sapply(1:length(args$lon),
                            function(i) 1 - (dateObs[i] - dateStates[prevState[i]]) / na.omit(c(diff(dateStates[prevState[i] + 0:1]),1))[1])


pars <- fitdsbs$optimization$par

i <- which.max(sd)-1
if(i==0)i <- 1
if(i == args$nStates) i <- args$nStates-1
j <- which.min(sd)-1

## par(mfrow=c(2,2))

xx <- seq(-pi,pi,len=100)
plot(xx,dwrpcauchy(xx,phis[i],exp(-exp(pars["logbeta"]))),main=paste("Største sd",sd[i+1]),type="l",col=1)
lines(xx,dwrpcauchy(xx,phis[i+1],exp(-exp(pars["logbeta"]))),col=2)
abline(v=phis[i+1])
abline(h = dwrpcauchy(phis[i+1],phis[i],exp(-exp(pars["logbeta"]))))
xx <- seq(-pi,pi,len=100)
plot(xx,dwrpcauchy(xx,phis[j],exp(-exp(pars["logbeta"]))),main=paste("Mindste sd",sd[j+1]),type="l",col=1)
lines(xx,dwrpcauchy(xx,phis[j+1],exp(-exp(pars["logbeta"]))),col=2)
abline(v=phis[j+1])
abline(h = dwrpcauchy(phis[j+1],phis[j],exp(-exp(pars["logbeta"]))))



ns <- 20000
## Largest sd
phin <- rwrpcauchy(ns,phis[i],exp(-exp(pars["logbeta"])))
sn <- rweibull(ns,scale=exp(2*pars[names(pars)=="logSdObs"][1]),shape=exp(2*pars[names(pars)=="logSdObs"][2]))
xn <- sn*cos(phin)
yn <- sn*sin(phin)
## contour(kde2d(phin,sn))
## points(phis[i+1],ss[i+1],col="red",pch=16)
contour(kde2d(xn,yn),xlab="dx",ylab="dy")
phin <- rwrpcauchy(ns,phis[i+1],exp(-exp(pars["logbeta"])))
sn <- rweibull(ns,scale=exp(2*pars[names(pars)=="logSdObs"][1]),shape=exp(2*pars[names(pars)=="logSdObs"][2]))
xn <- sn*cos(phin)
yn <- sn*sin(phin)
## contour(kde2d(phin,sn))
## points(phis[i+1],ss[i+1],col="red",pch=16)
contour(kde2d(xn,yn),add=TRUE,col=2)
points(diff(fitdsbs$tmb_object$report()$x[c(j,j+1)]),diff(fitdsbs$tmb_object$report()$y[c(j,j+1)]),col="orange",pch=16)
points(rbind(c(0,0),diff(yobs))[prevState %in% c(i,i+1),],col="grey",pch=16)
## Smallest sd
phin <- rwrpcauchy(ns,phis[j],exp(-exp(pars["logbeta"])))
sn <- rweibull(ns,scale=exp(2*pars[names(pars)=="logSdObs"][1]),shape=exp(2*pars[names(pars)=="logSdObs"][2]))
xn <- sn*cos(phin) ## DETTE PASSER IKKE !!!!
yn <- sn*sin(phin)
## contour(kde2d(phin,sn))
## points(phis[i+1],ss[i+1],col="red",pch=16)
contour(kde2d(xn,yn),xlab="dx",ylab="dy")
phin <- rwrpcauchy(ns,phis[j+1],exp(-exp(pars["logbeta"])))
sn <- rweibull(ns,scale=exp(2*pars[names(pars)=="logSdObs"][1]),shape=exp(2*pars[names(pars)=="logSdObs"][2]))
xn <- sn*cos(phin)
yn <- sn*sin(phin)
## contour(kde2d(phin,sn))
## points(phis[i+1],ss[i+1],col="red",pch=16)
contour(kde2d(xn,yn),add=TRUE,col=2)
points(ss[j+1]*cos(phis[j+1]),ss[j+1]*sin(phis[j+1]),col="blue",pch=16)
points(diff(fitdsbs$tmb_object$report()$x[c(j,j+1)]),diff(fitdsbs$tmb_object$report()$y[c(j,j+1)]),col="orange",pch=16)
points(rbind(c(0,0),diff(yobs))[prevState %in% c(j,j+1),],col="grey",pch=16)

}


objOld <- fitdsbs$tmb_object
pars <- objOld$env$parList(x=fitdsbs$optimization$par)
names.random <- unique(names(objOld$env$par[objOld$env$random]))
names.all <- names(pars)
fix <- setdiff(names.all, names.random)
map <- lapply(pars[fix], function(x)factor(x*NA))
mapNF <- objOld$env$map
objmc <- TMB::MakeADFun(objOld$env$data,
                        pars,
                        map,
                        DLL=objOld$env$DLL)
objmcNF <- TMB::MakeADFun(objOld$env$data,
                          pars,
                          mapNF,
                          DLL=objOld$env$DLL)

eps <- TMB::find.epsilon(objmc$par,objmc$fn,objmc$gr)
mcr <- TMB::mcmc(objmc,100000,"HMC",eps=min(c(eps,1)),L=5)
epsNF <- TMB::find.epsilon(objmcNF$par,objmcNF$fn,objmcNF$gr)
mcrNF <- TMB::mcmc(objmcNF,100000,"HMC",eps=epsNF,L=5)


munames <- rbind(paste0("x",1:args$nStates),paste0("y",1:args$nStates))
names(mcr)[names(mcr)=="mu"] <- as.vector(munames)
names(mcrNF)[names(mcrNF)=="mu"] <- as.vector(munames)



pairs(mcr[,as.vector(outer(c("x","y"),1:args$nStates,paste0)[1:2,])],
      panel = function(x,y,...){
          points(x,y,...)
          contour(MASS::kde2d(x,y),add=TRUE,col="red")
      },
      pch=16,cex=0.5,col=rgb(0,0,0,0.1))

dev.new()
pairs(mcrNF,
      panel = function(x,y,...){
          points(x,y,...)
          contour(MASS::kde2d(x,y),add=TRUE,col="red")
      },
      pch=16,cex=0.5,col=rgb(0,0,0,0.1))



