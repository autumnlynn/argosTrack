
library(argosTrack)

context("Correct output of argosTrack")


dat <- argosTrack::subadult_ringed_seal

argsrw <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=FALSE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="rw",
             timeunit="hours"
             )
fitrw <- do.call(argosTrack,argsrw)
expect_is(fitrw,"argostrack")

argsrw2 <- list(lon = dat$lon,
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
fitrw2 <- do.call(argosTrack,argsrw2)
expect_is(fitrw2,"argostrack")

argsctcrw <- list(lon = dat$lon,
                lat = dat$lat,
                dates = as.character(dat$date),
                locationclass = dat$lc,
                verbose=FALSE,
                fixcorrection=FALSE,
                errordistribution="n",
                movementmodel="ctcrw",
                timeunit="hours"
                )
fitctcrw <- do.call(argosTrack,argsctcrw)
expect_is(fitctcrw,"argostrack")


argsdtcrw <- list(lon = dat$lon,
                lat = dat$lat,
                dates = as.character(dat$date),
                locationclass = dat$lc,
                verbose=FALSE,
                fixcorrection=FALSE,
                errordistribution="n",
                  movementmodel="dtcrw",
                  nStates = round(length(dat$lon)/10),
                timeunit="hours"
                )
fitdtcrw <- do.call(argosTrack,argsdtcrw)
expect_is(fitctcrw,"argostrack")



argsdsb <- list(lon = dat$lon,
                lat = dat$lat,
                dates = as.character(dat$date),
                locationclass = dat$lc,
                verbose=FALSE,
                fixcorrection=FALSE,
                errordistribution="n",
                  movementmodel="dsb",
                  nStates = round(length(dat$lon)/10),
                timeunit="hours"
                )
fitdtsb <- do.call(argosTrack,argsdsb)
expect_is(fitsb,"argostrack")
