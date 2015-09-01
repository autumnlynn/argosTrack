library(argosTrack)

context("Correct output of argosTrack")


dat <- argosTrack::subadult_ringed_seal

argsdtcrw <- list(lon = dat$lon,
                lat = dat$lat,
                dates = as.character(dat$date),
                locationclass = dat$lc,
                verbose=TRUE,
                fixcorrection=FALSE,
                errordistribution="n",
                  movementmodel="dtcrw",
                  nStates = round(length(dat$lon)/10),
                timeunit="hours"
                )
fitdtcrw <- do.call(argosTrack,argsdtcrw)
expect_is(fitctcrw,"argostrack")
