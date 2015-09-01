library(argosTrack)

context("Correct output of argosTrack")


dat <- argosTrack::subadult_ringed_seal

argsrw2 <- list(lon = dat$lon,
                lat = dat$lat,
                dates = as.character(dat$date),
                locationclass = dat$lc,
                verbose=TRUE,
                fixcorrection=FALSE,
                nauticalStates=TRUE,
                errordistribution="n",
                movementmodel="rw",
                timeunit="hours"
                )
fitrw2 <- do.call(argosTrack,argsrw2)
expect_is(fitrw2,"argostrack")
