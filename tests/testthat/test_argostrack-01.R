
library(argosTrack)

context("Correct output of argosTrack")


dat <- argosTrack::subadult_ringed_seal

argsrw <- list(lon = dat$lon,
             lat = dat$lat,
             dates = as.character(dat$date),
             locationclass = dat$lc,
             verbose=TRUE,
             fixcorrection=FALSE,
             errordistribution="n",
             movementmodel="rw",
             timeunit="hours"
             )
fitrw <- do.call(argosTrack,argsrw)
expect_is(fitrw,"argostrack")
