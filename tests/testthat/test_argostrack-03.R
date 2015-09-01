library(argosTrack)

context("Correct output of argosTrack")


dat <- argosTrack::subadult_ringed_seal

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
expect_that(plot(fitctcrw), not(gives_warning()))
expect_that(plot(summary(fitctcrw)), not(gives_warning()))
expect_that(plot(summary(fitctcrw),bearings=TRUE), not(gives_warning()))
