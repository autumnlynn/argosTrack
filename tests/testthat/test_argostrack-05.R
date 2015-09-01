library(argosTrack)

context("Correct output of argosTrack")


dat <- argosTrack::subadult_ringed_seal

argsdsb <- list(lon = dat$lon,
                lat = dat$lat,
                dates = as.character(dat$date),
                locationclass = dat$lc,
                verbose=TRUE,
                fixcorrection=FALSE,
                errordistribution="n",
                movementmodel="dsb",
                nauticalStates = TRUE,
                nauticalObs = TRUE,
                nStates = round(length(dat$lon)/20),
                timeunit="hours"
                )
fitdsb <- do.call(argosTrack,argsdsb)
expect_is(fitdsb,"argostrack")
expect_that(plot(fitdsb), not(gives_warning()))
expect_that(plot(summary(fitdsb)), not(gives_warning()))
expect_that(plot(summary(fitdsb),bearings=TRUE), not(gives_warning()))
