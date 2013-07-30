# install.packages("RHmm")
library("RHmm")

sunProb <- c(0.6, 0.2, 0.15, 0.05)
cloudProb <- c(0.25, 0.25, 0.25, 0.25)
rainProb <- c(0.05, 0.10, 0.35, 0.50)

weatherDist <- distributionSet(dis="DISCRETE", proba=list(sunProb, cloudProb, rainProb), labels =c("DRY", "DRYISH", "DAMP", "SOGGY"))
weatherTransitions <- rbind(c(0.5, 0.375, 0.125), c(0.25, 0.125, 0.625), c(0.25, 0.375, 0.375))

weatherHmm <- HMMSet(initProb=c(0.63, 0.17, 0.20), transMat=weatherTransitions, distribution=weatherDist)
weatherPath <- viterbi(HMM=weatherHmm, obs=c("DRY", "DAMP", "SOGGY", "SOGGY", "DRYISH"))

weatherPath