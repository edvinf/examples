library(Rstox)
projectpath <- "~/workspace/stox/project/Hyse_2018_Elise"
Rstox::runRScripts(projectpath)

prepReca <- Rstox::loadProjectData(projectpath)$prepareRECA
runReca <- Rstox::loadProjectData(projectpath)$runRECA

sampledata <- prepReca$StoxExport$biotic
landingdata <- prepReca$StoxExport$landing
resultdata <- runReca

saveRDS(sampledata, "NEA_HAD_2018_samples.rds")
saveRDS(landingdata, "NEA_HAD_2018_landings.rds")
saveRDS(resultdata, "NEA_HAD_2018_ECA_results.rds")