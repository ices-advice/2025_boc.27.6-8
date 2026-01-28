#WGWIDE 2025 Boarfish
#2.3.WGWIDE24_Q with quarterly time step and acoustic survey length compositions

library(icesTAF)
library(r4ss)

taf.bootstrap()

runName <- "2.3.WGWIDE24_Q_withAcoLComp"

icesTAF::mkdir("model")
icesTAF::mkdir(paste0("model/",runName))

source("data.R")

# Get input files
cp("data/BOC.dat", "model")
cp("data/BOC.ctl", "model")
cp("data/forecast.ss", "model")
cp("data/starter.ss", "model")

#copy model executable (this is the version used in 2024)
cp("boot/software/ss_win.exe", "model")

#run assessment
r4ss::run(dir = "model", exe = "SS_win.exe", show_in_console = TRUE, skipfinished = FALSE)
replist <- r4ss::SS_output(dir = "model", verbose = TRUE, printstats = TRUE)
replist$timeseries$SpawnBio[replist$timeseries$Yr==2024]/1e6
replist$maximum_gradient_component

#save the fit, copy outputs
save(replist, file=file.path("model",paste0(runName,"_fit.Rdata")))
TAF::cp(from = "model/*.sso", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/*.ss_new", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/*.log", to = paste0("model/",runName), quiet=FALSE)

#retrospectives
r4ss::retro(dir = "model", exe = "SS_win.exe", oldsubdir = "", newsubdir = "Retrospective", years = 0:-5, 
            extras = "-nox", skipfinished = F, verbose = TRUE)

retroModels <- r4ss::SSgetoutput(dirvec=file.path("model", "Retrospective",paste("retro",0:-5, sep="")))
retroSummary <- r4ss::SSsummarize(retroModels)
save(retroSummary, retroModels, file=file.path("model",paste0(runName,"_retro_fit.Rdata")))

source("output.R")
