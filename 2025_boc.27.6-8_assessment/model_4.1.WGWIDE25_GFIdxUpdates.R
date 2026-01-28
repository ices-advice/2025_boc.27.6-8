#WGWIDE 2025 Boarfish
#V4.1 - working dev version + update LAmin prior SD

rm(list=ls())
gc()

library(icesTAF)
library(r4ss)
library(parallel)
library(doParallel)

do.STF <- FALSE  #switch this on when run is finalised to save needless processing
do.retro <- TRUE

taf.bootstrap()

runName <- "4.1.WGWIDE25_GFIdxUpdates"

icesTAF::mkdir("model")

run.dir <- file.path("model",runName)

icesTAF::clean(run.dir)
icesTAF::mkdir(run.dir)

source("data.R")

# Get input files
icesTAF::cp("data/BOC.dat", "model")
icesTAF::cp("data/BOC.ctl", "model")
icesTAF::cp("data/forecast.ss", "model")
icesTAF::cp("data/starter.ss", "model")

#copy model executable
cp("boot/software/ss3_win.exe", "model")

#run assessment
r4ss::run(dir = "model", exe = "SS3_win.exe", show_in_console = TRUE, skipfinished = FALSE)
replist <- r4ss::SS_output(dir = "model", verbose = TRUE, printstats = TRUE)
replist$timeseries$SpawnBio[replist$timeseries$Yr==2025]/1e6
replist$maximum_gradient_component

#save the fit, copy outputs
save(replist, file=file.path("model",paste0(runName,"_fit.Rdata")))
TAF::cp(from = "model/*.sso", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/*.ss_new", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/*.log", to = paste0("model/",runName), quiet=FALSE)
TAF::cp(from = "model/ss3.par", to = paste0("model/",runName), quiet=FALSE)

#retrospectives
if (do.retro) {
  r4ss::retro(dir = "model", exe = "SS3_win.exe", oldsubdir = "", newsubdir = "Retrospective", years = 0:-5, 
              extras = "-nox", skipfinished = F, verbose = TRUE)
  
  retroModels <- r4ss::SSgetoutput(dirvec=file.path("model", "Retrospective",paste("retro",0:-5, sep="")))
  retroSummary <- r4ss::SSsummarize(retroModels)
  save(retroSummary, retroModels, file=file.path("model",paste0(runName,"_retro_fit.Rdata")))
}

#STF

if (do.STF) {

  #Reference Points
  Blim <- 156762
  Bpa <- 190845
  Flim <- 0.175
  Fp05 <- 0.042
  Fpa <- 0.042
  Fmsy <- 0.042
  MSYBtrigger <- 190845
  
  Naver <- 2  # number of average years-1
  Term.yr <- 2025 # terminal assessment year
  Adv.yr <- 2026   #advice year
  Adv.yr.next <- Adv.yr + 1 #year following advice year
  
  STF.dir <- file.path(run.dir,"STF")
  icesTAF::mkdir(file.path(STF.dir,"Scenarios"))
  
  #copy the forecast.ss from the assessment to the forecast folder
  icesTAF::cp("data/forecast.ss", STF.dir)
  
  #read in the forecast file, assessment output
  fore <- r4ss::SS_readforecast(file = file.path(STF.dir,"forecast.ss"),verbose = FALSE)
  replist <- r4ss::SS_output(dir = "model", verbose = TRUE, printstats = TRUE)
  
  #get the F time series
  dat <- replist$exploitation
  dat <- dat[-c(3,4,6,7)]
  head(dat)
  
  #number of forecast years
  Nfor = fore$Nforecastyrs
  #period for recent averages
  #average of the last Nfor+1 years across seasons and fleets - easy here as we only have 1 season and 1 fleet
  startyear = max(dat$Yr) - Nfor - Naver
  endyear = max(dat$Yr) - Nfor
  
  cat(startyear,endyear,"\n")
  
  data <- subset(dat,dat$Yr>=startyear & dat$Yr<=endyear)
  Fsq <- data$annual_F[data$Yr==endyear]
  data <- data[,-1] # remove year column
  data_int <- aggregate(.~Seas,data=data,mean) 
  
  #FMultipliers around FMSY
  #the multiplier of 1 will not lead to an F equivalent to FMSY as the F provided in the forecast is the apical F
  #it will be necessary to run a number of scenarios to achieve the desired F targets such as FMSY, status quo F, Fpa and Flim
  
  #broad scale of multipliers to investigate
  FMult <- c(0,seq(1,5,by=0.1))
  names(FMult) <- c(paste0("F_",Adv.yr," = 0"),rep("Scan",length(FMult)-1))
  
  #homing in...
  #on FMSY
  FMult.msy <- seq(1.164,1.166,by=0.001)
  names(FMult.msy) <- rep(paste0("F_",Adv.yr," = FMSY"),length(FMult.msy))
  FMult <- c(FMult,FMult.msy)
  
  #on Flim
  FMult.lim <- seq(4.855,4.857,by=0.001)
  names(FMult.lim) <- rep(paste0("F_",Adv.yr," = Flim"),length(FMult.lim))
  FMult <- c(FMult,FMult.lim)
  
  #on Fsq
  FMult.Fsq <- seq(0.811,0.813,by=0.001)
  names(FMult.Fsq) <- rep(paste0("F_",Adv.yr," = F_",Term.yr),length(FMult.Fsq))
  FMult <- c(FMult,FMult.Fsq)
  
  #on SSB = Blim in year following advice
  FMult.Blim <- seq(46.133,46.135,by=0.001)
  names(FMult.Blim) <- rep(paste0("SSB_",Adv.yr.next," = Blim"),length(FMult.Blim))
  FMult <- c(FMult,FMult.Blim)
  
  #on SSB = MSYBtrigger in year following advice
  FMult.MSYBtrigger <- seq(40.6462,40.6464,by=0.0001)
  names(FMult.MSYBtrigger) <- rep(paste0("SSB_",Adv.yr.next," = MSYBtrigger"),length(FMult.MSYBtrigger))
  FMult <- c(FMult,FMult.MSYBtrigger)
  
  l_FMult <- length(FMult)
  
  #catch scenarios
  #terminal year catch
  tCatch <- replist$catch$Obs[replist$catch$Yr==max(replist$catch$Yr)]
  catch.scenarios <- round(c(0.8*tCatch,tCatch,1.25*tCatch))
  names(catch.scenarios) <- c(paste0("Catch_",Adv.yr," = 0.8*Catch_",Term.yr),
                              paste0("Catch_",Adv.yr," = Catch_",Term.yr),
                              paste0("Catch_",Adv.yr," = 1.25*Catch_",Term.yr))
  l_Catch <- length(catch.scenarios)
  Catch_names <- paste0("Catch",catch.scenarios)
  
  for (i in 1:l_FMult){
    
    fore_dat = data.frame(Year = seq(endyear+1, endyear+Nfor))
    fore_dat$Seas = 1
    fore_dat$Fleet = 1
    fore_dat$F = FMult[i]*Fmsy
    
    fore$InputBasis <- 99
    fore$ControlRuleMethod <- 0
    fore$ForeCatch <- fore_dat
    
    ## write all forecast files/scenarios
    r4ss::SS_writeforecast(fore, dir = file.path(STF.dir,"Scenarios"), 
                           file = paste0("forecast",paste0("FMult_",FMult[i]), ".ss"), 
                           overwrite = TRUE, verbose = FALSE)
  }
  
  for (i in 1:l_Catch) {
    
    fore_dat = data.frame(Year = seq(endyear+1, endyear+Nfor))
    fore_dat$Seas = 1
    fore_dat$Fleet = 1
    fore_dat$Catch <- catch.scenarios[i]
    
    fore$InputBasis <- 2
    fore$ControlRuleMethod <- 0
    fore$ForeCatch <- fore_dat
    
    ## write all forecast files/scenarios
    r4ss::SS_writeforecast(fore, dir = file.path(STF.dir,"Scenarios"), 
                           file = paste0("forecast",Catch_names[i], ".ss"), 
                           overwrite = TRUE, verbose = FALSE)
    
  }
  
  # create forecast subfolders for F multiplier options & copy in files for the run
  for (i in 1:l_FMult){
    
    FMult.dir <- file.path(STF.dir,"Scenarios",paste0("FMult",FMult[i]))
    icesTAF::mkdir(FMult.dir)
    
    #copy the files for the assessment
    #cp("data/BOC.dat", "model")
    #cp("data/BOC.ctl", "model")
    #cp("data/forecast.ss", "model")
    icesTAF::cp("data/starter.ss", FMult.dir)
    #icesTAF::cp("data/ss.par", FMult.dir)
    icesTAF::cp("data/BOC.dat", FMult.dir)
    icesTAF::cp("data/BOC.ctl", FMult.dir)
    
    #copy the scenario forecast file into the scenario specific folder and rename to forecast.ss
    icesTAF::cp(file.path(STF.dir,"Scenarios",paste0("forecastFmult_",FMult[i],".ss")),
                file.path(FMult.dir, "forecast.ss"))
    
    # #Edit "starter.ss" 
    # starter.file <- readLines(paste(dir.FMult, "/starter.ss", sep=""))
    # linen <- NULL
    # # linen <- grep("# 0=use init values in control file; 1=use ss.par", starter.file)
    # # starter.file[linen] <- paste0("1 # 0=use init values in control file; 1=use ss.par") # tells it to use the estimate parameters
    # linen <- grep("#_init_values_src", starter.file) # # 0=use init values in control file; 1=use ss.par
    # starter.file[linen] <- paste0("1 #_init_values_src") # tells it to use the estimate parameters
    # 
    # # linen <- grep("# Turn off estimation for parameters entering after this phase", starter.file)
    # # starter.file[linen] <- paste0("0 # Turn off estimation for parameters entering after this phase")
    # linen <- grep("#_last_estimation_phase", starter.file)
    # starter.file[linen] <- paste0("0 #_last_estimation_phase")
    # write(starter.file, paste(dir.FMult, "/starter.ss", sep=""))
    
  }
  
  # create forecast subfolders for catch scenarios & copy in files for the run
  for (i in 1:l_Catch){
    
    Catch.dir <- file.path(STF.dir,"Scenarios",paste0("Catch",catch.scenarios[i]))
    icesTAF::mkdir(Catch.dir)
    
    icesTAF::cp("data/starter.ss", Catch.dir)
    #icesTAF::cp("data/ss.par", Catch.dir)
    icesTAF::cp("data/BOC.dat", Catch.dir)
    icesTAF::cp("data/BOC.ctl", Catch.dir)
    
    #copy the scenario forecast file into the scenario specific folder and rename to forecast.ss
    icesTAF::cp(file.path(STF.dir,"Scenarios",paste0("forecastCatch",catch.scenarios[i],".ss")),
                file.path(Catch.dir, "forecast.ss"))
    
    # #Edit "starter.ss" 
    # starter.file <- readLines(paste(dir.FMult, "/starter.ss", sep=""))
    # linen <- NULL
    # # linen <- grep("# 0=use init values in control file; 1=use ss.par", starter.file)
    # # starter.file[linen] <- paste0("1 # 0=use init values in control file; 1=use ss.par") # tells it to use the estimate parameters
    # linen <- grep("#_init_values_src", starter.file) # # 0=use init values in control file; 1=use ss.par
    # starter.file[linen] <- paste0("1 #_init_values_src") # tells it to use the estimate parameters
    # 
    # # linen <- grep("# Turn off estimation for parameters entering after this phase", starter.file)
    # # starter.file[linen] <- paste0("0 # Turn off estimation for parameters entering after this phase")
    # linen <- grep("#_last_estimation_phase", starter.file)
    # starter.file[linen] <- paste0("0 #_last_estimation_phase")
    # write(starter.file, paste(dir.FMult, "/starter.ss", sep=""))
    
  }
  
  runSS <- function(dir){r4ss::run(dir = dir, exe = "SS3_win.exe", 
                                   show_in_console = FALSE, skipfinished = FALSE)}
  
  #run the forecast models
  cat("Running",length(FMult),"F based forecasts\n")
  n.cores <- parallel::detectCores()
  registerDoParallel(cores=n.cores/2)
  system.time(foreach(dir = file.path(STF.dir,"Scenarios",paste0("FMult", FMult))) %dopar% runSS(dir))[3]
  registerDoSEQ()
  
  cat("Running",length(catch.scenarios),"catch based forecasts\n")
  n.cores <- parallel::detectCores()
  registerDoParallel(cores=n.cores/2)
  system.time(foreach(dir = file.path(STF.dir,"Scenarios", paste0("Catch",catch.scenarios))) %dopar% runSS(dir))[3]
  registerDoSEQ()
  
  #retrieve and summarise outputs
  forecastModels <- r4ss::SSgetoutput(dirvec = paste0("model/",runName,"/STF/Scenarios/", c(paste0("FMult",FMult),Catch_names)), getcovar = FALSE)
  save(FMult, catch.scenarios, forecastModels, file=file.path("model",paste0(runName,"_STF_fit.Rdata")))
} 

source("output.R")

replist$likelihoods_used[1,] #

# Get input files
icesTAF::cp("data/BOC.dat", "model")
icesTAF::cp("data/BOC.ctl", "model")
icesTAF::cp("data/forecast.ss", "model")
icesTAF::cp("data/starter.ss", "model")

#copy model executable
cp("boot/software/ss3_win.exe", "model")

jit.likes <- r4ss::jitter(dir = "model", exe = "SS3_win.exe",
                          Njitter=25, jitter_fraction = 0.1, init_values_src = 0)

#likesaved
#615.883 618.392 620.441 625.272  627.99 628.321 630.239  636.84 675.479 847.901 877.403 877.493 921.606 5909.47 
#6       2       1       2       1       1       1       1       1       2       3       1       1       1 
#16542.2 
#1 

profilemodels <- SSgetoutput(dirvec = "model", keyvec = 1:25, getcovar = FALSE)
profilesummary <- SSsummarize(profilemodels)
profilesummary[["likelihoods"]][1, ]
profilesummary[["pars"]]



