#generate the outputs from stock assessment for boc.27.6-8

library(icesTAF)
library(r4ss)
library(tidyverse)
library(ss3diags)
library(Cairo)
library(officer)
library(flextable)

#Reference Points as per 2024 benchmark
Blim <- 156762
Bpa <- 190845
Flim <- 0.175
Fp05 <- 0.042
Fpa <- 0.042
Fmsy <- 0.042
MSYBtrigger <- 190845

mkdir("output")
op.dir <- file.path("output",runName)
mkdir(file.path(op.dir,"retros"))
mkdir(file.path(op.dir,"diags"))
mkdir(file.path(op.dir,"STF"))

load(file.path("model",paste0(runName,"_fit.Rdata")))

retros.done <- file.exists(file.path("model",paste0(runName,"_retro_fit.Rdata")))
STF.done <- file.exists(file.path("model",paste0(runName,"_STF_fit.Rdata")))

if (retros.done) load(file.path("model",paste0(runName,"_retro_fit.Rdata")))
if (STF.done) load(file.path("model",paste0(runName,"_STF_fit.Rdata")))

#main run outputs - pdf format & html
r4ss::SS_plots(replist, pdf = TRUE, png = FALSE, dir = paste0("output/",runName))
r4ss::SS_plots(replist, dir = paste0("output/",runName), printfolder = "SS_Plots")

#table of parameters
table_pars <- r4ss::table_pars(replist)

#filter for those estimate & exclude deviations
dfPars <- data.frame(table_pars[["table"]] %>% 
  filter(Phase>0) %>% 
  filter(!grepl("DEV",toupper(Label))))

ft <- flextable::flextable(dfPars) %>%
  set_table_properties(layout="autofit")

doc <- read_docx() %>%
  body_add_par(paste0(runName," Parameter Estimates"),style = "centered") %>%
  body_add_par(Sys.time(),style = "centered") %>%
  body_add_par("") %>%
  body_add_flextable(value = ft, split = TRUE) %>%
  body_end_section_landscape() %>% 
  print(target = file.path("output",runName,"ParameterEstimates.docx"))

#retro outputs
if (retros.done) {
  # Set the ending year of each model in the set
  endyrvec <- retroSummary$endyrs + 0:-5
   
  ###Calculate Mohn's Rho values for select quantities
  rho.SSB <- r4ss::SSmohnsrho(retroSummary, endyrvec = endyrvec+1, verbose = TRUE)
  rho.RecFBar <- r4ss::SSmohnsrho(retroSummary, endyrvec = endyrvec, verbose = TRUE)
  
  cat("rho.SSB$AFSC_Hurtado_SSB",rho.SSB$AFSC_Hurtado_SSB,"\n") #0.22
  cat("rho.RecFBar$AFSC_Hurtado_F",rho.RecFBar$AFSC_Hurtado_F,"\n") #-0.19
  cat("rho.RecFBar$AFSC_Hurtado_Rec",rho.RecFBar$AFSC_Hurtado_Rec,"\n") #0.12
   
  #SSB retroplot
  r4ss::SSplotComparisons(retroSummary, subplots = 2, endyrvec = endyrvec+1, plot=F, 
                         legendlabels = paste("Data",0:-5,"years"), 
                         png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T, 
                         pwidth = 4, pheight = 4, filenameprefix = "SSB", legendloc = "topleft",
                         labels = c("Year","SSB (kt)"), lwd=1, pch=20, xlim = c(1997,2026))
   
  retroSummary$FvalueLabels <- "FBar (4-14)"
  
  #F retro
  r4ss::SSplotComparisons(retroSummary, subplots = 8, endyrvec = endyrvec+1, plot=F,
                          legendlabels = paste("Data",0:-5,"years"),
                          png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "FBar", legendloc = "topleft",
                          labels = c("Year"), lwd=1, pch=20, xlim = c(1997,2026))

  r4ss::SSplotComparisons(retroSummary, subplots = 8, plot=T, endyrvec = endyrvec+1,
                          legendlabels = paste("Data",0:-5,"years"),
                          uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "FBar", legendloc = "topleft",
                          labels = c("Year"), lwd=1, pch=20, xlim = c(1997,2026))
  
    
  #recruits
  r4ss::SSplotComparisons(retroSummary, subplots = 10, endyrvec = endyrvec, plot=F,
                          legendlabels = paste("Data",0:-5,"years"),
                          png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "Recr", legendloc = "topleft",
                          labels = c("Year","","","Recruits (billions)"), lwd=1, pch=20, xlim = c(1997,2025))
  
  #recent recruits
  r4ss::SSplotComparisons(retroSummary, subplots = 10, endyrvec = endyrvec, plot=F,
                          legendlabels = paste("Data",0:-5,"years"),
                          png = T, plotdir = paste0("output/",runName,"/retros"), uncertainty = T,
                          pwidth = 4, pheight = 4, filenameprefix = "Recr_recent_", legendloc = "topright",
                          labels = c("Year","","","Recruits (billions)"), lwd=1, pch=20, xlim = c(2015,2025))
  
  
  #Reference run
  ss3rep = retroModels[[1]]
  
  # #retro test calculations
  # base <- retroModels[[1]]
  # peel1 <- retroModels[[2]]
  # peel2 <- retroModels[[3]]
  # peel3 <- retroModels[[4]]
  # peel4 <- retroModels[[5]]
  # peel5 <- retroModels[[6]]
  # 
  # #FBar
  # 0.2*(
  # (peel1$derived_quants["F_2024","Value"]-base$derived_quants["F_2024","Value"])/base$derived_quants["F_2024","Value"] +
  # (peel2$derived_quants["F_2023","Value"]-base$derived_quants["F_2023","Value"])/base$derived_quants["F_2023","Value"] +
  # (peel3$derived_quants["F_2022","Value"]-base$derived_quants["F_2022","Value"])/base$derived_quants["F_2022","Value"] +
  # (peel4$derived_quants["F_2021","Value"]-base$derived_quants["F_2021","Value"])/base$derived_quants["F_2021","Value"] +
  # (peel5$derived_quants["F_2020","Value"]-base$derived_quants["F_2020","Value"])/base$derived_quants["F_2020","Value"])
  # 
  # #Rec
  # 0.2*(
  # (peel1$derived_quants["Recr_2024","Value"]-base$derived_quants["Recr_2024","Value"])/base$derived_quants["Recr_2024","Value"] +
  # (peel2$derived_quants["Recr_2023","Value"]-base$derived_quants["Recr_2023","Value"])/base$derived_quants["Recr_2023","Value"] +
  # (peel3$derived_quants["Recr_2022","Value"]-base$derived_quants["Recr_2022","Value"])/base$derived_quants["Recr_2022","Value"] +
  # (peel4$derived_quants["Recr_2021","Value"]-base$derived_quants["Recr_2021","Value"])/base$derived_quants["Recr_2021","Value"] +
  # (peel5$derived_quants["Recr_2020","Value"]-base$derived_quants["Recr_2020","Value"])/base$derived_quants["Recr_2020","Value"])
  # 
  # #SSB (reported on Jan 1 of following year)
  # 0.2*(
  # (peel1$derived_quants["SSB_2025","Value"]-base$derived_quants["SSB_2025","Value"])/base$derived_quants["SSB_2025","Value"] +
  # (peel2$derived_quants["SSB_2024","Value"]-base$derived_quants["SSB_2024","Value"])/base$derived_quants["SSB_2024","Value"] +
  # (peel3$derived_quants["SSB_2023","Value"]-base$derived_quants["SSB_2023","Value"])/base$derived_quants["SSB_2023","Value"] +
  # (peel4$derived_quants["SSB_2022","Value"]-base$derived_quants["SSB_2022","Value"])/base$derived_quants["SSB_2022","Value"] +
  # (peel5$derived_quants["SSB_2021","Value"]-base$derived_quants["SSB_2021","Value"])/base$derived_quants["SSB_2021","Value"])
  

  #data plot
  Cairo(file = file.path(getwd(),"output",runName,"diags","datasetup.png"), 
        type = "png", width = 6, height = 4, res = 300, units = "in")
  r4ss::SSplotData(ss3rep, subplots = 2)
  dev.off()
  
  #residuals
  
  #indices
  
  nIdx <- nrow(replist$index_variance_tuning_check)
  
  for (f in seq(1,nIdx)){
    ss3diags::SSplotRunstest(ss3rep, subplots="cpue", indexselect=f, mixing="two.sided", 
                             print = TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Idx_")
  }
  
  #multi plot
  sspar(mfrow=c(1,nIdx),plot.cex = 0.8)
  
  ss3diags::SSplotRunstest(ss3rep, subplots="cpue", add=T,
                           legendcex = 0.6, mixing="two.sided",
                           plotdir = paste0("output/",runName,"/diags"),
                           filenameprefix = "Idx_All")
  
  dev.print(jpeg, paste0("output/",runName,"/diags/Idx_residruns.jpg"),
            width = 8, height = 7, res = 300, units = "in")
  
  
  #Mean Length
  
  sspar()
  
  nFleets <- length(unique(replist$len_comp_fit_table$Fleet))
  
  for (f in seq(1,nFleets)) {
    ss3diags::SSplotRunstest(ss3rep, subplots="len", indexselect=f, mixing="two.sided", 
                             print=TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Len_")
  }
  
  
  #multi plot
  sspar(mfrow=c(1,nFleets),plot.cex = 0.8)
  
  SSplotRunstest(ss3rep, subplots="len",add=T, legendcex = 0.6, mixing="two.sided")
  
  dev.print(jpeg, paste0("output/",runName,"/diags/Len_residruns.jpg"),
            width = 8, height = 7, res = 300, units = "in")
  
  
  #check for conflicts between indices and mean len
  ss3diags::SSplotJABBAres(ss3rep, subplots="len", col=sscol(3)[c(1,3,2)],
                           print = TRUE, plotdir = paste0("output/",runName,"/diags"), 
                           filenameprefix = "Len_")
  
  ss3diags::SSplotJABBAres(ss3rep, subplots="cpue", col=sscol(3)[c(1,3,2)],
                           print = TRUE, plotdir = paste0("output/",runName,"/diags"), 
                           filenameprefix = "Idx_")
  
  
  sspar(mfrow=c(1,2),plot.cex = 0.8)
  SSplotJABBAres(ss3rep,subplots="cpue",add=T,col=sscol(3)[c(1,3,2)])
  SSplotJABBAres(ss3rep,subplots="len",add=T,col=sscol(3)[c(1,3,2)])
  
  dev.print(jpeg, paste0("output/",runName,"/diags/JointResids.jpg"),
            width = 8, height = 7, res = 300, units = "in")
  
  
  
  
  
  sspar(mfrow=c(1,2),plot.cex = 0.8)
  #Survey Indices
  
  ss3diags::SSplotRunstest(ss3rep, subplots="cpue", add=T,
                           legendcex = 0.6, mixing="two.sided",
                           plotdir = paste0("output/",runName,"/diags"),
                           filenameprefix = "Idx_All")
  
  #dev.print(jpeg, file.path(diag.dir,paste0("RunsTestResiduals_Idx_",runref,".jpg")), 
  #          width = 8, height = 7, res = 300, units = "in")
  
  
  
  #retro analysis with hindcasting
  
  if (retros.done){
    for (idx in seq(1,nIdx)) {
      ss3diags::SSplotHCxval(retroSummary, subplots="cpue", indexselect=idx, xmin=2006,
                             print=TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Index_")
    }
    
    hccomps = ss3diags::SSretroComps(retroModels)
    
    for (flt in seq(1,nFleets)){
      ss3diags::SSplotHCxval(hccomps, subplots="len", indexselect=flt, ylim=c(5,16),
                             uncertainty=TRUE, print=TRUE, plotdir = paste0("output/",runName,"/diags"), 
                             filenameprefix = "Len_")
    }
  }
  
}


#STF

if (STF.done) {

  #build a summary of the forecast runs
  forecastSummary <- r4ss::SSsummarize(forecastModels)
  
  #SSB, F, Recr
  SSB <- as.data.frame(forecastSummary[["SpawnBio"]])
  SSB.SD <- as.data.frame(forecastSummary[["SpawnBioSD"]])
  SSB.Lower <- as.data.frame(forecastSummary[["SpawnBioLower"]])
  SSB.Upper <- as.data.frame(forecastSummary[["SpawnBioUpper"]])
  Fvalue <- as.data.frame(forecastSummary[["Fvalue"]])
  Recr <- as.data.frame(forecastSummary[["recruits"]])
  
  #summarise results
  all.scen <- c(FMult,catch.scenarios)
  num.scen <- length(all.scen)
  dfSummary <- 
    data.frame(Scenario.Type = c(rep("FMult",length(FMult)),(rep("Catch",length(catch.scenarios)))),
               Scenario.Name = c(names(FMult),names(catch.scenarios)),
               Val = all.scen,
               SSB_2026 = as.numeric(SSB[SSB$Yr==2026,paste0('replist',seq(1,num.scen))]),
               SSB_2026_SD = as.numeric(SSB.SD[SSB$Yr==2026,paste0('replist',seq(1,num.scen))]),
               SSB_2027 = as.numeric(SSB[SSB$Yr==2027,paste0('replist',seq(1,num.scen))]),
               SSB_2027_SD = as.numeric(SSB.SD[SSB$Yr==2027,paste0('replist',seq(1,num.scen))]),
               F_2025 = as.numeric(Fvalue[Fvalue$Yr==2025,paste0('replist',seq(1,num.scen))]),
               F_2026 = as.numeric(Fvalue[Fvalue$Yr==2026,paste0('replist',seq(1,num.scen))]),
               F_2027 = as.numeric(Fvalue[Fvalue$Yr==2027,paste0('replist',seq(1,num.scen))]),
               Rec_2026 = as.numeric(Recr[Recr$Yr==2026,paste0('replist',seq(1,num.scen))]),
               Rec_2027 = as.numeric(Recr[Recr$Yr==2027,paste0('replist',seq(1,num.scen))]),
               Catch_2026 = NA, Catch_2027 = NA, Fmsy=Fmsy, Blim=Blim, Adv_2025 = 38295, Catch_2025 = 40172)
  
  #probablility of being below Blim
  dfSummary$pBlim_2027 <- pnorm(Blim, dfSummary$SSB_2027, dfSummary$SSB_2027_SD)
  
  #catches
  for (i in 1:num.scen){
    output = forecastModels[[i]]
    catch <- output$timeseries %>% filter(Era == "FORE" ) %>% 
      select("Yr", starts_with("dead(B)")) %>% 
      mutate(FMult = FMult[i])
    names(catch) <- c("Year","Catch","FMult")
    catch <- catch %>% pivot_wider(names_from = Year, values_from = Catch, names_prefix = "Catch_")
    dfSummary$Catch_2026[i] <- catch$Catch_2026
    dfSummary$Catch_2027[i] <- catch$Catch_2027
  }
  
  # ggplot(data = dfSummary %>% filter(Scenario.Type=="FMult" & F_2026<2*max(Flim,Fpa,Fmsy)), 
  #        mapping = aes(x=F_2026,y=Catch_2026,label=Val)) + 
  #   xlim(0.0415,0.0425) +
  #   geom_point() + geom_label() + 
  #   geom_vline(xintercept = Fmsy, col="red") +
  #   geom_vline(xintercept = Flim, col="red") +
  #   geom_vline(xintercept = Fpa, col="red")  +
  #   geom_vline(xintercept = data_int$Fishery, col="red") 
  
  dfSummary$Delta <- NA
  dfSummary$Selected <- FALSE
  
  #best approximation for Fmsy, Fpa in 2026
  dfSummary[dfSummary$Scenario.Name=="F_2026 = FMSY",]$Delta <- abs(dfSummary[dfSummary$Scenario.Name=="F_2026 = FMSY",]$F_2026 - Fmsy)
  dfSummary[dfSummary$Scenario.Name=="F_2026 = FMSY",]$Selected <- dfSummary[dfSummary$Scenario.Name=="F_2026 = FMSY",]$Delta == min(dfSummary[dfSummary$Scenario.Name=="F_2026 = FMSY",]$Delta)
  
  dfSummary %>% filter(Scenario.Name=="F_2026 = FMSY")
  
  #F=0
  dfSummary[dfSummary$Scenario.Name=="F_2026 = 0",]$Selected <- TRUE
  
  #best approximation for Flim in 2026
  dfSummary[dfSummary$Scenario.Name=="F_2026 = Flim",]$Delta <- abs(dfSummary[dfSummary$Scenario.Name=="F_2026 = Flim",]$F_2026 - Flim)
  dfSummary[dfSummary$Scenario.Name=="F_2026 = Flim",]$Selected <- dfSummary[dfSummary$Scenario.Name=="F_2026 = Flim",]$Delta == min(dfSummary[dfSummary$Scenario.Name=="F_2026 = Flim",]$Delta)
  
  dfSummary %>% filter(Scenario.Name=="F_2026 = Flim")
  
  #Fsq
  Fsq <- dfSummary$F_2025[1]
  ggplot(data = dfSummary %>% filter(Scenario.Type=="FMult" & F_2026 > 0.8*Fsq & F_2026<1.22*Fsq), 
         mapping = aes(x=F_2026,y=Catch_2026,label=Val)) + 
    geom_point() + geom_label() + 
    geom_vline(xintercept = Fsq, col="red")
  
  dfSummary[dfSummary$Scenario.Name=="F_2026 = F_2025",]$Delta <- abs(dfSummary[dfSummary$Scenario.Name=="F_2026 = F_2025",]$F_2026 - Fsq)
  dfSummary[dfSummary$Scenario.Name=="F_2026 = F_2025",]$Selected <- dfSummary[dfSummary$Scenario.Name=="F_2026 = F_2025",]$Delta == min(dfSummary[dfSummary$Scenario.Name=="F_2026 = F_2025",]$Delta)
  
  dfSummary %>% filter(Scenario.Name=="F_2026 = F_2025")
  
  
  #SSB2027 vs F_2026
  ggplot(data = dfSummary %>% filter(Scenario.Type=="FMult" & SSB_2027 > 0.9*Blim & SSB_2027 < 1.1*Blim), 
         mapping = aes(x=F_2026,y=SSB_2027,label=Val)) + 
    geom_point() + 
    geom_label(nudge_x = 0.01) +
    geom_hline(yintercept = Blim, col="red")
  
  #SSB 2027 & Blim
  dfSummary[dfSummary$Scenario.Name=="SSB_2027 = Blim",]$Delta <- abs(dfSummary[dfSummary$Scenario.Name=="SSB_2027 = Blim",]$SSB_2027 - Blim)
  dfSummary[dfSummary$Scenario.Name=="SSB_2027 = Blim",]$Selected <- dfSummary[dfSummary$Scenario.Name=="SSB_2027 = Blim",]$Delta == min(dfSummary[dfSummary$Scenario.Name=="SSB_2027 = Blim",]$Delta)
  
  dfSummary %>% filter(Scenario.Name=="SSB_2027 = Blim")
  
  #SSB 2027 & MSYBtrigger
  ggplot(data = dfSummary %>% filter(Scenario.Type=="FMult" & SSB_2027 > 0.9*MSYBtrigger & SSB_2027 < 1.1*MSYBtrigger), 
         mapping = aes(x=F_2026,y=SSB_2027,label=Val)) + 
    geom_point() + 
    geom_label(nudge_x = 0.01) +
    geom_hline(yintercept = MSYBtrigger, col="red")
  
  dfSummary[dfSummary$Scenario.Name=="SSB_2027 = MSYBtrigger",]$Delta <- abs(dfSummary[dfSummary$Scenario.Name=="SSB_2027 = MSYBtrigger",]$SSB_2027 - MSYBtrigger)
  dfSummary[dfSummary$Scenario.Name=="SSB_2027 = MSYBtrigger",]$Selected <- dfSummary[dfSummary$Scenario.Name=="SSB_2027 = MSYBtrigger",]$Delta == min(dfSummary[dfSummary$Scenario.Name=="SSB_2027 = MSYBtrigger",]$Delta)
  
  dfSummary %>% filter(Scenario.Name=="SSB_2027 = MSYBtrigger")
  
  
  #catch constraints
  dfSummary$Selected[dfSummary$Scenario.Name=="Catch_2026 = 0.8*Catch_2025"] <- TRUE
  dfSummary$Selected[dfSummary$Scenario.Name=="Catch_2026 = Catch_2025"] <- TRUE
  dfSummary$Selected[dfSummary$Scenario.Name=="Catch_2026 = 1.25*Catch_2025"] <- TRUE
  
  dfOptions <- dfSummary %>% 
    filter(Selected) %>%
    mutate(Catch_2026 = round(Catch_2026),
           F_2026 = round(F_2026,3),
           SSB_chg = round(100*((SSB_2027/SSB_2026)-1),3), 
           Catch_chg = round(100*((Catch_2026/Catch_2025)-1),3), 
           Adv_chg = round(100*((Catch_2026/Adv_2025)-1),3),
           pBlim_2027 = round(pBlim_2027,2)) %>%
    select(Scenario.Name,Catch_2026,F_2026,SSB_2027,SSB_chg,Catch_chg,Adv_chg,pBlim_2027)
  
  #re-order for table
  dfOptions <- dfOptions[c(2,1,3,4,5,6,7,8,9),]
  
  library(flextable)
  library(officer)
  
  doc <- officer::read_docx()
  ftOptions <- flextable(dfOptions)
  ftOptions <- flextable::colformat_num(ftOptions,j=2,big.mark="",decimal.mark = ".")
  ftOptions <- flextable::colformat_num(ftOptions,j=4,big.mark="",decimal.mark = ".")
  ftOptions <- flextable::colformat_num(ftOptions,j=6,big.mark="",decimal.mark = ".")
  ftOptions <- flextable::autofit(ftOptions)
  doc <- flextable::body_add_flextable(doc,ftOptions)
  print(doc,target=file.path(op.dir,"STFOptions.docx"))
  
}


