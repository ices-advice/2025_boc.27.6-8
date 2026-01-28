#Stock Summary

rm(list=ls())
gc()

library(tidyverse)
library(Cairo)
library(flextable)

runName <- "WGWIDE25_Final"
showForecastPeriod <- TRUE

#Reference Points as per 2024 benchmark
Blim <- 156762
Bpa <- 190845
Flim <- 0.175
Fp05 <- 0.042
Fpa <- 0.042
Fmsy <- 0.042
MSYBtrigger <- 190845

dfRefPts <- data.frame(
  Name = c("Blim","Bpa","Flim","Fp05","Fpa","Fmsy","MSYBtrigger"),
  Val = c(156.762,190.845,0.175,0.042,0.042,0.042,190.845),
  Qty = c("SSB (kt)","SSB (kt)","FBar (4-14)","FBar (4-14)","FBar (4-14)","FBar (4-14)","SSB (kt)")
)

load(file.path("model",paste0(runName,"_retro_fit.Rdata")))
load(file.path("model",paste0(runName,"_STF_fit.Rdata")))

#need forecast for this
FMSY.model <- forecastModels[[3]]

# Reference run
ss3rep = retroModels[[1]]
#forecast models
#forecastSummary <- SSsummarize(forecastModels)

yrs <- ss3rep$timeseries$Yr[ss3rep$timeseries$Era=="TIME"]
#SSB is given in the first year of the forecast period
yrs.ssb <- c(yrs,max(yrs)+1)


dfStockSummary <- bind_rows(
  data.frame(Year = c(yrs.ssb),
             Qty = "SSB (kt)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
             Era = "TIME",
             WG = "WGWIDE25"),
  data.frame(Year = yrs,
             Qty = "FBar (4-14)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
             Era = "TIME",
             WG = "WGWIDE25"),
  data.frame(Year = yrs,
             Qty = "Recr (billions)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
             Era = "TIME",
             WG = "WGWIDE25"),
  data.frame(Year = yrs,
             Qty = "Catch (kt)",
             Val = ss3rep$catch$Obs[ss3rep$catch$Yr %in% yrs]/1e3,
             SD = 0,
             Era = "TIME",
             WG = "WGWIDE25")
)
  
#append previous assessment for comparison
#previous assessment
load(file.path("model",paste0("1.0.WGWIDE24","_retro_fit.Rdata")))
ss3rep <- retroModels[[1]]

yrs <- ss3rep$timeseries$Yr[ss3rep$timeseries$Era=="TIME"]
#SSB is given in the first year of the forecast period
yrs.ssb <- c(yrs,max(yrs)+1)

dfStockSummary <- bind_rows(
  dfStockSummary,
  data.frame(Year = c(yrs.ssb),
             Qty = "SSB (kt)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("SSB_",yrs.ssb)]/1e3,
             Era = "TIME",
             WG = "WGWIDE24"),
  data.frame(Year = yrs,
             Qty = "FBar (4-14)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("F_",yrs)],
             Era = "TIME",
             WG = "WGWIDE24"),
  data.frame(Year = yrs,
             Qty = "Recr (billions)",
             Val = ss3rep$derived_quants$Value[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
             SD = ss3rep$derived_quants$StdDev[ss3rep$derived_quants$Label %in% paste0("Recr_",yrs)]/1e6,
             Era = "TIME",
             WG = "WGWIDE24"),
  data.frame(Year = yrs,
             Qty = "Catch (kt)",
             Val = ss3rep$catch$Obs[ss3rep$catch$Yr %in% yrs]/1e3,
             SD = 0,
             Era = "TIME",
             WG = "WGWIDE24")
)

if (showForecastPeriod) {

  #manually add forecast period details for FMSY
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2026, Qty = "Catch (kt)", Val = 29.720, SD=0, Era = "FORE",
                WG = "WGWIDE25")
   )
   
   #FBar
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2026, Qty = "FBar (4-14)", 
                Val = FMSY.model$derived_quants$Value[FMSY.model$derived_quants$Label=="F_2026"], 
                SD = FMSY.model$derived_quants$StdDev[FMSY.model$derived_quants$Label=="F_2026"], Era = "FORE",
                WG = "WGWIDE25")
   )
   
   #Recruits
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2026, Qty = "Recr (billions)", 
                Val = FMSY.model$derived_quants$Value[FMSY.model$derived_quants$Label=="Recr_2026"]/1e6, 
                SD = FMSY.model$derived_quants$StdDev[FMSY.model$derived_quants$Label=="Recr_2026"]/1e6, Era = "FORE",
                WG = "WGWIDE25")
   )
  
   #SSB
   dfStockSummary <- dplyr::bind_rows(
     dfStockSummary,
     data.frame(Year = 2027, 
                Qty = "SSB (kt)", 
                Val = FMSY.model$derived_quants$Value[FMSY.model$derived_quants$Label=="SSB_2027"]/1e3,
                SD = FMSY.model$derived_quants$StdDev[FMSY.model$derived_quants$Label=="SSB_2027"]/1e3, 
                Era = "FORE",
                WG = "WGWIDE25")
   )
  
}

#summary plot with RPs
p <- ggplot(data = dfStockSummary %>% filter(WG=="WGWIDE25"), 
            mapping = aes(x=Year,y=Val,col=Era)) + geom_line() + geom_point(size=2,pch=19) + 
  geom_errorbar(aes(ymin=Val-2*SD, ymax=Val+2*SD), width=.2, position=position_dodge(0.05)) +
  geom_hline(data = filter(dfRefPts, Name %in% c("Blim","Fmsy")), aes(yintercept = Val), linetype=2) + 
  theme_bw() + facet_wrap(~Qty, scales="free_y",ncol=2) + ylab("") + theme(legend.position = "right")


Cairo(file = file.path(getwd(),"output",runName,"StockSummary.png"), 
      type = "png", width = 9, height = 9, res = 300, units = "in")
print(p)
dev.off()


ggplot(data = dfStockSummary, mapping = aes(x=Year,y=Val,col=Era)) + geom_line() + geom_point(size=2) + 
  geom_errorbar(aes(ymin=Val-2*SD, ymax=Val+2*SD), width=.2, position=position_dodge(0.05)) +
  geom_hline(data = filter(dfRefPts, Name %in% c("Blim","Fmsy")), aes(yintercept = Val), linetype=2) + 
  theme_bw() + facet_wrap(~Qty, scales="free_y",ncol=2) + ylab("")

doc <- officer::read_docx()
ftStockSummary <- flextable(dfStockSummary %>% 
  filter(Era=="TIME" & WG == "WGWIDE25") %>%
  select(-SD,-Era,-WG) %>% 
  pivot_wider(names_from = Qty, values_from = Val))
ftStockSummary <- flextable::colformat_num(ftStockSummary,j=1,big.mark="")
ftStockSummary <- flextable::colformat_double(ftStockSummary,j=c(2,4),digits=3)
ftStockSummary <- flextable::colformat_double(ftStockSummary,j=c(3,5),digits=3)
ftStockSummary <- flextable::autofit(ftStockSummary)
doc <- flextable::body_add_flextable(doc,ftStockSummary)
print(doc,target=file.path(getwd(),"output",runName,"StockSummary.docx"))


#SAG inputs
dfSAG.Recr <- dfStockSummary %>%
  filter(WG=="WGWIDE25" & Era=="TIME" & Qty=="Recr (billions)") %>%
  mutate(values = Val, CI_lower = Val-2*SD, CI_upper = Val+2*SD) %>%
  select(Year,CI_lower,Val,CI_upper)
write.csv(dfSAG.Recr %>% select(CI_lower,Val,CI_upper),
          file = file.path(getwd(),"output",runName,"SAG_Recr.csv"),
          row.names = FALSE, quote = FALSE)

dfSAG.SSB <- dfStockSummary %>%
  filter(WG=="WGWIDE25" & Era=="TIME" & Qty=="SSB (kt)") %>%
  mutate(values = 1e3*Val, CI_lower = 1e3*(Val-2*SD), CI_upper = 1e3*(Val+2*SD)) %>%
  select(Year,CI_lower,values,CI_upper)
write.csv(dfSAG.SSB %>% select(CI_lower,values,CI_upper),
          file = file.path(getwd(),"output",runName,"SAG_SSB.csv"),
          row.names = FALSE, quote = FALSE)

dfSAG.FBar <- dfStockSummary %>%
  filter(WG=="WGWIDE25" & Era=="TIME" & Qty=="FBar (4-14)") %>%
  mutate(values = Val, CI_lower = Val-2*SD, CI_upper = Val+2*SD) %>%
  select(Year,CI_lower,Val,CI_upper)
write.csv(dfSAG.FBar %>% select(CI_lower,Val,CI_upper),
          file = file.path(getwd(),"output",runName,"SAG_FBar.csv"),
          row.names = FALSE, quote = FALSE)

#recruitment with estimates of 95% asymptotic intervals
#these are used to replace the hi/lo estimates above, otherwise, 
#there are a number of negative estimates for the lower boundary
dfRecs <- dfStockSummary %>% 
  filter(substr(Qty,1,4) == "Recr" & WG == "WGWIDE25") %>%
  mutate(logint = sqrt(log(1+(SD/Val)^2))) %>%
  mutate(Lower = qlnorm(p=0.025, meanlog=log(Val), sdlog=logint),
         Upper = qlnorm(p=0.975, meanlog=log(Val), sdlog=logint))

write.table(dfRecs,file.path(getwd(),"output",runName,"Recruits.csv"))

#ggplot(data = dfRecs, mapping = aes(x=Year, y=Val)) + geom_point() + 
  geom_errorbar(aes(ymin=Lower, ymax=Upper))
