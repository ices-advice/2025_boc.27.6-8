#WGWIDE 2025 Boarfish
##Preprocess data, write TAF data tables

library(icesTAF)

icesTAF::mkdir("data")

# Get input files
icesTAF::cp(file.path(getwd(),"boot","data",runName,"BOC.dat"), "data")
icesTAF::cp(file.path(getwd(),"boot","data",runName,"BOC.ctl"), "data")
icesTAF::cp(file.path(getwd(),"boot","data",runName,"forecast.ss"), "data")
icesTAF::cp(file.path(getwd(),"boot","data",runName,"starter.ss"), "data")

# read input files
inputs <- r4ss::SS_read(dir = file.path(getwd(),"data"), verbose = TRUE)
