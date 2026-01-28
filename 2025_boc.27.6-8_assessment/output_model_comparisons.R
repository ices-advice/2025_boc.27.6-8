#compare multiple SS3 runs/outputs

library(icesTAF)
library(r4ss)
library(ggplot2)

mkdir("output")
mkdir("output/model comparisons")

#model1 <- "1.0.WGWIDE24"
#model2 <- "2.0.WGWIDE24_SS3_V3.30.23.2"

#model1 <- "1.0.WGWIDE24"
#model2 <- "3.0.WGWIDE25_Baseline"

#model1 <- "1.0.WGWIDE24"
#model2 <- "3.1.WGWIDE25_Ex2025Survey"

#model1 <- "3.0.WGWIDE25_Baseline"
#model2 <- "3.1.WGWIDE25_Ex2025Survey"

#model1 <- "3.0.WGWIDE25_Baseline"
#model2 <- "4.0.WGWIDE25_GFIdxUpdates"

#model1 <- "1.0.WGWIDE24"
#model2 <- "99.Working"

#model1 <- "1.0.WGWIDE24"
#model2 <- "4.0.WGWIDE25_GFIdxUpdates"

model1 <- "1.0.WGWIDE24"
model2 <- "3.1.WGWIDE25_Ex2025Survey"

runs2comp <- c(model1,model2)

#run comparison plots
lCompare <- r4ss::SSgetoutput(dirvec=file.path(getwd(),"model",runs2comp))

#versions
ulCompare <- unlist(lCompare)
ulCompare[names(ulCompare)[grepl("SS_version$",names(ulCompare))]]

#gradients
ulCompare[names(ulCompare)[grepl("maximum_gradient_component$",names(ulCompare))]]

r4ss::SSplotComparisons(r4ss::SSsummarize(lCompare),
                        legendlabels = runs2comp,
                        pdf = TRUE, 
                        plotdir = file.path(getwd(),"output","model comparisons"),
                        filenameprefix = paste0(model1,"_vs_",model2,"_"))

