library("rsprite2")
library("scrutinity")
library("esc")
library("xlsx")

options(scipen = 999)

setwd("/media/ploederl/ssdata/data/artikel/meine/diverses-spielereien/Pathak-2021-ketamine")

d = read.xlsx(file="Table_2b_MSSI_Pathak_2021.xlsx", sheetIndex=1)

# https://jamesheathers.curve.space/#assessing-p-values-with-stalt

pfromt = function(mean1,sd1,n1,mean2,sd2,n2){
  t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
  df <- n1 + n2 - 2
  p_value <- 2 * pt(-abs(t_stat), df)
  p = paste0("p = ", p_value)
  return(p)
}

cohendci = function(mean1,sd1,n1,mean2,sd2,n2){
  cd = esc_mean_sd(grp1m = mean1, grp1sd = sd1, grp1n = n1,
                          grp2m = mean2, grp2sd = sd2, grp2n = n2, 
                          es.type="d")
  cdci=paste0(round(cd$es,2), " [", 
              round(cd$ci.lo,2)," to ",
              round(cd$ci.hi,2),"]")
  return(cdci)
}

effsizes = pvalues = rep(NA,nrow(d))

for(i in 1:nrow(d)){
  mean1 = d$mean1[i]; sd1 = d$sd1[i]; n1=d$n1[i]
  mean2 = d$mean2[i]; sd2 = d$sd2[i]; n2=d$n2[i]
  pvalues[i] = pfromt(mean1=mean1, sd1=sd1, n1=n1, 
                      mean2=mean2, sd2=sd2, n2=n2)
  
  effsizes[i] = cohendci(mean1=mean1, sd1=sd1, n1=n1, 
                       mean2=mean2, sd2=sd2, n2=n2)
}
  
d$Cohen_d = effsizes
d$p = pvalues

write.xlsx2(d, "results1.xlsx")


# Now assuming the "standard-error" (conflating SE and SD)

d$sd1=d$sd1*sqrt(d$n1)
d$sd2=d$sd2*sqrt(d$n2)

effsizes = pvalues = rep(NA,nrow(d))

for(i in 1:nrow(d)){
  mean1 = d$mean1[i]; sd1 = d$sd1[i]; n1=d$n1[i]
  mean2 = d$mean2[i]; sd2 = d$sd2[i]; n2=d$n2[i]
  pvalues[i] = pfromt(mean1=mean1, sd1=sd1, n1=n1, 
                      mean2=mean2, sd2=sd2, n2=n2)
  
  effsizes[i] = cohendci(mean1=mean1, sd1=sd1, n1=n1, 
                         mean2=mean2, sd2=sd2, n2=n2)
}

d$Cohen_d = effsizes
d$p = pvalues

write.xlsx2(d, "results2_correctedSDs.xlsx")




d = read.xlsx(file="Table_2b_MSSI_Pathak_2021.xlsx", sheetIndex=1)


for(i in 1:nrow(d)){
  mean = d$mean1[i]; sd = d$sd1[i]; n = d$n1[i]
  print(GRIM_test(mean = mean, n_obs=n))
  mean = d$mean2[i]; sd = d$sd2[i]; n=d$n2[i]
  print(GRIM_test(mean = mean, n_obs=n)) 
}


for(i in 1:nrow(d)){
  mean = d$mean1[i]; sd = d$sd1[i]; n = d$n1[i]
  print(GRIMMER_test(mean = mean, sd = sd, n_obs = n))
  mean = d$mean2[i]; sd = d$sd2[i]; n=d$n2[i]
  print(GRIMMER_test(mean = mean, sd = sd, n_obs = n))  
}


