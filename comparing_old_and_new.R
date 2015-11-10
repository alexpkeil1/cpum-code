#comparing my dissertation data sets with one derived under a faster approach (better dataset)

library(haven)

new <- read_sas("~/EpiProjects/CPUM/data/an0001.sas7bdat")
old <- read_sas("/Users/akeil/School/Dissertation/data/cpum_an02.sas7bdat")


#ok
length(tapply(old$id, old$id, sum))#[1] 4134
length(tapply(new$id, new$id, sum))#[1] 4134


#looks good
sum(tapply(old$d_lc, old$id, sum))#[1] 617
sum(tapply(new$d_lc, new$id, sum))#[1] 617

#ok - moved censoring date to 12/31/2005
sum(tapply(old$d, old$id, sum))#[1] 3126
sum(tapply(new$d_any, new$id, sum))#[1] 3105


#ok
mean(tapply(old$wlm, old$id, sum))#[1] 791.4941
mean(tapply(new$cumwlm+new$BL_cumwlm, new$id, max))#[1] 791.511



#ok
mean(tapply(old[old$pyar>0,]$wlm, old[old$pyar>0,]$id, sum))#[1] 417.518
mean(tapply(new$wlm, new$id, sum))#[1] 417.5473

#ok
mean(tapply(old[old$atwork==1 & old$pyar>0,]$wlm, old[old$atwork==1 & old$pyar>0,]$id, mean))#[1] 68.12803
mean(tapply(new[new$atwork==1,]$wlm, new[new$atwork==1,]$id, mean))#[1] 68.91225

#ok?
mean(tapply(old[old$pyar>0,]$atwork*old[old$pyar>0,]$pyar, old[old$pyar>0,]$id, sum))#[1] 5.728546
mean(tapply(new$atwork*new$py, new$id, sum))#[1] 5.602862

sum(old$pyar) #[1] 135,323.2
sum(new$py)   #[1] 136,635.8
#comparing some with schubauer berigan et al 2009
#schubauer berigan reports 120,437 after 1960
sum(new[new$dateout>1960,]$py) # [1] 123670.2
sum(old[old$dateout>0,]$pyar)

#reported 794 (1090) by schubauer berigan
mean(tapply(new[new$dateout>1960,]$cumwlm+new[new$dateout>1960,]$BL_cumwlm, new[new$dateout>1960,]$id, max))#[1] 782.7683 
sd(tapply(new[new$dateout>1960,]$cumwlm+new[new$dateout>1960,]$BL_cumwlm, new[new$dateout>1960,]$id, max)) #[1] 1004.462

#808 (1090)
mean(tapply(new[new$dateout>1960,]$cumwlm+new[new$dateout>1960,]$BL_cumwlm+new[new$dateout>1960,]$priorwlm, new[new$dateout>1960,]$id, max))#[1] 797.2768
sd(tapply(new[new$dateout>1960,]$cumwlm+new[new$dateout>1960,]$BL_cumwlm+new[new$dateout>1960,]$priorwlm, new[new$dateout>1960,]$id, max))#[1] 1006.37


#422 (44.1, 2050)
median(tapply(new[new$dateout>1960,]$cumwlm+new[new$dateout>1960,]$BL_cumwlm, new[new$dateout>1960,]$id, max)) #[1] 421.3727
quantile(tapply(new[new$dateout>1960,]$cumwlm+new[new$dateout>1960,]$BL_cumwlm, new[new$dateout>1960,]$id, max), c(.1, .9)) #[1] 44.0603 2045.2935

