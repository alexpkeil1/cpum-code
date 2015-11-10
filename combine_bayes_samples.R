datadir <- "/Users/akeil/EpiProjects/CPUM/output/test/"
#datadir <- "/lustre/scr/a/k/akeil/bgf/measurement/warmup/"


read_samples <- function(dir){
	require(data.table)
	wdOld <- getwd()
	setwd(dir)
	
	fl <- system("ls *csv", intern=TRUE)
	outfiles <- grep("csv", fl)
	nocor <- grep("20151029", fl)
	cor <- grep("20151101", fl)
	cat('reading files:', fl)

	for(i in 1:length(outfiles)){
	  ec <- system('rm tmpfile.csv', intern=TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE) #clean up after messy previous run
	  #grep/sed  commands to clean up csv files before reading in
	  syscmd1 <- paste("grep lp__", fl[i], "> tmpfile.csv") #move headers to a temp file 
	  syscmd2 <- paste("sed -e '/^[#l]/d' -e '/^$/d'", fl[i], ">> tmpfile.csv") #move all data lines to temp file (includes warmup samples, if in file)
	  system(syscmd1)
	  system(syscmd2)
    cat('\n','reading', fl[i], '\n')
		f <- fread("tmpfile.csv", data.table=FALSE, stringsAsFactors=FALSE, sep=",", header=TRUE)
    ec <- system('rm tmpfile.csv', intern=TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE)
    len <- length(names(f)[grep("berkson", names(f))])
		 if(len>0) {
		   dropped <- c(paste0("berkson.", 1:len), paste0("truewlm.", 1:len), 
		                paste0("truecumwlm2lag.", 1:len), paste0("truecumwlm2lagcen.", 1:len))
		   f <- f[, -(which(names(f) %in% dropped))]
		 }
		 sig <- f[, grep("z|b.", names(f))]
		 print("measurement variables")
		 print(apply(sig, 2, function(x) c(median(x), quantile(x, 0.025), quantile(x, 0.975))), 4)
		 f <- f[, grep("cilc", names(f))]
		 f$iter <- 1:dim(f)[1]
		 f$file <- fl[i] 
		 rd <- f[, grep("cilc_nc", names(f))]-f[, grep("cilc_33", names(f))]
		 names(rd) <- paste0("rd", 1:73)
		 f <- cbind(f, rd)

		 
		 if(i==1){datcor <- datnocor <- f[-(1:length(f$file)),]}
		 if(i %in% nocor) datnocor <- rbind(datnocor, f)
		 if(i %in% cor)     datcor <- rbind(datcor, f)	}
	 setwd(wdOld)
	list(datcor, datnocor)
}

bayes.summary <- function(dat, trim=1){	
	nonNum <- which(names(cor.dat)=="file")
	med <- apply(dat[dat$iter>trim,-nonNum], 2, median)
 	lcl <- apply(dat[dat$iter>trim,-nonNum], 2, function(x) quantile(x, 0.025))
 	ucl <- apply(dat[dat$iter>trim,-nonNum], 2, function(x) quantile(x, 0.975))
   outdat <- data.frame(
                      rd=(med[grep( "rd", names(med))]), 
                      nc=(med[grep( "_nc", names(med))]), 
                      int24=(med[grep( "_1", names(med))]), 
                      int12=(med[grep( "_2", names(med))]), 
                      int4=(med[grep( "_3", names(med))]),
                      rdcl=(lcl[grep( "rd", names(lcl))]), 
                      ncl=(lcl[grep( "_nc", names(lcl))]), 
                      int24l=(lcl[grep( "_1", names(lcl))]), 
                      int12l=(lcl[grep( "_2", names(lcl))]), 
                      int4l=(lcl[grep( "_3", names(lcl))]),
                      rdcu=(ucl[grep( "rd", names(ucl))]), 
                      ncu=(ucl[grep( "_nc", names(ucl))]), 
                      int24u=(ucl[grep( "_1", names(ucl))]), 
                      int12u=(ucl[grep( "_2", names(ucl))]), 
                      int4u=(ucl[grep( "_3", names(ucl))])
                      )
 outdat$age <- 18:90
 outdat	
}


lst <- read_samples(datadir)

cor.dat <-lst[[1]]
uncor.dat <-lst[[2]]


ci.cor <- bayes.summary(cor.dat, trim=max(cor.dat$iter)/3)
ci.uncor <- bayes.summary(uncor.dat, trim=max(uncor.dat$iter)/3)


plot(ci.cor$age, ci.cor$nc, type="l", ylim=c(0, .11))
lines(ci.cor$age, ci.cor$int24, type="l", col="gray30")
lines(ci.cor$age, ci.cor$int12, type="l", col="gray50")
lines(ci.cor$age, ci.cor$int4, type="l", col="gray80")
legend("topleft", lty=rep(1, 4), col=paste0("gray", c(0, 30, 50, 80)), legend=c("NC", "2 WL", "1 WL", "0.33 WL"))
title(main="Corrected")



plot( ci.uncor $age, ci.uncor $nc, type="l", ylim=c(0, .11))
lines(ci.uncor $age, ci.uncor $int24, type="l", col="gray30")
lines(ci.uncor $age, ci.uncor $int12, type="l", col="gray50")
lines(ci.uncor $age, ci.uncor $int4, type="l", col="gray80")
legend("topleft", lty=rep(1, 4), col=paste0("gray", c(0, 30, 50, 80)), legend=c("NC", "2 WL", "1 WL", "0.33 WL"))
title(main="Uncorrected")

plot( ci.uncor $age, ci.uncor $nc, type="l", ylim=c(0, .11))
lines(ci.uncor$age, ci.uncor$int4, lty=3)
lines(ci.cor $age, ci.cor $nc, lty=1, col="gray50")
lines(ci.cor$age, ci.cor$int4, lty=3, col="gray50")
legend("topleft", lty=c(1,1,3,3), col=paste0("gray", c(0, 50, 0, 50)), legend=c("NC-uncorrected", "NC-corrected", "<0.33 WL-uncorrected", "<0.33 WL-corrected"))
title(main="Comparing corrected vs. uncorrected")


plot( ci.uncor $age, ci.uncor $rd, type="l", ylim=c(0, .1))
lines(ci.uncor$age, ci.uncor$rdcu, lty=3)
lines(ci.uncor$age, ci.uncor$rdcl, lty=3)
lines( ci.cor $age, ci.cor $rd, col="gray50")
lines(ci.cor $age, ci.cor $rdcu, lty=3, col="gray50")
lines(ci.cor $age, ci.cor $rdcl, lty=3, col="gray50")
legend("topleft", lty=c(1,1,3,3), col=paste0("gray", c(0, 50, 0, 50)), legend=c("RD-uncorrected", "RD-corrected", "CI-uncorrected", "CI-corrected"))
title(main="Comparing corrected vs. uncorrected\n risk difference (NC vs. <0.33)")

options(scipen=10)
#risk, natural course
with(ci.uncor, print(cbind(age, nc, ncl, ncu)[-(1:42),], 5))
with(  ci.cor, print(cbind(age, nc, ncl, ncu)[-(1:42),], 5))

#risk, intervention <0.33 WL
with(ci.uncor, print(cbind(age, int4, int4l, int4u)[-(1:42),], 5))
with(  ci.cor, print(cbind(age, int4, int4l, int4u)[-(1:42),], 5))


with(ci.uncor, print(cbind(age, rd, rdcl, rdcu)[-(1:42),], 5))
with(  ci.cor, print(cbind(age, rd, rdcl, rdcu)[-(1:42),], 5))
#total iterations
dim(cor.dat)[1]
dim(uncor.dat)[1]
max(cor.dat$iter)
max(uncor.dat$iter)




tail(cor.dat[,1:10])
tail(cor.dat[,41:50])
tail(cor.dat[,61:73], 10)