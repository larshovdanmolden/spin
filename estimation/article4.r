source("/Users/larshovdanmolden/Documents/git/spin/estimation/packages.r")
source("/Users/larshovdanmolden/Documents/git/spin/estimation/article_functions.r")
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
Sys.setenv(LC_ALL="en_US.UTF-8")

setwd("/Users/larshovdanmolden/Documents/Data/Article_4")
qnames <- data.frame(read.csv("qnames.csv", sep=";", stringsAsFactors=FALSE, header=FALSE));colnames(qnames) <- c("q","desc")

### Importing and cleaningemail address and the project number
setwd("/Users/larshovdanmolden/Documents/Data/Article_3")
regdataraw = read.spss("regdata_tommy.sav", to.data.frame=TRUE, labels=F)
regdatazw <- regdataraw[,c("epost","prosjektnummer")] #could add ny_epost too
regdatazw$epost <- str_trim(as.character(regdatazw$epost))
names(regdatazw) <- c("email","pnr")

### Importing Skattefunn data // UNDER DEVELOPMENT
regdatasf <- sfmain[,c("Prosjektnummmer","Organisasjonsnummer")]
names(regdatasf) <- c("pnr","orgnr") #orgnr is a unique identifier for each firm / same for pnr at project level
datazw = read.csv("skattefunn_10_clean.csv", sep=";",dec=",", header=T)
md <- datazw[c("Resp","dc_t0","dc_t1")]
md$Resp <- str_trim(as.character(md$Resp))
colnames(md) <- c("email","dc0","dc1")

### NB / regdf comes from skattefunn_data.r contains all projects, not only endreport
matchvec <- match(md$email,regdf$email)
regdata <- cbind(md,regdf[matchvec,])
datazw <- cbind(datazw,regdata)

#Splitting datazw in first wave and second wave
zw0 <- data.frame(orgnr=datazw$orgnr);zw0$yr <- 2004
zw1 <- data.frame(orgnr=datazw$orgnr);zw1$yr <- 2014

zw0[,3:117] <- datazw[,2:116]
zw1[,3:70] <- datazw[,117:184];zw1 <- zw1[1:263,]

### ADD PROJECT RESULTS EACH YEAR - PRODUCT LAUNCHES PR YEAR PR FIRM
### sfmain has the end reports
### datazw has the register information and the DC data
year <- c(seq(2002,2015,1)) ; org <- unique(datazw$orgnr)

### This dataset contains a panel of projects for each firm laid in time. Financial data to be added to this dataset
sfp <- expand.grid(orgnr=org, year = year)
sfp <- merge(sfp, sfmain, by.x=c("orgnr","year"), by.y=c("Organisasjonsnummer","ar_til"), all.x=TRUE)
regnskapadj <- regnskap[ , !(names(regnskap) %in% c("ansatte"))]
sfp <- merge(sfp, regnskapadj, by.x=c("orgnr","year"), by.y=c("orgnr","yr"), all.x=TRUE)
sfp$timepnr <- paste(sfp$orgnr,sfp$Prosjektnummmer,sfp$year)
sfp <- sfp[!duplicated(sfp$timepnr),]
sfp <- merge(sfp, zw0, by.x=c("orgnr","year"), by.y=c("orgnr","yr"), all.x=TRUE)
sfp$fousam <- ifelse(sfp$Har.FoU.samarbeid=="Ja",1,0)


print("resulting data frame is the skattefunn panel - sfp")






### Making cross sections based on the first DC obs

exvec <- c("Videreringspinoff",
           "Nyeprodtjenoppn",
           "Nyeprodtjenforv",
           "Nyeprosoppn",
           "Nyeprosforv",
           "Patoppn",
           "Patforv",
           "phdoppn")

namvec <- c("spinoff",
           "npra",
           "npre",
           "npoa",
           "npoe",
           "pata",
           "pate",
           "phda")




extres <- list()
for(i in 1:length(exvec)){

    extress[[i]] <- extsfres(quo(get(exvec[i])),namvec[i])
}

newprod <- do.call("cbind",extres);newprod <- newprod[c(1,2,4,6,8,10,12,14,16)]
newprod <- merge(newprod, zw0,by="orgnr")
newprod[newprod == -1] <- NA

