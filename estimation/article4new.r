library(foreign)
library(stringr)
library(Hmisc)
library(reshape2)
library(dplyr)
library(broom)
library(tableHTML)
library(condformat)
library(factoextra)
library(stargazer)
library(xtable)
library(pixiedust)
library(RColorBrewer)
library(glmnet)
library(mice)
library(basicTrendline)
#This takes a long time to run / about 10 minutes
                                        #source("/Users/larshovdanmolden/Documents/gitrep/scripts/skattefunn_data.r")
                                        #save.image("art4.RData")
#load("/Users/larshovdanmolden/Documents/Data/skattefunn/sf/art4.RData")
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
setwd("/Users/larshovdanmolden/Documents/Data/Article_4")

qnames <- data.frame(read.csv("qnames.csv", sep=";", stringsAsFactors=FALSE, header=FALSE));colnames(qnames) <- c("q","desc")



setwd("/Users/larshovdanmolden/Documents/Data/Article_3")


# Importing and cleaning email address and the project number
regdataraw = read.spss("regdata_tommy.sav", to.data.frame=TRUE, labels=F)
regdatazw <- regdataraw[,c("epost","prosjektnummer")] #could add ny_epost too
regdatazw$epost <- str_trim(as.character(regdatazw$epost))
names(regdatazw) <- c("email","pnr")

# Importing Skattefunn data // UNDER DEVELOPMENT
regdatasf <- sfmain[,c("Prosjektnummmer","Organisasjonsnummer")]
names(regdatasf) <- c("pnr","orgnr") #orgnr is a unique identifier for each firm / same for pnr at project level

datazw = read.csv("skattefunn_10_clean.csv", sep=";",dec=",", header=T)
#datazw = read.delim("skattefunn_10_year_test.dat", header=T)
#head(datazw)

#datazw = read.spss("skattefunn_10_year_test.sav", to.data.frame=TRUE, use.value.labels=T)
md <- datazw[c("Resp","dc_t0","dc_t1")]
md$Resp <- str_trim(as.character(md$Resp))
colnames(md) <- c("email","dc0","dc1")

# NB / regdf comes from skattefunn_data.r contains all projects, not only endreport

matchvec <- match(md$email,regdf$email)
regdata <- cbind(md,regdf[matchvec,])
datazw <- cbind(datazw,regdata)

#Splitting datazw in first wave and second wave
zw0 <- data.frame(orgnr=datazw$orgnr);zw0$yr <- 2004
zw1 <- data.frame(orgnr=datazw$orgnr);zw1$yr <- 2014

zw0[,3:117] <- datazw[,2:116]
zw1[,3:70] <- datazw[,117:184];zw1 <- zw1[1:263,]


### ADD PROJECT RESULTS EACH YEAR - PRODUCT LAUNCHES PR YEAR PR FIRM

#sfmain has the end reports
#datazw has the register information and the DC data
year <- c(seq(2002,2015,1)) ; org <- unique(datazw$orgnr)

#This dataset contains a panel of projects for each firm laid in time. Financial data to be added to this dataset
sfp <- expand.grid(orgnr=org, year = year)

sfp <- merge(sfp, sfmain, by.x=c("orgnr","year"), by.y=c("Organisasjonsnummer","ar_til"), all.x=TRUE)


regnskapadj <- regnskap[ , !(names(regnskap) %in% c("ansatte"))]
sfp <- merge(sfp, regnskapadj, by.x=c("orgnr","year"), by.y=c("orgnr","yr"), all.x=TRUE)

sfp$timepnr <- paste(sfp$orgnr,sfp$Prosjektnummmer,sfp$year)
sfp <- sfp[!duplicated(sfp$timepnr),]
sfp <- merge(sfp, zw0, by.x=c("orgnr","year"), by.y=c("orgnr","yr"), all.x=TRUE)

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

extsfres <- function(var,nam){

    res <- data.frame(sfp  %>% dplyr::group_by(orgnr) %>% dplyr::summarise(sum(!! var,na.rm=TRUE)))
    colnames(res) <- c("orgnr",nam)

    return(res)
    }



extres <- list()
for(i in 1:length(exvec)){

    extres[[i]] <- extsfres(quo(get(exvec[i])),namvec[i])
}

newprod <- do.call("cbind",extres);newprod <- newprod[c(1,2,4,6,8,10,12,14,16)]
newprod <- merge(newprod, zw0,by="orgnr")
newprod[newprod == -1] <- NA

### READY FOR ANALYSIS
## making large correlation matrix

npracor <- cbind(newprod[,12:124], newprod$npra)
nprecor <- cbind(newprod[,12:124], newprod$npre)
npoacor <- cbind(newprod[,12:124], newprod$npoa)
npoecor <- cbind(newprod[,12:124], newprod$npoe)
patacor <- cbind(newprod[,12:124], newprod$pata)
patecor <- cbind(newprod[,12:124], newprod$pate)
spincor <- cbind(newprod[,12:124], newprod$spinoff)

npracor <- cbind(newprod$orgnr, newprod$npra,newprod[,13:124])
colnames(npracor)[1:2] <- c("orgnr","npra")
nprecor <- cbind(newprod$orgnr, newprod$npre,newprod[,13:124])
colnames(nprecor)[1:2] <- c("orgnr","npre")
npoacor <- cbind(newprod$orgnr, newprod$npoa,newprod[,13:124])
colnames(npoacor)[1:2] <- c("orgnr","npoa")
npoecor <- cbind(newprod$orgnr, newprod$npoe,newprod[,13:124])
colnames(npoecor)[1:2] <- c("orgnr","npoe")
patacor <- cbind(newprod$orgnr, newprod$pata,newprod[,13:124])
colnames(patacor)[1:2] <- c("orgnr","pata")
patecor <- cbind(newprod$orgnr, newprod$pate,newprod[,13:124])
colnames(patecor)[1:2] <- c("orgnr","pate")
spincor <- cbind(newprod$orgnr, newprod$spinoff,newprod[,13:124])
colnames(spincor)[1:2] <- c("orgnr","spin")


dfnpra <- melt(data.frame(npracor),id.vars = "npra")
dfnpre <- melt(data.frame(nprecor),id.vars = "npre")
dfnpoa <- melt(data.frame(npoacor),id.vars = "npoa")
dfnpoe <- melt(data.frame(npoecor),id.vars = "npoe")
dfpata <- melt(data.frame(patacor),id.vars = "pata")
dfpate <- melt(data.frame(patecor),id.vars = "pate")
dfspin <- melt(data.frame(spincor),id.vars = "spin")


resnpra <- dfnpra %>% group_by(variable) %>% do(tidy(glm(npra ~ value, data=.,family="poisson")))
resnpre <- dfnpre %>% group_by(variable) %>% do(tidy(glm(npre ~ value, data=.,family="poisson")))
resnpoa <- dfnpoa %>% group_by(variable) %>% do(tidy(glm(npoa ~ value, data=.,family="poisson")))
resnpoe <- dfnpoe %>% group_by(variable) %>% do(tidy(glm(npoe ~ value, data=.,family="poisson")))
respata <- dfpata %>% group_by(variable) %>% do(tidy(glm(pata ~ value, data=.,family="poisson")))
respate <- dfpate %>% group_by(variable) %>% do(tidy(glm(pate ~ value, data=.,family="poisson")))
resspin <- dfspin %>% group_by(variable) %>% do(tidy(glm(spin ~ value, data=.,family="poisson")))


cleanres <- function(df){
    tokeep <- which(df$term=="value")
    df2 <- data.frame(df[tokeep,])
    return(df2)
    }


stackres <- function(ls,sign){

    names <- names(ls)
    templist1 <- list()
    templist2 <- list()

    for(i in 1:length(ls)){
        templist1[[i]] <- ls[[i]][,c(1,3,6)]
        templist2[[i]] <- ls[[i]][,c(1,3)]
    }

    res <- data.frame(do.call("cbind",templist1))
    estv <- grep("estimate",colnames(res))
    pv <- grep("p.value",colnames(res))

    beta <- res[,estv];colnames(beta) <- paste("coef.",names, sep="")
    sig <- res[,pv];colnames(sig) <- paste("p.",names, sep="")
    q <- res[,1]

    finres1 <- cbind(q,beta,sig)
    finres2 <- cbind(q,beta)

    if(sign){return(finres1)} else {return(finres2)}

 }

modlist <- list(resnpra,resnpre,resnpoa,resnpoe,respata,respate,resspin)
namvec <- c("npra",
            "npre",
            "npoa",
            "npoe",
            "pata",
            "pate",
            "spin")
names(modlist) <- namvec



modlist1 <- lapply(modlist, FUN=cleanres)
finalres <- stackres(modlist1, sign=FALSE) #DONT INCLUDE SIG IN THIS PART
is.num <- sapply(finalres, is.numeric)
finalres[is.num] <- lapply(finalres[is.num], round, 4)
finalres <- finalres[-1,]

qmatch <- as.character(finalres$q)
qnames$q <- as.character(qnames$q)

qnames <- qnames[,1:2]

finalresexp <- merge(qnames,finalres, by="q",all.y=TRUE)
#finalresexp$q <- qnames
finalresexp <- subset(finalresexp, !is.na(finalresexp$desc))


result <- condformat(finalresexp) %>%
rule_fill_discrete(coef.npra, expression = coef.npra < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
rule_fill_discrete(coef.npre, expression = coef.npre < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.npoa, expression = coef.npoa < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.npoe, expression = coef.npoe < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.pata, expression = coef.pata < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.pate, expression = coef.pate < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
rule_fill_discrete(coef.spin, expression = coef.spin < 0, colours=c("TRUE"="red", "FALSE"="green"))


resall <- condformat2html(result)
#write.table(resall,
           # file='martinres.html',
           # quote = FALSE,
           # col.names = FALSE,
           # row.names = FALSE)








### ADDING FOR MARTIN ANALYSIS
finalres <- stackres(modlist1, sign=TRUE) #DONT INCLUDE SIG IN THIS PART
is.num <- sapply(finalres, is.numeric)
finalres[is.num] <- lapply(finalres[is.num], round, 4)
finalres <- finalres[-1,]
finalresexp <- merge(qnames,finalres, by="q",all.y=TRUE)
finalresexp <- subset(finalresexp, !is.na(finalresexp$desc))


#Getting adjusted datasets
spinadj <- spincor[,c(1,2,c(26:99))]
finalresadj <- finalresexp[,c("q","coef.spin","p.spin")]
fixedf <- sfp[,c("orgnr","Fylke","Sektor","ansatte")]
fixedf <- fixedf[complete.cases(fixedf), ]

fixedf <- regnskap[,c("orgnr","ansatte","nace","kommune","fylke","yr")]

fixedf <- regnskap[!duplicated(fixedf$orgnr),]



spinadjest <- merge(spinadj, fixedf, by="orgnr",all=FALSE, all.x=TRUE)
spinadj <- spinadjest[,c(2:76)]
spinadj$ind <- factor(spinadjest$nace)
spinadj$geo <- factor(spinadjest$fylke)
spinadj$emp <- spinadjest$ansatte



aiv <- spinadj[ , !(names(spinadj) %in% c("ind","geo","emp"))]
ind <- spinadj[ , !(names(spinadj) %in% c("geo","emp"))]
emp <- spinadj[ , !(names(spinadj) %in% c("geo"))]
geo <- spinadj[ , !(names(spinadj) %in% c("ind"))]

po <- function(df){


    mod <- glm(spin~., data=df, family="poisson")
    ext <- grep("Q",names(mod$coefficients))
    res.coef <- mod$coefficients[ext]
    res.p <- summary(mod)$coefficients[ext,4]

    return(list(coef=res.coef,p=res.p))
    }


reslist <- list()
reslist[["q"]] <- finalresexp$q
reslist[["desc"]] <- finalresexp$desc

reslist[["base"]]  <- finalresadj$coef.spin
reslist[["base.p"]]  <- finalresadj$p.spin
reslist[["aiv"]] <- po(aiv)$coef
reslist[["aiv.p"]] <- po(aiv)$p
reslist[["ind"]] <- po(ind)$coef
reslist[["ind.p"]] <- po(ind)$p
reslist[["emp"]] <- po(emp)$coef
reslist[["emp.p"]] <- po(emp)$p
reslist[["geo"]] <- po(geo)$coef
reslist[["geo.p"]] <- po(geo)$p


resdf <- data.frame(reslist)
is.num <- sapply(resdf, is.numeric)
resdf[is.num] <- lapply(resdf[is.num], round, 4)


result2 <- condformat(resdf) %>%


    rule_fill_discrete(aiv, expression = aiv < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
    rule_fill_discrete(ind, expression = ind < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_fill_discrete(base, expression = base < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_fill_discrete(emp, expression = emp < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_fill_discrete(geo, expression = geo < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%

    rule_text_bold(emp, expression = emp.p < 0.05 )%>%
    rule_text_bold(geo, expression = geo.p < 0.05 )%>%
    rule_text_bold(aiv, expression = aiv.p < 0.05 )%>%
    rule_text_bold(ind, expression = ind.p < 0.05 )%>%
    rule_text_bold(base, expression = base.p < 0.05)




setwd("/Users/larshovdanmolden/Documents/Data/Article_4")

allinone <- condformat2html(result2)

#write.table(allinone,
#            file='allinone.html',
#            quote = FALSE,
#            col.names = FALSE,
#            row.names = FALSE)


### PCA

spinpca <-na.omit(spinadjest[,c(3:76)])

pca1 <- prcomp(spinpca,scores=TRUE)
summary(pca1)
pca_viz<- data.frame(pca1$rotation[,1:10])


head(unclass(pca1$rotation)[, 1:4])



result3 <- condformat(pca_viz) %>%


    #rule_fill_discrete(pca_viz, expression = abs(pca_viz) >  0.2, colours=c("TRUE"="red", "FALSE"="green"))


    rule_fill_gradient2(PC1)  %>%
    rule_fill_gradient2(PC2)  %>%
    rule_fill_gradient2(PC3)  %>%
    rule_fill_gradient2(PC4)  %>%
    rule_fill_gradient2(PC5)  %>%
    rule_fill_gradient2(PC6)  %>%
    rule_fill_gradient2(PC7)  %>%
    rule_fill_gradient2(PC8)  %>%
    rule_fill_gradient2(PC9)  %>%
    rule_fill_gradient2(PC10)


pca_viz_res<- condformat2html(result3)



write.table(pca_viz_res,
            file='pca.html',
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)



#factors to extract
eig.val <- get_eigenvalue(pca1)
eig.val



#Correlations between them. Most are grouped
fviz_pca_var(pca1)

#18c Top management generally believes that the firms environment should be explored gradually and with cautios
# 17c We relate to our competitors by avoiding as little direct competition as possible. We prefer that we all leave eachother alone



#Variables factor map
fviz_pca_var(pca1, col.var="contrib") +
scale_color_gradient2(low="white", mid="blue",
      high="red", midpoint=5) + theme_minimal()

#none is really sticking out

### LASSSO

#install.packages("mice", repos = "http://cran.us.r-project.org")


### ALL COMPLETE OBS
lassoraw <- spinadjest[,c(2:76)]
lassodf <- na.omit(spinadjest[,c(2:76)])
nrow(lassodf)

y <- c(lassodf$spin)
x <- as.matrix(lassodf[ , !(names(lassodf) %in% c("spin"))])



lasso <- glmnet(x,y)
plot(lasso)
print(lasso)
c1 <- coef(lasso,s=0.05)

##### IMPUTED MISSING VALUES
                                        #https://www.r-bloggers.com/imputing-missing-data-with-r-mice-package/
#https://cran.r-project.org/web/packages/mice/mice.pdf

lassoraw2 <- lassoraw[,1:10]
#impLassorf <- impLasso

#impLasso <- mice(lassoraw,m=5,maxit=25,meth='lda',seed=500)

summary(impLasso)

implassodf <- complete(impLasso,1)

y <- c(implassodf$spin)
x <- as.matrix(implassodf[ , !(names(implassodf) %in% c("spin"))])

mod1 <- lm(y ~ x);summary(mod1)
mod1 <- lm(spin ~., data=lassoraw2);summary(mod1)

lassoimp <- glmnet(x,y)
plot(lassoimp)
print(lassoimp)
c2 <- coef(lassoimp,s=0.05)


lassores <- data.frame("q"=rownames(c1)[-1],"full"=c1[-1,1], "imp"=c2[-1,1])
is.num <- sapply(lassores, is.numeric)
lassores[is.num] <- lapply(lassores[is.num], round, 4)
lassores[lassores==0.00000]<-NA



result4 <- condformat(lassores) %>%


    #rule_fill_discrete(pca_viz, expression = abs(pca_viz) >  0.2, colours=c("TRUE"="red", "FALSE"="green"))


    rule_fill_discrete(full, expression = full < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
    rule_fill_discrete(imp, expression = imp < 0, colours=c("TRUE"="red", "FALSE"="green"))


lasso_viz<- condformat2html(result4)
lasso_viz


write.table(lasso_viz,
            file='lasso.html',
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE)


### ADDING ONLY VARIABLES FROM THE LASSO
tmp_coeffs<-  coef(lasso,s=0.05,allCoef=TRUE)
tmp_coeffs <- data.frame(name = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
lassovar <- tmp_coeffs$name[-1]
lassotmp <- spinadjest[ , (names(spinadjest) %in% lassovar)]
lassotmp$spin <- spinadjest$spin
lassotmp$orgnr <- spinadjest$orgnr

lassotmp2 <- lassotmp[ , !(names(lassotmp) %in% c("Q22","Q22b","Q28b"))]
lassotmp3 <- lassotmp[ , !(names(lassotmp) %in% c("Q28b"))]


mod1 <- glm(spin ~ ., data=lassotmp, family="poisson");summary(mod1)
mod2 <-  glm(spin ~ ., data=lassotmp2, family="poisson");summary(mod2)
mod3 <-  glm(spin ~ ., data=lassotmp3, family="poisson");summary(mod3)

addvarres <- list(mod1,mod2,mod3)




### ARE SPIN OFFS DEVLINING OVER TIME
## FIRST CRUDE COUNT ALL OVER THE BOARD


spin_t <- data.frame(sfmain  %>% dplyr::group_by(ar_til) %>% dplyr::summarise(sum(Videreringspinoff,na.rm=TRUE)))

spin_t <- data.frame(sfmain  %>% dplyr::group_by(ar_fra) %>% dplyr::summarise(sum(Videreringspinoff,na.rm=TRUE)))
colnames(spin_t) <- c("yr","spin")
spin_t

spin_t <- data.frame(spinadjest  %>% dplyr::group_by(yr) %>% dplyr::summarise(sum(spin,na.rm=TRUE)))
colnames(spin_t) <- c("yr","spin")

test <- spin_t %>% group_by() %>% mutate(cumsum = cumsum(spin))


plot(test$cumsum, type="l")
plot(spin_t, type="l")

foo <- cbind(spin_t,foo2$n)
foo3 <- foo$spin
foo4 <- foo2$n
foo4

test <- foo3/foo4
foo3
plot(test)

test

foo2 <- sfmain %>% dplyr::group_by(ar_til) %>% tally()

df2 <- count(spin_t, c("spin",'yr'))

spin_nr <- data.frame(sfmain  %>% dplyr::group_by(ar_til) %>% dplyr::summarise(count(Prosjektnummmer,na.rm=TRUE)))


### making SPINOFF PANEL PR FIRM

spin_time <- data.frame(sfmain  %>% dplyr::group_by(Organisasjonsnummer,ar_til) %>% dplyr::summarise(sum(Videreringspinoff,na.rm=TRUE)));colnames(spin_time) <- c("orgnr","yr","spin")

test <- data.frame(spin_time%>% dplyr::group_by(yr) %>% dplyr::summarise(sum(spin,na.rm=TRUE)))
colnames(test) <- c("yr","spin")
plot(test)

spinoff.panel <- merge(spin_time, lassotmp,by="orgnr", all.x=TRUE)
spinoff.panel$t <-spinoff.panel$yr - 2002
spinoff.panel$spin <- spinoff.panel$spin.x
spinoff.panel$spinbin <- ifelse(spinoff.panel$spin>0,1,0)
spinoff.panel$t2 <- spinoff.panel$t^2

foo <- subset(spinoff.panel, spin<2)

mod3 <-  glm(spin.x ~ Q20*t, data=spinoff.panel, family="poisson");summary(mod3)
mod3 <-  glm(spin.x ~ Q17 + Q17:t, data=spinoff.panel, family="poisson");summary(mod3)
mod3 <-  glm(spin.x ~ Q17, data=spinoff.panel, family="poisson");summary(mod3)
mod3 <-  glm(spin.x ~ t, data=spinoff.panel, family="poisson");summary(mod3)
mod3 <-  glm(spinbin ~ t, data=spinoff.panel, family="binomial");summary(mod3)

mod3 <-  glm(spin ~ t + t2, data=foo, family="poisson");summary(mod3)
mod3 <-  lm(spin ~ t, data=foo);summary(mod3)


dfspinp <- melt(data.frame(spinoff.panel),id.vars = c("spin.x","t"))
dfspinp$value2 <- dfspinp$value
dfspinp$value <- dfspinp$value*dfspinp$t
dfspinp$spinalt <- ifelse(dfspinp$spin.x>0,1,0)


mod3 <-  glm(spin.x ~ Q17, data=dfspinp, family="poisson");summary(mod3)


# models for both count - where number of spin offs can be more than one yearly, and as binomial
spinp <- dfspinp %>% group_by(variable) %>% do(tidy(glm(spin.x~ value, data=.,family="poisson")))
spinpalt <- dfspinp %>% group_by(variable) %>% do(tidy(glm(spinalt~ value, data=.,family="binomial")))

modlistspin <- list(spinp=spinp,spinpalt=spinpalt)
modlistspin <- lapply(modlistspin, FUN=cleanres)

spintime <- stackres(modlistspin, sign=TRUE) #DONT INCLUDE SIG IN THIS PART
head(spintime)
is.num <- sapply(spintime, is.numeric)
spintime[is.num] <- lapply(spintime[is.num], round, 4)
spintime <- spintime[grep("Q",spintime$q),] #removing orgnr and yr regressions

result5 <- condformat(spintime) %>%


    rule_fill_discrete(coef.spinp, expression = coef.spinp < 0, colours=c("TRUE"="red", "FALSE"="green")) %>%
    rule_fill_discrete(coef.spinpalt, expression = coef.spinpalt < 0, colours=c("TRUE"="red", "FALSE"="green"))%>%
    rule_text_bold(coef.spinp, expression =p.spinp < 0.05 )%>%
    rule_text_bold(coef.spinpalt, expression = p.spinpalt < 0.05 )



timeint <- condformat2html(result5)
timeintlat <- condformat2latex(result5)



######### New plots
# yearly plots in number of projects

yearpro <- sfmain %>% group_by(ar_fra) %>% dplyr::count()
yearcost <- sfmain %>% group_by(ar_fra) %>% dplyr::summarise(sum(SR_Sum_Total_finansiering,na.rm=TRUE));colnames(yearcost) <- c("ar","cost")
yearother <-  sfmain %>% group_by(ar_fra) %>% dplyr::summarise(sum(SR_Annen.offentlig.finansiering,na.rm=TRUE));colnames(yearother) <- c("ar","othercost")

yrvec <- yearcost$ar
avgyearcost <- yearcost$cost/yearpro$n
avgyearcostother <- yearother$othercost/yearpro$n
yearcostsummary <- data.frame(cbind(yrvec,avgyearcost,avgyearcostother))
yearcostsummary


o





f1 <- yearcostsummary[1:9,2]
f2 <- yearcostsummary[1:9,3]
e1 <- yearcostsummary[10:15,2]
e2 <- yearcostsummary[10:15,3]


cor(f1,f2)



res <- colSums(sfmain$size==0)/nrow(sfmain$size)*100


sum(sfmain$size == 0,na.rm=TRUE)


plot(yearcostsummary$yrvec,yearcostsummary$avgyearcost,type="l",
     ylim=c(0,1.4),
     ylab="million NOK - average",
     col="blue")

lines(yearcostsummary$yrvec,yearcostsummary$avgyearcostother, col="red")
title(main="Average budgeted tax credit pr firm")
legend(2007, 0.2, legend=c("Avg budgeted tax credit", "Avg budget other grants"),
       col=c("blue", "red"), lty=1:1)



foo <- trendline(yearcostsummary$yrvec,yearcostsummary$avgyearcost,model="line2P",summary=TRUE,eDigit=10)
trendline(yearcostsummary$yrvec[10:15],yearcostsummary$avgyearcost[10:15],model="line2P",summary=TRUE,eDigit=10)

trendline(yearcostsummary$yrvec[1:9],yearcostsummary$avgyearcostother[1:9],model="line2P",summary=TRUE,eDigit=10)
trendline(yearcostsummary$yrvec[10:15],yearcostsummary$avgyearcostother[10:15],model="line2P",summary=TRUE,eDigit=10)
#yearpro is sum of projects
