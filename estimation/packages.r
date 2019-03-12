


list.of.packages = c("ltm",
                     "feather",
                     "kableExtra",
                     "readstata13",
                     "plyr",
                     "dplyr",
                     "lme4",
                     "lavaan",
                     "kutils",
                     "semPlot",
                     "semTools",
                     "lme4",
                     "nlme",
                     "reshape2",
                     "plm",
                     "reporttools",
                     "stargazer",
                     "knitr",
                     "interplot",
                     "kableExtra")



new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {install.packages(new.packages)}

lapply(list.of.packages, require, character.only=T)








