
library("oaxaca")
data("chicago")
chicago$real.wage <- exp(chicago$ln.real.wage)

results <- oaxaca(formula = real.wage ~ age + female + LTHS +
+    some.college + college + advanced.degree | foreign.born | LTHS +
+    some.college + college + advanced.degree, data = chicago, R = 1000)

results$n


Får en framtidsrettet organisering og et kraftfullt fagmiljø i samhandling med næringsliv og andre relevante samfunnsaktører gjennom Innovasjonscampus Steinkjer (se vedlegg 2)

Gode grep for å implementere Nords strategi (se vedlegg 1 for detaljer)
Får en framtidsrettet organisering og et kraftfullt fagmiljø i samhandling med næringsliv og andre relevante samfunnsaktører gjennom Innovasjonscampus Steinkjer (se vedlegg 2)
Får en prioritert (jf. Pwc-rapporten) og tydelig regional rolle som kunnskapsaktør i Trøn-delag - og Norge
Får et samlet, koordinert, stort og tverrfaglig forskningsmiljø i Trøndelag innen bio-, næ-rings- og samfunnsfag som understøtter regional utvikling og innovasjon
Økt attraktivitet for studenter og ansatte
Relevant lokalisert i Trøndelag fylkes administrasjonssenter, innen et nasjonalt satsings-område  og en region som har ressursfortrinn og ambisjoner  innen bioøkonomi






https://www.forskningsradet.no/prognett-biookonomi/Forside/1254016050630



•	Fagstab med nettverk i næringslivet, forvalt-ningen og forskningsmiljøene: Fagstaben i Fa-kultet for biovitenskap og akvakultur har et verdifullt nettverk innen regionens næringsliv og forvaltning. Nærhet til et av de fremste landbruks¬miljøene i landet er viktig, og et for-trinn med tanke på næringslivets deltakelse i forskningen.
•	Forskning med relevans for næringsliv og for-valtning: Landbruksforskningen har fokus på å besvare problemstillinger som bidrar bedre be-slutningsstøtte og mer optimal drift. Eksempel på prosjekter er «Optibeef», «Grazeland» og «Fôreff» som alle handler om driftsoptimalise-ring innen husdyrholdet, og hvor vi samarbei-der med andre forskningsinstitusjoner i inn- og utland. Nylig har vi i samarbeid med SINTEF og Københavns universitet fått på plass et GHG-prosjekt om metanfangst i storfefjøs, finansiert over NFR. Forskning innen naturforvaltnings-gruppen har hovedfokus på utmarksressurser (skog og fjell) og kulturlandskap. Vår forskning gir viktige bidrag til forvaltnings¬myndighetene i forhold til både høstbart vilt og tiltak rettet mot truede og sårbare arter.
•	Den «grønne gren» av blå-grønn satsing: Med fagmiljø både innen blå og grønn sektor i FBA, har vi gode forutsetninger for å bidra til blå og grønn vekst. Det gir også muligheter for syner-gier mellom miljøene, og Fakultet for bioviten-skap og akvakultur har nylig ansatt to PhD-stipendiater som skal arbeide med makroalger som fôr til husdyr.
•	Mære landbruksskole: Mære ligger 12 km fra Steinkjer, og er ikke bare en videregående sko-le, men en aktiv arena for grønn forskning. For Fakultet for biovitenskap og akvakultur er Mære svært viktig, og fungerer på mange må-ter som vår forskningsstasjon. I samarbeid med Mære og andre regionale aktører videre-utvikler Fakultet for biovitenskap og akvakultur forskningsfasilitetene ved skolen.
•	Del av landbruksklyngen i Steinkjer: Steinkjer har alltid vært et senter for aktivitet innen jord-bruk og skogbruk, og Innovasjon Norge, Norsk landbruks¬rådgivning, Tine, Felleskjøpet, Nortu-ra m.fl. er godt representert i Steinkjer. Denne posisjonen har blitt betydelig styrket bl.a. gjen-nom at landbruksdirektoratet har flyttet deler av sin virksomhet hit, og NIBIO har etablert en regional hovedenhet i Steinkjer. For NIBIO er fôr, husdyr og skog viktige satsingsområder for Steinkjerkontoret, - mye på grunn av NORD universitet sin aktivitet i byen.




library(xlsx)           # for reading in Excel data
library(dplyr)          # for data manipulation
library(tidyr)          # for data manipulation
library(magrittr)       # for easier syntax in one or two areas
library(gridExtra)      # for generating some comparison plots
library(ggplot2)        # for generating the visualizations


installed.packages("rJava")
library(rJava)

income <- read.xlsx("/Users/larshovdanmolden/Downloads/PEW Middle Class Data.xlsx",
                    sheetIndex = "1. Distribution, metro",
                    startRow = 10, colIndex = c(1:4, 6:8)) %>%
  set_colnames(c("Metro", "Lower_00", "Middle_00", "Upper_00", "Lower_14",
                 "Middle_14", "Upper_14")) %>%
    filter(Metro != "NA")

install.packages("datapasta")
library(reprex)
install.packages("installr"); library(installr)
library(datapasta)



lm_fit <- lm(Sepal.Length ~ . , data = iris)
summary(lm_fit)

head(iris)


df <- tibble::tribble(
                                ~X,          ~Location, ~Min, ~Max,
                  "Partly cloudy.",         "Brisbane",   19,   29,
                  "Partly cloudy.", "Brisbane Airport",   18,   27,
                "Possible shower.",       "Beaudesert",   15,   30,
                  "Partly cloudy.",        "Chermside",   17,   29,
  "Shower or two. Possible storm.",           "Gatton",   15,   32,
                "Possible shower.",          "Ipswich",   15,   30,
                  "Partly cloudy.",    "Logan Central",   18,   29,
                   "Mostly sunny.",            "Manly",   20,   26,
                  "Partly cloudy.",    "Mount Gravatt",   17,   28,
                "Possible shower.",            "Oxley",   17,   30,
                  "Partly cloudy.",        "Redcliffe",   19,   27
  )

df
