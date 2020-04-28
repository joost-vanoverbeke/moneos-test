
library(tidyverse)
library(INBOtheme)
library(htmlwidgets)
library(DT) # Interactieve tabellen
library(inborutils)
library(DBI)
library(kableExtra)
library(lubridate)
library(RcppRoll)
library(ggpubr)

con <- connect_inbo_dbase("W0004_00_Waterbirds")

con <- DBI::dbConnect(odbc::odbc(),
                                driver = "SQL Server",
                                server = "inbo-sql07-prd.inbo.be", # or inbo-sql08-prd.inbo.be
                                port = 1433,
                                database = "W0004_00_Waterbirds", # or your database of interest
                                trusted_connection = "Yes")

con <- dbConnect(odbc::odbc(), "watervogels")

# Query Warehouse - Zeeschelde-analyseset
#########################################
# Deze query gebruiken we in de toekomst steeds voor gegevens Zeeschelde. Eén bedenking hierbij: ik denk dat Koen Devos de selectie maakt van de tellingen. Best vragen we hem eensn welke regels hierbij gehanteerd worden.

WaterbirdsBT <- dbGetQuery(con,
"SELECT
  SU.SurveyCode as ProjectCode
, SU.SurveyNaam as Project
, L.RegioWVCode as RegioCode
, L.RegioWVNaam as Regio
, L.LocationWVCode as GebiedsCode
, L.LocationWVNaam as Gebied
, SS.Seasonname as Telseizoen
, E.EventCode as TellingCode
, E.EventLabel as Telling
, E.SortOrder as TellingSortOrder
, S.sampleDate as Teldatum
--, S.IceCoverCode as Ijsbedekking
--, S.SnowCoverCode as Sneeuwbedekking
, S.CoverageCode as Telvolledigheid
, T.Commonname as NedNaam
, T.scientificname as WetNaam
, T.TaxonGroupCode
, F.Taxoncount as Aantal

FROM FactAnalyseSetOccurrence F
inner join DimSurvey SU on SU.surveykey = F.surveykey
inner join DimLocationWV L on L.locationwvkey = F.locationwvkey
inner join DimSeason SS on SS.Seasonkey = F.seasonkey
inner join DimEvent E on E.eventkey = F.eventkey
inner join DimSample S on F.samplekey = S.samplekey
inner join DimTaxonWV T on T.taxonwvkey = F.taxonwvkey
WHERE 1=1
  AND S.samplestatus = 'CHECKED' /**alleen gevalideerde records**/
  AND S.coveragecode not in ('-', 'N')  /**niet getelde tellingen zijn irrelevant**/
  AND F.taxoncount > 0
  AND F.Analysesetkey in (1) /**alleen boottellingen, i.e. uit ZSCH-analyseset**/
--and SS.seasonname in ('2013/14','2014/15','2015/16','2016/17', '2017/18')
--and T.isgoose = 1;")
dbDisconnect(con)
WaterbirdsBT %>% write_csv("data-output/WaterbirdsBT.csv") # BT van boottellingen
WaterbirdsBT  <-  read_csv("data-output/WaterbirdsBT.csv")

#########################
# Correcties WaterbirdsBT
#########################
# Er zitten dubbels in de dataset:
WaterbirdsBT  %>% 
  group_by(Gebied, Telseizoen, Telling, Teldatum, NedNaam, ProjectCode) %>% 
  summarise(n = n()) %>% 
  filter(n>1) %>% 
  group_by(ProjectCode) %>% 
  count() %>% 
  ungroup()
# Correctie hiervoor
WaterbirdsBT <- WaterbirdsBT  %>% 
  group_by(Gebied, ProjectCode, RegioCode, Telseizoen, TellingCode, Telling, TellingSortOrder, 
           Teldatum, Telvolledigheid, NedNaam, WetNaam, TaxonGroupCode) %>% 
  summarise(Aantal = max(Aantal)) %>% 
  ungroup()

# Twee gebieden in dataset die er niet thuishoren: 
WaterbirdsBT %>% 
  filter(str_detect(Gebied, "aapskooi")|str_detect(Gebied, "Oude Dijle ")) %>% 
  group_by(Gebied) %>% summarise()
# Correctie
WaterbirdsBT <- WaterbirdsBT %>% filter(!str_detect(Gebied, "aapskooi"),
                                        !str_detect(Gebied, "Oude Dijle "))
# Enkel wintertellingen
WaterbirdsBTw <- WaterbirdsBT %>% filter(month(Teldatum) %in% c(10, 11, 12, 1, 2, 3))


##################################################
# Query Warehouse - alle data van MIDMA-analyseset
##################################################
# query all valid(ated) records from MIDMA-alaysesets (takes a while), further selection is carried out in R (another option would be to carry out selection in SQl query below)
Waterbirds <- dbGetQuery(con,
"SELECT
  SU.SurveyCode as ProjectCode
, SU.SurveyNaam as Project
, L.RegioWVCode as RegioCode
, L.RegioWVNaam as Regio
, L.LocationWVCode as GebiedsCode
, L.LocationWVNaam as Gebied
, SS.Seasonname as Telseizoen
, E.EventCode as TellingCode
, E.EventLabel as Telling
, E.SortOrder as TellingSortOrder
, S.sampleDate as Teldatum
--, S.IceCoverCode as Ijsbedekking
--, S.SnowCoverCode as Sneeuwbedekking
, S.CoverageCode as Telvolledigheid
, T.Commonname as NedNaam
, T.scientificname as WetNaam
, T.TaxonGroupCode
, F.Taxoncount as Aantal

FROM FactAnalyseSetOccurrence F
inner join DimSurvey SU on SU.surveykey = F.surveykey
inner join DimLocationWV L on L.locationwvkey = F.locationwvkey
inner join DimSeason SS on SS.Seasonkey = F.seasonkey
inner join DimEvent E on E.eventkey = F.eventkey
inner join DimSample S on F.samplekey = S.samplekey
inner join DimTaxonWV T on T.taxonwvkey = F.taxonwvkey
WHERE 1=1
  AND S.samplestatus = 'CHECKED' /**alleen gevalideerde records**/
  AND S.coveragecode not in ('-', 'N')  /**niet getelde tellingen zijn irrelevant**/
  AND F.taxoncount > 0
  AND F.Analysesetkey in (2, 3, 4) /**alleen midmaandelijkse tellingen, i.e. uit MIDMA-analysesets**/
--and SS.seasonname in ('2013/14','2014/15','2015/16','2016/17', '2017/18')
--and T.isgoose = 1;")
Waterbirds %>% write_csv("data-output/Waterbirds.csv")

dbDisconnect(con)

# Correctie dubbels
Waterbirds <- Waterbirds  %>% 
  group_by(Gebied, ProjectCode, RegioCode, Telseizoen, TellingCode, Telling, TellingSortOrder, 
           Teldatum, Telvolledigheid, NedNaam, WetNaam, TaxonGroupCode) %>% 
  summarise(Aantal = max(Aantal)) %>% 
  ungroup()

# Gebiedskenmerken
##################
# Ik heb een lijst met telgebieden aangemaakt met een aantal kenmerken (pad aanpassen)
Gebied2 <- read_csv(file = "c://R/Projects/OverwinterendeVogels/data/Gebieden2.csv")
str(Gebied2)
# Gebied: naam van telgebied zoals in databank
# Estuarien: 1: getijderivier, 0: geen getijderivier (estuariene Sigmaprojectgebieden: 0)
# Vallei: 1: valleigebied; 0: geen valleigebied
# Sigma: 1 Sigmaprojectgebied, 0: geen Sigmaprojectgebied
# NOHaven: 1: Natuurontwikkelingsgebiede van de haven, 0: neen NOgebied van haven
# Bijkomend voor de Sigmaprojectgebieden is er bijkomende info.

# Selectie Zijrivieren
######################
# Berekenen zijrivieren populatie (midma analyseset)
WaterbirdsZR <- Waterbirds %>% 
  inner_join(Gebied2, by = c("Gebied")) %>% # Gebied2.csv is een tabel met kenmerken van telgebieden
  filter(Estuarien == 1) %>%                # enkel estuariene gebieden (getijderivieren) selecteren
  anti_join(WaterbirdsBTw, by = c("Gebied"))# verwijder de telgebieden van Zeeschelde/boottellingen
Zijriviergebieden <- WaterbirdsZR %>% group_by(Gebied) %>% summarise()

# Ook Gunther heeft lijst met gebieden gemaakt:
GebiedGunther <- read_csv("G:/Mijn Drive/PRJ_SCHELDE/Watervogels/R/Zeeschelde_gebiedsgroepen.csv")
ZijrivierenGunther <- GebiedGunther %>% filter(rivier != "Zeeschelde", gebiedsgroep == "Estuarien")
# Test of het over dezelfdegebieden gaat
ZijrivierenGunther %>% 
  anti_join(Zijriviergebieden, by = c("gebiedsnaam" = "Gebied")) %>% 
  select(gebiedsnaam)
# GGG Zennegat is een Sigmagebied, dit mag niet meegeteld worden bij de zijrivieren. De drie andere trajecten (Rupel ...) zijn bij de boottellingen zitten en dus ook in Zeeschelde analyseset.
Zijriviergebieden %>% 
  anti_join(ZijrivierenGunther, by = c("Gebied" = "gebiedsnaam")) %>% 
  select(Gebied)

# Populatie Zeeschelde (Zeeschelde analyseset) + zijrivierern (midma analyseset)
WaterbirdsZSCZR <- WaterbirdsBTw %>% bind_rows(WaterbirdsZR)


# Selectie Sigmapopulatie
#########################
# Bereken sigmapopulatie (midma analyseset)
Waterbirds_Sigma <- Waterbirds %>%
  inner_join(Gebied2, by = c("Gebied")) %>%
  filter(Sigma == 1)
WaterbirdsZSCZRSigma <- Waterbirds_Sigma %>% bind_rows(WaterbirdsZSCZR)


###########################
# Bereken wintermax functie
###########################
# 1. Per telseizoen, maand (telling) wordt de som van de aantallen van alle getelde vogels berekend
# 2. Per telseizoen wordt het maximum bepaald van de maandsommen.

SeizMax <- function(data, type){
  data %>% 
    mutate(Telseizoen1 = as.numeric(str_sub(Telseizoen, 1, 4))) %>% 
    filter(Telseizoen1 %in% c(1991:2018), 
           TaxonGroupCode != "MS") %>%   # geen meeuwen of sternen
    mutate(maand = month(Teldatum)) %>% 
    group_by(Telseizoen1, maand) %>% 
    summarise(Aantal = sum(Aantal)) %>% 
    group_by(Telseizoen1) %>% 
    summarise(Wmax = max(Aantal)) %>% 
    mutate(Type = type)
}
# Toepassing
Wmax_Zsch <- SeizMax(WaterbirdsBTw, "Zeeschelde")
Wmax_ZschZR <- SeizMax(WaterbirdsZSEZR, "Zeeschelde + zijrivieren")
Wmax_ZschZRSigma <- SeizMax(WaterbirdsZSCZRSigma, "Zeeschelde + zijrivieren + Sogmagebieden")

########################
# Plot wintermax functie
########################
# Bereken glijdend gemiddelde met CI
# data = tibble, x = column for x-data, value = column with data for rolling mean, n = width of average(window size)
mav_wm <- function(data, x, value, n){
  rol <- data %>%
    mutate(x = x,
           v = value,
           m = roll_mean(value,n=n, fill = NA),
           sd = roll_sd(value,n=n, fill = NA),
           ci_min = m + 1.96*sd/sqrt(n),
           ci_max = m - 1.96*sd/sqrt(n)) 
  return(rol)
}
mav_wm(Wmax_Zsch, Wmax_Zsch$Telseizoen1, Wmax_Zsch$Wmax, n=5)

# Plot glijdend gemiddelde
plot_mav <- function(data){
  if(n_distinct(data$type)<2){
    data %>% ggplot(aes(x=x)) + 
      geom_errorbar(aes(ymin=ci_min, ymax=ci_max), width=.2) +
      geom_point(aes(y=m), size = 2.5, color="blue") +
      geom_line(aes(y=m), color="blue") +
      geom_hline(yintercept = 40000, linetype = 2, size = 1.4) +
      labs(y = "Wintermaximum") +
      scale_x_continuous(breaks = seq(1990, 2020, by = 5)) + 
      scale_y_continuous(breaks = seq(0, 85000, by = 20000),
                         limits = c(0, 85000)) +
      theme(axis.title.x = element_blank()) }
  else{
    data %>% ggplot(aes(x=x)) + 
      geom_errorbar(aes(ymin=ci_min, ymax=ci_max, color = type), width=.2,
                    position=position_dodge(0.1)) +
      geom_point(aes(y=m, color = type), size = 2.5) +
      geom_line(aes(y=m, color = type)) +
      geom_hline(yintercept = 40000, linetype = 2, size = 1.4) +
      labs(y = "Wintermaximum") +
      scale_x_continuous(breaks = seq(1990, 2020, by = 5)) + 
      scale_y_continuous(breaks = seq(0, 85000, by = 20000),
                         limits = c(0, 85000)) +
      theme(axis.title.x = element_blank()) +
      scale_color_manual(values=c("blue", "red"))+
      theme(legend.title=element_blank(),
            legend.position="top") }
}

# Combineer twee vorige functies
graph_mav <- function(data, x, value, n){
  rol <- mav_wm(data, x, value, n)
  plot_mav(rol)
}

# Voorbeelden
a <- Wmax_Zsch %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
plot_mav(a)
Wmax_ZschZR %>% graph_mav(.$Telseizoen1, .$Wmax, 5)
graph_mav(Wmax_ZschZRSigma, Wmax_ZschZRSigma$Telseizoen1, Wmax_ZschZRSigma$Wmax, 5)
Wmax_ZschZR %>% bind_rows(Wmax_ZschZRSigma) %>% 
  graph_mav(.$Telseizoen1, .$Wmax, 5)

# Grafieken
###########
# Wintermaxima Zeeschelde (boottrajecten)
Wmax_Zsch %>% graph_mav(.$Telseizoen1,.$Wmax, 5) +
  labs(title = "Wintermaxima Zeeschelde (boottrajecten)")
# Wintermaxima Zeeschelde (boottrajecten) + zijrivieren
Wmax_ZschZR %>% graph_mav(.$Telseizoen1,.$Wmax, 5) +
  labs(title = "Wintermaxima Zeeschelde (boottrajecten) + zijrivieren")
# Wintermaxima Zeeschelde (boottrajecten) + zijrivieren + sigmaprojectgebieden
Wmax_ZschZRSigma %>% graph_mav(.$Telseizoen1,.$Wmax, 5) +
  labs(title = "Wintermaxima Zeeschelde (boottrajecten) + zijrivieren + sigmagebieden")
# Wintermaxima Zeeschelde (boottrajecten) + zijrivieren met en zonder sigmaprojectgebieden
a <- Wmax_ZschZR %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
b <- Wmax_ZschZRSigma %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
a %>% bind_rows(b) %>% plot_mav() + labs(title = "Wintermaxima")

# Wintermaxima Zeeschelde (boottrajecten) + zijrivieren met en zonder 3 soorten
WmaxZschZR_2000 <- WaterbirdsZSCZR %>% 
  filter(as.numeric(str_sub(Telseizoen, 1, 4)) > 1999) %>% 
  SeizMax(type = "met kievit, wulp en waterhoen")
WmaxZschZR_KiWhW <- WaterbirdsZSCZR %>% 
  filter(!NedNaam %in% c("Kievit", "Waterhoen", "Wulp")) %>% 
  SeizMax(type = "zonder kievit, wulp en waterhoen")
a <- WmaxZschZR_2000 %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
b <- WmaxZschZR_KiWhW %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
a %>% bind_rows(b) %>% plot_mav() + labs(title = "Wintermaxima Zeeschelde + zijrivieren")

# Wintermaxima Zeeschelde (boottrajecten) + zijrivieren + Sigmaprojectgebieden met en zonder 3 soorten
WmaxZschZRSigma_2000 <- WaterbirdsZSCZRSigma %>% 
  filter(as.numeric(str_sub(Telseizoen, 1, 4)) > 1999) %>% 
  SeizMax(type = "met kievit, wulp en waterhoen")
WmaxZschZRSigma_KiWhW <- WaterbirdsZSCZRSigma %>% 
  filter(!NedNaam %in% c("Kievit", "Waterhoen", "Wulp")) %>% 
  SeizMax(type = "zonder kievit, wulp en waterhoen")
a <- WmaxZschZRSigma_2000 %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
b <- WmaxZschZRSigma_KiWhW %>% mav_wm(.$Telseizoen1, .$Wmax, 5)
a %>% bind_rows(b) %>% plot_mav() + labs(title = "Wintermaxima Zeeschelde + zijrivieren + Sigmagebieden")
