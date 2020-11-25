##### Economic Cost #####

rm(list = ls())
graphics.off()
library(lubridate)
library(tidyverse)
library(readxl)

#### Tabelle Kurzarbeit ####
Kurzarbeit=read_xlsx("Data/Lohn_Vollzeitäquivalente_Wirtschaftsabteilung.xlsx")

Kurzarbeit=Kurzarbeit[, c(2,1,3:ncol(Kurzarbeit))]

Kurzarbeit=add_column(Kurzarbeit, "Fragliche Lohnsumme"=Kurzarbeit$`Monatlicher Bruttolohn`*
                        Kurzarbeit$Vollzeitäquivalente)


#### Angaben Notkredite ####
Kreditausfallquote=0.05
KreditvolumenKMU=20000000000
KreditvolumenGrossunternehmen=20000000000
ZinsGrosskredite=0.005
BürgungsquoteBundGrosskredite=0.85
BürgungsquoteKMU=1

#### Tabelle Steuerausfälle ####
Steuerausfall=read_xlsx("Data/Gewinnberechnung_Wirtschaftsabteilung_in1000.xlsx")
Steuerausfall=Steuerausfall[-c(11:12),]

# Berechnung erwarteter Gewinnsteuereinnahmen ohne Corona (einfach mal 1'000)
Bundessteuersatz=0.085
Steuerausfall=add_column(Steuerausfall, "Erwartete Gewinnsteuereinnahmen ohne Corona"=
                           Steuerausfall$`Gewinn (in 1000)`*1000*Bundessteuersatz)

# Ersetze alle negativen Steuereinnahmen durch 0
for (i in 1:nrow(Steuerausfall)) {
  if(Steuerausfall[i,"Erwartete Gewinnsteuereinnahmen ohne Corona"]<0){
    Steuerausfall[i,"Erwartete Gewinnsteuereinnahmen ohne Corona"]=0
  }
}

# Berechnung erwarteter Mehrwertsteuereinnahmen ohne Corona
Steuerausfall=add_column(Steuerausfall, "Erwartete Mehrwertsteuereinnahmen ohne Corona"=
                           Steuerausfall$`Geschaffener Mehrwert (in 1000)`*1000*
                           Steuerausfall$Mehrwertsteuersatz) 

# Füge die Exposure zur Tabelle hinzu
Steuerausfall=cbind(Steuerausfall, Kurzarbeit[,c("Exposure_Ld","Exposure_NoLd_S1","Exposure_NoLd_S2",
                                                 "Exposure_NoLd_S3")])

#### Costs in case of Lockdown, No Lockdown 1, NoLd2, NoLd3 ####

# Kurzarbeit Expenses
t_Ld=1.1 # Lockdown duration = 33 days = 1.1 months

# Lockdown Act
KaExpLd=sum(Kurzarbeit$`Fragliche Lohnsumme`*Kurzarbeit$Exposure_Ld*0.8*t_Ld)

# No Lockdown Act (3 scenarios)
KaExpNoLd1=sum(Kurzarbeit$`Fragliche Lohnsumme`*Kurzarbeit$Exposure_NoLd_S1*0.8*t_Ld)
KaExpNoLd2=sum(Kurzarbeit$`Fragliche Lohnsumme`*Kurzarbeit$Exposure_NoLd_S2*0.8*t_Ld)
KaExpNoLd3=sum(Kurzarbeit$`Fragliche Lohnsumme`*Kurzarbeit$Exposure_NoLd_S3*0.8*t_Ld)


# Credit Defaults

# Lockdown Act
CredDefLd=Kreditausfallquote*KreditvolumenKMU+Kreditausfallquote*BürgungsquoteBundGrosskredite*
  KreditvolumenGrossunternehmen

# No Lockdown Act (3 scenarios)
CredDefNoLd1=0
CredDefNoLd2=0
CredDefNoLd3=0

# Corporate Profit Tax Shortfalls

# Lockdown Act
CorpProfTaxShortLd=sum(Steuerausfall$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
  Steuerausfall$Exposure_Ld*t_Ld/12, na.rm = T)

# No Lockdown Act (3 scenarios)
CorpProfTaxShortNoLd1=sum(Steuerausfall$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                         Steuerausfall$Exposure_NoLd_S1*t_Ld/12, na.rm = T)
CorpProfTaxShortNoLd2=sum(Steuerausfall$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                            Steuerausfall$Exposure_NoLd_S2*t_Ld/12, na.rm = T)
CorpProfTaxShortNoLd3=sum(Steuerausfall$`Erwartete Gewinnsteuereinnahmen ohne Corona`*
                            Steuerausfall$Exposure_NoLd_S3*t_Ld/12, na.rm = T)

# Value Added Tax Shortfalls

# Lockdown Act
ValAddTaxShortLd=sum(Steuerausfall$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
  Steuerausfall$Exposure_Ld*t_Ld/12, na.rm = T)

# No Lockdown Act (3 scenarios)
ValAddTaxShortNoLd1=sum(Steuerausfall$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                       Steuerausfall$Exposure_NoLd_S1*t_Ld/12, na.rm = T)
ValAddTaxShortNoLd2=sum(Steuerausfall$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                          Steuerausfall$Exposure_NoLd_S2*t_Ld/12, na.rm = T)
ValAddTaxShortNoLd3=sum(Steuerausfall$`Erwartete Mehrwertsteuereinnahmen ohne Corona`*
                          Steuerausfall$Exposure_NoLd_S3*t_Ld/12, na.rm = T)

#### Übrige einmalige Kosten ####

# Lockdown Act
OEELd=4000000000

# No Lockdown Act (3 scenarios)
OEENoLd1=0
OEENoLd2=0
OEENoLd3=0


#### Economic Cost Table ####

# Sum the costs up
EconCostsLd=KaExpLd+CredDefLd+CorpProfTaxShortLd+ValAddTaxShortLd+OEELd
EconCostsNoLd1=KaExpNoLd1+CredDefNoLd1+CorpProfTaxShortNoLd1+ValAddTaxShortNoLd1+OEENoLd1
EconCostsNoLd2=KaExpNoLd2+CredDefNoLd2+CorpProfTaxShortNoLd2+ValAddTaxShortNoLd2+OEENoLd2
EconCostsNoLd3=KaExpNoLd3+CredDefNoLd3+CorpProfTaxShortNoLd3+ValAddTaxShortNoLd3+OEENoLd3

# Create the Economic Table
Prior=c(1/3,1/3,1/3)
EconCostsLd=c(EconCostsLd,EconCostsLd,EconCostsLd)
EconCostsNoLd=c(EconCostsNoLd1,EconCostsNoLd2,EconCostsNoLd3)
AddEconCostsLd=EconCostsLd-EconCostsNoLd
Scenario=c("pessimistic", "neutral","optimistic")
EconShockExp=c("low", "medium", "high")
EconCost=data.frame(Prior,EconCostsLd,EconCostsNoLd, AddEconCostsLd, Scenario, EconShockExp)
EconCostMrd=cbind(Prior,EconCost[,c(-1,-5,-6)]/1000000000,Scenario, EconShockExp)
EconCost
EconCostMrd




