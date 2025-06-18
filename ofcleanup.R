library(tidyverse)
library(data.table)
library(readxl)
library(googlesheets4)
library(googledrive)
library(nanoparquet)
library(spData)
library(sf)

options(scipen = 9999) #I hate scientific notation

setwd("C:/Dropbox/Data/shinyapps/devfinance")

crs <- read_parquet("CRS.parquet")
cydata <- fread("C:/Dropbox/Data/shinyapps/cydata_may2023.csv", encoding = 'UTF-8')
mergenames <- read_sheet("https://docs.google.com/spreadsheets/d/1rMtYcKZ76UMS8RPX4yRxS859hHl7P6xkLFP3241H-Ks/edit?usp=sharing") %>%
  unique() #Merge names to get standardized country names.

# CRS Transactions Level preparation -- this is designed to match transactions from AidData as much as possible.
crstrans <- crs %>%
  transmute(year, recipient = recipient_name, donor = donor_name, agency = agency_name, flow = flow_name, title = project_title,
            description = short_description, amtdisb = usd_disbursement_defl, amtcommit = usd_commitment_defl, 
            sectorcode = sector_code, sectorname = sector_name, bi_multi) %>%
  mutate(amtdisb = amtdisb*1000000) %>%
  mutate(amtcommit = amtcommit*1000000) %>%
  mutate(multilat = ifelse(bi_multi==4, 1, 0)) %>%
  mutate(private = ifelse(bi_multi==6, 1, 0))
  
crsctry <- crstrans %>%
  mutate(private = ifelse(donor=="Denmark", 0, private)) %>%  #for some reason there are a handful of observations where the country is coded as private flow.
  mutate(private = ifelse(donor=="Hungary", 0, private)) %>%
  mutate(private = ifelse(donor=="Korea", 0, private)) %>%
  group_by(year, recipient, donor, multilat, private, flow, sectorcode, sectorname) %>%
  summarise(commit = sum(amtcommit, na.rm=T), disb = sum(amtdisb, na.rm = T)) %>%
  ungroup() %>%
  left_join(mergenames, by=c("recipient" = "cname_merge")) %>%
  mutate(country_display_short = ifelse(is.na(country_display_short), recipient, country_display_short)) %>%
  rename(recipient_display_short = country_display_short) %>%
  left_join(mergenames, by=c("donor" = "cname_merge")) %>%
  mutate(donor_display_short = ifelse(is.na(country_display_short), donor, country_display_short)) %>%
  filter(commit!=0 | disb!=0) %>% #get rid of no data entries so the file size is smaller.
  select(recipient_display_short, donor_display_short, year, multilat, private, flow, sectorcode, sectorname, commit, disb)


ofwide <- crsctry %>%
  left_join(cydata, by=c("donor_display_short" = "country_display_short", "year")) %>%
  transmute(recipient_display_short, donor_display_short, year, donor_multilat = multilat
            , donor_private = private, donor_eu = eu, flow, sectorcode, sectorname, commit, disb) %>%
  mutate(donor_eu = ifelse(donor_display_short=="EU Institutions", 1, donor_eu),
         donor_bilat = ifelse(donor_multilat==0 & donor_private==0, 1, 0)) %>%
  left_join(cydata, by=c("recipient_display_short" = "country_display_short", "year")) %>%
  transmute(recipient_display_short, donor_display_short, year, donor_multilat, donor_private, donor_eu, donor_bilat,
            recipient_iso = iso_code, recipient_region_display_complete_abb = region_display_complete_abb, 
            flow, sectorcode, sectorname, commit, disb) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Africa, regional", "SSA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="America, regional", "LAC", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Asia, regional", "ASIA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Caribbean & Central America, regional", "LAC", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Caribbean, regional", "LAC", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Central America, regional", "LAC", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Central Asia, regional", "ASIA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Eastern Africa, regional", "SSA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Europe, regional", "E&E", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Far East Asia, regional", "ASIA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Melanesia, regional", "OCE", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Micronesia, regional", "OCE", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Middle Africa, regional", "SSA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Middle East, regional", "MENA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="North of Sahara, regional", "MENA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Oceania, regional", "OCE", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Polynesia, regional", "OCE", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="South & Central Asia, regional", "ASIA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="South America, regional", "LAC", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="South Asia, regional", "ASIA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="South of Sahara, regional", "SSA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Southern Africa, regional", "SSA", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="States Ex-Yugoslavia unspecified", "E&E", recipient_region_display_complete_abb)) %>%
  mutate(recipient_region_display_complete_abb = ifelse(recipient_display_short=="Western Africa, regional", "SSA", recipient_region_display_complete_abb))

##getting long form -- we have aggregates already in wide form -- these need to be removed
##from the ctry-ctry calculation, but included in agg-ctry and ctry-agg calculation.
##We do not do agg-agg in long form.

ofctrylong <- ofwide %>%
  filter(recipient_display_short!="Africa, regional") %>%
  filter(recipient_display_short!="America, regional") %>%
  filter(recipient_display_short!="Asia, regional") %>%
  filter(recipient_display_short!="Bilateral, unspecified") %>%
  filter(recipient_display_short!="Caribbean & Central America, regional") %>%
  filter(recipient_display_short!="Caribbean, regional") %>%
  filter(recipient_display_short!="Central America, regional") %>%
  filter(recipient_display_short!="Central Asia, regional") %>%
  filter(recipient_display_short!="Eastern Africa, regional") %>%
  filter(recipient_display_short!="Europe, regional") %>%
  filter(recipient_display_short!="Far East Asia, regional") %>%
  filter(recipient_display_short!="Melanesia, regional") %>%
  filter(recipient_display_short!="Micronesia, regional") %>%
  filter(recipient_display_short!="Middle Africa, regional") %>%
  filter(recipient_display_short!="Middle East, regional") %>%
  filter(recipient_display_short!="Multi-Region") %>%
  filter(recipient_display_short!="North of Sahara, regional") %>%
  filter(recipient_display_short!="Oceania, regional") %>%
  filter(recipient_display_short!="Polynesia, regional") %>%
  filter(recipient_display_short!="South & Central Asia, regional") %>%
  filter(recipient_display_short!="South America, regional") %>%
  filter(recipient_display_short!="South Asia, regional") %>%
  filter(recipient_display_short!="South of Sahara, regional") %>%
  filter(recipient_display_short!="Southern Africa, regional") %>%
  filter(recipient_display_short!="States Ex-Yugoslavia unspecified") %>%
  filter(recipient_display_short!="Western Africa, regional") %>%
  transmute(recipient_display_short, recipient_iso, donor_display_short, year, flow, sectorcode, sectorname,
            commit, disb, recipientagg = 0, donoragg = 0, donor_multilat, donor_private, donor_eu, donor_bilat)

ofragglong <- ofwide %>%
  select(-c("recipient_display_short", "recipient_iso")) %>%
  mutate(recipient_world = "World") %>%
  pivot_longer(cols = starts_with("recipient_"), names_to = "recipient", names_prefix = "recipient_") %>%
  filter(!is.na(value)) %>%
  filter(value!="") %>%
  group_by(donor_display_short, year, value, flow, sectorcode, sectorname, donor_multilat, donor_private, donor_eu, donor_bilat) %>%
  summarize(commit = sum(commit, na.rm = T), disb = sum(disb, na.rm = T)) %>%
  ungroup() %>%
  transmute(recipient_display_short = value, recipient_iso = "", donor_display_short, year, flow, sectorcode, sectorname,
            commit, disb, recipientagg = 1, donoragg = 0, donor_multilat, donor_private, donor_eu, donor_bilat)

ofdagglong <- rbind(ofctrylong, ofragglong) %>%
  select(-c("donor_display_short", "donoragg")) %>%
  mutate(donor_multilat = ifelse(donor_multilat==1, "Multilateral", "")) %>%
  mutate(donor_eu = ifelse(donor_eu==1, "EU Members and Institutions", "")) %>%
  mutate(donor_private = ifelse(donor_private==1, "Private Donors", "")) %>%
  mutate(donor_bilat = ifelse(donor_bilat==1, "Bilateral Donors", "")) %>%
  mutate(donor_world = "World") %>%
  pivot_longer(cols = starts_with("donor_"), names_to = "donor", names_prefix = "donor_") %>%
  filter(!is.na(value)) %>%
  filter(value!="") %>%
  group_by(recipient_display_short, recipient_iso, year, value, flow, sectorcode, sectorname, recipientagg) %>%
  summarize(commit = sum(commit, na.rm = T), disb = sum(disb, na.rm = T)) %>%
  ungroup() %>%
  transmute(recipient_display_short, recipient_iso, donor_display_short=value, year, flow, sectorcode, sectorname,
            recipientagg, donoragg = 1, commit, disb)

worldcodes <- as.data.frame(world) %>%
  select(iso_a2, name_long) %>%
  left_join(mergenames, by=c("name_long" = "cname_merge")) 

oflong <- rbind(ofctrylong, ofragglong) %>%
  transmute(recipient_display_short, recipient_iso, donor_display_short, year, flow, sectorcode, sectorname,
            recipientagg, donoragg, commit, disb) %>%
  rbind(ofdagglong) %>%
  left_join(worldcodes, by=c("recipient_display_short" = "country_display_short")) %>%
  mutate(recipient_display_short = recode(recipient_display_short,
                                          "ASIA" = "Asia",
                                          "LAC" = "Latin America and the Caribbean",
                                          "MENA" = "Middle East and North Africa",
                                          "NAM" = "North America",
                                          "OCE" = "Oceania",
                                          "SSA" = "Sub-Saharan Africa",
                                          "World" = "World")) %>%
  filter(year>1999) %>%
  mutate(topsec = ifelse(sectorcode>99 & sectorcode<200, "Social Infrastructure & Services",
                   ifelse(sectorcode>199 & sectorcode<300, "Economic Infrastructure & Services",
                    ifelse(sectorcode>299 & sectorcode<400, "Production Sectors",
                     ifelse(sectorcode>399 & sectorcode<500, "Multi-Sector/Cross-Cutting",
                      ifelse(sectorcode>499 & sectorcode<700, "Budget, Debt, or Food Assistance",
                       ifelse(sectorcode>699 & sectorcode<900, "Humanitarian Aid", "Other and Administrative Costs"))))))) %>%
  transmute(recipient_display_short, recipient_isocc = iso_a2, donor_display_short, year, flow, topsec, sectorname,
            recipientagg, donoragg, commit, disb)

#getting donor factors right
ofldonors <- oflong %>%
  select(donor_display_short) %>%
  unique()

donorfactors <- ofwide %>%
  select(donor_display_short, donor_multilat, donor_private) %>%
  unique() %>%
  right_join(ofldonors, by=c("donor_display_short")) %>%
  mutate(donor_private = ifelse(donor_display_short=="EU Members and Institutions", 0, donor_private),
         donor_multilat = ifelse(donor_display_short=="EU Members and Institutions", 0, donor_multilat)) %>%
  arrange(donor_private, donor_multilat, donor_display_short) %>%
  mutate(rank = row_number()) %>%
  mutate(rank = ifelse(donor_display_short=="World", -5,
                 ifelse(donor_display_short=="Multilateral", -4,
                  ifelse(donor_display_short=="Bilateral Donors", -3,
                   ifelse(donor_display_short=="Private Donors", -2, rank))))) %>%
  arrange(rank)

oflong$donor_display_short <- factor(oflong$donor_display_short, 
                                levels=donorfactors$donor_display_short[order(donorfactors$rank)], ordered = T)

of <- oflong %>%
  arrange(donor_display_short)

save(of, file="devfinance/of.Rda")
saveRDS(of, file="devfinance/of.rds")

write.csv(oflong, "devfinance/oflong.csv", row.names = F, na = "")
