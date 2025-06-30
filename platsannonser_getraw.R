# Funktionen plattar ut listor som finns i listkolumner. För att det ska
# fungera krävs att listan i listkolumen har variabelnamn 


library(tidyverse)
library(rvest)
library(httr)
library(xml2)
library(RCurl)
library(magrittr)
library(jsonlite)
library(purrr)
library(ggfittext)
library(writexl)
library(rio)



# URL till Jobtechs api ange från när annonserna ska vara utlagda och senaste uppdaterade
annonsyrkesomr_url <- paste0("https://jobstream.api.jobtechdev.se/stream?date=2025-04-01T00:00:00&updated-before-date=2025-06-24T12:00:00")

# Ladda hem annonser
dfannonser = content(GET(annonsyrkesomr_url,
                         config =
                           (
                             add_headers()
                           )),
                     simplifyDataFrame = TRUE) %>%
  as_tibble()


# Ta bort annonser som blivit tillbakatagna av arbetsgivaren (de saknar data!) och
# platta ut de kolumner som låter sig plattas ut. Oklar över varför det inte funkar
# för alla kolumner

dftraffar <- dfannonser %>%
  filter(removed == FALSE) %>%
  flatten(recursive = TRUE) %>% 
  select(-description.text_formatted)


# Av någon anledning går det inte att ladda hem alla annonser i ett svep. Jag måste 
# ladda hem tidiga annonser för sig, annars får jag ett diffust
# "incorrect end of file"-fel

annonsyrkesomr_url_tidiga <- paste0("https://jobstream.api.jobtechdev.se/stream?date=2024-12-01T00:00:00&updated-before-date=2025-03-31T23:59:59")

dfannonser_tidiga = content(GET(annonsyrkesomr_url_tidiga,
                                config =
                                  (
                                    add_headers()
                                  )),
                            simplifyDataFrame = TRUE) %>%
  as_tibble()

dftraffar_tidiga <- dfannonser_tidiga %>%
  filter(removed == FALSE) %>%
  flatten(recursive = TRUE) %>% 
  select(-description.text_formatted)

# Sortera bort eventuella dubletter mellan dataseten med nyare och tidigare annonser
dftraffar_tidiga <- dftraffar_tidiga %>% 
  filter(!(id %in% dftraffar$id))

# Slå samman senare annonser med tidigare
dftraffar_sammanslagna <- bind_rows(dftraffar, dftraffar_tidiga) %>% 
  tibble()

# Kolla om antalet platser stämmer någorlunda med vad som uppges i Platsbanken
sum(dftraffar_sammanslagna$number_of_vacancies, na.rm = TRUE)


# Välj ut relevanta kol.
df <- dftraffar_sammanslagna %>% 
  select(id, number_of_vacancies, employment_type.label, working_hours_type.label,
         duration.label,
         
         employer.workplace, workplace_address.municipality_code,
         
         occupation_field.label, 
         ssyk4 = occupation_group.legacy_ams_taxonomy_id, 
         occupation_group.label,
         
         must_have.languages, access_to_own_car, driving_license_required,
         
         description.text)

# Un-list col.
df$must_have.languages <- sapply(df$must_have.languages, function(lst) lst$label)

# Try to make cell-values with string="c()" to only view chr 
df$must_have.languages <- gsub('[c\\(\\)]', '', df$must_have.languages)

## Mutate driving: T/F to 1/0 and kval-niva, case_when() är ordningsstryd
df <- df %>%
  mutate(driving_license_value = as.integer(driving_license_required)) %>%
  mutate(kvalifikations_niva = case_when(
    substr(ssyk4, 1, 2) == "17" ~ 3,
    substr(ssyk4, 1, 2) == "01" ~ 4,
    substr(ssyk4, 1, 2) == "02" ~ 3,
    substr(ssyk4, 1, 2) == "03" ~ 2,
    substr(ssyk4, 1, 1) %in% c("9") ~ 1,
    substr(ssyk4, 1, 1) %in% c("4", "5", "6", "7", "8") ~ 2,
    substr(ssyk4, 1, 1) %in% c("3") ~ 3,
    substr(ssyk4, 1, 1) %in% c("1", "2") ~ 4
  )) %>% 
  mutate(kval_label = case_when(
    kvalifikations_niva == 1 ~ "Inga eller låga formella utbildningskrav",
    kvalifikations_niva == 2 ~ "Gymnasie eller eftergymnasial utbildning kortare än 2 år",
    kvalifikations_niva == 3 ~ "Praktiska eller yrkesspecifika eftergymnasiala utbildningar på 2-3 år",
    kvalifikations_niva == 4 ~ "Teoretiska eftergymnasiala utbildningar om minst 3 år",
    is.na(kvalifikations_niva) ~"Uppgift saknas"
    
  ))


## Create ssyk2_code from ssyk4
df <- mutate(df, ssyk2_code = substr(ssyk4, 1, 2))

## Mutate w ssyk2_text
df <- df %>%
  mutate(ssyk2_text = case_when(
    ssyk2_code == 11 ~ "Politiker, verkställande direktörer och högre ämbetsmän m.fl.",
    ssyk2_code == 12 ~ "Chefer inom ekonomi, personal, marknadsföring och försäljning samt annan administration m.m.",
    ssyk2_code == 13 ~ "Chefer inom IT, logistik, FoU, fastighetsbolag, bygg- och ingenjörsverksamhet samt tillverkning m.m.",
    ssyk2_code == 14 ~ "Chefer inom utbildning",
    ssyk2_code == 15 ~ "Chefer inom hälso- och sjukvård samt annan samhällsservice",
    ssyk2_code == 16 ~ "Chefer inom bank, finans och försäkring",
    ssyk2_code == 17 ~ "Chefer inom övrig servicenäring",
    ssyk2_code == 21 ~ "Yrken med krav på fördjupad högskolekompetens inom naturvetenskap och teknik",
    ssyk2_code == 22 ~ "Yrken med krav på fördjupad högskolekompetens inom hälso- och sjukvård",
    ssyk2_code == 23 ~ "Yrken med krav på fördjupad högskolekompetens inom utbildning",
    ssyk2_code == 24 ~ "Yrken med krav på fördjupad högskolekompetens inom ekonomi och förvaltning",
    ssyk2_code == 25 ~ "Yrken med krav på fördjupad högskolekompetens inom IT",
    ssyk2_code == 26 ~ "Yrken med krav på fördjupad högskolekompetens inom juridik, kultur och socialt arbete m.m.",
    ssyk2_code == 31 ~ "Yrken med krav på högskolekompetens eller motsvarande inom teknik",
    ssyk2_code == 32 ~ "Yrken med krav på högskolekompetens eller motsvarande inom hälso- och sjukvård samt laboratorium",
    ssyk2_code == 33 ~ "Yrken med krav på högskolekompetens eller motsvarande inom ekonomi och förvaltning",
    ssyk2_code == 34 ~ "Yrken med krav på högskolekompetens eller motsvarande inom kultur, friskvård och socialt arbete",
    ssyk2_code == 35 ~ "Yrken med krav på högskolekompetens eller motsvarande inom IT, ljud- och ljusteknik m.m.",
    ssyk2_code == 41 ~ "Kontorsassistenter och sekreterare",
    ssyk2_code == 42 ~ "Kundserviceyrken",
    ssyk2_code == 43 ~ "Yrken inom materialförvaltning m.m.",
    ssyk2_code == 44 ~ "Andra kontors- och kundserviceyrken",
    ssyk2_code == 51 ~ "Serviceyrken",
    ssyk2_code == 52 ~ "Försäljningsyrken inom detaljhandeln m.m.",
    ssyk2_code == 53 ~ "Omsorgsyrken",
    ssyk2_code == 54 ~ "Andra bevaknings- och säkerhetsyrken",
    ssyk2_code == 61 ~ "Lantbruks- och trädgårdsyrken",
    ssyk2_code == 62 ~ "Skogsarbetare, fiskodlare och fiskare",
    ssyk2_code == 71 ~ "Byggnads- och anläggningsyrken",
    ssyk2_code == 72 ~ "Metallhantverks- och reparatörsyrken",
    ssyk2_code == 73 ~ "Finmekaniska, grafiska och konsthantverksyrken",
    ssyk2_code == 74 ~ "Installations- och serviceyrken inom el och elektronik",
    ssyk2_code == 75 ~ "Andra hantverksyrken inom trä och textil m.m.",
    ssyk2_code == 76 ~ "Hantverksyrken inom livsmedel",
    ssyk2_code == 81 ~ "Process- och maskinoperatörer",
    ssyk2_code == 82 ~ "Montörer",
    ssyk2_code == 83 ~ "Transport- och maskinföraryrken",
    ssyk2_code == 91 ~ "Städyrken",
    ssyk2_code == 92 ~ "Bärplockare och plantörer m.fl.",
    ssyk2_code == 93 ~ "Andra yrken inom bygg, tillverkning och godshantering",
    ssyk2_code == 94 ~ "Snabbmatspersonal, köks- och restaurangbiträden m.fl.",
    ssyk2_code == 95 ~ "Torg- och marknadsförsäljare",
    ssyk2_code == 96 ~ "Återvinningsarbetare, tidningsdistributörer och övriga servicearbetare",
    ssyk2_code == 01 ~ "Officerare",
    ssyk2_code == 02 ~ "Specialistofficerare",
    ssyk2_code == 03 ~ "Soldater m.fl.",
    is.na(ssyk2_code) ~"Uppgift saknas"
    
  ))


## Create ssyk1_code from ssyk4
df <- mutate(df, ssyk1_code = substr(ssyk4, 1, 1))

## Mutate with ssyk1 text
df <- df %>%
  mutate(ssyk1_text = case_when(
    ssyk1_code == 1 ~ "1 - Chefsyrken",
    ssyk1_code == 2 ~ "2 - Fördjupad högskolekompetens",
    ssyk1_code == 3 ~ "3 - Högskolekompetens eller mots.",
    ssyk1_code == 4 ~ "4 - Administration och kundtjänst",
    ssyk1_code == 5 ~ "5 - Service-, omsorg- och frsj.",
    ssyk1_code == 6 ~ "6 - Lantbruk, trädgård, skog och fiske",
    ssyk1_code == 7 ~ "7 - Bygg och tillv.",
    ssyk1_code == 8 ~ "8 - Maskinell tillv. och transport m.m.",
    ssyk1_code == 9 ~ "9 - Kortare utb. eller introduktion",
    ssyk1_code == 0 ~ "0 - Militära yrken"))


## Create ssyk3_code from ssyk4
df <- mutate(df, ssyk3_code = substr(ssyk4, 1, 3))


df_ssyk3 <- import("Platsannonser/ssyk3.xlsx")
df_ssyk3$ssyk3_code <- as.character((df_ssyk3$ssyk3_code))

df <- left_join(df,df_ssyk3)
df$ssyk3_text <- trimws(df$ssyk3_text)


## Add plaintext for municipality and county
### Source (read) function
source("functions/f_add_plaintext_from_municipalitycode.R")

### Use function on df
df <- f_add_plaintext_from_municipalitycode(df, kodkolumn = "workplace_address.municipality_code")


## Re-arrange order in df
df <- select(df, 
             ### META
             id, number_of_vacancies,
             employment_type.label,working_hours_type.label, duration.label,
             
             ### Arbetsgivare och kommun
             employer.workplace, workplace_address.municipality_code,
             municipality_plaintext, county_plaintext,
             
             ### Område, nivå
             occupation_field.label, ssyk1_code, ssyk1_text,
             
             ### SSYK-detalj
             ssyk4, occupation_group.label,
             ssyk3_code, ssyk3_text,
             ssyk2_code, ssyk2_text,
             
             ### Krav
             driving_license_required, driving_license_value, access_to_own_car,
             must_have.languages,
             
             ### Färdighetskrav
             kvalifikations_niva, kval_label,
             
             ### Annonstext
             description.text
)



# OUTPUT
## Prepare filename
datum <- format(Sys.Date(), "%y%m%d")  # Ger t.ex. "250623"

### 2 diffrent file-names for output file
#filnamn <- paste0("Platsannonser/platsannons_wtext_", datum, ".xlsx")
filnamn <- "platsannons_wtext.xlsx" # used in .rmd-files

write_xlsx(df, filnamn)

