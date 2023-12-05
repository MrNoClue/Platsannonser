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
annonsyrkesomr_url <- paste0("https://jobstream.api.jobtechdev.se/stream?date=2023-08-31T00:00:00&updated-before-date=2023-12-03T23:59:59")

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

annonsyrkesomr_url_tidiga <- paste0("https://jobstream.api.jobtechdev.se/stream?date=2023-06-01T00:00:00&updated-before-date=2023-08-30T23:59:59")

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
  select(id, number_of_vacancies, access_to_own_car, driving_license_required, employment_type.label,
         employer.workplace, ssyk4 = occupation_group.legacy_ams_taxonomy_id, 
         occupation_group.label, working_hours_type.label, duration.label, workplace_address.municipality_code,
         occupation_field.label, must_have.languages)

# Un-list col.
df$must_have.languages <- sapply(df$must_have.languages, function(lst) lst$label)

# Try to make cell-values with string="c()" to only view chr 
df$must_have.languages <- gsub('[c\\(\\)]', '', df$must_have.languages)

## Mutate driving: T/F to 1/0 and kval-niva
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

df_ssyk3 <- import("Ad-hoc/Af-platser/ssyk3.xlsx")
df_ssyk3$ssyk3_code <- as.character((df_ssyk3$ssyk3_code))

df <- left_join(df,df_ssyk3)
df$ssyk3_text <- trimws(df$ssyk3_text)

write_xlsx(df,"Ad-hoc/Af-platser/platsannonser.xlsx")




#==== IMPORT DF ====
library(tidyverse)
library(ggthemes)
library(rio)


# Get the current working directory (R root directory)
root_dir <- getwd()

# Specify the relative file path from the R root directory
relative_path <- "Ad-hoc/Af-platser/platsannonser.xlsx"

# Construct the full file path
file_path <- file.path(root_dir, relative_path)

# Import the file using the full file path
df <- import(file_path)



#==== GGPLOT: 22 - ssyk1 och kommun ====
library(viridis)

# Change mun.code to name
geografi <- "Västernorrland"

## Filter on 22
df_22_ssyk1 <- filter(df, substr(workplace_address.municipality_code, 1, 2) == 22)

df_22_ssyk1 <- df_22_ssyk1 %>%
  group_by(ssyk1_text, workplace_address.municipality_code) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE)) %>%
  ungroup()

max_value <- df_22_ssyk1 %>%
  group_by(ssyk1_text) %>%
  summarize(total_vac_sum = sum(total_vac)) %>%
  arrange(desc(total_vac_sum)) %>%
  slice(1) %>%
  pull(total_vac_sum)


df_22_ssyk1$total_vac <- as.numeric(df_22_ssyk1$total_vac)

df_22_ssyk1 <- df_22_ssyk1 %>%
  mutate(ssyk1_text = forcats::fct_reorder(ssyk1_text, total_vac, .fun = sum),
         workplace_address.municipality_code = factor(workplace_address.municipality_code, 
                                                      levels = c("2260", "2281", "2262", "2280", "2282","2283","2284")))

#df_22_kval$kvalifikations_niva <- as.character(df_22_kval$kvalifikations_niva)
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "4"] <- "Eftergymn. >3år"
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "3"] <- "Praktisk elr yrkesspec. eftergymn."
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "2"] <- "Gymn. elr eftergymn (2år)"
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "1"] <- "Låga"
#df_22_kval$kvalifikations_niva <- factor(df_22_kval$kvalifikations_niva)


ggplot(df_22_ssyk1, aes(x = ssyk1_text, y = total_vac, fill = factor(workplace_address.municipality_code, levels = c("2260", "2281", "2262", "2280", "2282","2283","2284")))) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, direction = 1, guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = seq(from = 0, to = max_value, by = 50), color = "white") +
  theme_tufte() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.x = element_blank()) +
  labs(title = "Lediga tjänster",
       subtitle = sprintf("Yrken efter nivå - %s", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland") +
  coord_flip()



#==== GGPLOT: 22 - ssyk1-2 och kommun ====
library(scales)

## Filter on 22
df_22_ssyk1_2_omrade <- filter(df, substr(workplace_address.municipality_code, 1, 2) == 22, ssyk1_code == 2)

df_22_ssyk1_2_omrade <- df_22_ssyk1_2_omrade %>%
  group_by(occupation_field.label, workplace_address.municipality_code) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE)) %>%
  ungroup()

max_value <- df_22_ssyk1_2_omrade %>%
  group_by(occupation_field.label) %>%
  summarize(total_vac_sum = sum(total_vac)) %>%
  arrange(desc(total_vac_sum)) %>%
  slice(1) %>%
  pull(total_vac_sum)


df_22_ssyk1_2_omrade$total_vac <- as.numeric(df_22_ssyk1_2_omrade$total_vac)

df_22_ssyk1_2_omrade <- df_22_ssyk1_2_omrade %>%
  mutate(occupation_field.label = forcats::fct_reorder(occupation_field.label, total_vac, .fun = sum),
         workplace_address.municipality_code = factor(workplace_address.municipality_code, 
                                                      levels = c("2260", "2281", "2262", "2280", "2282","2283","2284")))

#df_22_kval$kvalifikations_niva <- as.character(df_22_kval$kvalifikations_niva)
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "4"] <- "Eftergymn. >3år"
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "3"] <- "Praktisk elr yrkesspec. eftergymn."
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "2"] <- "Gymn. elr eftergymn (2år)"
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "1"] <- "Låga"
#df_22_kval$kvalifikations_niva <- factor(df_22_kval$kvalifikations_niva)


ggplot(df_22_ssyk1_2_omrade, aes(x = occupation_field.label, y = total_vac, fill = factor(workplace_address.municipality_code, levels = c("2260", "2281", "2262", "2280", "2282","2283","2284")))) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, direction = 1, guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = seq(from = 0, to = max_value, by = 50), color = "white") +
  theme_tufte() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.x = element_blank()) +
  labs(title = "Lediga tjänster",
       subtitle = sprintf("SSYK-2 och yrkesområde - %s", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland") +
  scale_y_continuous(labels = number_format(accuracy = 1))+
  coord_flip()



#==== GGPLOT: 22 - område och typ av anställning ====


## Filter on 22
df_22_sommar <- filter(df, substr(workplace_address.municipality_code,1,2) == 22)

## Group and summarize
df_22_sommar <- df_22_sommar %>%
  group_by(occupation_field.label,employment_type.label) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE)) %>%
  ungroup()

df_22_sommar$total_vac <- as.numeric(df_22_sommar$total_vac)

max_value <- df_22_sommar %>%
  group_by(occupation_field.label) %>%
  summarize(total_vac_sum = sum(total_vac)) %>%
  arrange(desc(total_vac_sum)) %>%
  slice(1) %>%
  pull(total_vac_sum)

## Korta ner namn på fill-serie
df_22_sommar$employment_type.label[df_22_sommar$employment_type.label == "Behovsanställning"] <- "Behov"
df_22_sommar$employment_type.label[df_22_sommar$employment_type.label == "Sommarjobb / feriejobb"] <- "Ferie"
df_22_sommar$employment_type.label[df_22_sommar$employment_type.label == "Vanlig anställning"] <- "Vanlig"

df_22_sommar <- df_22_sommar %>%
  mutate(occupation_field.label = forcats::fct_reorder(occupation_field.label, total_vac, .fun = sum))

ggplot(df_22_sommar, aes(x = occupation_field.label, 
                         y = total_vac, fill = employment_type.label)) +
  geom_col()+
  scale_fill_manual(values = c("#005ca9", "#95c11f", "#cbcbcb"))+
  geom_hline(yintercept = seq(from = 0, to = max_value, by = 50), color = "white") +
  theme_tufte()+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_blank(), #droppar legend title
        legend.position = "top",
        axis.ticks.x = element_blank()) +
  labs(title = "Lediga tjänster",
       subtitle=sprintf("Typ av anställning - %s", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland")+
  coord_flip()


#==== GGPLOT: 22 - område och kvalifikationsnivå ====
library(viridis)

## Filter on 22
df_22_kval <- filter(df, substr(workplace_address.municipality_code, 1, 2) == 22)

df_22_kval <- df_22_kval %>%
  group_by(occupation_field.label, kvalifikations_niva) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE)) %>%
  ungroup()

max_value <- df_22_kval %>%
  group_by(occupation_field.label) %>%
  summarize(total_vac_sum = sum(total_vac)) %>%
  arrange(desc(total_vac_sum)) %>%
  slice(1) %>%
  pull(total_vac_sum)


df_22_kval$total_vac <- as.numeric(df_22_kval$total_vac)

df_22_kval <- df_22_kval %>%
  mutate(occupation_field.label = forcats::fct_reorder(occupation_field.label, total_vac, .fun = sum),
         kvalifikations_niva = factor(kvalifikations_niva, levels = c("1", "2", "3", "4")))

#df_22_kval$kvalifikations_niva <- as.character(df_22_kval$kvalifikations_niva)
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "4"] <- "Eftergymn. >3år"
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "3"] <- "Praktisk elr yrkesspec. eftergymn."
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "2"] <- "Gymn. elr eftergymn (2år)"
#df_22_kval$kvalifikations_niva[df_22_kval$kvalifikations_niva == "1"] <- "Låga"
#df_22_kval$kvalifikations_niva <- factor(df_22_kval$kvalifikations_niva)


ggplot(df_22_kval, aes(x = occupation_field.label, y = total_vac, fill = factor(kvalifikations_niva, levels = c("4", "3", "2", "1")))) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, direction = 1, guide = guide_legend(reverse = TRUE)) +
  geom_hline(yintercept = seq(from = 0, to = max_value, by = 50), color = "white") +
  theme_tufte() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.ticks.x = element_blank()) +
  labs(title = "Lediga tjänster",
       subtitle = sprintf("Kvalifikationsnivå - %s", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland") +
  coord_flip()





#==== GGPLOT: 2284 - område och körkort ====

## Filter on 2284
df_2284_drive <- filter(df, workplace_address.municipality_code == 2284)

# Change mun.code to name
geografi <- "Örnsköldsvik"

## Mutate sum for 2 diff col.
df_2284_drive <- df_2284_drive %>%
  group_by(occupation_field.label) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE), total_driving = sum(driving_license_value)) %>%
  ungroup()

df_2284_drive$total_vac <- as.numeric(df_2284_drive$total_vac)


ggplot(df_2284_drive) +
  geom_bar(aes(x = reorder(occupation_field.label, total_vac), 
               y = total_vac),
           stat = "identity", fill = "#cbcbcb") +
  geom_col(aes(reorder(x = occupation_field.label, total_vac), 
               y = total_driving),
           fill = "#eb6209") +
  geom_text(aes(x = reorder(occupation_field.label, total_vac), y = total_driving, label = total_driving),
            vjust = 0.3, hjust = -0.3, angle = 0, size = 3.5) +
  theme_tufte()+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        axis.ticks.x = element_blank()) +
  labs(title = "Lediga tjänster",
       subtitle=sprintf("%s", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland")+
  coord_flip()



sum(is.na(df_2284$total_vac))
sum(is.na(df_2284$total_driving))

class(df_2284$total_vac)
class(df_2284$total_driving)



#==== GGPLOT: 2284 - låg nivå ====

## Filter on 2284
df_2284_lowlvl <- filter(df, workplace_address.municipality_code == 2284)

## Filter on low level
df_2284_lowlvl <- filter(df_2284_lowlvl, kvalifikations_niva == 1)

## Mutate sum for 2 diff col.
df_2284_lowlvl <- df_2284_lowlvl %>%
  group_by(occupation_field.label) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE), total_driving = sum(driving_license_value)) %>%
  ungroup()

df_2284_lowlvl$total_vac <- as.numeric(df_2284_lowlvl$total_vac)


ggplot(df_2284_lowlvl) +
  geom_bar(aes(x = reorder(occupation_field.label, total_vac), 
               y = total_vac),
           stat = "identity", fill = "#cbcbcb") +
  geom_col(aes(reorder(x = occupation_field.label, total_vac), 
               y = total_driving),
           fill = "#eb6209") +
  geom_text(aes(x = reorder(occupation_field.label, total_vac), y = total_driving, label = total_driving),
            vjust = 0.3, hjust = -0.3, angle = 0, size = 3.5) +
  theme_tufte()+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        axis.ticks.x = element_blank()) +
  labs(title = "Inga eller låga krav",
       subtitle=sprintf("%s - SSYK 9...", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland")+
  coord_flip()



#==== GGPLOT: 2284 - gymn. ====

## Filter on 2284
df_2284_2ndlowlvl <- filter(df, workplace_address.municipality_code == 2284)

## Filter on low level
df_2284_2ndlowlvl <- filter(df_2284_2ndlowlvl, kvalifikations_niva == 2)

## Mutate sum for 2 diff col.
df_2284_2ndlowlvl <- df_2284_2ndlowlvl %>%
  group_by(occupation_field.label) %>%
  summarize(total_vac = sum(number_of_vacancies, na.rm = TRUE), total_driving = sum(driving_license_value)) %>%
  ungroup()

df_2284_2ndlowlvl$total_vac <- as.numeric(df_2284_2ndlowlvl$total_vac)


ggplot(df_2284_2ndlowlvl) +
  geom_bar(aes(x = reorder(occupation_field.label, total_vac), 
               y = total_vac),
           stat = "identity", fill = "#cbcbcb") +
  geom_col(aes(reorder(x = occupation_field.label, total_vac), 
               y = total_driving),
           fill = "#eb6209") +
  geom_text(aes(x = reorder(occupation_field.label, total_vac), y = total_driving, label = total_driving),
            vjust = 0.3, hjust = -0.3, angle = 0, size = 3.5) +
  theme_tufte()+
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.position = "none",
        axis.ticks.x = element_blank()) +
  labs(title = "Gymn. eller kortare eftergymn.",
       subtitle=sprintf("%s - SSYK 4/5/6/7/8", geografi),
       y = "Tjänster",
       x = "",
       caption = "Källa: Af och bearbetningar av Region Västernorrland")+
  coord_flip()



#==== OUTPUT:xlsx ====

df_filter <- df %>% filter(workplace_address.municipality_code == 2284 | workplace_address.municipality_code == 2283 | workplace_address.municipality_code == 2282 | workplace_address.municipality_code == 2280)

# Change mun.code to name
df_filter$workplace_address.municipality_code <- ifelse(df_filter$workplace_address.municipality_code == 2284, "Örnsköldsvik",
                                                 ifelse(df_filter$workplace_address.municipality_code == 2283, "Sollefteå",
                                                 ifelse(df_filter$workplace_address.municipality_code == 2280, "Härnösand",
                                                 ifelse(df_filter$workplace_address.municipality_code == 2282, "Kramfors",
                                                               df_filter$workplace_address.municipality_code))))


write_xlsx(df_filter,"Ad-hoc/Af-platser/platsannonser_filter.xlsx")

#==== OTHER ====

# Kolla hur många annonser som saknar uppgiven adress
sum(is.na(df$workplace_address.region))



