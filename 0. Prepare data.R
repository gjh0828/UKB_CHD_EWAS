library(tidyverse);library(mgcv);library(readxl);library(mice);library(igraph)
library(data.table);library(purrr);library(missForest);library(doParallel)
variable.names <- readRDS("variable.names.rds")
Excluded <- read_excel("Variable.xlsx")
data <- list.files(path = "E:/UKB/data",pattern = "*.csv", full.names = TRUE) %>%
  map(fread) %>% reduce(full_join, by = "eid") %>%
  left_join(fread("E:/UKB/15_Recruitment.csv")[,c(1,2)],by="eid") %>%
  mutate(across(where(is.character), ~na_if(., "Do not know"))) %>%
  mutate(across(where(is.character), ~na_if(., "Prefer not to answer"))) %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(p21022 = as.numeric(p21022)) %>%
  mutate(p31 = case_when(p31 == "Male"~"0", 
                         p31 == "Female"~"1",
                         TRUE ~ as.character(p31))) %>%
  mutate(p22189 = as.numeric(p22189)) %>%
  mutate(p670_i0 = case_when(p670_i0=="A house or bungalow"~"0",
                             p670_i0=="A flat, maisonette or apartment"~"0",
                             p670_i0=="None of the above"~"1",#other
                             p670_i0=="Mobile or temporary structure (i.e. caravan)"~"1",#other
                             p670_i0=="Care home"~"1",
                             p670_i0=="Sheltered accommodation"~"1",
                             is.na(p670_i0) ~ NA_character_,
                             TRUE ~ as.character(p670_i0))) %>%
  mutate(p6139_i0 = case_when(grepl("An open solid fuel", p6139_i0, ignore.case = TRUE) ~ "2",#Open solid fuel
                              grepl("A gas hob or gas cooker", p6139_i0, ignore.case = TRUE) ~ "1",#Gas fuel
                              grepl("A gas fire that you use regularly in winter time", p6139_i0, ignore.case = TRUE) ~ "1",#Gas fuel
                              p6139_i0=="None of the above"~"0", #Other
                              is.na(p6139_i0) ~ NA_character_,
                              TRUE ~ as.character(p6139_i0))) %>%
  mutate(p738_i0 = case_when(p738_i0 == "Less than 18,000" ~ "0",
                             p738_i0 == "18,000 to 30,999" ~ "1",
                             p738_i0 == "31,000 to 51,999" ~ "2",
                             p738_i0 == "52,000 to 100,000" ~ "3",
                             p738_i0 == "Greater than 100,000" ~ "4",
                             is.na(p738_i0) ~ NA_character_,
                             TRUE ~ as.character(p738_i0))) %>%
  mutate(p6142_i0 = case_when(grepl("Unable to work because of sickness or disability", 
                                    p6142_i0, ignore.case = TRUE) ~ "1",
                              grepl("Unemployed", p6142_i0, ignore.case = TRUE) ~ "1",
                              grepl("Looking after home and/or family", p6142_i0, ignore.case = TRUE) ~ "0",#Unemployed
                              grepl("In paid employment or self-employed", p6142_i0, ignore.case = TRUE) ~ "0",#Employed
                              grepl("Retired", p6142_i0, ignore.case = TRUE) ~ "0",
                              grepl("Doing unpaid or voluntary work", p6142_i0, ignore.case = TRUE) ~ "0",
                              grepl("Full or part-time student", p6142_i0, ignore.case = TRUE) ~ "0",
                              grepl("None of the above", p6142_i0, ignore.case = TRUE) ~ "0",
                              is.na(p6142_i0) ~ NA_character_,
                              TRUE ~ as.character(p6142_i0))) %>%
  mutate(p6138_i0 = case_when(grepl("College or University degree", p6138_i0, ignore.case = TRUE) ~ "0",#Higher education
                              grepl("A levels/AS levels or equivalent", p6138_i0, ignore.case = TRUE) ~ "1",#Lower education
                              grepl("O levels/GCSEs or equivalent", p6138_i0, ignore.case = TRUE) ~ "1",
                              grepl("CSEs or equivalent", p6138_i0, ignore.case = TRUE) ~ "1",
                              grepl("NVQ or HND or HNC or equivalent", p6138_i0, ignore.case = TRUE) ~ "1",
                              grepl("Other professional qualifications eg: nursing, teaching", 
                                    p6138_i0, ignore.case = TRUE) ~ "0",
                              p6138_i0=="None of the above"~"1",
                              is.na(p6138_i0) ~ NA_character_,
                              TRUE ~ as.character(p6138_i0))) %>%
  mutate(p21000_i0 = case_when(p21000_i0 == "White"~ "1",
                               p21000_i0 == "British"~ "1",
                               p21000_i0 == "Irish"~ "1",
                               p21000_i0 == "Any other white background"~ "1",
                               p21000_i0 == "Asian or Asian British"~ "2",
                               p21000_i0 == "Indian"~ "2",
                               p21000_i0 == "Chinese"~ "2",
                               p21000_i0 == "Pakistani"~ "2",
                               p21000_i0 == "Bangladeshi"~ "2",
                               p21000_i0 == "Any other Asian background"~ "2",
                               p21000_i0 == "Black or Black British"~ "3",
                               p21000_i0 == "Caribbean"~ "3",
                               p21000_i0 == "African"~ "3",
                               p21000_i0 == "Any other Black background"~ "3",
                               TRUE ~ "4")) %>%
  mutate(p6146_i0 = case_when(p6146_i0=="None of the above"~"0",#No disability
                               grepl("Attendance", p6146_i0, ignore.case = TRUE) ~ "1",
                               grepl("Disability", p6146_i0, ignore.case = TRUE) ~ "1",
                               grepl("Blue badge", p6146_i0, ignore.case = TRUE) ~ "1",
                               p6146_i0==""~NA_character_,
                               is.na(p6146_i0) ~ NA_character_,
                               TRUE ~ as.character(p6146_i0))) %>%#Disability
  mutate(across(where(is.character), ~na_if(., "Do not know"))) %>%
  mutate(across(where(is.character), ~na_if(., "Prefer not to answer"))) %>%
  mutate(p22037_i0 = as.numeric(p22037_i0), p22038_i0 = as.numeric(p22038_i0),
         p22039_i0 = as.numeric(p22039_i0), p22040_i0 = as.numeric(p22040_i0)) %>%
  mutate(p1021_i0 = case_when(p1021_i0=="Less than 15 minutes"~"0",
                              p1021_i0=="Between 15 and 30 minutes"~"0",
                              p1021_i0=="Between 30 minutes and 1 hour"~"0",
                              is.na(p1021_i0) ~ NA_character_,
                              TRUE ~ "1")) %>%
  mutate(p1070_i0 = case_when(p1070_i0=="Less than an hour a day"~"0.5",
                              TRUE ~ as.character(p1070_i0)),
         p1070_i0 = as.numeric(p1070_i0)) %>%
  mutate(p1080_i0 = case_when(p1080_i0=="Less than an hour a day"~"0.5",
                              TRUE ~ as.character(p1080_i0)),
         p1080_i0 = as.numeric(p1080_i0)) %>%
  mutate(p1120_i0 = case_when(p1120_i0=="Less than 5mins"~"0",#Less than 1 hours
                              p1120_i0=="5-29 mins"~"0",
                              p1120_i0=="30-59 mins"~"0",
                              p1120_i0=="1-3 hours"~"1",#Over 1 hours
                              p1120_i0=="4-6 hours"~"1",
                              p1120_i0=="More than 6 hours"~"1",
                              is.na(p1120_i0) ~ NA_character_,
                              TRUE ~ as.character(p1120_i0))) %>% 
  mutate(p1130_i0 = case_when(p1130_i0=="Never or almost never"~"0",
                              p1130_i0=="Less than half the time"~"1",
                              p1130_i0=="About half the time"~"2",
                              p1130_i0=="More than half the time"~"3",
                              p1130_i0=="Always or almost always"~"4",
                              is.na(p1120_i0) ~ NA_character_,
                              TRUE ~ as.character(p1130_i0))) %>%
  mutate(p2237_i0 = case_when(p2237_i0=="Never/rarely"~"0",
                              p2237_i0=="Often"~"1",
                              p2237_i0=="Sometimes"~"1",
                              is.na(p2237_i0) ~ NA_character_,
                              TRUE ~ as.character(p2237_i0))) %>%
  mutate(p1239_i0 = case_when(p1239_i0=="No"~"No", is.na(p1239_i0) ~ NA_character_,
                              TRUE ~ "Yes")) %>%
  mutate(p1249_i0 = case_when(p1249_i0=="I have never smoked"~"No", 
                              p1249_i0=="Just tried once or twice"~"No", 
                              is.na(p1249_i0) ~ NA_character_,
                              TRUE ~ "Yes")) %>%
  mutate(p20160_i0 = case_when(p20160_i0 == "Yes" | p2644_i0 == "Yes" | 
                                 p1239_i0 == "Yes" | p1249_i0 == "Yes" ~ "1",
                               is.na(p20160_i0) & is.na(p2644_i0) & is.na(p1239_i0) & is.na(p1249_i0) ~ NA, TRUE ~ "0")) %>%
  mutate(p1269_i0 = case_when(p1269_i0==0~"No",TRUE ~ "Yes")) %>%
  mutate(p1279_i0 = case_when(p1279_i0==0~"No",TRUE ~ "Yes")) %>%
  mutate(p1269_i0 = case_when(p1269_i0 == "Yes" | p1279_i0 == "Yes" ~ "1",
                              is.na(p1269_i0) & is.na(p1279_i0) ~ NA, TRUE ~ "0")) %>%
  mutate(p1050_i0 = case_when(p1050_i0=="Less than an hour a day"~"0.5",
                              TRUE ~ as.character(p1050_i0)),
         p1050_i0 = as.numeric(p1050_i0)) %>%
  mutate(p1060_i0 = case_when(p1060_i0=="Less than an hour a day"~"0.5",
                              TRUE ~ as.character(p1060_i0)),
         p1060_i0 = as.numeric(p1060_i0)) %>%
  mutate(p2139_i0 = case_when(p2139_i0=="Never had sex"~NA,
                              TRUE ~ as.character(p2139_i0)),
         p2139_i0 = as.numeric(p2139_i0)) %>%
  mutate(p2149_i0 = as.numeric(p2149_i0)) %>%
  mutate(p2159_i0 = case_when(p2159_i0=="No"~"0", 
                              p2159_i0=="Yes"~"1",
                              is.na(p2159_i0) ~ NA_character_,
                              TRUE ~ as.character(p2159_i0))) %>%
  mutate(p1160_i0 = case_when(p1160_i0 <7 ~ "1", #Sleep duration <7 or >9
                              p1160_i0 >9 ~ "1", 
                              is.na(p1160_i0) ~ NA_character_,
                              TRUE ~ "0")) %>% #Moderate sleep duration
  mutate(p1170_i0 = case_when(p1170_i0 == "Very easy" ~ "0",
                              p1170_i0 == "Fairly easy" ~ "1",
                              p1170_i0 == "Not very easy" ~ "2",
                              p1170_i0 == "Not at all easy" ~ "3",
                              TRUE ~ as.character(p1170_i0))) %>%
  mutate(p1180_i0 = case_when(p1180_i0 == "Definitely a 'morning' person" ~ "0",
                              p1180_i0 == "Definitely an 'evening' person" ~ "1",
                              p1180_i0 == "More a 'morning' than 'evening' person" ~ "2",
                              p1180_i0 == "More an 'evening' than a 'morning' person" ~ "3",
                              TRUE ~ as.character(p1180_i0))) %>%
  mutate(p1190_i0 = case_when(p1190_i0 == "Never/rarely" ~ "0",
                              p1190_i0 == "Sometimes" ~ "1",
                              p1190_i0 == "Usually" ~ "2",
                              TRUE ~ as.character(p1190_i0))) %>%
  mutate(p1200_i0 = case_when(p1200_i0 == "Never/rarely" ~ "0",
                              p1200_i0 == "Sometimes" ~ "1",
                              p1200_i0 == "Usually" ~ "2",
                              TRUE ~ as.character(p1200_i0))) %>%
  mutate(p1210_i0 = case_when(p1210_i0 == "No" ~ "0",
                              p1210_i0 == "Yes" ~ "1",
                              TRUE ~ as.character(p1210_i0))) %>%
  mutate(p1220_i0 = case_when(p1220_i0 == "Never/rarely" ~ "0",
                              p1220_i0 == "Sometimes" ~ "1",
                              p1220_i0 == "Often" ~ "2",
                              p1220_i0 == "All of the time" ~ "2",
                              TRUE ~ as.character(p1220_i0))) %>%
  mutate(p1289_i0 = ifelse(p1289_i0== "Less than one", 0.5, p1289_i0),
         p1289_i0 = as.numeric(p1289_i0)) %>%
  mutate(p1299_i0 = ifelse(p1299_i0== "Less than one", 0.5, p1299_i0),
         p1299_i0 = as.numeric(p1299_i0)) %>%
  mutate(p1309_i0 = ifelse(p1309_i0== "Less than one", 0.5, p1309_i0),
         p1309_i0 = as.numeric(p1309_i0)) %>%
  mutate(p1319_i0 = ifelse(p1319_i0== "Less than one", 0.5, p1319_i0),
         p1319_i0 = as.numeric(p1319_i0)) %>%
  mutate(p1329_i0 = case_when(p1329_i0 == "Never"~"0",
                              p1329_i0 == "Less than once a week" ~ "0", 
                              p1329_i0 == "Once a week" ~ "1",
                              p1329_i0 == "2-4 times a week" ~ "1",
                              p1329_i0 == "5-6 times a week" ~ "1",
                              p1329_i0 == "Once or more daily" ~ "1",
                              TRUE ~ as.character(p1329_i0)),
         p1329_i0 = as.numeric(p1329_i0)) %>%
  mutate(p1339_i0 = case_when(p1339_i0 == "Never"~ "0",
                              p1339_i0 == "Less than once a week"~ "0", 
                              p1339_i0 == "Once a week" ~ "1",
                              p1339_i0 == "2-4 times a week" ~ "1",
                              p1339_i0 == "5-6 times a week" ~ "1",
                              p1339_i0 == "Once or more daily" ~ "1",
                              TRUE ~ as.character(p1339_i0)),
         p1339_i0 = as.numeric(p1339_i0)) %>%
  mutate(p1438_i0 = ifelse(p1438_i0== "Less than one", 0.5, p1438_i0),
         p1438_i0 = as.numeric(p1438_i0)) %>%
  mutate(p1458_i0 = ifelse(p1458_i0== "Less than one", 0.5, p1458_i0),
         p1458_i0 = as.numeric(p1458_i0)) %>%
  mutate(p1468_i0 = case_when(p1468_i0 == "Bran cereal (e.g. All Bran, Branflakes)"~"1",
                              p1468_i0 == "Oat cereal (e.g. Ready Brek, porridge)"~"1", 
                              p1468_i0 == "Biscuit cereal (e.g. Weetabix)"~"0",
                              p1468_i0 == "Muesli"~"0",
                              p1468_i0 == "Other (e.g. Cornflakes, Frosties)"~"0",
                              TRUE ~ as.character(p1468_i0)),
         p1468_i0 = as.numeric(p1468_i0)) %>%
  mutate(p1369_i0 = case_when(p1369_i0 == "Never"~ "1",
                              p1369_i0 == "Less than once a week"~"1", 
                              p1369_i0 == "Once a week" ~ "0",
                              p1369_i0 == "2-4 times a week" ~ "0",
                              p1369_i0 == "5-6 times a week" ~ "0",
                              p1369_i0 == "Once or more daily" ~ "0",
                              TRUE ~ as.character(p1369_i0)),
         p1369_i0 = as.numeric(p1369_i0)) %>%
  mutate(p1379_i0 = case_when(p1379_i0 == "Never"~"1",
                              p1379_i0 == "Less than once a week"~"1", 
                              p1379_i0 == "Once a week" ~ "0",
                              p1379_i0 == "2-4 times a week" ~ "0",
                              p1379_i0 == "5-6 times a week" ~ "0",
                              p1379_i0 == "Once or more daily" ~ "0",
                              TRUE ~ as.character(p1379_i0)),
         p1379_i0 = as.numeric(p1379_i0)) %>%
  mutate(p1389_i0 = case_when(p1389_i0 == "Never"~"1",
                              p1389_i0 == "Less than once a week"~"1", 
                              p1389_i0 == "Once a week" ~ "0",
                              p1389_i0 == "2-4 times a week" ~ "0",
                              p1389_i0 == "5-6 times a week" ~ "0",
                              p1389_i0 == "Once or more daily" ~ "0",
                              TRUE ~ as.character(p1389_i0)),
         p1389_i0 = as.numeric(p1389_i0)) %>%
  mutate(p1349_i0 = case_when(p1349_i0 == "Never"~"1",
                              p1349_i0 == "Less than once a week"~"1", 
                              p1349_i0 == "Once a week" ~ "0",
                              p1349_i0 == "2-4 times a week" ~ "0",
                              p1349_i0 == "5-6 times a week" ~ "0",
                              p1349_i0 == "Once or more daily" ~ "0",
                              TRUE ~ as.character(p1349_i0)),
         p1349_i0 = as.numeric(p1349_i0)) %>%
  mutate(HD1 = ifelse(p1289_i0 >=12|p1299_i0>=12,1,0)) %>%
  mutate(HD2 = ifelse(p1309_i0 >=3|p1319_i0>=3,1,0)) %>%
  mutate(HD3 = ifelse(p1329_i0 ==1|p1339_i0==1,1,0)) %>%
  mutate(HD4_1 = ifelse(p1438_i0/2 + p1458_i0 >= 3, 2, 0)) %>%
  mutate(HD4_2 = ifelse((p1448_i0=="White"|p1468_i0==1)&(p1438_i0/2 + p1458_i0 <= 1.5), 1, 0)) %>%
  mutate(HD5 = ifelse(p1369_i0 == 1 & p1379_i0 == 1 & p1389_i0 == 1, 1, 0)) %>%
  mutate(HD6 = ifelse(p1349_i0 == 1, 1, 0),
         Healthy_diet_level = HD1 + HD2 + HD3 + HD4_1 + HD4_2 + HD5 + HD6) %>%
  mutate(p1478_i0 = case_when(p1478_i0 == "Never/rarely" ~ "0",
                              p1478_i0 == "Sometimes" ~ "1",
                              p1478_i0 == "Usually" ~ "2",
                              p1478_i0 == "Always" ~ "3",
                              TRUE ~ as.character(p1478_i0))) %>%
  mutate(p1488_i0 = ifelse(p1488_i0== "Less than one", 0.5, p1488_i0),
         p1488_i0 = as.numeric(p1488_i0)) %>%
  mutate(p1498_i0 = ifelse(p1498_i0== "Less than one", 0.5, p1498_i0),
         p1498_i0 = as.numeric(p1498_i0)) %>%
  mutate(p1518_i0 = case_when(p1518_i0 == "Warm" ~ "0",
                              p1518_i0 == "Do not drink hot drinks" ~ "0",
                              p1518_i0 == "Hot" ~ "1",
                              p1518_i0 == "Very hot" ~ "2",
                              TRUE ~ as.character(p1518_i0))) %>%
  mutate(p1528_i0 = ifelse(p1528_i0== "Less than one", 0.5, p1528_i0),
         p1528_i0 = as.numeric(p1528_i0)) %>%
  mutate(p1538_i0 = ifelse(p1538_i0 == "No", "0", "1")) %>%
  mutate(p1548_i0 = case_when(p1548_i0 == "Never/rarely" ~ "0",
                              p1548_i0 == "Sometimes" ~ "1",
                              p1548_i0 == "Often" ~ "2",
                              TRUE ~ as.character(p1548_i0))) %>%
  mutate(across(c(p1568_i0, p4407_i0, p1578_i0, p4418_i0, p1588_i0, p4429_i0,
                  p1598_i0, p4440_i0, p1608_i0, p4451_i0, p5364_i0, p4462_i0), ~as.numeric(.))) %>%
  mutate(Alcohol = ifelse(!is.na(p1558_i0),
                          coalesce(p1568_i0, 0) / 7 + coalesce(p4407_i0, 0) / 30 + 
                            coalesce(p1578_i0, 0) / 7 + coalesce(p4418_i0, 0) / 30 + 
                            coalesce(p1588_i0, 0) / 7 + coalesce(p4429_i0, 0) / 30 + 
                            coalesce(p1598_i0, 0) / 7 + coalesce(p4440_i0, 0) / 30 + 
                            coalesce(p1608_i0, 0) / 7 + coalesce(p4451_i0, 0) / 30 + 
                            coalesce(p5364_i0, 0) / 7 + coalesce(p4462_i0, 0) / 30, NA))  %>%
  mutate(Alcohol1 = ifelse(p1558_i0 == "0", 0,
                          ifelse(!is.na(p1558_i0),
                                 coalesce(p1568_i0, 0) * 175 * 0.12 * 0.8 / 7 +  # ???ƣ?ÿ??175ml??12% ABV
                                   coalesce(p4407_i0, 0) * 175 * 0.12 * 0.8 / 30 + # ???ƣ?ÿ??175ml??12% ABV
                                   coalesce(p1578_i0, 0) * 175 * 0.12 * 0.8 / 7 +  # ?׾ƣ?ÿ??175ml??12% ABV
                                   coalesce(p4418_i0, 0) * 175 * 0.12 * 0.8 / 30 + # ?׾ƣ?ÿ??175ml??12% ABV
                                   coalesce(p1588_i0, 0) * 568 * 0.045 * 0.8 / 7 + # ơ?ƣ?ÿ??568ml??4.5% ABV
                                   coalesce(p4429_i0, 0) * 568 * 0.045 * 0.8 / 30 +# ơ?ƣ?ÿ??568ml??4.5% ABV
                                   coalesce(p1598_i0, 0) * 25 * 0.4 * 0.8 / 7 +    # ?Ҿƣ?ÿ??25ml??40% ABV
                                   coalesce(p4440_i0, 0) * 25 * 0.4 * 0.8 / 30 +   # ?Ҿƣ?ÿ??25ml??40% ABV
                                   coalesce(p1608_i0, 0) * 75 * 0.18 * 0.8 / 7 +   # ??ǿ?ƣ?ÿ??75ml??18% ABV
                                   coalesce(p4451_i0, 0) * 75 * 0.18 * 0.8 / 30 +  # ??ǿ?ƣ?ÿ??75ml??18% ABV
                                   coalesce(p5364_i0, 0) * 275 * 0.045 * 0.8 / 7 + # ???????࣬ÿ??275ml??4.5% ABV
                                   coalesce(p4462_i0, 0) * 275 * 0.045 * 0.8 / 30, # ???????࣬ÿ??275ml??4.5% ABV
                                 NA))) %>%
  mutate(across(where(is.character), ~na_if(., "Do not know (group 1)"))) %>%
  mutate(across(where(is.character), ~na_if(., "Do not know (group 2)"))) %>%
  mutate(across(where(is.character), ~na_if(., "Prefer not to answer (group 1)"))) %>%
  mutate(across(where(is.character), ~na_if(., "Prefer not to answer (group 2)"))) %>%
  mutate(p1647_i0 = ifelse(p1647_i0 == "England", "0", "1")) %>%
  mutate(p1677_i0 = ifelse(p1677_i0 == "Yes", "0", "1")) %>%
  mutate(p1687_i0 = case_when(p1687_i0 == "About average" ~ "0",
                              p1687_i0 == "Plumper" ~ "2",
                              p1687_i0 == "Thinner" ~ "1",
                              TRUE ~ as.character(p1687_i0))) %>%
  mutate(p1697_i0 = case_when(p1697_i0 == "About average" ~ "0",
                              p1697_i0 == "Shorter" ~ "1",
                              p1697_i0 == "Taller" ~ "2",
                              TRUE ~ as.character(p1697_i0))) %>%
  mutate(p1767_i0 = case_when(p1767_i0 == "No" ~ "0", 
                              p1767_i0 == "Yes" ~ "1",
                              is.na(p1767_i0) ~ "2",
                              TRUE ~ as.character(p1767_i0))) %>%
  mutate(p1787_i0 = case_when(p1787_i0 == "No" ~ "0", 
                              p1787_i0 == "Yes" ~ "1",
                              TRUE ~ as.character(p1787_i0))) %>%
  mutate(p1777_i0 = case_when(p1777_i0 == "No" ~ "0", 
                              p1777_i0 == "Yes" ~ "1",
                              TRUE ~ as.character(p1777_i0))) %>%
  mutate(Family_history = case_when(is.na(p20107_i0) & is.na(p20110_i0) & is.na(p20111_i0) ~ NA,
                                    grepl("Severe depression", p20107_i0, ignore.case = TRUE) ~ "1",
                                    grepl("Parkinson's disease", p20107_i0, ignore.case = TRUE) ~ "1",
                                    grepl("Alzheimer's disease/dementia", p20107_i0, ignore.case = TRUE) ~ "1",
                                    TRUE ~ "0")) %>%#1:Yes;0:No
  mutate(p1031_i0 = case_when(p1031_i0 == "Never or almost never"~"1",#Less than once a month
                              p1031_i0 == "No friends/family outside household"~"1",
                              p1031_i0 == "Once every few months"~"1",
                              p1031_i0 == "About once a month"~"0",
                              p1031_i0 == "About once a week"~"0",
                              p1031_i0 == "2-4 times a week"~"0",
                              p1031_i0 == "Almost daily"~"0",
                              TRUE ~ as.character(p1031_i0))) %>%
  mutate(p6160_i00 = case_when(grepl("Sports club or gym", p6160_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6160_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6160_i01 = case_when(grepl("Pub or social club", p6160_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6160_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6160_i02 = case_when(grepl("Religious group", p6160_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6160_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6160_i03 = case_when(grepl("Adult education class", p6160_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6160_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6160_i04 = case_when(grepl("Other group activity", p6160_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6160_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6160_i05 = case_when(grepl("None of the above", p6160_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6160_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p2110_i0 = case_when(p2110_i0 == "Almost daily" ~ "0", #At least once a week
                              p2110_i0 == "2-4 times a week" ~ "0",
                              p2110_i0 == "About once a week" ~ "0",
                              p2110_i0 == "About once a month" ~ "1",#About once a month
                              p2110_i0 == "Once every few months" ~ "2",#Once every few months
                              p2110_i0 == "Never or almost never" ~ "3",#Never or almost never
                              TRUE ~ as.character(p2110_i0))) %>% 
  mutate(across(c(p1920_i0, p1930_i0, p1940_i0, p1950_i0, p1960_i0, p1970_i0,
                  p1980_i0, p1990_i0, p2000_i0, p2010_i0, p2020_i0, p2030_i0), 
                ~ ifelse(. == "Yes", 1, ifelse(. == "No", 0, NA_real_)))) %>%
  mutate(p20127_i0 = rowSums(across(c(p1920_i0, p1930_i0, p1940_i0, p1950_i0, 
                                      p1960_i0, p1970_i0, p1980_i0, p1990_i0, 
                                      p2000_i0, p2010_i0, p2020_i0, p2030_i0)), na.rm = F)) %>%
  mutate(across(c(p2050_i0, p2060_i0, p2070_i0, p2080_i0), 
                ~ ifelse(. == "Not at all", 0, 
                         ifelse(. == "Several days", 1, 
                                ifelse(. == "More than half the days", 2, 
                                       ifelse(. == "Nearly every day", 3, NA_real_)))))) %>%
  mutate(PHQ_4 = rowSums(across(c(p2050_i0, p2060_i0, p2070_i0, p2080_i0)), na.rm = F)) %>%
  mutate(p24003 = rowSums(across(c(p24003, p24016, p24017, p24018)), na.rm = F)) %>%
  mutate(p24019 = rowSums(across(c(p24019, p24005)), na.rm = F)) %>%
  mutate(p2040_i0 = ifelse(p2040_i0 == "Yes", "0", "1")) %>%
  mutate(p2178_i0 = case_when(p2178_i0 == "Excellent" ~ "0",
                              p2178_i0 == "Good" ~ "1",
                              p2178_i0 == "Fair" ~ "2",
                              p2178_i0 == "Poor" ~ "3",
                              is.na(p2178_i0) ~ NA_character_, 
                              TRUE ~ as.character(p2178_i0))) %>%
  mutate(p2188_i0 = case_when(p2188_i0 == "No" ~ "0",
                              p2188_i0 == "Yes" ~ "1",
                              is.na(p2188_i0) ~ NA_character_, 
                              TRUE ~ as.character(p2188_i0))) %>%
  mutate(p2296_i0 = case_when(p2296_i0 == "No falls" ~ "0", 
                              p2296_i0 == "Only one fall" ~ "1",
                              p2296_i0 == "More than one fall" ~ "1",
                              is.na(p2296_i0) ~ NA_character_,
                              TRUE ~ as.character(p2296_i0))) %>%#1:Yes;0:No
  mutate(p2306_i0 = case_when(p2306_i0 == "No - weigh about the same" ~ "0", 
                              p2306_i0 == "Yes - gained weight" ~ "1", 
                              p2306_i0 == "Yes - lost weight" ~ "1",
                              is.na(p2306_i0) ~ NA_character_,
                              TRUE ~ as.character(p2306_i0))) %>%#1:Yes;0:No
  mutate(p2375_i0 = case_when(p2375_i0 == "About average age" ~ "0", 
                              p2375_i0 == "Older than average" ~ "1", 
                              p2375_i0 == "Younger than average" ~ "1",
                              is.na(p2375_i0) ~ NA_character_,
                              TRUE ~ as.character(p2375_i0))) %>%
  mutate(p2385_i0 = case_when(p2385_i0 == "About average age" ~ "0", 
                              p2385_i0 == "Older than average" ~ "1", 
                              p2385_i0 == "Younger than average" ~ "1",
                              is.na(p2385_i0) ~ NA_character_,
                              TRUE ~ as.character(p2385_i0))) %>%
  mutate(p2714_i0 = as.numeric(p2714_i0)) %>%
  mutate(p2724_i0 = case_when(p2724_i0 == "Not sure - had a hysterectomy" ~ "0",#Not sure
                              p2724_i0 == "Not sure - other reason" ~ "0", 
                              p2724_i0 == "Yes" ~ "1",
                              is.na(p2724_i0) ~ NA_character_,
                              TRUE ~ as.character(p2724_i0))) %>%
  mutate(p2814_i0 = case_when(p2814_i0 == "No" ~ "0",
                              p2814_i0 == "Yes" ~ "1",
                              is.na(p2814_i0) ~ NA_character_, 
                              TRUE ~ as.character(p2814_i0))) %>%
  mutate(p6155_i00 = case_when(grepl("None of the above", p6155_i0, ignore.case = TRUE) ~ "0",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "1")) %>%
  mutate(p6155_i01 = case_when(grepl("Vitamin A", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6155_i02 = case_when(grepl("Vitamin B", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6155_i03 = case_when(grepl("Vitamin C", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6155_i04 = case_when(grepl("Vitamin D", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6155_i05 = case_when(grepl("Vitamin E", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6155_i06 = case_when(grepl("Folic acid or", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6155_i07 = case_when(grepl("Multivitamins", p6155_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6155_i0) ~ NA_character_,TRUE ~ "0")) %>%                         
  mutate(p6179_i00 = case_when(grepl("None of the above", p6179_i0, ignore.case = TRUE) ~ "0",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "1")) %>%
  mutate(p6179_i01 = case_when(grepl("Fish oil", p6179_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6179_i02 = case_when(grepl("Glucosamine", p6179_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6179_i03 = case_when(grepl("Calcium", p6179_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6179_i04 = case_when(grepl("Zinc", p6179_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6179_i05 = case_when(grepl("Iron", p6179_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p6179_i06 = case_when(grepl("Selenium", p6179_i0, ignore.case = TRUE) ~ "1",
                              is.na(p6179_i0) ~ NA_character_,TRUE ~ "0")) %>%
  mutate(p24016 = rowMeans(across(c(p24016, p24017, p24018, p24003)), na.rm = F)) %>%
  mutate(p24019 = rowMeans(across(c(p24019, p24005)), na.rm = F)) %>%
  mutate(p4079_i0_a1 = rowMeans(across(c(p4079_i0_a1, p94_i0_a1)), na.rm = T)) %>%
  mutate(p95_i0_a1 = rowMeans(across(c(p95_i0_a1, p102_i0_a1)), na.rm = T)) %>%
  mutate(p4080_i0_a1 = rowMeans(across(c(p4080_i0_a1, p93_i0_a1)), na.rm = T)) %>%
  mutate(p4080_i0_a1 = rowMeans(across(c(p4080_i0_a1, p93_i0_a1)), na.rm = T)) %>%
  mutate(p4080_i0_a1 = rowMeans(across(c(p4080_i0_a1, p93_i0_a1)), na.rm = T)) %>%
  mutate(p46_i0 = rowMeans(across(c(p46_i0, p47_i0)), na.rm = T)) %>%
  mutate(p23123_i0 = rowMeans(across(c(p23123_i0, p23119_i0)), na.rm = T)) %>%
  mutate(p3062_i0_a0 = rowMeans(across(c(p3062_i0_a1, p3062_i0_a2, p3062_i0_a0)), na.rm = T)) %>%
  mutate(p3063_i0_a0 = rowMeans(across(c(p3063_i0_a1, p3063_i0_a2, p3063_i0_a0)), na.rm = T)) %>%
  mutate(p3064_i0_a0 = rowMeans(across(c(p3064_i0_a1, p3064_i0_a2, p3064_i0_a0)), na.rm = T)) %>%
  as.data.frame() %>% mutate(across(where(is.character), as.factor))
