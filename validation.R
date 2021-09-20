# Last Updated 25-08-2021

# Validation
rm(list = ls())

# Libraries
library(tidyverse)
library(openxlsx)
library(cluster)
library(cleaninginspectoR)

# Sources
source("./R/check_log.R")
source("./R/check_time.R")
source("./R/data_falsification.R")
source("./R/minimum_standards.R")
source("./R/item_boxplots.R")

# Load inputs
data <- "./input/REACH_DRC_Data Cleaning_Initiative Conjointe de Suivi des Marchés_July2021.xlsx"
sheets <- getSheetNames(data)
SheetList <- lapply(sheets, read.xlsx, xlsxFile = data)
names(SheetList) <- sheets

clean <- SheetList[["Clean Data"]]
log <- SheetList[["Cleaning Log"]]
del <- SheetList[["Deletions"]]

# tool <- read.xlsx("./input/DRC_JMMI_Questionnaire_juin2021.xlsx")
tool <- read.csv("./input/questions.csv", stringsAsFactors = FALSE)



# Rename stuff
names(clean)[names(clean) == "_uuid"] <- "uuid"
clean$`_index` <- NULL
clean <- clean %>% mutate(., index = 1:nrow(.))

`%nin%` <- Negate(`%in%`)

# CleaningInspector
outliers <- inspect_all(clean) %>% filter(!is.na(index)) %>% left_join(., select(clean, "uuid", "index"), "index")
outliers <- outliers %>% filter(uuid %nin% log$uuid)

write.xlsx(outliers, paste0("./output/outliers_",Sys.Date(),".xlsx"))

# Check deletions
del$`_uuid` %in% clean$uuid # ok

# Check cleaning log application
q_not_in_log <- log %>% filter(Question %nin% names(clean))
log <- log %>% filter(Question %in% names(clean))

uuid_not_in_log <- log %>% filter(uuid %nin% clean$uuid)
write.xlsx(uuid_not_in_log, paste0("./output/uuid not in log_",Sys.Date(),".xlsx"))

log <- log %>% filter(uuid %in% clean$uuid)


check.log <- check_log(clean, log,variable = "Question", 
                       old_log_var = "Old.Value", new_log_var = "New.Value") %>% 
             mutate(., check = ifelse(New.Value == value_extracted, 1, 0)) %>% filter(check == 0)

write.xlsx(check.log, paste0("./output/log issues_",Sys.Date(),".xlsx"))

# Check minimum standards
min.standards <- minium_standards(clean, group_var = "q0d4_commune", items_vec = ends_with("_prix_final")) %>% filter(n <3)
write.xlsx(min.standards, paste0("./output/minimum standards_",Sys.Date(),".xlsx"))

# Falsification
enum.false <- calculateEnumeratorSimilarity(clean, tool, col_enum = "q0a_enqueteur", col_admin = "q0d4_commune")
simil.surv <- calculateDifferences(clean, tool) %>% filter(number.different.columns < 5)
write.xlsx(simil.surv, paste0("./output/similar surveys_",Sys.Date(),".xlsx"))

# Check prices
data <- read.xlsx("./input/REACH_RDC_Clean Data_ICSM_Juillet 2021_Au20210810.xlsx")

library(plyr)
item_boxplots_formatted(data, "q0d4_commune", ends_with("_prix_final"))
# large variatins for: natte, pagne, moustiquaire

