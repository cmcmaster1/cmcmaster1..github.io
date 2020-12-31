library(tidyverse)

biologics_list <- tibble(drug = c("tocilizumab", "infliximab", "adalimumab",
                                  "etanercept", "abatacept", "rituximab",
                                  "baricitinib", "tofacitinib", "upadacitinib",
                                  "ustekinumab", "ixekizumab", "secukinumab",
                                  "golimumab", "certolizumab pegol"))

pbs_api <- function(table, format = "csv", header = "true", download = "true", ...){
  
  base_address <- glue::glue("https://aucapiapppbspilot.azurewebsites.net/{table}")
  
  query <- list(format = format,
                include_header = header,
                download = download,
                ...)
  
  got <- httr::GET(base_address, query = query)
  
  httr::content(got)
}

schedules <- pbs_api("SCHEDULE")
res <- pbs_api("ITEM_RESTRICTION_RLTD") %>% 
  filter(str_detect(CONDITION_TYPE_CODE, regex("rheumatoid|psoriatic|arteritis|ankylosing|spondylo", ignore_case = TRUE)))

res <- pbs_api("RESTRICTION_TEXT") %>% 
  inner_join(res, by = c("RES_CODE", "SCHEDULE_CODE"))

biologics_list_expanded <- biologics_list %>% 
  mutate(item = map(drug, ~pbs_api("ITEM", DRUG_NAME = .x))) %>% 
  unnest(item)

full_data <- biologics_list_expanded %>% 
  inner_join(res, by = c("PBS_CODE", "SCHEDULE_CODE", "BENEFIT_TYPE_CODE")) %>% 
  inner_join(schedules, by = "SCHEDULE_CODE") %>% 
  select(LI_DRUG_NAME, BRAND_NAME, LI_FORM, PBS_CODE, BENEFIT_TYPE_CODE, 
         MAXIMUM_QUANTITY, NUMBER_OF_REPEATS, PACK_SIZE, 
         STREAMLINE = TREATMENT_OF_CODE, LI_HTML_TEXT, CONDITION_TYPE_CODE,
         EFFECTIVE_DATE, LATEST_SCHEDULE_INDICATOR) %>% 
  filter(LATEST_SCHEDULE_INDICATOR == "Y") %>% 
  mutate(CONDITION = case_when(str_detect(CONDITION_TYPE_CODE, regex("rheumatoid", ignore_case = TRUE)) ~ "Rheumatoid Arthritis",
                               str_detect(CONDITION_TYPE_CODE, regex("psoriatic", ignore_case = TRUE)) ~ "Psoriatic Arthritis",
                               str_detect(CONDITION_TYPE_CODE, regex("ankylosing", ignore_case = TRUE)) ~ "Ankylosing Spondylitis",
                               str_detect(CONDITION_TYPE_CODE, regex("spondylo", ignore_case = TRUE)) ~ "Non-radiographic Axial SpA",
                               str_detect(CONDITION_TYPE_CODE, regex("arteritis", ignore_case = TRUE)) ~ "Giant Cell Arteritis",
                               TRUE ~ CONDITION_TYPE_CODE),
         BENEFIT_TYPE = case_when(BENEFIT_TYPE_CODE == "U" ~ "unrestricted",
                                  BENEFIT_TYPE_CODE == "R" ~ "restricted",
                                  BENEFIT_TYPE_CODE == "A" ~ "authority required",
                                  BENEFIT_TYPE_CODE == "S" ~ "streamlined"),
         DOSE = str_extract(LI_FORM, "[:digit:]+ (?=mg)")) %>% 
  mutate(init = str_locate(LI_HTML_TEXT, regex("initial", ignore_case = TRUE))[,1],
         cont = str_locate(LI_HTML_TEXT, regex("continuing", ignore_case = TRUE))[,1]) %>% 
  replace_na(list(init = 10000, cont = 10000)) %>% 
  mutate(PHASE = if_else(init < cont, "Initial", "Continuing")) %>% 
  select(-init, -cont)

write_rds(full_data, "data/full_data.RDS")


full_data %>% 
  mutate(PHASE = case_when(str_detect(LI_HTML_TEXT, regex("initial", ignore_case = TRUE)) ~ "Initial", 
                           str_detect(LI_HTML_TEXT, regex("continuing", ignore_case = TRUE)) ~ "Continuing"))

full_data %>% 
  mutate(init = case_when(str_detect(LI_HTML_TEXT, regex("initial", ignore_case = TRUE)) ~ 1, TRUE ~ 0),
         cont = case_when(str_detect(LI_HTML_TEXT, regex("continuing", ignore_case = TRUE)) ~ 1, TRUE ~ 0)) %>% 
  filter(init == cont) %>% 
  View()


full_data %>% 
   %>% 
  
