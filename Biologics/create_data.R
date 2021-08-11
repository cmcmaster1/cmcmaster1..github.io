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

schedule_code <- schedules %>%
  filter(LATEST_SCHEDULE_INDICATOR == "Y") %>%
  pull(SCHEDULE_CODE)

res <- pbs_api("ITEM_RESTRICTION_RLTD", SCHEDULE_CODE = schedule_code) %>%
  filter(str_detect(CONDITION_TYPE_CODE, regex("rheumatoid|psoriatic|arteritis|ankylosing|spondylo|granulomatosis|polyangiitis", ignore_case = TRUE)))

res <- pbs_api("RESTRICTION_TEXT") %>%
  inner_join(res, by = c("RES_CODE", "SCHEDULE_CODE"))

pbs_codes <- res %>%
  distinct(PBS_CODE) %>%
  pull(PBS_CODE)

biologics_list <- pbs_api("ITEM", SCHEDULE_CODE = schedule_code) %>%
  filter(PBS_CODE %in% pbs_codes)


full_data <- biologics_list %>%
  inner_join(res, by = c("PBS_CODE", "SCHEDULE_CODE", "BENEFIT_TYPE_CODE")) %>%
  inner_join(schedules, by = "SCHEDULE_CODE") %>%
  select(LI_DRUG_NAME, BRAND_NAME, LI_FORM, PBS_CODE, BENEFIT_TYPE_CODE,
         MAXIMUM_QUANTITY = MAXIMUM_PRESCRIBABLE_PACK, NUMBER_OF_REPEATS,
         PACK_SIZE = MAX_PRESCRIBABLE_UNIT_OF_USE, STREAMLINE = TREATMENT_OF_CODE,
         LI_HTML_TEXT, CONDITION_TYPE_CODE) %>%
  mutate(CONDITION = case_when(str_detect(CONDITION_TYPE_CODE, regex("rheumatoid", ignore_case = TRUE)) ~ "RA",
                               str_detect(CONDITION_TYPE_CODE, regex("psoriatic", ignore_case = TRUE)) ~ "PsA",
                               str_detect(CONDITION_TYPE_CODE, regex("ankylosing", ignore_case = TRUE)) ~ "AS",
                               str_detect(CONDITION_TYPE_CODE, regex("spondylo", ignore_case = TRUE)) ~ "nr-axSpA",
                               str_detect(CONDITION_TYPE_CODE, regex("arteritis", ignore_case = TRUE)) ~ "GCA",
                               str_detect(CONDITION_TYPE_CODE, regex("granulomatosis", ignore_case = TRUE)) ~ "GPA",
                               str_detect(CONDITION_TYPE_CODE, regex("microscopic", ignore_case = TRUE)) ~ "MPA",
                               TRUE ~ CONDITION_TYPE_CODE),
         STREAMLINE = if_else(BENEFIT_TYPE_CODE == "S", STREAMLINE, NULL),
         BENEFIT_TYPE = case_when(BENEFIT_TYPE_CODE == "U" ~ "Unrestricted",
                                  BENEFIT_TYPE_CODE == "R" ~ "Restricted",
                                  BENEFIT_TYPE_CODE == "A" ~ "Authority",
                                  BENEFIT_TYPE_CODE == "S" ~ "Streamlined"),
         DOSE = str_extract(LI_FORM, "[:digit:]+ (?=mg)"),
         LI_DRUG_NAME = str_remove(LI_DRUG_NAME, " pegol"),
         init = str_locate(LI_HTML_TEXT, regex("initial", ignore_case = TRUE))[,1],
         cont = str_locate(LI_HTML_TEXT, regex("continuing", ignore_case = TRUE))[,1],
         ind = CONDITION %in% c("GPA", "MPA") & str_detect(LI_HTML_TEXT, regex("(?<!-)induction", ignore_case = TRUE)),
         reind = CONDITION %in% c("GPA", "MPA") & str_detect(LI_HTML_TEXT, regex("re-induction", ignore_case = TRUE)),
         grand = str_detect(LI_HTML_TEXT, regex("Grandfather", ignore_case = TRUE))) %>%
  replace_na(list(init = 10000, cont = 10000)) %>%
  mutate(PHASE = case_when(ind ~ "Initial",
                           grand ~ "Grandfathered",
                           reind ~ "Re-induction",
                           init < cont ~ "Initial",
                           TRUE ~ "Continuing")) %>%
  select(-init, -cont)

write_rds(full_data, "Biologics/docs/data/full_data.RDS")

