# title: Electricity supply quality and use among rural and peri-urban households 
# and small firms in Nigeria
# author: Setu Pelz
# date created: 2021/10/1
# date last revised: 2023/03/08

# LOAD NECESSARY PACKAGES ------------------------------------------------------

#install.packages(pacman)
library(pacman)

# tidyverse
p_load(dplyr, tidyr, stringr, forcats, readxl, ggplot2, patchwork, ggrepel)

#misc
p_load(here, kableExtra)

# Avoid scientific notation
options(scipen = 999)

# READ IN HOUSEHOLD DATA -------------------------------------------------------

maindata_hh <- read.csv(here("Data", "Survey", "peoplesun_hh_anon.csv"))
apps_hh <- read.csv(here("Data", "Survey", "peoplesun_hhapps_anon.csv"))
stoves_hh <- read.csv(here("Data", "Survey", "peoplesun_hhstoves_anon.csv"))

strata <- read.csv(here("Data", "Survey", "eaidgeokey_eas_anon.csv"))

codebook_hh <- readxl::read_xlsx(here("Data", "Survey", "peoplesun_hh_odk_codebook.xlsx"))
choices_hh <- readxl::read_xlsx(here("Data", "Survey", "peoplesun_hh_odk_choices.xlsx"))

# Generate additional variables ------------------------------------------------

# Add strata and factor levels
maindata_hh <- maindata_hh %>% 
  left_join(strata %>% select(eaid, strata = urca_cat, urca)) %>% 
  mutate(strata = factor(strata, levels = c("Town", "<1hr to large city", 
                                            "<1hr to int. city", 
                                            "<1hr to small city/town+")))


# Key socio-economic variables
maindata_hh <- maindata_hh %>% 
  group_by(zone, eaid) %>% 
  mutate(
    agehead = ifelse(q104 == 1, q106, q109),
    adults = q201, bicycle = q207_1, motorbike = q207_2, car = q207_3, 
    bankcc = q212, mobmoney = q215, exp = q218,
    hh_grid = as.numeric(q301__1 == 1, na.rm = T),
    com_grid = as.numeric(sum(q301__1 == 1, na.rm = T) > 0 | 
                            sum(q302 == 1, na.rm = T) > 0 | 
                            sum(q303 == 1, na.rm = T) > 0)
  ) %>% 
  ungroup()

# READ IN ENTERPRISE DATA ------------------------------------------------------

maindata_ent <- read.csv(here("Data", "Survey", "peoplesun_ent_anon.csv"))
apps_ent <- read.csv(here("Data", "Survey", "peoplesun_entapps_anon.csv"))
stoves_ent <- read.csv(here("Data", "Survey", "peoplesun_entstoves_anon.csv"))

codebook_ent <- readxl::read_xlsx(here("Data", "Survey", "peoplesun_ent_odk_codebook.xlsx"))
choices_ent <- readxl::read_xlsx(here("Data", "Survey", "peoplesun_ent_odk_choices.xlsx"))

# Generate additional variables ------------------------------------------------

maindata_ent <- maindata_ent %>% 
  left_join(strata %>% select(eaid, strata = urca_cat, urca)) %>% 
  mutate(strata = factor(strata, levels = c("Town", "<1hr to large city", 
                                            "<1hr to int. city", 
                                            "<1hr to small city/town+")))

# Add in national weights of enumeration area from household data
maindata_ent <- maindata_ent %>% 
  left_join(maindata_hh %>% distinct(eaid, natweight))

# DESCRIPTIVE STATISTICS -------------------------------------------------------

# General summary of household sample
maindata_hh %>% 
  select(c(zone, "agehead", "adults", 
           "bicycle", "motorbike", "car", "bankcc", "mobmoney", "exp")) %>% 
  pivot_longer(-zone, names_to = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = c("agehead", "adults", "bicycle", 
                                                "motorbike", "car", "bankcc", 
                                                "mobmoney", "exp"), 
                       labels = c("Age of household head", "Household adults", 
                                  "Number of bicycles", "Number of motorbikes",
                                  "Number of cars",  "Share using Bank", 
                                  "Share using Mobile Money", 
                                  "Weekly (non-energy) expenditures")) 
  ) %>% 
  group_by(zone, Variable) %>% 
  summarise(mean = mean(value ,na.rm = T)) %>% 
  mutate(mean = round(mean,2)) %>% 
  pivot_wider(names_from = zone, values_from = mean) %>% 
  kbl(., booktabs = T, format = "latex", align = c("l", rep("r",3)), 
      linesep = "") %>% 
  kable_styling() %>% 
  gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>% 
  save_kable(file = here("Manuscript", "Tables", "summarystats_hh.tex"))

# General summary of enterprise sample
maindata_ent %>% 
  select(c("ageowner" = q213_2, "ownerfemale" = q213_1,
           "bicycle" = q209_1, "motorbike" = q209_2, "car" = q209_3, 
           "bankacc" = q214, "mobmoney" = q217, "assets" = q210, "fte" = q211,
           "salaries" = q212, zone)) %>% 
  pivot_longer(-zone, names_to = "Variable") %>% 
  mutate(Variable = factor(Variable, levels = c("ageowner", "ownerfemale",
                                                "bicycle", "motorbike", "car", 
                                                "bankacc", "mobmoney", "assets",
                                                "fte", "salaries"), 
                           labels = c("Age of owner", "Female owner",
                                      "Number of bicycles", "Number of motorbikes", 
                                      "Number of cars", "Share using Bank", 
                                      "Share using Mobile Money",  "Total assets", 
                                      "Full-time employees", "Monthly salaries")) 
  ) %>% 
  group_by(zone, Variable) %>% 
  summarise(mean = mean(value ,na.rm = T)) %>% 
  mutate(mean = round(mean,2)) %>% 
  pivot_wider(names_from = zone, values_from = mean) %>% 
  kbl(., booktabs = T, format = "latex", align = c("l", rep("r",3)), 
      linesep = "") %>% 
  kable_styling() %>% 
  gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>% 
  save_kable(file = here("Manuscript", "Tables", "summarystats_ent.tex"))

# Zone-wise electricity access summary
zonecomelec <- maindata_hh %>% 
  group_by(zone) %>% 
  summarise(com_grid = scales::percent(sum(com_grid)/ n())) %>% 
  pivot_wider(names_from = zone, values_from = com_grid) %>% 
  mutate(Access = "National grid connection")

zonelightsrc_hh <- maindata_hh %>% 
  select(zone, hhid, q301) %>% 
  separate_rows(q301, sep = " ") %>% 
  left_join(choices_hh %>% filter(list == "lightlist") %>% select(name, label),
            by = c("q301" = "name")) %>%
  select(zone, hhid, q301 = label) %>% 
  mutate(use = 1) %>% 
  complete(nesting(zone, hhid), q301, fill = list(use = 0)) %>% 
  ungroup() %>% 
  group_by(zone, q301) %>% 
  summarise(use = sum(use)/ n()) %>% 
  pivot_wider(names_from = zone, values_from = use) %>% 
  rename(Access = q301) %>% 
  mutate(Access = ifelse(!Access %in% (choices_hh %>% 
                                         filter(list == "lightlist") %>% 
                                         pull(label) %>% .[1:7]), 
                         "Other", as.character(Access))) %>% 
  group_by(Access) %>% 
  summarise(across(`North Central`:`South South`, ~sum(.))) %>% 
  mutate(Access = factor(Access, levels = choices_hh %>% 
                         filter(list == "lightlist") %>% 
                         pull(label))
  ) %>% 
  mutate(across(`North Central`:`South South`, ~scales::percent(., accuracy = 1))
         )%>% 
  arrange(Access) 

rbind(zonecomelec,zonelightsrc_hh) %>% 
  select(Access, everything()) %>% 
  kbl(., booktabs = T, format = "latex", align = c("l", rep("r",3))) %>% 
  kable_styling() %>% 
  pack_rows("Community-level", 1,1) %>% 
  pack_rows("Household-level",2, 9) %>% 
  gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>% 
  save_kable(file = here("Manuscript", "Tables", "access_commhh.tex"))

# Primary and secondary supply -------------------------------------------------

maindata_hh %>% 
  select(hhid, q302, q303, q301, zone) %>%
  rowwise() %>% 
  # Remove primary and secondary sources from 'all' column
  mutate(q301 = str_replace_all(q301, paste0("\\b(?:", paste(c(q302, q303), collapse = "|"), ")\\b"), ""),
         q301 = paste(sort(as.numeric(strsplit(q301, " ")[[1]])), collapse = ", "), 
         q301 = as.integer(str_extract(q301, pattern = "\\b\\d+\\b"))) %>% 
  left_join(choices_hh %>% filter(list == "eleclist") %>% 
              transmute(q302 = as.integer(name), pri = label)) %>% 
  left_join(choices_hh %>% filter(list == "eleclist") %>% 
              transmute(q303 = as.integer(name), sec = label)) %>%
  left_join(choices_hh %>% filter(list == "lightlist") %>% 
              transmute(q301 = as.integer(name), ter = label)) %>%
  select(hhid, zone, pri, sec, ter) %>% 
  pivot_longer(-c(hhid, zone)) %>% 
  complete(nesting(hhid, zone), name, fill = list(value = "None")) %>% 
  mutate(value = case_when(
    value == "National grid connection" ~ "National grid connection",
    value == "Electric generator" ~ "Electric generator",
    value == "Rechargeable battery" ~ "Rechargeable battery",
    value == "Dry cell battery / torch" ~ "Dry cell battery / torch",
    value == "None" ~ "None",
    value == "Don't know" ~ "Don't know",
    TRUE ~ "Other")) %>% 
  arrange(hhid, zone, name, value) %>% 
  group_by(zone, name) %>% 
  dplyr::count(name, value) %>% 
  group_by(zone, name) %>% 
  mutate(prop = scales::percent(n / sum(n), .1)) %>%
  select(-n) %>% 
  pivot_wider(names_from = name, values_from = prop, values_fill = "0%") %>% 
  mutate(value = factor(value, levels = choices_hh %>% filter(list == "eleclist") %>% pull(label))) %>% 
  arrange(zone, value) %>%
  ungroup() %>% 
  filter(value != "None") %>% 
  select(-zone) %>% 
  rename(Primary = pri, Secondary = sec, Tertiary = ter, " " = value) %>% 
  kbl(., booktabs = T, format = "latex", align = c("l", rep("r",12))) %>% 
  kable_styling() %>%
  group_rows("North Central", 1, 5) %>%
  group_rows("North West", 6, 10) %>%
  group_rows("South South", 11, 15) %>% 
  gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>% 
  save_kable(file = here("Manuscript", "Tables", "prisecelec_hh_tbl.tex"))

maindata_ent %>% 
  select(entid, q302, q303, q301, zone) %>%
  rowwise() %>% 
  # Remove primary and secondary sources from 'all' column
  mutate(q301 = str_replace_all(q301, paste0("\\b(?:", paste(c(q302, q303), collapse = "|"), ")\\b"), ""),
         q301 = paste(sort(as.numeric(strsplit(q301, " ")[[1]])), collapse = ", "), 
         q301 = as.integer(str_extract(q301, pattern = "\\b\\d+\\b"))) %>% 
  left_join(choices_ent %>% filter(list == "eleclist") %>% 
              transmute(q302 = as.integer(name), pri = label)) %>% 
  left_join(choices_ent %>% filter(list == "eleclist") %>% 
              transmute(q303 = as.integer(name), sec = label)) %>%
  left_join(choices_ent %>% filter(list == "lightlist") %>% 
              transmute(q301 = as.integer(name), ter = label)) %>%
  select(entid, zone, pri, sec, ter) %>% 
  pivot_longer(-c(entid, zone)) %>% 
  complete(nesting(entid, zone), name, fill = list(value = "None")) %>% 
  mutate(value = case_when(
    value == "National grid connection" ~ "National grid connection",
    value == "Electric generator" ~ "Electric generator",
    value == "Rechargeable battery" ~ "Rechargeable battery",
    value == "Dry cell battery / torch" ~ "Dry cell battery / torch",
    value == "None" ~ "None",
    TRUE ~ "Other")) %>% 
  arrange(entid, zone, name, value) %>% 
  group_by(zone, name) %>% 
  dplyr::count(name, value) %>% 
  group_by(zone, name) %>% 
  mutate(prop = scales::percent(n / sum(n), .1)) %>%
  select(-n) %>% 
  pivot_wider(names_from = name, values_from = prop, values_fill = "0%") %>% 
  mutate(value = factor(value, levels = choices_ent %>% filter(list == "eleclist") %>% pull(label))) %>% 
  arrange(zone, value) %>%
  ungroup() %>% 
  filter(value != "None") %>% 
  select(-zone) %>% 
  rename(Primary = pri, Secondary = sec, Tertiary = ter, " " = value) %>% 
  kbl(., booktabs = T, format = "latex", align = c("l", rep("r",12))) %>% 
  kable_styling() %>%
  group_rows("North Central", 1, 5) %>%
  group_rows("North West", 6, 10) %>%
  group_rows("South South", 11, 15) %>% 
  gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>% 
  save_kable(file = here("Manuscript", "Tables", "prisecelec_ent_tbl.tex"))

# Cooking access ---------------------------------------------------------------

prisecstove_hh <- stoves_hh %>% 
  left_join(maindata_hh %>% select(hhid, zone)) %>% 
  select(hhid, zone, stove = q502, number = q502_1, minutes = q502_2) %>% 
  mutate(stove = ifelse(grepl(stove, pattern = "Kero"), "Kerosene", stove)) %>% 
  filter(number > 0) %>% 
  group_by(hhid, zone) %>% 
  mutate(prop = minutes / sum(minutes, na.rm = T)) %>% 
  arrange(hhid, desc(prop)) %>%
  group_by(hhid) %>%
  slice_max(order_by = prop, n = 2, with_ties = F) %>% 
  group_by(hhid, zone) %>% 
  mutate(type = ifelse(prop == max(prop), "Pri", "Sec")) %>% 
  select(-number, -minutes) %>% 
  mutate(stove = stove, type = factor(type)) %>% 
  ungroup() %>% 
  complete(hhid, type, fill = list(stove = "None", type = "Sec", prop = 0)) %>% 
  dplyr::count(zone, type, stove)

prisecstove_hh %>% 
  group_by(zone, type) %>% 
  mutate(prop = n / sum(n),
         stove = fct_reorder(stove, desc(prop))) %>% 
  select(-n) %>% 
  pivot_wider(names_from = type, values_from = prop) %>% 
  rowwise() %>% 
  ungroup() %>% 
  mutate(across(c(Pri, Sec), ~scales::percent(., accuracy = .1))) %>% 
  select(zone, Stove = stove, Pri, Sec) %>% 
  mutate(
    Stove = str_replace(Stove, pattern = " stove", ""),
    Stove = factor(Stove, levels = c("Electric cooking", "LPG", "Biogas", "Kerosene",
                                     "Solid-fuel"),
                   labels = c("Electric", "LPG", "Biogas", "Kerosene",
                              "Biomass"))) %>% 
  arrange(zone, Stove) %>% 
  filter(!is.na(Stove)) %>% 
  select(-zone) %>% 
  rename(Primary = Pri, Secondary = Sec, " " = Stove) %>% 
  kbl(., booktabs = T, format = "latex", align = c("l", rep("r",3))) %>% 
  kable_styling() %>% 
  group_rows("North Central", 1, 5) %>%
  group_rows("North West", 6, 10) %>%
  group_rows("South South", 11, 15) %>% 
  gsub(".(begin|end){table.*}", "", ., perl = TRUE) %>% 
  save_kable(file = here("Manuscript", "Tables", "prisecstove_hh_tbl.tex"))

# Grid supply hours ------------------------------------------------------------

rbind(maindata_hh %>% select(q307_3, zone) %>% mutate(survey = "Household"),
      maindata_ent %>% select(q307_3, zone) %>% mutate(survey = "Enterprise")) %>% 
  mutate(survey = factor(survey, levels = c("Household", "Enterprise"))) %>% 
ggplot(aes(q307_3, fill = survey)) +
  stat_ecdf(pad = FALSE, geom = "line", aes(colour = survey)) +
  geom_histogram(aes(y = stat(width*density)), bins = 24, binwidth = 0.5,
                 position = "identity", alpha = 0.5) +
  facet_wrap(~zone) +
  scale_x_continuous(breaks = seq(0,24,2)) +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  coord_cartesian(xlim = c(0,24), ylim = c(0,1)) +
  labs(x = "Average hours supplied over the last 7 days",
       y = "Proportion of sample (line = cumulative)",
       fill = "Survey",
       colour = "Survey") +
  theme_bw() +
  theme(legend.position = "top")

ggsave(filename = here("Manuscript", "Figures", "grid_supplyhrs.png"),
       width = 10, height = 4)

# Appliances ownership ---------------------------------------------------------

genapp_hh_tbl <- maindata_hh %>% 
  select(hhid, zone, matches("q402__")) %>% 
  pivot_longer(-c(hhid, zone), names_to = "genapp") %>% 
  mutate(genapp = str_extract(genapp, "\\d$")) %>% 
  left_join(choices_hh %>% 
              filter(list == "genapplist") %>% 
              select(genapp = name, appliance = label)) %>% 
  pivot_wider(id_cols = c(hhid,zone),
              names_from = appliance, values_from = value)

othapp_hh_tbl <- maindata_hh %>% 
  select(hhid, matches("q404__")) %>% 
  pivot_longer(-c(hhid), names_to = "othapp") %>% 
  mutate(othapp = str_extract(othapp, "\\d+$")) %>% 
  # Remove "None" record
  filter(othapp != 0) %>% 
  left_join(choices_hh %>% 
              filter(list == "othapplist") %>% 
              select(othapp = name, appliance = label)) %>% 
  pivot_wider(id_cols = hhid, names_from = appliance, values_from = value) %>% 
  rename("(Kettle, Iron, etc.)" = "Home thermal appliances (e.g., Kettle, Iron)",
         "(Mixer, Blender, etc.)" = "Home mechanical appliances (e.g., Mixer, Blender)")

appown_hh <- left_join(genapp_hh_tbl, othapp_hh_tbl) %>% 
  pivot_longer(-c(hhid, zone)) %>% 
  group_by(zone, name) %>% 
  summarise(prop = mean(value))

genapp_ent_tbl <- maindata_ent %>% 
  select(entid, zone, matches("q401__")) %>% 
  pivot_longer(-c(entid, zone), names_to = "genapp") %>% 
  mutate(genapp = str_extract(genapp, "\\d$")) %>% 
  left_join(choices_ent %>% 
              filter(list == "genapplist") %>% 
              select(genapp = name, appliance = label)) %>% 
  pivot_wider(id_cols = c(entid,zone),
              names_from = appliance, values_from = value)

othapp_ent_tbl <- maindata_ent %>% 
  select(entid, matches("q403__")) %>% 
  pivot_longer(-c(entid), names_to = "othapp") %>% 
  mutate(othapp = str_extract(othapp, "\\d+$")) %>% 
  # Remove "None" record
  filter(othapp != 0) %>% 
  left_join(choices_ent %>% 
              filter(list == "othapplist") %>% 
              select(othapp = name, appliance = label)) %>% 
  pivot_wider(id_cols = entid, names_from = appliance, values_from = value) %>% 
  rename("(Kettle, Iron, etc.)" = "Home thermal appliances (e.g., Kettle, Iron)",
         "(Mixer, Blender, etc.)" = "Home mechanical appliances (e.g., Mixer, Blender)")

appown_ent <- left_join(genapp_ent_tbl, othapp_ent_tbl) %>% 
  pivot_longer(-c(entid, zone)) %>% 
  group_by(zone, name) %>% 
  summarise(prop = mean(value))

rbind(appown_hh %>% mutate(survey = "Household"),
      appown_ent %>% mutate(survey = "Enterprise")) %>% 
  mutate(name = fct_reorder(name, prop, .desc = T),
         survey = factor(survey, levels = c("Household", "Enterprise"))) %>% 
  ggplot(aes(name, y = prop, fill = survey)) +
  geom_col(position = position_dodge2(preserve = "single"), width = 0.5) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1)) +
  facet_wrap(~zone) +
  labs(x = NULL,
       y = "Share owning appliance ...",
       fill = "Survey") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

ggsave(filename = here("Manuscript", "Figures", "genapp_own.png"),
       width = 10, height = 4)

# Energy services --------------------------------------------------------------

maindata_hh %>% 
  select(hhid, zone,
         "... can breathe air free of smoke or pollution in the home all day." = q604_1, 
         "... has adequate electrical light when they need it." = q604_2, 
         "... can keep the house at a comfortable temperature." = q604_3, 
         "... can store perishable foods over long periods of time." = q604_4, 
         "... has access to a communications device in the home." = q604_5, 
         "... has access to digital information and entertainment in the home." = q604_6) %>% 
  pivot_longer(-c(hhid, zone)) %>% 
  left_join(choices_hh %>% 
              filter(list == "likerttrustlist") %>% 
              transmute(value = as.integer(name), label = label)) %>% 
  mutate(labelshort = 
           ifelse(label %in% c("Somewhat", "A great deal"), "At least somewhat", "Not very much / at all")) %>% 
  dplyr::count(zone, name, labelshort) %>% 
  group_by(zone, name) %>% 
  mutate(prop = n / sum(n),
         n = sum(n)) %>% 
  filter(labelshort == "At least somewhat") %>% 
  ggplot(aes(zone, prop)) +
  geom_col() +
  facet_wrap(~name, ncol = 2) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(),
                     limits = c(0,1)) +
  labs(x = NULL,
       y = "Proportion of households",
       subtitle = "I agree, at least somewhat, that our household ... ") +
  theme_bw()

ggsave(filename = here("Manuscript", "Figures", "enerserve_hh.png"),
       width = 10, height = 4)

maindata_ent %>% 
  select(entid, zone,
         "... our electricity supply is reliable enough to grow the business." = q604_1, 
         "... our electricity supply is affordable enough to grow the business." = q604_2, 
         "... our electricity supply provides sufficient power to grow the business." = q604_3, 
         "... we have the appliances / equipment needed to grow the business." = q604_4) %>% 
  pivot_longer(-c(entid, zone)) %>% 
  left_join(choices_hh %>% 
              filter(list == "likerttrustlist") %>% 
              transmute(value = as.integer(name), label = label)) %>% 
  mutate(labelshort = 
           ifelse(label %in% c("Somewhat", "A great deal"), "At least somewhat", "Not very much / at all")) %>% 
  dplyr::count(zone, name, labelshort) %>% 
  group_by(zone, name) %>% 
  mutate(prop = n / sum(n),
         n = sum(n)) %>% 
  filter(labelshort == "At least somewhat") %>% 
  ggplot(aes(zone, prop)) +
  geom_col() +
  facet_wrap(~name, ncol = 2) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = scales::pretty_breaks(),
                     limits = c(0,1)) +
  labs(x = NULL,
       y = "Proportion of enterprises",
       subtitle = "I agree, at least somewhat, that ... ") +
  theme_bw()

ggsave(filename = here("Manuscript", "Figures", "enerserve_ent.png"),
       width = 10, height = 3)
