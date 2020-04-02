# --------------------------------------------------------------------------- #

# 10 - Data Cleaning - MgR - Open Data

# --------------------------------------------------------------------------- #

# Libraries and Functions --------------------------------------------------- #
source(file = "syntax/00-LibrariesAndFunctions-MgR-OpenData.R")

# >>> grcs = GLES - Rolling Cross Sectional <<<
grcs <- read_dta(file = "data/GLES-RCS/ZA6803_v4-0-1_en.dta")

# >>> GLES Wahlkampfpanel <<<
gles <- read_dta(file = "data/GLES-Panel/ZA6804_en_v7-0-0.dta")


# --------------------------------------------------------------------------- #


# I. RCS ----

# Recoding Variables ----------------------------------------------------

# a. AfD votes ----------------------------------------------------

# i. Vote intentions AfD ----
attributes(grcs$pre005a)
table(grcs$pre005a)


# ii. Actual vote 2017 ----
attributes(grcs$pos004a)
table(grcs$pos004a)

# Recoding ----
grcs %<>% mutate(voteint = case_when(pre005a == -98 ~ "ADontKnow",
                                     pre005a == 1 ~ "CDU/CSU",
                                     pre005a == 4 ~ "SPD",
                                     pre005a == 5 ~ "FDP",
                                     pre005a == 6 ~ "Grüne",
                                     pre005a == 7 ~ "LINKE",
                                     pre005a == 322 ~ "AfD",
                                     pre005a == 801 ~ "Andere",
                                     FALSE ~ NA_character_),
                 voteact = case_when(pos004a < 1 ~ NA_character_,
                                     pos004a == 1 ~ "CDU/CSU",
                                     pos004a == 4 ~ "SPD",
                                     pos004a == 5 ~ "FDP",
                                     pos004a == 6 ~ "Grüne",
                                     pos004a == 7 ~ "LINKE",
                                     pos004a == 322 ~ "AfD",
                                     pos004a == 801 ~ "Andere"),
                 afdvote = case_when(voteact == "AfD" ~ 1,
                                     voteact != "AfD" ~ 0,
                                     FALSE ~ NA_real_))

grcs$afd <- if_else(grcs$afdvote == 1, "AfD", "Andere Partei") %>% factor()
table(grcs$afd)
proptable(grcs$afd)

# b. Independent variables ----

# i. Links/Rechts
attributes(grcs$pre017)
table(grcs$pre017)

grcs %<>% mutate(lr = if_else(pre017 > 0, 
                              pre017 - 1, NA_real_))

# ii. Ost-West ----
attributes(grcs$ostwest)
table(grcs$ostwest)
grcs$ost <- if_else(grcs$ostwest == 1, "West", "Ost") %>% 
  factor(., levels = c("West", "Ost"))

# iii. Gender
attributes(grcs$pre102)
table(grcs$pre102, useNA = "always")

grcs$gender <- if_else(grcs$pre102 == 1, "M", "W") %>% factor()

# iv. Ego: Zuwanderung
attributes(grcs$pre019)
table(grcs$pre019)

grcs %<>% mutate(ego_immi1 = if_else(pre019 > 0, pre019 - 1, NA_real_))

# II. Panel ----

# Recoding Variables ----------------------------------------------------

# Quality control ----

# Question: Please choose 'disagree'

# Wave 1
attributes(gles$kp1_040q)
table(gles$kp1_040q)

# Wave 8
table(gles$kp8_040q)
table(gles$kp8_050q)

# Question: Please choose 'agree'
attributes(gles$kp1_050q)
table(gles$kp1_050q)

attributes(gles$kp8_050q)
table(gles$kp8_050q)

# Recoding:
gles %<>% mutate(quality1_040 = case_when(kp1_040q == 2 ~ 1,
                                          kp1_040q %in% c(1, 3:5) ~ 0,
                                          FALSE ~ NA_real_),
                 quality1_050 = case_when(kp1_050q == 4 ~ 1,
                                          kp1_050q %in% c(1:3, 5) ~ 0,
                                          FALSE ~ NA_real_),
                 quality8_040 = case_when(kp8_040q == 2 ~ 1,
                                          kp8_040q %in% c(1, 3:5) ~ 0,
                                          FALSE ~ NA_real_),
                 quality8_050 = case_when(kp8_050q == 4 ~ 1,
                                          kp8_050q %in% c(1:3, 5) ~ 0,
                                          FALSE ~ NA_real_),
                 quality1 = quality1_040 + quality1_050,
                 quality8 = quality8_040 + quality8_050,
                 quality_a = quality1 + quality8,
                 quality_b = if_else(quality1 > 0 & quality8 > 0, 
                                     1, 0),
                 quality_c = if_else(quality1 > 0 & quality8 == 2, 
                                     1, 0))

gles_old <- gles
gles %<>% filter(quality_a > 2)

# a. AfD votes ----

# i. Vote intentions AfD ----
attributes(gles$kp1_190ba)
table(gles$kp1_190ba)

attributes(gles$kp7_190ba)
table(gles$kp7_190ba)


# ii. Actual vote 2017 ----
# Turnout
attributes(gles$kp8_180)
table(gles$kp8_180)

# Vote (Zweitstimme)
attributes(gles$kp8_200ba)
table(gles$kp8_200ba)

# Recoding ----
gles %<>% mutate(voteint1 = case_when(kp1_190ba == -98 ~ "Dont Know",
                                      kp1_190ba == 1 ~ "CDU/CSU",
                                      kp1_190ba == 4 ~ "SPD",
                                      kp1_190ba == 5 ~ "FDP",
                                      kp1_190ba == 6 ~ "Grüne",
                                      kp1_190ba == 7 ~ "LINKE",
                                      kp1_190ba == 322 ~ "AfD",
                                      kp1_190ba == 801 ~ "Andere",
                                      FALSE ~ NA_character_),
                 voteint7 = case_when(kp7_190ba == -98 ~ "Dont Know",
                                      kp7_190ba == 1 ~ "CDU/CSU",
                                      kp7_190ba == 4 ~ "SPD",
                                      kp7_190ba == 5 ~ "FDP",
                                      kp7_190ba == 6 ~ "Grüne",
                                      kp7_190ba == 7 ~ "LINKE",
                                      kp7_190ba == 322 ~ "AfD",
                                      kp7_190ba == 801 ~ "Andere",
                                      FALSE ~ NA_character_),
                 voteact1 = case_when(kp8_200ba == 1 ~ "CDU/CSU",
                                      kp8_200ba == 4 ~ "SPD",
                                      kp8_200ba == 5 ~ "FDP",
                                      kp8_200ba == 6 ~ "Grüne",
                                      kp8_200ba == 7 ~ "LINKE",
                                      kp8_200ba == 322 ~ "AfD",
                                      kp8_200ba == 801 ~ "Andere",
                                      kp8_180 == 2 ~ "Nichtwahl",
                                      kp8_200ba < 1 ~ NA_character_),
                 voteact1a = case_when(kp8_200ba == 1 ~ "CDU/CSU",
                                       kp8_200ba == 4 ~ "SPD",
                                       kp8_200ba == 5 ~ "FDP",
                                       kp8_200ba == 6 ~ "Grüne",
                                       kp8_200ba == 7 ~ "LINKE",
                                       kp8_200ba == 322 ~ "AfD",
                                       kp8_200ba == 801 ~ "Andere",
                                       kp8_180 == 2 ~ NA_character_,
                                       kp8_200ba < 1 ~ NA_character_),
                 voteact1b = case_when(kp8_200ba == 1 ~ "CDU/CSU",
                                       kp8_200ba == 4 ~ "SPD",
                                       kp8_200ba == 5 ~ "FDP",
                                       kp8_200ba == 6 ~ "Grüne",
                                       kp8_200ba == 7 ~ "LINKE",
                                       kp8_200ba == 322 ~ "AfD",
                                       kp8_200ba == 801 ~ "Andere",
                                       kp8_180 == 2 ~ "Nichtwahl",
                                       kp8_200ba < 1 ~ NA_character_),
                 afdvote1 = case_when(voteact1 == "AfD" ~ 1,
                                      voteact1 %in% c("CDU/CSU", "SPD", "FDP",
                                                      "Grüne", "LINKE", 
                                                      "Andere", "Nichtwahl") ~ 0,
                                      FALSE ~ NA_real_),
                 afdvote1a = case_when(voteact1 == "AfD" ~ 1,
                                       voteact1 %in% c("CDU/CSU", "SPD", "FDP",
                                                       "Grüne", "LINKE",
                                                       "Andere") ~ 0,
                                       FALSE ~ NA_real_))

# Cross tables:

# Vote intention 06.10. - 10.11.2016 x Actual voting decision (27.09. - 0.10.2017):
table(gles$voteint1, gles$voteact1) %>% sum()
table(gles$voteint1, gles$voteact1) %>% prop.table(margin = 1) %>% {.*100} %>% round(2)

# Vote intention 06.10. - 10.11.2016 x Actual voting decision (27.09. - 0.10.2017):
table(gles$voteint7, gles$voteact1) %>% sum()
table(gles$voteint7, gles$voteact1) %>% prop.table(margin = 1) %>% {.*100} %>% round(2)

gles$voteint7 %<>% as.factor() %>% relevel(., ref = "Dont Know")

# Actual voting decision x AfD vote
table(gles$voteact1, gles$afdvote1, useNA = "always")

gles$voteact1 %<>% factor(., levels = c("AfD", "CDU/CSU", "FDP",
                                        "SPD", "Grüne", "LINKE",
                                        "Andere", "Nichtwahl"))

proptable(gles$voteact1)

# b. Independent variables --------------------------------------------------

# i. Outgroup index ----

# Deport economic refugees
attributes(gles$kp1_2880b)

# Surveil Islamic communities
attributes(gles$kp1_2880c)

# Restrict Islamic practices
attributes(gles$kp1_2880f)

# Islam fits Germany
attributes(gles$kp1_2880i)

gles %<>% mutate(adoptio1 = if_else(kp1_2880a %in% c(1:5),
                                    kp1_2880a - 1, NA_real_),
                 econref1 = if_else(kp1_2880b %in% c(1:5),
                                    kp1_2880b - 1, NA_real_),
                 survisl1 = if_else(kp1_2880c %in% c(1:5),
                                    kp1_2880c - 1, NA_real_),
                 unequal1 = if_else(kp1_2880d %in% c(1:5),
                                    kp1_2880d - 1, NA_real_),
                 plebisc1 = if_else(kp1_2880e %in% c(1:5),
                                    kp1_2880e - 1, NA_real_),
                 restisl1 = if_else(kp1_2880f %in% c(1:5),
                                    kp1_2880f - 1, NA_real_),
                 ecolibe1 = if_else(kp1_2880g %in% c(1:5),
                                    kp1_2880g - 1, NA_real_),
                 secsurv1 = if_else(kp1_2880h %in% c(1:5),
                                    kp1_2880h - 1, NA_real_),
                 fitisla1 = if_else(kp1_2880i %in% c(1:5),
                                    -(kp1_2880i - 5), NA_real_),
                 taxrich2 = if_else(kp2_2880j %in% c(1:5),
                                    kp2_2880j - 5, NA_real_),
                 retrump2 = if_else(kp2_2880k %in% c(1:5),
                                    kp2_2880k - 5, NA_real_),
                 reputin2 = if_else(kp2_2880m %in% c(1:5),
                                    kp2_2880m - 5, NA_real_))

# Check consistency/success
apply(gles[, c("adoptio1", "econref1", "survisl1",
               "unequal1", "plebisc1", "restisl1",
               "ecolibe1", "secsurv1", "fitisla1",
               "taxrich2", "retrump2", "reputin2")], 
      2,
      table, useNA = "always")

apply(gles[, c("adoptio1", "econref1", "survisl1",
               "unequal1", "plebisc1", "restisl1",
               "ecolibe1", "secsurv1", "fitisla1",
               "taxrich2", "retrump2", "reputin2")], 
      2,
      proptable, useNA = "always")


# Create index:
gles %<>% mutate(outgrindex1 = (econref1 + survisl1 + restisl1 + fitisla1))
gles %<>% mutate(outgrindex1stand = outgrindex1/max(gles$outgrindex1, na.rm = T))

table(gles$outgrindex1stand)

gles %<>% mutate(outgrindex = case_when(outgrindex1stand <= 0.1875 ~ 0,
                                        outgrindex1stand >= 0.25 & outgrindex1stand <= 0.375 ~ 1,
                                        outgrindex1stand >= 0.4375 & outgrindex1stand <= 0.5625 ~ 2,
                                        outgrindex1stand >= 0.625 & outgrindex1stand <= 0.75 ~ 3,
                                        outgrindex1stand >= 0.875 ~ 4,
                                        FALSE ~ NA_real_))

# ii. Ost-West ----
gles$ost <- if_else(gles$ostwest == 1, "West", "Ost")
gles$ost %<>% factor(., levels = c("West", "Ost"))


# iii. Socio-economic variables ----
# Age ----
table(gles$kpx_2290)
gles$age <- 2017 - gles$kpx_2290
table(gles$age)

gles$age_sq <- gles$age^2

gles %<>% mutate(age2 = case_when(age < 30 ~ "18 - 29",
                                  age >= 30 & age < 40 ~ "30 - 39",
                                  age >= 40 & age < 50 ~ "40 - 49",
                                  age >= 50 & age < 60 ~ "50 - 59",
                                  age >= 60 & age < 70 ~ "60 - 69",
                                  age >= 70 ~ "70+"))

# Gender ----
attributes(gles$kpx_2280)
table(gles$kpx_2280)
gles$gender <- if_else(gles$kpx_2280 == 2, "W", "M") %>% factor()

gles$gender2 <- gles$kpx_2280 - 1 

# Income satisfaction ----
attributes(gles$kp1_780)
table(gles$kp1_780)

gles$income1 <- if_else(gles$kp1_780 < 0,
                        NA_real_, as.numeric(-(gles$kp1_780 - 5)))


# Education ----
attributes(gles$kp1_2320)
table(gles$kp1_2320)

gles$edu <- case_when(gles$kp1_2320 %in% c(1, 2) ~ 0,
                      gles$kp1_2320 == 3 ~ 1,
                      gles$kp1_2320 %in% c(4, 5) ~ 2,
                      FALSE ~ NA_real_)


# City ----
attributes(gles$kp1_2600)
table(gles$kp1_2600)

gles %<>% mutate(wohnort = case_when(kp1_2600 == 1 ~ 0,
                                     kp1_2600 %in% c(2:3) ~ 1,
                                     kp1_2600 %in% c(4:5) ~ 2,
                                     FALSE ~ NA_real_),
                 city = if_else(wohnort == 0, 1, 0),
                 medcity = if_else(wohnort == 1, 1, 0),
                 town = if_else(wohnort == 2, 1, 0))

table(gles$kp1_2600, gles$wohnort, useNA = "always")

# iv. Political Interest ----
attributes(gles$kp1_010)
table(gles$kp1_010)

# Recoding:
gles %<>% mutate(polint1 = if_else(kp1_010 > 0,
                                   -(kp1_010 - 5), NA_real_),
                 polint12 = polint1^2,
                 polint7 = if_else(kp7_010 > 0,
                                   -(kp7_010 - 5), NA_real_))

table(gles$polint1)
proptable(gles$polint1)
summary(gles$polint1)

table(gles$polint7)
proptable(gles$polint7)
summary(gles$polint7)

cor.test(gles$polint1, gles$polint7)
table(gles$polint1, gles$polint7)

# v. Links-rechts ----
attributes(gles$kp7_1500)
table(gles$kp7_1500)

gles$lr <- if_else(gles$kp7_1500 > 0, gles$kp7_1500 - 1, NA_real_)


# vi. Migrationsgeschichte ----
attributes(gles$kpx_2571a)
attributes(gles$kpx_2571b)

table(gles$kpx_2571a, gles$kpx_2571b, useNA = "always")

gles %<>% mutate(mum = if_else(kpx_2571a > 0,
                               kpx_2571a - 1, 
                               NA_real_),
                 dad = if_else(kpx_2571b > 0, 
                               kpx_2571b - 1, 
                               NA_real_),
                 eltern = dad+mum)
table(gles$eltern)

# vii. Herkunftsland der Eltern ----

# Mutter
attributes(gles$kpx_2572a)
table(gles$kpx_2572a)

# Vater
attributes(gles$kpx_2572b)
table(gles$kpx_2572b)

gles %<>% mutate(land_mum = case_when(kpx_2572a == -97 ~ 0,
                                      kpx_2572a == 1 ~ 1,
                                      kpx_2572a == 2 ~ 2,
                                      kpx_2572a %in% c(3, 6, 8, 10,
                                                       11, 12, 13, 14) ~ 3,
                                      kpx_2572a %in% c(4, 5, 9) ~ 4,
                                      kpx_2572a == 7 ~ 5,
                                      kpx_2572a == 15 ~ 6,
                                      kpx_2572b == 16 ~ 7,
                                      FALSE ~ NA_real_), 
                 land_dad = case_when(kpx_2572b == -97 ~ 0,
                                      kpx_2572b == 1 ~ 1,
                                      kpx_2572b == 2 ~ 2,
                                      kpx_2572b %in% c(3, 6, 8, 10,
                                                       11, 12, 13, 14) ~ 3,
                                      kpx_2572b %in% c(4, 5, 9) ~ 4,
                                      kpx_2572b == 7 ~ 5,
                                      kpx_2572b == 15 ~ 6,
                                      kpx_2572b == 16 ~ 7,
                                      FALSE ~ NA_real_))

table(gles$land_mum, gles$land_dad)

gles %<>% mutate(fDE = case_when(land_dad == 1 & land_mum == 1 ~ 2,
                                  land_dad == 1 | land_mum == 1 ~ 1,
                                  land_dad == 0 & land_mum == 0 ~ 0,
                                  FALSE ~ NA_real_),
                 TUE = case_when(land_dad == 2 & land_mum == 2 ~ 2,
                                land_dad == 2 | land_mum == 2 ~ 1,
                                land_dad == 0 & land_mum == 0 ~ 0,
                                FALSE ~ NA_real_),
                 EUR = case_when(land_dad == 3 & land_mum == 3 ~ 2,
                                land_dad == 3 | land_mum == 3 ~ 1,
                                land_dad == 0 & land_mum == 0 ~ 0,
                                FALSE ~ NA_real_),
                 OST = case_when(land_dad == 4 & land_mum == 4 ~ 2,
                                 land_dad == 4 | land_mum == 4 ~ 1,
                                 land_dad == 0 & land_mum == 0 ~ 0,
                                 FALSE ~ NA_real_),
                 BAL = case_when(land_dad == 5 & land_mum == 5 ~ 2,
                                 land_dad == 5 | land_mum == 5 ~ 1,
                                 land_dad == 0 & land_mum == 0 ~ 0,
                                 FALSE ~ NA_real_),
                 USA = case_when(land_dad == 6 & land_mum == 6 ~ 2,
                                 land_dad == 6 | land_mum == 6 ~ 1,
                                 land_dad == 0 & land_mum == 0 ~ 0,
                                 FALSE ~ NA_real_),
                 OTH = case_when(land_dad == 7 & land_mum == 7 ~ 2,
                                 land_dad == 7 | land_mum == 7 ~ 1,
                                 land_dad == 0 & land_mum == 0 ~ 0,
                                 FALSE ~ NA_real_))

table(gles$fDE)

eltern_fde <- gles %>%
  select(afdvote1, fDE) %>%
  na.omit() %>%
  group_by(fDE, afdvote1) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n),
         freq_r = paste0(round(freq, 3) * 100, "%")) %>% 
  ungroup()

eltern_fde$label <- paste0("Frühere Ostgebiete\nVerhältnis = ",
                           sum(eltern_fde$n[eltern_fde$fDE == 0]), "/",
                           sum(eltern_fde$n[eltern_fde$fDE > 0]))

eltern_tue <- gles %>% 
  select(afdvote1, TUE) %>% 
  na.omit() %>% 
  group_by(TUE, afdvote1) %>% 
  summarize(n = n()) %>%
  mutate(freq = n/sum(n),
         freq_r = paste0(round(freq, 3)*100, "%")) %>% 
  ungroup()

eltern_tue$label <- paste0("Türkei\nVerhältnis = ",
                           sum(eltern_tue$n[eltern_tue$TUE == 0]), "/",
                           sum(eltern_tue$n[eltern_tue$TUE > 0]))


eltern_eur <- gles %>% 
  select(afdvote1, EUR) %>% 
  na.omit() %>% 
  group_by(EUR, afdvote1) %>% 
  summarize(n = n()) %>%
  mutate(freq = n/sum(n),
         freq_r = paste0(round(freq, 3)*100, "%")) %>% 
  ungroup()

eltern_eur$label <- paste0("Europa\nVerhältnis = ",
                           sum(eltern_eur$n[eltern_eur$EUR == 0]), "/",
                           sum(eltern_eur$n[eltern_eur$EUR > 0]))


eltern_ost <- gles %>% 
  select(afdvote1, OST) %>% 
  na.omit() %>% 
  group_by(OST, afdvote1) %>% 
  summarize(n = n()) %>%
  mutate(freq = n/sum(n),
         freq_r = paste0(round(freq, 3)*100, "%")) %>% 
  ungroup()

eltern_ost$label <- paste0("Osteuropa/Russland\nVerhältnis = ",
                           sum(eltern_ost$n[eltern_ost$OST == 0]), "/",
                           sum(eltern_ost$n[eltern_ost$OST > 0]))

eltern_bal <- gles %>% 
  select(afdvote1, BAL) %>% 
  na.omit() %>% 
  group_by(BAL, afdvote1) %>% 
  summarize(n = n()) %>%
  mutate(freq = n/sum(n),
         freq_r = paste0(round(freq, 3)*100, "%")) %>% 
  ungroup()

eltern_bal$label <- paste0("Balkanländer\nVerhältnis = ",
                           sum(eltern_bal$n[eltern_bal$BAL == 0]), "/",
                           sum(eltern_bal$n[eltern_bal$BAL > 0]))

eltern_usa <- gles %>% 
  select(afdvote1, USA) %>% 
  na.omit() %>% 
  group_by(USA, afdvote1) %>% 
  summarize(n = n()) %>%
  mutate(freq = n/sum(n),
         freq_r = paste0(round(freq, 3)*100, "%")) %>% 
  ungroup()

eltern_usa$label <- paste0("USA\nVerhältnis = ",
                           sum(eltern_usa$n[eltern_usa$USA == 0]), "/",
                           sum(eltern_usa$n[eltern_usa$USA > 0]))

eltern_oth <- gles %>% 
  select(afdvote1, OTH) %>% 
  na.omit() %>% 
  group_by(OTH, afdvote1) %>% 
  summarize(n = n()) %>%
  mutate(freq = n/sum(n),
         freq_r = paste0(round(freq, 3)*100, "%")) %>% 
  ungroup()

eltern_oth$label <- paste0("Andere Länder\nVerhältnis = ",
                           sum(eltern_oth$n[eltern_oth$OTH == 0]), "/",
                           sum(eltern_oth$n[eltern_oth$OTH > 0]))

colnames(eltern_fde)[1] <- "neltern"
colnames(eltern_tue)[1] <- "neltern"
colnames(eltern_eur)[1] <- "neltern"
colnames(eltern_ost)[1] <- "neltern"
colnames(eltern_bal)[1] <- "neltern"
colnames(eltern_oth)[1] <- "neltern"

eltern_plotdat <- bind_rows(eltern_fde,
                            eltern_tue,
                            eltern_eur,
                            eltern_ost,
                            eltern_bal,
                            eltern_oth)

# unique(eltern_plotdat$label_f)

eltern_plotdat$label_f <- factor(eltern_plotdat$label,
                                 levels = c("Frühere Ostgebiete\nVerhältnis = 5174/520",
                                            "Europa\nVerhältnis = 5174/129",
                                            "Osteuropa/Russland\nVerhältnis = 5174/259",
                                            "Balkanländer\nVerhältnis = 5174/38",
                                            "Türkei\nVerhältnis = 5174/33",
                                            "Andere Länder\nVerhältnis = 5174/101"))

# viii. Religion ----
attributes(gles$kp1_2480)
table(gles$kp1_2480)

attributes(gles$kpa1_2480)
table(gles$kpa1_2480)

gles %<>% mutate(relig = case_when(kp1_2480 %in% c(1:4) | 
                                     kpa1_2480 %in% c(1:4) ~ "Christlich",
                                   kp1_2480 == 5 | 
                                     kpa1_2480 == 5 ~ "Andere (nicht christlich)",
                                   kp1_2480 == 6 | 
                                     kpa1_2480 == 6 ~ "Muslimisch",
                                   kp1_2480 == 9 | 
                                     kpa1_2480 == 9 ~ "Keine Zugehörigkeit",
                                   FALSE ~ NA_character_),
                 islam = if_else(relig == "Muslimisch", 1, 0))
table(gles$islam)