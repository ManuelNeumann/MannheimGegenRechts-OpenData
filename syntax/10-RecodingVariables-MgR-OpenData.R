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
