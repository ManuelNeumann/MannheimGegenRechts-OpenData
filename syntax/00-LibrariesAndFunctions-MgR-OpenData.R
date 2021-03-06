# --------------------------------------------------------------------------- #

# 00 - Libraries and functions - MgR - Open Data

# --------------------------------------------------------------------------- #

# Libraries ----

citation("base")

# The pacman function allows one to load packages and install them if needed
# with one call:
library(pacman)

p_load(magrittr)
citation("magrittr")

p_load(MASS)

p_load(tidyverse)
citation("tidyverse")

# For plots
p_load("gganimate")
citation("gganimate")
p_load("gifski")
citation("gifski")
p_load("png")
citation("png")
p_load("cowplot")
citation("cowplot")
p_load("scales")
citation("scales")
p_load("ggmap")
citation()

p_load(haven)
citation("haven")
p_load(knitr)
citation("knitr")
p_load(stargazer)
citation("stargazer")
p_load(here)
citation("here")

# Functions ----
source(file = "functions/basicfunctions.R")