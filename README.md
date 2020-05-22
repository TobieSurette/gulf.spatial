# gulf.spatial
Spatial data manipulation and mapping for the sGSL

# Draw base maps:
map() 
map(land = FALSE)
map(empty = TRUE)
map(border = "grid"). 

map(sea = TRUE)

# Draw survey strata:
map(survey = "rv", layer = "strata")
map(survey = "rv", layer = "strata", stratum = 424:432)
map(survey = "ns", layer = "strata", stratum = 1:5)

# Draw fishing zones:
map(layer = "fishing zones", species = 2526)
map(layer = "fishing zones", species = "snow crab")
map(layer = "fishing zones", species = "american lobster")

## Draw specific zones:
map(layer = "fishing zones", species = "snow crab", zone = "12")
map(layer = "fishing zones", species = "american lobster", lfa = "24")

