library(gulf.spatial)

# — 1. Your raw vectors (with NAs for missing coords)
site <- c(
   "Arisaig",   "Auld Cove",  "Ballantyne", "Cheticamp 1", "Cheticamp 2",
   "Margaree",  "Pictou",     "Pleasant Bay","Port Hood 1","Port Hood 2",
   "Pugwash 1", "Pugwash 2",  "Pugwash 3",  "River John",  "Wallace"
)

lat  <- c(
   NA,         45.649000,    NA,         46.649583,   46.645972,
   NA,         45.826722,    46.841694,   46.031028,   45.999500,
   45.901750,  45.881722,    45.880472,   NA,          45.815750
)

long <- c(
   NA,        -61.434278,   NA,        -61.007861,  -61.008750,
   NA,        -62.496861,   -60.804889, -61.565889,  -61.542389,
   -63.688556,-63.694444,   -63.687361,  NA,         -63.349722
)

# — 2. Identify & keep only the “complete” rows
valid     <- !is.na(lat) & !is.na(long)
site      <- site[valid]
lat       <- lat[valid]
long      <- long[valid]

# — 3. Now call depth() on the cleaned coords
depths    <- depth(long, lat)

# — 4. Assemble final data.frame
df_clean  <- data.frame(
   Site  = site,
   Lat   = lat,
   Long  = long,
   Depth = depths
)

print(df_clean)
