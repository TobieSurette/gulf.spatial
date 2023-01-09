# gulf.spatial
Southern Gulf of Saint Lawrence (sGSL) spatial data access, manipulation and mapping package.

## Installation:

Packages from the `gulf` series of packages can be installed directly from GitHub by running the following code from R:

```
library(devtools)
install_github("TobieSurette/gulf.utils")
gulf.utils::install.gulf()
```

# Basic maps:
```
# Draw base maps:
map()                # sGSL Base map.         
map(land = FALSE)    # Draw map axes only.
map(border = "grid") # Draw grid border.
map(sea = TRUE)      # Draw bathymetry.
```
Different areas of the southern Gulf can be brought into focus by using either the `region` option, or by setting `xlim` and `ylim`:
```
map(region = "GSL")         # The entire Gulf of St. Lawrence.
map(region = "Cape Breton") # Western Cape Breton.
map(region = "PEI")         # Prince Edward Island.
map(xlim = c(-66, -63), ylim = (46, 48))  # Define explicit map bounds.
```

The following region options are implemented:

 `region`                                           | Description
--------------------------------------------------- | --------------------------------------------------------------------------
`"4t", "sgsl", "gulf", "southern.gulf"`             | Southern Gulf of St. Lawrence
`"4rst", "gsl"`                                     | Gulf of St. Lawrence
`"4s", "ngsl", "northern.gulf"`                     | Northern Gulf of St. Lawrence
`"cb", "cape breton", "wcb", "western.cape.breton"` | Western Cape Breton
`"ns", "northumberland.strait"`                     | Northumberland Strait
`"PEI"`                                             | Prince Edward Island
      
A powerful mapping feature is to use the `layer` option to successively add layers to a map. For example, the following commands draw subsets of science survey stata:
```
# Draw survey strata:
map(layer = "strata", survey = "rv", stratum = 424:432)
map(layer = "strata", survey = "ns", stratum = 1:5)

# Draw fishing zones:
map(layer = "fishing zones", species = 2526)
map(layer = "fishing zones", species = "snow crab")
map(layer = "fishing zones", species = "american lobster")

## Draw specific zones:
map(layer = "fishing zones", species = "snow crab", zone = "12")
map(layer = "fishing zones", species = "american lobster", lfa = "24")
```

The following is a list of planned `layer` options:                            
                                        
`layer`                                    | Description
------------------------------------------ | -----------------------------------------------------------
`"coast", "coastline"`                     | Coastline polygons.
`bounds`, `boundaries`                     | Regional or survey boundaries.
`"stations"`                               | Survey stations.
`"stratum", "strata"`                      | Science survey spatial strata.
`"fishing.zones"`                          | Fishing zone boundaries and polygons.
`"kriging" `                               | Snow crab kriging polygons.
`"altitude", "bathymetry", "depth", "dem"` | Altitude and bathymetry Digital Elevation Maps (DEM).
`"ports"`                                  | Marine port names and point locations.
`"towns", "cities"`                        | Town and city names and point locations.
`"geography", "features"`                  | Geographical features.
      
