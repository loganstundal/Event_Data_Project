rm(list=ls())
library(tidyverse)
library(sf)


# load('data/colombia_admin.rdata')

st_crs(colombia)
st_crs(gnames)


gnames <- st_transform(gnames, crs = st_crs(colombia))

# # https://r-spatial.github.io/sf/reference/geos_unary.html
# mun_cents <- st_centroid(colombia)






# Municipality distance from department capital:
# STEP 1 - ESTIMATE MUNICIPALITY CENTROID DISTANCE TO EVERY DEPT CAPITAL (1116 * 31)
z <- st_nearest_points(x = mun_cents, y = gnames)

# STEP 2 - CONVERT THE RESULTING LINESTRING OBJECTS TO LENGHTS (UNITS = METERS)
z <- as.numeric(st_length(z))

# STEP 3 - CONVERT TO A SENSIBLE DF WITH DEPTs and MuNICIPALITY GROUPS, SUMMARIZE
# tHE DISTANCE WHEN GROUPED BY MUN.
tmp <- data.frame('Distance' = z, 
                  'Deptartment' = rep(seq(1,31,1),1116),
                  'Municipality' = unlist(lapply(1:1116, function(x) rep(x, 31)))) %>%
  # mutate(Municipality = as_factor(Municipality)) %>%
  group_by(Municipality) %>%
  summarize(Distance = min(Distance))

# ggplot() + 
#   geom_sf(data = colombia) + 
#   geom_sf(data = colombia[255,], color = 'red') + 
#   geom_sf(data = mun_cents, color = 'blue') + 
#   geom_sf(data = gnames, color = 'orange', size = 1.5)





# Identify all municipalities with an international border (ref salehayn's work about cross border activity)
# ecuador   <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_ECU_0_sf.rds')) %>%
#   st_transform(., crs = st_crs(colombia))
# peru      <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_PER_0_sf.rds')) %>%
#   st_transform(., crs = st_crs(colombia))
# brazil    <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_BRA_0_sf.rds')) %>%
#   st_transform(., crs = st_crs(colombia))
# venezuela <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_VEN_0_sf.rds')) %>%
#   st_transform(., crs = st_crs(colombia))
# panama    <- read_rds(url('https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_PAN_0_sf.rds')) %>%
#   st_transform(., crs = st_crs(colombia))

# sa <- rbind(ecuador, peru, brazil, venezuela, panama)

# ggplot() + 
#   geom_sf(data = sa) + 
#   geom_sf(data = colombia, fill = 'red')


# library(rmapshaper)
# colombia <- raster::shapefile('data/col_admbnda_adm2_unodc_ocha/col_admbnda_adm2_unodc_ocha.shp')
# col <- ms_simplify(colombia, keep = 0.01)
# col <- col %>% st_as_sf() %>% st_transform(., crs = st_crs(peru))

# ggplot(data = col) + geom_sf()

### 
# bound <- st_is_within_distance(x = col, y = ecuador, dist = 1e3, sparse = FALSE)
# col$ecuador <- bound

# ggplot(data = col) + 
#   geom_sf(aes(fill = ecuador)) + 
#   geom_sf(data = ecuador)



