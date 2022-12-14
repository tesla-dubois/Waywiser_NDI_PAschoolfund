library(waywiser)
library(dplyr)
library(tidyr)
library(sfdep)
library(spdep)
library(gridExtra)
library(grid)
library(ndi)
library(ggplot2)
library(sf)
library(tidycensus) # a dependency for the "ndi"" package
library(tigris) # a dependency for the "ndi"" package
options(scipen=999)


## Access Key for census data download
### Obtain one at http://api.census.gov/data/key_signup.html
# tidycensus::census_api_key("...") # INSERT YOUR OWN KEY FROM U.S. CENSUS API
path <- "C:/GitHub/Waywiser_NDI_PAschoolfund"

# Compute the NDI (Messer) values (2016-2020 5-year ACS) for Washington, D.C. census tracts
PA2020messer <- ndi::messer(state = "PA", year = 2020)

# Obtain the 2020 census tracts from the "tigris" package
tract2020PA <- tigris::tracts(state = "PA", year = 2020, cb = TRUE)

# Join the NDI (Messer) values to the census tract geometry
PA2020messer <- merge(tract2020PA, PA2020messer$ndi, by = "GEOID")

###### PULL IN SCHOOL FUNDING 
funding <- read.csv(paste0(path, "/PA_Education_Funding.csv"))|>
  mutate(sd_id = as.character(as.numeric(ï..AUN)))

funding <- read_sf(paste0(path,"/Pennsylvania School Districts Boundaries/geo_export_e3be8142-14d4-4784-bc5d-d622f81ab614.shp"))|>
  left_join(funding, by = c("aun_schdis"="sd_id"))

######  GET THE DATASETS AT SAME GEOGRAPHY 
st_crs(PA2020messer)
st_crs(funding)

# Get these into the same (most appropriate) projection
funding <- st_transform(funding, crs = st_crs(PA2020messer))

PA2020messer$centroid <- PA2020messer |>
  st_centroid()|> # save the centroid as a variable 
  st_geometry()

PA2020messer <- PA2020messer |>
  mutate(geometry = centroid) # Make the geometry field the centroid coordinates (instead of the polygon coordinates)

sf::sf_use_s2(FALSE)
NDI_by_district <- st_join(funding, PA2020messer)|> # Join the points of NDI to polygons of funding
  group_by(school_dis)|>
  summarise(av_NDI = mean(NDI, na.rm = TRUE))|># get an average NDI per school district
  st_drop_geometry()|> # drop geometry so we can use a left join to add back to funding 
  drop_na()

funding <- left_join(funding, NDI_by_district, by = "school_dis")

#### CHECK GLOBAL MORAN'S I FOR FUNDING
nb <- poly2nb(funding, queen=TRUE, sf::sf_use_s2(FALSE))
## check to make sure it worked
nb[[1]] ## gives the neighbors of first polygon
lw <- nb2listw(nb, style="W", zero.policy=TRUE) ## this line creates weights, averaged across polygon neighbors.
## moran test for funding
moran.test(funding$Percent.Change, lw, zero.policy = TRUE, na.action=na.exclude)

# Moran I statistic standard deviate = 10.02, p-value < 0.00000000000000022
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.2794961000     -0.0020408163      0.0007895338 
# Less clustered than NDI, but still clustered, and super significant


## Test NDI
moran.test(funding$av_NDI, lw, zero.policy = TRUE, na.action=na.exclude)

# Moran I statistic standard deviate = 11.171, p-value < 0.00000000000000022
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.3063063416     -0.0020161290      0.0007617928 


##################################
# LINEAR REGRESSION 
funding2 <- subset(funding, !is.na(av_NDI))|>
  select("school_dis", "Percent.Change", "av_NDI", "geometry")

sapply(funding, function(x) sum(is.na(x)))

str(funding2)
model <- lm(Percent.Change ~ av_NDI, funding2) # Here's our model
funding2$predictions <- predict(model, funding2) # predicted values
summary(model) # NDI is a significant predictor of funding change, effect is positive (that's good!) 


# Get the global Moran's I of the residuals (they are significantly clustered)
ww_global_moran(funding2, Percent.Change, predictions)


# Get ready to map the residuals 
weights <- ww_build_weights(funding2)
funding2 <- funding2 %>%
  mutate(pred = predict(lm(Percent.Change ~ av_NDI, .)),
         .estimate = ww_local_moran_i_vec(Percent.Change, pred, weights))
funding2$pred
funding2$.estimate

# Map out the model residuals 
morans_map <- funding2 %>%
  sf::st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = .estimate)
          , color = NA) + 
  theme_bw()+
  scale_fill_viridis_c() +
  ggplot2::labs(fill = "Morans's I")+#,
                # caption = "S: Pennsylvania Department of Education")+
  ggplot2::ggtitle("Spatial Autocorrelation of LM Residuals")+
  theme(
    plot.title = element_text(size=12))#,
                   # subtitle = "Pennsylvania School Districts")
morans_map
# Map out the funding change 
change_map <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = funding2, 
                   ggplot2::aes(fill = Percent.Change*100),
                   color = NA) +
  ggplot2::theme_bw() +  
  ggplot2::scale_fill_viridis_c() +
  ggplot2::labs(fill = "% Change")+#,
                # caption = "Data source: Pennsylvania Department of Education")+
  ggplot2::ggtitle("Percent Change in Total School District Funding")+
  theme(
    plot.title = element_text(size=12))#,
                   # subtitle = "Pennsylvania School Districts")
change_map
# Map out the NDI 
sd_ndi_map <- ggplot2::ggplot() + 
  ggplot2::geom_sf(data = funding2, 
                   ggplot2::aes(fill = av_NDI),
                   color = NA) +
  ggplot2::theme_bw() +  
  ggplot2::scale_fill_viridis_c() +
  ggplot2::labs(fill = "Average NDI")+#,
                # caption = "Data source: U.S. Census ACS 2016-2020 estimates, via {NDI}")+
  ggplot2::ggtitle("Average Neighborhood Deprivation Index")+
  theme(
    plot.title = element_text(size=12))#,
                   # subtitle = "Pennsylvania School Districts")
sd_ndi_map
# Put it all together 
grid.arrange(sd_ndi_map, change_map, morans_map, 
             ncol = 1, nrow = 3,
             heights = c(1, 1, 1))

             
