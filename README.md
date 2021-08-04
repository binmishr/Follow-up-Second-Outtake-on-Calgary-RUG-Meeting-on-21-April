# Follow-up-Second-Outtake-on-Calgary-RUG-Meeting-on-21-April

For the matter at hand, here’s a second “out take” from the talk, where I explain how to create a map which labels cities across the world with dots corresponding in size to their annual solar radiation. Have fun!

#To get the longitude, latitude for the cities in question, you will need to install nominatim for Open Street Maps
#devtools::install_github("hrbrmstr/nominatim")
# You will ned an OSM API key which you can get for free from Map Quest 
# Then you just populate the data frame like so 

calgary.rug.cities = c("Calgary", "Vancouver", "Edmonton", "Yellowknife", "Winnipeg", "Utrecht", "Buenos Aires", "Ramallah", "Doha", "Berlin")
calgary.rug.countries = c("CA", "CA", "CA", "CA", "CA", "NL", "AR", "PS", "QA", "DE")
#You need to know the ISO country codes for the countries in question, I suppose 


calgary.rug.solar.df = data.frame(matrix(nrow = length(calgary.rug.cities), ncol = 2))
for(i in 1:length(calgary.rug.cities))
{
  place_holder = nominatim::osm_geocode(query = calgary.rug.cities[i], country_codes = calgary.rug.countries[i], key = myosmapikey)
  calgary.rug.solar.df[i,1] = place_holder$lon
  calgary.rug.solar.df[i,2] = place_holder$lat
  print("One more city")
}
#We will need 13 columns of insolation data, one for each month and one for the annual values
calgary.rug.solar.monthly = data.frame(matrix(nrow = nrow(calgary.rug.solar.df), ncol = 13))
for(i in 1:nrow(calgary.rug.solar.monthly))
{
  request_solar_ghi = nasapower::get_power(community = "SSE", pars = "ALLSKY_SFC_SW_DWN", temporal_average = "CLIMATOLOGY", lonlat = c(calgary.rug.solar.df[i,1], calgary.rug.solar.df[i,2]))
  #To see how this next line works, you really have to know what the structure of the returned response from NASAPOWER
  calgary.rug.solar.monthly[i,] = data.frame(request_solar_ghi[which(request_solar_ghi$PARAMETER == "ALLSKY_SFC_SW_DWN"),][,4:16])
}
colnames(calgary.rug.solar.monthly) = colnames(data.frame(request_solar_ghi[,4:16]))
#Check this to see what it is
calgary.rug.solar.monthly
rownames(calgary.rug.solar.monthly) = calgary.rug.cities
calgary.rug.solar.df = cbind(calgary.rug.solar.df, calgary.rug.solar.monthly$ANN)
calgary.rug.solar.df
row.names(calgary.rug.solar.df) = calgary.rug.cities
colnames(calgary.rug.solar.df) = c("LON", "LAT", "Annual GHI")
calgary.rug.solar.df

#The annual value is given as a "average day," so we need to fix it 
calgary.rug.solar.df$`Annual GHI` = calgary.rug.solar.df$`Annual GHI`*365
calgary.rug.solar.df

calgary.rug.cities.diffuse = vector(length = length(calgary.rug.cities))
for(i in 1:length(calgary.rug.cities.diffuse))
{
  diffuse_request = nasapower::get_power(community = "SSE", pars = "DIFF", temporal_average = "CLIMATOLOGY", lonlat = c(calgary.rug.cities.for.presenting$LON[i], calgary.rug.cities.for.presenting$LAT[i]))
  annual_diffuse = data.frame(diffuse_request)$ANN
  calgary.rug.cities.diffuse[i] = annual_diffuse
}

calgary.rug.cities.dnr = vector(length = length(calgary.rug.cities))
for(i in 1:length(calgary.rug.cities))
{
  dnr_request = nasapower::get_power(community = "SSE", pars = "DNR", temporal_average = "CLIMATOLOGY", lonlat = c(calgary.rug.cities.for.presenting$LON[i], calgary.rug.cities.for.presenting$LAT[i]))
  annual_dnr = data.frame(dnr_request)$ANN
  calgary.rug.cities.dnr[i] = annual_dnr
}

calgary.rug.cities.diffuse[is.na(calgary.rug.cities.diffuse)] <- 0
calgary.cities.diff_to_dnr = calgary.rug.cities.diffuse/calgary.rug.cities.dnr

#Yelloknife is a special case it seems; just not a lot of sunlight in some parts of the year 
calgary.rug.yellowknife_diffuse1 = data.frame(nasapower::get_power(community = "SSE", pars = "DIFF", temporal_average = "CLIMATOLOGY", lonlat = c(calgary.rug.cities.for.presenting$LON[4], calgary.rug.cities.for.presenting$LAT[4])))
calgary.rug.yellowknife_diffuse = sum((calgary.rug.yellowknife_diffuse1)[,4:16], na.rm = TRUE)
calgary.rug.yellowknife_dnr1 = data.frame(nasapower::get_power(community = "SSE", pars = "DNR", temporal_average = "CLIMATOLOGY", lonlat = c(calgary.rug.cities.for.presenting$LON[4], calgary.rug.cities.for.presenting$LAT[4])))
calgary.rug.yellowknife_dnr = sum(calgary.rug.yellowknife_dnr1[,4:16], na.rm = TRUE)
calgary.rug.yellowknife_diff_to_dnr = calgary.rug.yellowknife_diffuse/calgary.rug.yellowknife_dnr
calgary.cities.diff_to_dnr[4] = calgary.rug.yellowknife_diff_to_dnr

calgary.rug.cities.for.presenting$DiffDNR = calgary.cities.diff_to_dnr


#Before going any further, please have a quick check to see that the different data frames are reasonable before trying to plot them

myPaletter = colorRampPalette(rev(brewer.pal(11, "RdBu")))
scaled_map = scale_color_gradientn(colors = myPaletter(25), limits = c(min(calgary.rug.solar.df$DiffDNR2), max(calgary.rug.solar.df$DiffDNR2)), name = "Diffuse-to-DNR \n %-age")


#The map for diffuse to DNI 
ggplot2::ggplot(data = world_map) +
  geom_sf() +
  geom_point(data = calgary.rug.solar.df[1:10,],
             mapping = aes(x = LON, y = LAT,
                           color = (DiffDNR2)),
             size = 5) +   coord_sf(xlim = c(min(calgary.rug.cities.for.presenting$LON)-10,
                                             max(calgary.rug.cities.for.presenting$LON) + 10),
                                    ylim = c(min(calgary.rug.cities.for.presenting$LAT)-10,
                                             max(calgary.rug.cities.for.presenting$LAT)+10), expand = F) +
  scaled_map +
  ggtitle("Diffuse to DNR annually \n selected cities") +theme(panel.background = element_rect(fill = "lightblue",  color = "blue", size = 0.5))
