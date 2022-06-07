###### Planeflight.dat Handler Code
# This program creates a planeflight.dat.YYYYMMDD for use in GEOS-Chem planeflight input files
#    Copyright (C) 2022  Jared F. Brewer
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   any later version.

#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.

#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Created by Jared F. Brewer for GEOS-Chem
# Last updated 05/05/22 for GEOS-Chem 13.4.0
# Direct questions to jfbrewer@alumni.stanford.edu

##### Source important libraries #####
library(tidyverse)
library(gdata)
library(data.table)

###### Version Control info - change as you see fit ########
GCversion = "GC12.9.3"
SOA_Scheme = "Tropchem"
iteration = 1

# Build the appropriate directories if you haven't already
GCversion.dir = paste0("~/",GCversion, "/")
scheme.dir = paste0(GCversion.dir, SOA_Scheme)

if(!dir.exists(GCversion)){dir.create(GCversion)}

if(!dir.exists(scheme.dir)){dir.create(scheme.dir)}

save.dir = paste0(scheme.dir, "/PlaneflightInput_v", iteration)
if(!dir.exists(save.dir)){dir.create(save.dir)} 

header.dir = paste0(scheme.dir, "/", GCversion, "_Headers_", SOA_Scheme)
if(!dir.exists(header.dir)){
  print("Hey, you don't have your headers ready for this run. You probably need to copy them in!")
  dir.create(header.dir)
  }

###### Build library of Observation points ######
# Essential columns needed are UTC - LATITUDE - LONGITUDE - PRESSURE
load("Saved_KORUS_R6.RData")

# Filter out points outside of the nested grid
# You could remove this if you didn't care
nested_grid_boundary <- tibble(maxLat = 55, minLat = 15, maxLon = 140, minLon = 70)

Planeflight.locs.df <- KORUS %>%
  filter(LATITUDE <= nested_grid_boundary$maxLat, #filter out points outside of the nested grid
         LATITUDE >= nested_grid_boundary$minLat,
         LONGITUDE <= nested_grid_boundary$maxLon,
         LONGITUDE >= nested_grid_boundary$minLon) %>%
  dplyr::select(UTC, LATITUDE, LONGITUDE, PRESSURE) %>% # select relevant columns
  mutate('DD-MM-YYYY' = format(UTC, format = '%d-%m-%Y'), # Format date
         'HH:MM' = format(UTC, format = '%H:%M'), # Format time
         Type = "DC8", # Set observation type
         LAT = round(LATITUDE, digits = 2), # Round LAT term for GC compatibility
         # Correct Obs to 0-360 Coords for GC compatibility, round
         LON = case_when(LONGITUDE <  0 ~ round(LONGITUDE + 360, digits = 2),
                         LONGITUDE >= 0 ~ round(LONGITUDE, digits = 2)),
         PRESS = round(PRESSURE, digits = 2)) %>% # Round pressure term for GC compatibility
  select(Type, 'DD-MM-YYYY', 'HH:MM', LAT, LON, PRESS, UTC) %>% # Select relevant columns
  filter(complete.cases(.)) # Remove any NAs

# This section loads in the KORUS ground sites in case you are interested in those
# Note that here I use TYPE to distinguish between surface sites.
# All that matters here is that TYPE cannot fit the accepted GC formats
# See http://wiki.seas.harvard.edu/geos-chem/index.php/Planeflight_diagnostic#Planeflight_type for details
load("KORUS_GroundSites_V1.RData")

Supersite.locs.df <- GroundSites %>%
  filter(LATITUDE <= nested_grid_boundary$maxLat, #filter out points outside of the nested grid
         LATITUDE >= nested_grid_boundary$minLat,
         LONGITUDE <= nested_grid_boundary$maxLon,
         LONGITUDE >= nested_grid_boundary$minLon) %>%
  mutate(PRESSURE = 1000) %>% # Arbitrary Pressure value, not important but needs an assignment
  dplyr::select(UTC, LATITUDE, LONGITUDE, Type, PRESSURE, Site) %>% # select relevant columns
  mutate('DD-MM-YYYY' = format(UTC, format = '%d-%m-%Y'), # Format date
         'HH:MM' = format(UTC, format = '%H:%M'), # Format time
         LAT = round(LATITUDE, digits = 2), # Round pressure term for GC compatibility
         # Correct Obs to 0-360 Coords for GC compatibility, round
         LON = case_when(LONGITUDE <  0 ~ round(LONGITUDE + 360, digits = 2),
                         LONGITUDE >= 0 ~ round(LONGITUDE, digits = 2)),
         PRESS = round(PRESSURE, digits = 2)) %>% # Round pressure term for GC compatibility
  select(Type, 'DD-MM-YYYY', 'HH:MM', LAT, LON, PRESS, UTC) %>% # Select relevant columns
  arrange(UTC) %>% # Arrange in chronological order, important for GC
  filter(complete.cases(.)) # Remove NAs

locs.df <- bind_rows(Planeflight.locs.df, Supersite.locs.df) %>% # Combine Planeflight, Surface sites
  arrange(UTC) # Reorder in chronological order

##### Formatting and Copying the Fortran File ########
# Format is very important from here on out, I don't recommend messing with it

fmt = c(5,6,10,6,6, 7, 7) # Fortran fixed-width file details

header1.fname <- paste0(header.dir, "/KORUS_Header_1.txt") # Header 1
header2.fname <- paste0(header.dir, "/KORUS_Header_2.txt") # Header 2
header2copy.fname <- paste0(save.dir, "/KORUS_Header_2_copy.txt") # Temporary Header 2 copy name

# This loop writes each day's file
for(ii in 1:length(unique(format(locs.df$UTC, format = "%Y%m%d")))){ # For each day
  # First, select only that day's observations and ungroup them
  temp <- filter(locs.df, format(UTC, format = "%Y%m%d") == unique(format(locs.df$UTC, format = "%Y%m%d"))[[ii]]) %>%
    ungroup() %>%
    # In order to make these points continuous between surface sites and aircraft observations,
    # I have to create one variable ("TEMPORARY_INDEX") that has the same value in all cases.
    # Rowidv can then index off of this common value. Otherwise, numbering conventions are not
    # consistent between surface and aircraft observations, which will make GC barf all over the place.
    mutate(TEMPORARY_INDEX = 100) %>%
    mutate(Point = (rowidv(., cols = "TEMPORARY_INDEX")-1)) %>% 
    dplyr::select(-TEMPORARY_INDEX) %>%
    as.data.frame(.)
  
  # Create this day's main file name
  header1copy.fname <- paste0(save.dir, # "ATom_", unique(temp$ATom_number),
                              "/Planeflight.dat.", unique(format(temp$UTC, format = "%Y%m%d")))
  file.copy(from = header1.fname, to = header1copy.fname) # Now, create the file
  file.copy(header2.fname, header2copy.fname) # copy header2 into the file
  select(temp, Point, Type, 'DD-MM-YYYY', 'HH:MM', LAT, LON, PRESS) %>%
    # Select only the needed columns and write them to the precise fortran format
    # Save this as output.txt, a temporary name
    write.fwf(file= paste0(save.dir,'/output.txt'), width = fmt, colnames = F, rownames = F)
  # Append this fixed width file to the header file
  file.append(header1copy.fname, paste0(save.dir,'/output.txt'))
  # Now append the trailing tail file header2 to the final file
  file.append(header1copy.fname, header2copy.fname)
}

#Clean up unneeded temporary files
file.remove(header2copy.fname)
file.remove(paste0(save.dir, "/output.txt"))
