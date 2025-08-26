###########################################

# fluxfinder example: GC based fluxes

# author: Stephanie J. Wilson
# data provided by: Pat Megonigal

#INFO:
  # In this example metadata is already input on each observation,
  # so we can skip metadata matching and file functions from fluxfinder.
  # In this case we will read in the data, take a look at it, convert units
  # and then simply calculate fluxes.

###############################################################################

############# Step 1: Load packages ######
    #if not done already:
    #install.packages(fluxfinder)
library(fluxfinder)
library(ggplot2)
library(dplyr)
library(lubridate)
###############################################################################

############# Step 2: Read in Data ########

  #if required, set working directory to file path with data:
  #setwd(FILEPATH)

#read in data
raw_data <- read.csv("Syringe Sample CH4 Flux Example.csv")

#take a look at raw data:
head(raw_data)
  #looks like we have:
  #Site: where the samples were taken
  #Begtim: the start time (seems same for all)
  #Plots: plot ID for the flux
  #CH3F_Trt: treatment information
  #Time: Time 22.26 = 22:26
  #CH4: methane concentration from GC (assuming ppm units)
  #Flag: whether or not we are including this data


#We need to make a unique Plot identifier
#incorporate Site, Begtim, and Plots
raw_data <- raw_data %>%
  mutate(
    Plot_id = paste(Site, Begtim, Plots, sep = "_")
  )


#Deal with Pat's weird time column.....
#we need to add a MM/DD/YYYY column
raw_data$Date <- "08/25/2025"

#convert time from 22.26 to 22:26 and
#combine with the Date colume and add seconds
raw_data <- raw_data %>%
  group_by(Site, Begtim, Plots) %>%
  # Identify base time per group
  mutate(
    base_time_val = min(Time),                       # base HH.MM as number
    base_hour = floor(base_time_val),
    base_min = round((base_time_val - base_hour) * 100),
    base_time_str = sprintf("%02d:%02d:00", base_hour, base_min),
    base_datetime = mdy_hms(paste(Date, base_time_str))  # full POSIXct base datetime
  ) %>%
  # Calculate offset in hours and add to base time
  mutate(
    offset_hours = Time - base_time_val,
    final_datetime = base_datetime + dhours(offset_hours)
  ) %>%
  ungroup()

#remove Flagged data points:
noflag_data <- raw_data %>%
  filter(Flag != "n")     #removing rows with "n" in flag column

#check that it worked with a quick look
head(noflag_data)

###############################################################################

############# Step 3: Graph concentrations over time #####

#change the Plots column to a factor variable rather than numeric
noflag_data$Plot_id <- as.factor(noflag_data$Plot_id)

#plot the methane overtime by plot and site
ggplot(noflag_data, aes(final_datetime, CH4, shape=Site, color=Plot_id)) +
  geom_point() + theme_classic()  +
  guides(color = guide_legend(ncol = 4))
###############################################################################

############# Step 4: Convert gas units ##########

#temperature and volume data were not provided,
  #so we will assume temp = 25 C and volume = 0.1 m3
  #using the fluxfinder conversion function
noflag_data$CH4_umol <- ffi_ppm_to_umol(noflag_data$CH4,
                                        volume = 0.1, #m3
                                        temp = 25) #degrees C

#area of the flux was also not provided,
  #we will assume an area of 0.15m2
noflag_data$CH4_umol_m2 <- noflag_data$CH4_umol / 0.16

###############################################################################

############# Step 5:  Flux calculations ########

#use the fluxfinder function, define the groups, time, and gas column
  #we will assume a 0 deadband here
fluxes <- ffi_compute_fluxes(noflag_data,
                     group_column = "Plot_id",
                     time_column = "final_datetime",
                     gas_column = "CH4_umol_m2",
                     dead_band = 0)

head(fluxes)


#plot fluxes with r2 as color:
 ggplot(fluxes, aes(Plot_id, lin_flux.estimate, color = lin_r.squared)) +
   geom_point(size = 4) + theme_classic() +
   geom_linerange(aes(ymin = lin_flux.estimate- lin_flux.std.error,
                    ymax = lin_flux.estimate+ lin_flux.std.error)) +
   ylab("CH4 flux (μmol/m2/s)")

###############################################################################

############# Step 6:  Write output file ########

#let's get just data from the original file we need
raw_summary <- noflag_data %>%
   group_by(Plot_id) %>%
   slice(1) %>%                      # get the first row per group
   ungroup() %>%
   select(Plot_id, Site, Begtim, Plots, CH3F_Trt, Time, Date)  # specify only the columns you want

#bring any metadata back in from the original file
fluxes_final <- fluxes %>%
   left_join(raw_summary, by = "Plot_id")


#write out a csv with the flux estimates
  #write.csv(fluxes_final, "GC_Example_Fluxes_Data.csv")

###############################################################################




