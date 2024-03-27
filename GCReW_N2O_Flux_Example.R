
################################################ INFO #########################################################
#wWhattheflux Example with N2O data 
#link to whattheflux package: https://github.com/COMPASS-DOE/whattheflux
#Author: Stephanie J. Wilson
#Last Updated: 2024-03-20


#N2O data from GENX Experiment (Source: Genevieve Noyce & Roy Rich) 
#Data collected from a temperate salt marsh in automatic chambers
################################################################################################################

###########################################  N2o Fluxes ########################################################
#Set up
setwd("S:/Biogeochemistry/People/Wilson (Steph)/Manuscripts/Whattheflux Tech Note/GCReW N2O Flux Example")
library(whattheflux)

#Read in data file 
f <- "TG20-01182-2023-11-08T100000.data"
dat <- wtf_read_LI7820(f)
head(dat)

#plot raw data over time
library(ggplot2)
ggplot(dat, aes(TIMESTAMP, N2O)) + 
  geom_point() + ylim(325, 350) + 
  theme_classic() + 
  labs(title=" ", x="Time (seconds)") +
  ylab(expression(paste( N[2], "O (ppbv)"))) +
  geom_hline(yintercept = 334, col="tomato2",linetype="dashed",  linewidth=0.75) +   
  theme(legend.title = element_blank(), 
        axis.title.x = element_text(size=14), axis.text = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
  

#read in metadatfile
md <- "GCReW_N2O_Flux_Example.csv"
metadat <- read.csv(md)
head(metadat)

#Match metadata to raw data points
dat$metadat_row <- wtf_metadata_match(
  data_timestamps = dat$TIMESTAMP,
  start_dates = metadat$Date,
  start_times = metadat$Start_time,
  obs_lengths = metadat$Obs_length) 
head(dat)

dat$Plot <- metadat$Plot[dat$metadat_row]
metadat$metadat_row <- seq_len(nrow(metadat))

#plot data with meta-dataset matches to visualize fluxes; compare to atmospheric concentration
p <- ggplot(dat, aes(TIMESTAMP, N2O,  color = factor(metadat_row))) + 
  geom_point() + ylim(325,350) + 
  theme_classic() + 
  labs(title=" ", x="Time") +
  ylab(expression(paste( N [2], "O (ppbv)"))) +
  geom_hline(yintercept = 334, col="black", linetype="dashed",  linewidth=0.75) +   
  guides(color = guide_legend(title = "Plot")) +
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
print(p)

#change the units ####### ***** WE NEED TO CHANGE THIS FUNCTION HERE *****
dat$N2O_nmol <- wtf_ppm_to_umol(dat$N2O, 
                                volume = 0.192, # m3
                                temp = 24)    # degrees C
#> Assuming atm = 101325 Pa
#> Using R = 8.31446261815324 m3 Pa K-1 mol-1

#over a certain area
dat$N2O_nmol_m2 <- dat$N2O_nmol / 0.16

#Calculate fluxes
fluxes <- wtf_compute_fluxes(dat,
                             group_column = "Plot", 
                             time_column = "TIMESTAMP", 
                             conc_column = "N2O_nmol_m2",
                             dead_band = 10)
head(fluxes)


#make a plot of the flux estimates, error, and visualize with R2 as color
ggplot(fluxes, aes(Plot, slope_estimate, color = adj.r.squared)) + 
  geom_point(size=4) +
  geom_linerange(aes(ymin = slope_estimate - slope_std.error,
                     ymax = slope_estimate + slope_std.error)) +
  geom_hline(yintercept = 0, col="darkgray", linewidth=0.5) +
  theme_classic() +
  ylab(expression(paste( N [2], "O Flux (nmol m"^-2* " s"^-1*")"))) +
  labs(color=expression(Adj.~R^{2})) +
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1))


#make a barplot of flux estimates 
ggplot(fluxes, aes(x=Plot, y=slope_estimate, fill=Plot)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=slope_estimate-slope_std.error, ymax=slope_estimate+slope_std.error), width=0.2) +
  theme_classic() +
  xlab(" ") +
  ylab(expression(paste( N [2], "O Flux (nmol m"^-2* " s"^-1*")"))) +
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

################################################################################################################







