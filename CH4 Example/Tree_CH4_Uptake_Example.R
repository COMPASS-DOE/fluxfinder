
################################################ INFO #########################################################
#wWhattheflux Example with N2O data 
#link to whattheflux package: https://github.com/COMPASS-DOE/whattheflux
#Author: Stephanie J. Wilson
#Last Updated: 2024-03-21


#CH4 data from COMPASS Experiment (Source: Stephanie J. Wilson & Samuel Wright) 
#Data collected from tree chambers at the Sweet Hall Forested Swamp
################################################################################################################

###########################################  CH4 Fluxes ########################################################
#Set up
setwd("S:/Biogeochemistry/People/Wilson (Steph)/Manuscripts/Whattheflux Tech Note/Tree CH4 Uptake Example")
library(whattheflux)

#Read in data file 
f <- "TG10-01087-2023-06-21T060000b.data"
dat <- wtf_read_LI7810(f)
head(dat)

#load packages and visualize data
library(ggplot2)
ggplot(dat, aes(TIMESTAMP, CH4)) + geom_point()

#read in metadatfile
md <- "Tree_CH4_Uptake_Metadata.csv"
metadat <- read.csv(md)
head(metadat)

#match the metadata to the concentration data 
dat$metadat_row <- wtf_metadata_match(
  data_timestamps = dat$TIMESTAMP,
  start_dates = metadat$Date,
  start_times = metadat$Start_time,
  obs_lengths = metadat$Obs_length) 
head(dat)

dat$Plot <- metadat$Plot[dat$metadat_row]
metadat$metadat_row <- seq_len(nrow(metadat))

#Plot the data again, but now show the matched data and compare to ambient concentrations
p <- ggplot(dat, aes(TIMESTAMP, CH4,  color = factor(metadat_row))) + 
  geom_point() + ylim(1850,2050) +
  theme_classic() +
  labs(title=" ", x="Time") +
  ylab(expression(paste( CH [4], " (ppbv)"))) +
  geom_hline(yintercept = 2000, col="black", linetype="dashed",  linewidth=0.75) +   
  guides(color = guide_legend(title = "Tree")) +
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
print(p)

#change the units
dat$CH4_umol <- wtf_ppb_to_nmol(dat$CH4, 
                                volume = 0.000260542, # m3
                                temp = 21.38889)    # degrees C
#> Assuming atm = 101325 Pa
#> Using R = 8.31446261815324 m3 Pa K-1 mol-1

dat$CH4_nmol_m2 <- dat$CH4_nmol / 0.00202683

fluxes <- wtf_compute_fluxes(dat,
                               group_column = "Plot", 
                               time_column = "TIMESTAMP", 
                               gas_column = "CH4_umol_m2",
                               dead_band = 10)
head(fluxes)

#plot fluxes and compare R2 values 
ggplot(fluxes, aes(Plot, flux_estimate, color = adj.r.squared)) + geom_point() +
  geom_linerange(aes(ymin = flux_estimate - flux_std.error,
                     ymax = flux_estimate + flux_std.error)) +
  ylab("CO2 flux (nmol/m2/s)")

#plot the fluxes as a bar plot
ggplot(fluxes, aes(flux_estimate, color = Plot)) + geom_bar() +
  ylab("CH4 flux (nmol/m2/s)")

#round the adjusted R2 value so that we can place it on the plot 
fluxes$AdjR2 <- round(fluxes$adj.r.squared, 2)
head(fluxes)

#Plot resulting fluxes as a barplot with error bars and report R2 values
ggplot(fluxes, aes(x=Plot, y=flux_estimate, fill=Plot)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=flux_estimate-flux_std.error, ymax=flux_estimate+flux_std.error), width=0.2) +
  theme_classic() + 
  ylim(-5,0) +
  xlab("Tree ID") +
  ylab(expression(paste( CH [4], "Flux (nmol m"^-2* " s"^-1*")"))) +
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position="none") + 
  geom_text(size=4, data=fluxes,aes(label = paste("R^2: ", AdjR2, sep="")),
    parse=T,x=c(1,2,3),y=c(-0.5,-0.8,-3.8), show.legend=F)
############################################################################################################

