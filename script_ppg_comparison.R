rm(list = ls()) 

#Plotting libs
library(ggplot2)
library(ggthemes)
#Used to transform dataframes befor plotting
library(reshape)

#function to compute rMSSD
hrv_features_rmssd <- function(values)
{
  valuesDiff <- diff(values)
  return (sqrt(mean(valuesDiff^2, na.rm = TRUE)))
}

output_to_pdf = TRUE
#EDIT based on your machine settings
files_path_root <- paste("~/Dropbox/R workspace/github/ppg_sensors/", sep = "")
files_path_data <- paste(files_path_root, "data/", sep = "")
files_path <- paste(files_path_root, "figures/", sep = "")
setwd(files_path_root)

source(paste(files_path_root, "multiplot.R", sep = ""))

subjects <- c("001", "002")
df_rr <- data.frame()
for(index_subject in 1:length(subjects))
{
  curr_subject <- subjects[index_subject]
  
  #Load reference (Polar H7 data)
  rr_h7 = read.csv(paste(files_path_data, curr_subject, "/rr_h7.csv", sep = ""), header=TRUE)
  names(rr_h7) <- c("date", "rr", "since_start", "window", "lap")
  rr_h7 <- rr_h7[, c(1:3)] 
  rr_h7$since_start <- rr_h7$since_start / 1.024 / 1000 #convert to seconds
  rr_h7[, "sensor"] <- "0_Polar H7"
  
  #Load HRV4Training data (collected with the same app)
  rr_hrv4training = read.csv(paste(files_path_data, curr_subject, "/hrv4training/rr.csv", sep = ""), header=TRUE)
  names(rr_hrv4training) <- c("date", "rr", "since_start", "window", "lap")
  rr_hrv4training <- rr_hrv4training[, c(1:3)] #drop extra columns, not present for HRV Logger data collected for other sensors
  rr_hrv4training$since_start <- rr_hrv4training$since_start / 1000 #convert to seconds
  rr_hrv4training[, "sensor"] <- "1_Camera (HRV4Training)"
  
  #Load other sensors
  rr_mio = read.csv(paste(files_path_data, curr_subject, "/mio/rr.csv", sep = ""), header=TRUE)
  names(rr_mio) <- c("date", "rr", "since_start")
  rr_mio$since_start <- rr_mio$since_start / 1000 #convert to seconds
  rr_mio[, "sensor"] <- "3_Mio alpha"
  
  rr_schosche = read.csv(paste(files_path_data, curr_subject, "/schosche/rr.csv", sep = ""), header=TRUE)
  names(rr_schosche) <- c("date", "rr", "since_start")
  rr_schosche$since_start <- rr_schosche$since_start / 1000 #convert to seconds
  rr_schosche[, "sensor"] <- "4_Schosche Rhythm+"
  
  rr_kyto = read.csv(paste(files_path_data, curr_subject, "/kyto/rr.csv", sep = ""), header=TRUE)
  names(rr_kyto) <- c("date", "rr", "since_start")
  rr_kyto$since_start <- rr_kyto$since_start / 1000 #convert to seconds
  rr_kyto[, "sensor"] <- "2_Kyto HRM-2931"
  
  #Create data frame to plot using ggplot
  df_rr_curr_subj <- rbind(rr_h7, rr_hrv4training, rr_mio, rr_schosche, rr_kyto)
  df_rr_curr_subj[, "subject_ID"] <- curr_subject
  df_rr <- rbind(df_rr, df_rr_curr_subj)
}

#Segment windows for HRV computation and plotting (1 minute), force max to 6 for plotting reasons (6 mintues per row)
max_window <- 6 #round(max(df_rr$since_start))
df_rr[, "window_min"] <- NA
for(index_window_min in 1:max_window)
{
  df_rr[df_rr$since_start >= (60*(index_window_min-1)) & 
          df_rr$since_start < (60*index_window_min), "window_min"] <- index_window_min
}
#remove excluded windows (plotting reasons)
df_rr <- df_rr[!is.na(df_rr$window_min), ]

#Compute features over segmented windows
df_features <- data.frame()
for(index_subject in 1:length(subjects))
{
  curr_subject <- subjects[index_subject]
  curr_subject_data <- df_rr[df_rr$subject_ID == curr_subject, ]
  
  for(index_window_min in 1:max_window)
  {
    #Reference feature
    curr_window_h7 <- curr_subject_data[!is.na(curr_subject_data$window_min) & 
                                             curr_subject_data$window_min == index_window_min &
                                             curr_subject_data$sensor == "0_Polar H7", "rr"]
    rMSSD_h7 <- round(hrv_features_rmssd(curr_window_h7), 1)
    
    #HRV4Training
    curr_window_hrv4t <- curr_subject_data[!is.na(curr_subject_data$window_min) & 
                                             curr_subject_data$window_min == index_window_min &
                                             curr_subject_data$sensor == "1_Camera (HRV4Training)", "rr"]
    rMSSD_hrv4training <- round(hrv_features_rmssd(curr_window_hrv4t), 1)
    
    #Other sensors
    curr_window_mio <- curr_subject_data[!is.na(curr_subject_data$window_min) & 
                                             curr_subject_data$window_min == index_window_min &
                                             curr_subject_data$sensor == "3_Mio alpha", "rr"]
    rMSSD_mio <- round(hrv_features_rmssd(curr_window_mio), 1)
    
    curr_window_schosche <- curr_subject_data[!is.na(curr_subject_data$window_min) & 
                                           curr_subject_data$window_min == index_window_min &
                                           curr_subject_data$sensor == "4_Schosche Rhythm+", "rr"]
    rMSSD_schosche <- round(hrv_features_rmssd(curr_window_schosche), 1)
    
    curr_window_kyto <- curr_subject_data[!is.na(curr_subject_data$window_min) & 
                                                curr_subject_data$window_min == index_window_min &
                                                curr_subject_data$sensor == "2_Kyto HRM-2931", "rr"]
    rMSSD_kyto <- round(hrv_features_rmssd(curr_window_kyto), 1)
    
    curr_features <- data.frame(rMSSD_h7, rMSSD_hrv4training, rMSSD_mio, rMSSD_schosche, rMSSD_kyto)
    names(curr_features) <- c("Polar_h7", "HRV4Training", "Mio", "Schosche", "Kyto")
    curr_features[, "window"] <- index_window_min 
    curr_features[, "subject_ID"] <- curr_subject 
    df_features <- rbind(df_features, curr_features)
  }
}
df_features


#Plot data, RR intervals first (synch is not perfect but signals overlap decently, won't be shifting or aligning them any further)
hrv4t_color_blue <- rgb(0/256, 136/256, 202/256)
for(index_subject in 1:length(subjects))
{
  curr_subject <- subjects[index_subject]
  curr_subject_data <- df_rr[df_rr$subject_ID == curr_subject, ]
  
  #rr intervals 
  p1 <- ggplot(curr_subject_data, aes(since_start, rr, col = sensor)) +
    geom_line() +
    facet_wrap(sensor~window_min, scale = "free_x") +
    theme_fivethirtyeight() +
    scale_fill_fivethirtyeight() +
    ggtitle(paste("Comparison of PPG devices (RR intervals) - Subject", curr_subject)) +
    theme(legend.position="none") 
  
  if(output_to_pdf)
  {
    pdf(paste(files_path,"fig_rr_", curr_subject, ".pdf", sep=""), width=20, height=18)
  }
  multiplot(p1)
  if(output_to_pdf)
  {
    dev.off()
  }
}

#Plot features (rMSSD) for all sensors and subjects (one boxplot per person)
#in this plot we loose detailed minute by minute information, only useful to spot sensors that are way off
df_rmssd <- melt(df_features[, c("Polar_h7", "HRV4Training", "Kyto", "Mio", "Schosche", "window", "subject_ID")], id = c("window", "subject_ID"))
names(df_rmssd)[3:4] <- c("Sensor", "rMSSD")
p1 <- ggplot(df_rmssd, aes(Sensor, rMSSD, fill = Sensor)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  facet_wrap(~subject_ID) +
  ggtitle("Comparison of PPG devices (rMSSD in ms)") +
  xlab("Time window") +
  theme(legend.position="none") 

if(output_to_pdf)
{
  pdf(paste(files_path,"fig_rmssd_grouped.pdf", sep=""), width=20, height=10)
}
multiplot(p1)
if(output_to_pdf)
{
  dev.off()
}

#Plot features (rMSSD) for all sensors, subjects and windows
#This is the best way to analyze rMSSD and make sure things work well for a broad range of values (provided that they are part of the dataset)
p1 <- ggplot(df_rmssd, aes(window, rMSSD, fill = Sensor)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_fivethirtyeight() +
  facet_wrap(~subject_ID) +
  ggtitle("Comparison of PPG devices (rMSSD in ms)") +
  xlab("Time window")

if(output_to_pdf)
{
  pdf(paste(files_path,"fig_rmssd_all.pdf", sep=""), width=20, height=10)
}
multiplot(p1)
if(output_to_pdf)
{
  dev.off()
}
