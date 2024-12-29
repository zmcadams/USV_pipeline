library(tidyverse)
library(readxl)
library(tuneR)
library(bayestestR)
library(seewave)
library(sonicscrewdriver)

## DONE Add 80 kHz limit on fund
## DONE fpeak (freq = 100)
## DONE fpeak output graphs
## DONE fund output graphs
## define jitter/shimmer values 
## excel output

# FUNCTION: describe call
define_call <- function(df = sheet, file_name_input, call_number) {
  
  # Extract call information
  df_filt <- df %>% 
    filter(file_name == file_name_input)
  min_freq_kHz <- 1
  file_name <- file_name_input
  rat_number <- df_filt$rat_number[call_number]
  time_point <- df_filt$time_point[call_number]
  call_number <- df_filt$call_number[call_number]
  start_time <- df_filt$start_time[call_number]
  stop_time <- df_filt$stop_time[call_number]
  kHz_cutoff <- df_filt$kHz_cutoff[call_number]
  category <- df_filt$category[call_number]
  notes <- df_filt$notes[call_number]
  
  print(rat_number)
  
  # Prints progress
  print('CURRENT PROCESS')
  print(file_name)
  print(rat_number)
  print(time_point)
  print(call_number)
  
  # Read in .wav file
  wav_file <- readWave(paste0('data/',file_name))
  # Trim .wav file to specified position
  wav_file_cut <- cutw(wav_file, from = start_time, to = stop_time, output = 'Wave')

  # Generate spectrum & acoustat images
  if (call_number > 0) {
    png(filename = glue::glue('output/images/spectrum_color/spectrum_rat-{rat_number}_time-{time_point}_call-{call_number}.png'), 
        width = 6, height = 6, units = 'in', bg = 'white', res = 300)
    spectro(wave = wav_file_cut, flim = c(min_freq_kHz, kHz_cutoff), plot = T)
    dev.off()
    
    png(filename = glue::glue('output/images/acoustat_image/acoustat_rat-{rat_number}_time-{time_point}_call-{call_number}.png'), 
        width = 6, height = 6, units = 'in', bg = 'white', res = 300 )
    acoustat(wave = wav_file_cut, flim = c(min_freq_kHz, kHz_cutoff))    
    dev.off()
  }
  
  # Pull acoustat results
  acoustat_res <- acoustat(wave = wav_file_cut, flim = c(min_freq_kHz, kHz_cutoff))
  # Calcualte the acoustat information
  time_start = acoustat_res$time.P1
  time_end = acoustat_res$time.P2
  acoustat_duraction = time_end - time_start
  
  # Pull mean frequency spectrum 
  spectrum <- meanspec(wav_file_cut, plot = F)
  
  # Set to freq filter to 0.1 kHz
  # Determine top 10 freq peaks
  peaks <- fpeaks(spectrum, plot = F, freq = 100) %>%
    as_tibble() %>% 
    top_n(10, amp) %>% 
    arrange(-amp) %>%
    rownames_to_column(var = 'peak') 
  
  png(filename = glue::glue('output/images/peak_frequency/peak_freq_rat-{rat_number}_time-{time_point}_call-{call_number}.png'), 
      width = 6, height = 6, units = 'in', bg = 'white', res = 300 )
  fpeaks(spectrum, plot = T, freq = 100)
  dev.off()
  
  # write peak freq frequency and amplitude
  peak_freq <- peaks %>%
    select(peak, freq) %>%
    pivot_wider(names_from = 'peak', values_from = 'freq', names_glue = 'peak_freq_{peak}')
  
  peak_amp <- peaks %>%
    select(peak, amp) %>%
    pivot_wider(names_from = 'peak', values_from = 'amp', names_glue = 'peak_amp_{peak}')
  
  # Round to 3rd decimal point
  freq_data <- acoustat_res$freq.contour %>% 
    as_tibble(colnames = c('freq', 'contour')) 
  
  less_than_15 <- freq_data %>% 
    filter(frequency <= 15) # 15
  less_than_35 <- freq_data %>% 
    filter(frequency > 15 & frequency <= 35)
  between_35_60 <- freq_data %>% 
    filter(frequency > 35 & frequency <= 60)
  greater_than_60 <- freq_data %>% 
    filter(frequency > 60 & frequency <= 80)
  
  auc_15 <- area_under_curve(less_than_15$frequency, less_than_15$contour, method = 'trapezoid')  
  auc_15_35 <- area_under_curve(less_than_35$frequency, less_than_35$contour, method = 'trapezoid')  
  auc_35_60 <- area_under_curve(between_35_60$frequency, between_35_60$contour, method = 'trapezoid')  
  auc_60_80 <- area_under_curve(greater_than_60$frequency, greater_than_60$contour, method = 'trapezoid')  
  
  total <- (auc_15 + auc_15_35 + auc_35_60 + auc_60_80)
  
  ratio_15 <- auc_15/total
  ratio_35 <- auc_15_35/total
  ratio_60 <- auc_35_60/total
  ratio_60_80 <- auc_60_80/total
  
  acoustat_top_category <- tibble(category = c('1_15', '15_35', '35_60', '60_80'),
                                 value = c(ratio_15, ratio_35, ratio_60, ratio_60_80)) %>% 
    top_n(n = 1, wt = value) %>% 
    pull(category)
  
  fund_freq <- fund(wave = wav_file_cut, plot = F, fmax = kHz_cutoff * 1000) %>% 
    as_tibble() %>% 
    drop_na()
  
  png(filename = glue::glue('output/images/fundamental_freq/fundamental_freq_rat-{rat_number}_time-{time_point}_call-{call_number}.png'), 
      width = 6, height = 6, units = 'in', bg = 'white', res = 300 )
  fund(wave = wav_file_cut, plot = T,fmax = kHz_cutoff * 1000 ) 
  dev.off()
  
  min_fund_freq <- min(fund_freq$y)
  max_fund_freq <- max(fund_freq$y)
  mean_fund_freq <- mean(fund_freq$y)
  median_fund_freq <- median(fund_freq$y)
  
  abs_jitter <- jitter(wav_file_cut, method = 'absolute')
  rel_jitter <- jitter(wav_file_cut, method = 'relative')
  shimmer_val <- shimmer(wav_file_cut)
  
  metadata <- tibble(file_name = file_name,
                     rat_number = rat_number,
                     time_point = time_point,
                     call_number = call_number,
                     sound_start_time = start_time,
                     sound_stop_time =  stop_time,
                     user_duration = stop_time - start_time,
                     kHz_cutoff = kHz_cutoff,
                     acoustat_time_start_s = time_start,
                     acoustat_time_end_s = time_end,
                     acoustat_duraction_s = acoustat_duraction,
                     freq_auc_0_15 = auc_15,
                     freq_auc_15_35 = auc_15_35,
                     freq_auc_35_60 = auc_35_60,
                     freq_auc_60_80 = auc_60_80,
                     freq_total_auc = total,
                     ratio_freq_auc_0_15 = ratio_15,
                     ratio_freq_auc_15_35 = ratio_35,
                     ratio_freq_auc_35_60 = ratio_60,
                     ratio_freq_auc_60_80 = ratio_60_80,
                     acoustat_top_category = acoustat_top_category,
                     user_defined_category = category,
                     min_fund_freq_kHz = min_fund_freq,
                     max_fund_freq_kHz = max_fund_freq,
                     mean_fund_freq_kHz = mean_fund_freq,
                     median_fund_freq_kHz = median_fund_freq, 
                     abs_jitter = abs_jitter, 
                     rel_jitter = rel_jitter, 
                     shimmer = shimmer_val)
  
  output <- cbind(metadata, peak_freq, peak_amp, category, notes) %>%
    as_tibble()
  write_tsv(output, 
            file = glue::glue('output/output_per_file/rat-{rat_number}_time-{time_point}_call-{call_number}.tsv'))
  
}


shim_jit_calculation <- function(df = sheet, 
                                 file_name_input, 
                                 call_number,
                                 from, 
                                 to
                                 # rat_number = rat_number,
                                 # time_point = time_point, 
                                 # kHz_cutoff = kHz_cutoff,
) {
  
  # Extract call information
  df_filt <- df %>% 
    filter(file_name == file_name_input)
  file_name <- file_name_input
  rat_number <- df_filt$rat_number[call_number]
  time_point <- df_filt$time_point[call_number]
  call_number <- df_filt$call_number[call_number]
  start_time <- df_filt$start_time[call_number]
  stop_time <- df_filt$stop_time[call_number]
  kHz_cutoff <- df_filt$kHz_cutoff[call_number]
  category <- df_filt$category[call_number]
  notes <- df_filt$notes[call_number]
  
  # Prints progress
  print('CURRENT PROCESS')
  print(file_name)
  print(rat_number)
  print(time_point)
  print(call_number)
  
  # Read in .wav file
  wav_file <- readWave(paste0('data/',file_name))
  # Trim .wav file to specified position
  wav_file_cut <- cutw(wav_file, from = start_time, to = stop_time, output = 'Wave')
  
  wav_ffilt1 <- ffilter(wav_file_cut,from = from, output = 'Wave')
  wav_ffilt1 <- ffilter(wav_ffilt1, to = to, output = 'Wave')
  
  freq_a <- from / 1000
  freq_b <- to / 1000
  
  if (call_number > 0) {
    png(filename = glue::glue('output/images/spectrum_ranges/{freq_a}_{freq_b}/spectro_image/spectrum_rat-{rat_number}_time-{time_point}_call-{call_number}.png'),
        width = 6, height = 6, units = 'in', bg = 'white', res = 300)
    spectro(wave = wav_ffilt1, flim = c(0, kHz_cutoff), plot = T)
    dev.off()
    
    png(filename = glue::glue('output/images/spectrum_ranges/{freq_a}_{freq_b}/acoustat_image/acoustat_rat-{rat_number}_time-{time_point}_call-{call_number}.png'),
        width = 6, height = 6, units = 'in', bg = 'white', res = 300 )
    acoustat(wave = wav_ffilt1, flim = c(0, kHz_cutoff))
    dev.off()
  
  }
  
  # Pull acoustat results
  acoustat_res <- acoustat(wave = wav_ffilt1)
  # Calcualte the acoustat information
  time_start = acoustat_res$time.P1
  time_end = acoustat_res$time.P2
  acoustat_duraction = time_end - time_start
  
  # Pull mean frequency spectrum 
  spectrum <- meanspec(wav_ffilt1, plot = F)
  
  # Set to freq filter to 0.1 kHz
  # Determine top 10 freq peaks
  tryCatch({
    peaks <- fpeaks(spectrum, plot = F, freq = 100) %>%
      as_tibble() %>% 
      top_n(10, amp) %>% 
      rownames_to_column(var = 'peak') 
    
    png(filename = glue::glue('output/images/spectrum_ranges/{freq_a}_{freq_b}/peak_freq/peak_freq_rat-{rat_number}_time-{time_point}_call-{call_number}.png'), 
        width = 6, height = 6, units = 'in', bg = 'white', res = 300 )
    fpeaks(spectrum, plot = T, freq = 100)
    dev.off()
    
    # write peak freq frequency and amplitude
    peak_freq <- peaks %>%
      select(peak, freq) %>%
      pivot_wider(names_from = 'peak', values_from = 'freq', names_glue = 'peak_freq_{peak}')
    
    peak_amp <- peaks %>%
      select(peak, amp) %>%
      pivot_wider(names_from = 'peak', values_from = 'amp', names_glue = 'peak_amp_{peak}')
    
  },  error = function(e) {
    # Code to handle the error, e.g., print a message or skip to the next iteration
    message(paste("Skipping this call due to error:", e$message))
  })
  
  # Round to 3rd decimal point
  freq_data <- acoustat_res$freq.contour %>% 
    as_tibble(colnames = c('freq', 'contour')) 
  
  less_than_15 <- freq_data %>% 
    filter(frequency <= 15) # 15
  less_than_35 <- freq_data %>% 
    filter(frequency > 15 & frequency <= 35)
  between_35_60 <- freq_data %>% 
    filter(frequency > 35 & frequency <= 60)
  greater_than_60 <- freq_data %>% 
    filter(frequency > 60 & frequency <= 80)
  
  auc_15 <- area_under_curve(less_than_15$frequency, less_than_15$contour, method = 'trapezoid')  
  auc_15_35 <- area_under_curve(less_than_35$frequency, less_than_35$contour, method = 'trapezoid')  
  auc_35_60 <- area_under_curve(between_35_60$frequency, between_35_60$contour, method = 'trapezoid')  
  auc_60_80 <- area_under_curve(greater_than_60$frequency, greater_than_60$contour, method = 'trapezoid')  

  
  fund_freq <- fund(wave = wav_ffilt1, plot = F) %>% 
    as_tibble() %>% 
    drop_na()
  
  png(filename = glue::glue('output/images/spectrum_ranges/{freq_a}_{freq_b}/fundamental_freq/fundamental_freq_rat-{rat_number}_time-{time_point}_call-{call_number}.png'), 
      width = 6, height = 6, units = 'in', bg = 'white', res = 300 )
  fund(wave = wav_ffilt1, plot = T) 
  dev.off()
  
  min_fund_freq <- min(fund_freq$y)
  max_fund_freq <- max(fund_freq$y)
  mean_fund_freq <- mean(fund_freq$y)
  median_fund_freq <- median(fund_freq$y)
  
  abs_jitter <- jitter(wav_ffilt1, method = 'absolute')
  rel_jitter <- jitter(wav_ffilt1, method = 'relative')
  shimmer_val <- shimmer(wav_ffilt1)
  
  call_range = paste0(freq_a, '_', freq_b)
  
  
  metadata <- tibble(file_name = file_name,
                     rat_number = rat_number,
                     time_point = time_point,
                     call_number = call_number,
                     call_range = call_range,
                     sound_start_time = start_time,
                     sound_stop_time =  stop_time,
                     user_duration = stop_time - start_time,
                     kHz_cutoff = kHz_cutoff,
                     acoustat_time_start_s = time_start,
                     acoustat_time_end_s = time_end,
                     acoustat_duraction_s = acoustat_duraction,
                     freq_auc_0_15 = auc_15,
                     freq_auc_15_35 = auc_15_35,
                     freq_auc_35_60 = auc_35_60,
                     freq_auc_60_80 = auc_60_80,

                     min_fund_freq_kHz = min_fund_freq,
                     max_fund_freq_kHz = max_fund_freq,
                     mean_fund_freq_kHz = mean_fund_freq,
                     median_fund_freq_kHz = median_fund_freq, 
                     abs_jitter = abs_jitter, 
                     rel_jitter = rel_jitter, 
                     shimmer = shimmer_val)
  
  write_tsv(metadata,
            file = glue::glue('output/output_per_file_ranges/rat-{rat_number}_time-{time_point}_call-{call_number}-{call_range}.tsv'))
  
}

