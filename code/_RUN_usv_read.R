##### Click here before starting #####
source('code/load_libraries.R')
source('code/create_directories.R')
source('code/filt_quantify.R')

dev.set(dev.prev())
par(mar=c(1,1,1,1))

output <- readline(prompt = "Output file name: ")

# Read in call_tracker file
sheet <- read_excel(file.choose()) %>% 
  mutate(file_name = paste0(file_name, '.wav'))

files <- sheet %>% 
  pull(file_name) %>% 
  unique() 

# dev.off()
# for (i in 1:length(files)) {
#   file_name_pull <- files[i]
#   calls <- sheet %>%
#     filter(file_name == file_name_pull) %>%
#     nrow()
#   for (j in 1:calls) {
#     define_call(df = sheet, 
#                 file_name_input = file_name_pull, 
#                 call_number = j)
#   }
# }

dev.off()
for (i in 1:length(files)) {
  file_name_pull <- files[i]
  calls <- sheet %>%
    filter(file_name == file_name_pull) %>%
    nrow()
  for (j in 1:calls) {
    shim_jit_calculation(df = sheet, 
                         file_name_input = file_name_pull,
                         call_number = j, 
                         from = 1000, 
                         to = 15000)
    shim_jit_calculation(df = sheet, 
                         file_name_input = file_name_pull,
                         call_number = j, 
                         from = 15000, 
                         to = 35000)
    shim_jit_calculation(df = sheet, 
                         file_name_input = file_name_pull,
                         call_number = j, 
                         from = 35000, 
                         to = 60000)
    shim_jit_calculation(df = sheet, 
                         file_name_input = file_name_pull,
                         call_number = j, 
                         from = 60000, 
                         to = 80000)
  }
}

beepr::beep()

file_list <- list.files('output/output_per_file', full.names = T)

all_files <- lapply(file_list, read_tsv)

output_file_basic <- do.call(rbind, all_files)
output_file_basic %>% 
  as.data.frame() %>% 
  write.xlsx(file = glue::glue('output/{output}_summary.xlsx'), sheetName = 'full_spectrum', 
           append = T, row.names = F)


beepr::beep()

file_list <- list.files('output/output_per_file_ranges/', full.names = T)

all_files <- lapply(file_list, read_tsv)

output_file_ranges <- do.call(rbind, all_files)

output_file_ranges %>% 
  as.data.frame() %>% 
  write.xlsx(file = glue::glue('output/{output}_summary.xlsx'), sheetName = 'split_by_range', 
             append = T, row.names = F)


file.rename(from = 'output', to = glue::glue('output_{output}'))

beepr::beep(sound = 3)


