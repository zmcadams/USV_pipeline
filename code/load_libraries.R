if (!requireNamespace('tidyverse', quietly = T)) {
  install.packages('tidyverse')
  loadNamespace('tidyverse')
}
if (!requireNamespace('readxl', quietly = T)) {
  install.packages('readxl')
  loadNamespace('readxl')
}
if (!requireNamespace('tuneR', quietly = T)) {
  install.packages('tuneR')
  loadNamespace('tuneR')
}
if (!requireNamespace('bayestestR', quietly = T)) {
  install.packages('bayestestR')
  loadNamespace('bayestestR')
}
if (!requireNamespace('seewave', quietly = T)) {
  install.packages('seewave')
  loadNamespace('seewave')
}
if (!requireNamespace('sonicscrewdriver', quietly = T)) {
  install.packages('sonicscrewdriver')
  loadNamespace('sonicscrewdriver')
}
if (!requireNamespace('beepr', quietly = T)) {
  install.packages('beepr')
  loadNamespace('beepr')
}
# if (!requireNamespace('xlsx', quietly = T)) {
#   install.packages('xlsx')
#   loadNamespace('xlsx')
# }

require(tidyverse)
require(readxl)
require(tuneR)
require(bayestestR)
require(seewave)
require(sonicscrewdriver)
require(beepr)
# require(xlsx)

