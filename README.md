## Lever Lab USV analysis Pipeline
Semi-automatic R-based pipeline for the analysis of rat ultrasonic vocalizations (USVs).

### Installation
1. Download [code](https://github.com/zmcadams/USV_pipeline/blob/main/code) dirctory and [.Rproj](https://github.com/zmcadams/USV_pipeline/blob/main/usv_test.Rproj) into own directory named 'code'.
2. Create 'data' folder to store basic metadata file.

### General Process
1. Fill out metadata file for each USV call you are interested in.  
**__one__ metadata file per .wav recording**
2. Place metadata file and .wav file in 'data' folder
3. Run USV pipeline in .Rproj using [_RUN_usv_read.R](https://github.com/zmcadams/USV_pipeline/blob/main/code/_RUN_usv_read.R).
   R console will prompt you to **select the metadata file** and **name the output file**.  
