# parkR: WBA-MLA Solo Generator


The parkR package for R contains the WBA-MLA Solo Generator which is a small tool that is able to generate monophonic jazz solos. It also contains two data sets of jazz chord changes, the iRealPro collection and a set of changes taken from the Weimar Jazz Database. Paper preprint: https://osf.io/wn27k/

## Citation


## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install the parkR package from Github:

`devtools::install_github('klausfrieler/parkR')`

## Usage

### Quick demo 

To generate one solo chorus over an F-blues, you can type the following

```
  library('parkR')
  blues_sample <- generate_solo(parkR::F_blues) 
  blues_mcsv2 <- solo_to_mcsv2(blues_sample)  
  write_mcsv2(blues_mcsv2, "blues.csv")
```
The last line writes the solo as an MCSV2 file `blues.csv`in the workind directory. This file 
can converted to MIDI or Lilypond/PDF using the MeloSpyGUI, which
is freely available on the [Jazzomat Website](https://jazzomat.hfm-weimar.de/download/download.html)

More lead sheets can be found in the irb and wjd_chord_db data sets, incorporated in the package. For example, to generate a solo with two choruses over "All The Things You Are" you can use

```
  library(tidyverse)
  library(parkR)
  create_leadsheet_from_irb("All the things you are") %>% 
    generate_solo(n_chorus = 2) %>% 
    solo_to_mcsv2() %>%  
    write_mcsv2("all_the_things_you_are.csv")
```
