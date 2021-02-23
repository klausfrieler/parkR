# WBA-MLA Solo Generator


The WBA-MLA Solo Generator is a small tool that is able to generate monophonic jazz solos. It also contains two data sets of jazz chord changes, the iRealPro collection and changes taken from the Weimar Jazz Database

## Citation


## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('sologenerator')`

4. Install the WBA-MLA Solo Generator:

`devtools::install_github('klausfrieler/sologenerator')`

## Usage

### Quick demo 

To generate one solo chorus over an F-blues, you can type the following

```
  library('sologenerator')
  blues_sample <- generate_chorus(sologenerator::F_blues) 
  blues_mcsv2 <- chorus_to_mcsv2(blues_sample)  
  write_mcsv2(blues_mcsv2, "blues.csv")
```
The last line writes the solo as an MCSV2 file `blues.csv`in the workind directory. This file 
can converted to MIDI or Lilypond/PDF using the MeloSpyGUI, which
is freely available on the [Jazzomat Website](https://jazzomat.hfm-weimar.de/download/download.html)

More lead sheets can be found in the irb and wjd_chord_db data sets, incorporated in the package. For example, to generate a solo over "All The Things You Are" you can use (usingthe tidyverse)

```
  library(tidyverse)
  library(sologenerator)
  create_from_irb("All the things you are") %>% 
    generate_chorus() %>% 
    chorus_to_mcsv2() %>%  
    write_mcsv2("all_the_things_you_are.csv")
```
