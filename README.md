# REX-goldenrod
## Introduction ##
R scripts from the Kellogg Biological Station Long Term Ecological Research site Rain Exclusion eXperiment (KBS-LTER REX).

These scripts are used to organize, clean, analyze, and plot data from the individual goldenrod stems collected in 2021 & 2022.

### Response variables include: ###
- stem biomass
- stem height
- seed production and biomass
- gall measurements (biomass, larval chamber size and count)

## Workflow: ##
The associated L1 data must be downloaded prior to running the R script for each variable. Each response variable typically has its own R script for cleaning (L1) and analysis/plotting (L2). Some traits (e.g. seed production and biomass) are grouped for L2 analysis and figure-making scripts.

![image](https://github.com/user-attachments/assets/f64fd8a2-8ebf-4b15-bffd-92d553ad4879)
Workflow image credit: https://edirepository.org/resources/designing-a-data-package

## Location of data: ##
The L1 data assocaited with this repository are available in an EDI package (in prep).

## Spatiotemporal extent: ##
- Spatial extent: Kellogg Biological Station Long Term Ecological Research site (KBS-LTER), Hickory Corners, MI, USA. All data were collected in the early successional (T7) plots
- Temporal extent: end of growing season (September - October) 2021 & 2022

## Usage: ##
All analyses were conducted using R (R Core Team 2024)

### File naming convention: ###
- Data files: File names typically contain KBS-LTER treatment code (T7), project within the greater Rain Exclusion eXperiment or REX (warmx), plant identifier (soca - Solidago canadensis), response variable, and data stage identifier. Some may include the greater project identifier "REX".
- Scripts: Scripts are seperated into two folders: L1 and L2. L1 scripts use raw data and clean it, resulting in the cleaned L1 data in the EDI package. The L2 scripts take the cleaned data and analyze/plot it. The scripts are named with respect to their response variable, and contain code for both statistical analysis and figure making.

## Scripts: ##
### L1 ###
L1 scripts use raw data and clean it, resulting in the cleaned L1 data in the EDI package. We do not currently have raw L0 data available in our EDI pacakge, but it may be available upon reasonable request.

All L1 scripts are cleaning scripts. Some may include "clean" in the file name (e.g. gall_vol_clean_L1.R).

### L2 ### 
The L2 scripts take the cleaned data and analyze/plot it. The scripts are named with respect to their response variable, and contain code for both statistical analysis and figure making.

*file name incoming* is an optional cleaning script that unifies some metadata columns across all L1 files. This includes adding year columns to datasets that don't have them, capitalizing all subplot letters, and renaming the LTER treatment column to the standardized T7. This is an optional script, and does not affect any figures or analyses.

## Contributors: ##
PI: Phoebe L. Zarnetske

Collaborators: Emily Parker, Kara Dobson, Moriah Young, Mark Hammond

## Contact: ##
For inquiries about the scripts or data, please contact Emily Parker @ parkere5@oregonstate.edu

### Funding: ###
Emily Parker was supported by various scholarships through Michigan State University (the MSU KBS Undergraduate Research Apprenticeship program, the MSU College of Natural Science Undergraduate Research Support Program, the Patricia A. Werner Scholarship for Ecological Field Studies, and the Sharin E. & Thomas C. Noall and Charles & Thelma E. Noall Scholarship in Biological and Environmental Science at KBS). Support for this research was also provided by the NSF LTER Program (DEB: 2224712) at the Kellogg Biological Station and Michigan State University AgBioResearch.



