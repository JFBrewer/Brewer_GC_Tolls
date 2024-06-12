# Brewer_GC_Tools
A collection of my GEOS-Chem tools for public use

**Brewer_Planeflight_Handler**
Brewer_Planeflight_Handler.R is a simple R tool that can take as input a selection of LAT-LON-ALT-TIME points and a pair of easily modified text header files and output a properly formatted Planeflight.dat.YYYYMMDD for the GEOS-Chem planeflight menu.

A testbed dataset is included in the zip file, based on the KORUS-AQ data I used in my Brewer et al. 2023 paper, available publicly at https://www-air.larc.nasa.gov/cgi-bin/ArcView/korusaq. This zip file has everything set up precisely for the tool as written, but the tool itself can be easily modified and is extensively commented. 

If you do modify the code for Brewer_Planeflight_Handler.R, I recommend only modifying the first 84 lines until you really know what you are doing. The code should be fairly robust to changes, so long as you keep the variable names the same, but the fixed-width-file fortran output is very sensitive to precisely how you format the Planeflight.dat.YYYYMMDD files. 
