# Integration NDVI paper
This analysis investigates different methods of integrating the normalized difference vegetation index for use in crop modeling compared to simply using the maximum NDVI value for the season. NDVI measurements are taken from the MODIS satellite for the 50 US states for several crop types. NDVI measurements are taken every 16 days over the year.

A cubic spline response curve is fit to approximate NDVI's true response over the season. Integration start and end dates are determined by using accumulated growing degree units for each crop type from PRISM daily weather grids. The start and end dates are then matched to the NDVI values for each county and integration is performed. 

## Files can be run in sequence 
1. PrismCleaning.R
2. StartAndEndDatesFromGDD.R
3. MaxNDVIscript.R
4. IntegrationScript.R
5. MaxVsIntComparisonAllCropTypes.R