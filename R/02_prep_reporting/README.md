Workflows for putting the data in one place to build the report and figures (ex. hydrometric graphs).  Additional processing for datasets to get ready for reporting include:

  * Derive watershed areas upstream of Phase 2 sites using fwapg.
  * Derive watershed statistics for watershed areas using `elevatr` to download rasters and `raster` to process. 
  * Use poisspatial to derive elevation for stream crossing site locations and joint to watershed stats. Save to sqlite db.
  * Build the fiss species table and join to species at risk data.
  * Build graph that shows the breakdown of fish observations.
  * Download outputs for all modeled crossings in the watershed group and save to sqlite db.
  * Extract photo metadata so we can display the photos from the reporting on the interactive map.
