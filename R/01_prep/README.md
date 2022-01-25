Workflows for pre-processing of field data so it aligns with requirements for PSCIS upload and bcfishpass outputs. These are often 2 way workflows where we use output csv spreadsheets to update our raw input datasheets.  It may be a bit confusing to follow but we try to document it just the same.  Tasks completed include:

  *  Backup original photos on a remote drive or server.
  *  Resize photos to be under 1mb as per PSCIS requirements.
  *  Rename jpg and jpeg (not completed yet as hasn't been required) to JPG to simplify reporting and eliminate issues with PSCIS upload.
  *  Get UTMs of PSCIS, modelled crossing and dam sites from bcfishpass generated database when field survey indicates correct location.
  *  Find road tenure information from bcfishpass.
  *  Determine replacement structure type and size based on field measured metrics.
  *  Build directories of folders related to each site based on PSCIS input spreadsheet site ids.
  *  Do an initial drop of photos into the generated site folders based on dates, times and surveyor.
  *  QA renamed photos to determine that all 5 photos (upstream, downstream, inlet, outlet, barrel) required for PSCIS as well as a road photo are present.
  *  Generate an amalgamated photo for each site containing all 6 of the previously mentioned photos.
  *  Generate a csv file that contains the locations and names of all photos after they are sorted and renamed to facilitate reproducability (one day if required that is).
