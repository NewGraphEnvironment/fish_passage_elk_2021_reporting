
https://newgraphenvironment.github.io/fish_passage_elk_2021_reporting/

<br>

Reporting for fish passage and habitat confirmation assessments at road-stream crossings and dams within the Elk River watershed group as part of connectivity restoration planning targeting westslope cutthrout trout. The assessments conducted in 2021 compliment work completed in 2020 which can be viewed interactively online at https://newgraphenvironment.github.io/fish_passage_elk_2020_reporting_cwf/ [@irvine2021UpperElk].

Reporting was generated with `bookdown` [@bookdown2016] from `Rmarkdown` [@R-rmarkdown] with primarily `R` [@rcoreteam2020languageenvironment] and `SQL` scripts. In addition to numerous spatial layers sourced through the BC Data Catalogue then stored and queried in local `postgresql` and `sqlite` databases. [Raw data inputs](https://github.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/tree/master/data) for this project included: 

 + Populated [Fish Data Submission Spreadsheet Template - V 2.0, January 20, 2020 ](https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/fish-and-fish-habitat-data-information/fish-data-submission/submit-fish-data#submitfish) 

 + Populated [pscis_assessment_template_v24.xls](https://www2.gov.bc.ca/gov/content/environment/plants-animals-ecosystems/fish/aquatic-habitat-management/fish-passage/fish-passage-technical/assessment-projects)
 
 
 + [`Fish Habitat Model`/`bcfishpass`](https://github.com/smnorris/bcfishpass) outputs.


 + [Custom CSV file](https://github.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/raw/master/data/habitat_confirmations_priorities.csv) detailing Phase 2 site:
     - priority level for proceeding to design for replacement
     - length of survey upstream and downstream
     - a conservative estimate of the linear length of mainstem habitat potentially available upstream of the crossing 
     - fish species confirmed as present upstream of the crossing

 + [GPS tracks](https://github.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/tree/master/data/habitat_confirmation_tracks.gpx) from field surveys.  

 + [Photos](https://github.com/NewGraphEnvironment/fish_passage_elk_2021_reporting/tree/master/data/photos) and photo metadata.
