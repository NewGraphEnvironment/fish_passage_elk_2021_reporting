Workflows for putting the data in one place to build the report and figures.  In the past, sometimes we have used branch versions of packages to build custom graphs and these branches may have divergent histories with the upstream master repos. Therefore we build the graphs separately as figures and insert so that future rebuilds are not dependent on custom packages. Additionally, some of our graphs require a fair bit of processing beforehand to put them together and this amount of scripting within the Rmarkdown documents is cumbersome. Figures include:

  * Hydrometric graphs
