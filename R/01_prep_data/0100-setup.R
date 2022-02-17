
##we want to use secondary headers
# https://github.com/rstudio/bookdown/issues/619


##and we want to push to github as website https://bookdown.org/yihui/bookdown/github.html
##add the .nojeckyll file so you can biuld website

dir.create('data')
dir.create('data/inputs_extracted')
dir.create('data/inputs_extracted/temp')
dir.create('docs')
file.create('docs/.nojekyll')

# First, set the output directory of your book to be /docs by adding the line output_dir: "docs" to the configuration file _bookdown.yml
##test by running the following command from the main directory
bookdown::serve_book()

##changed the citr options options("citr.use_betterbiblatex" = F) but didn't work so changed it back
