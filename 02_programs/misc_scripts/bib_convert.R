#convert bibtex to word format
library(rbibutils)
library(here)

file <- paste0(here(), "/02_programs/misc_scripts/bib.bib")
outfile <- paste0(here(), "/02_programs/bibliography.xml")

bibConvert(infile=file, outfile=outfile, informat='bibtex',outformat = 'wordbib')
