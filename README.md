# FindMyPubs

R scripts to find Pubmed publications and BioRxiv preprints for a given author and save to Biblatex files. I used github actions to automate this process for my own use. I use the Biblatex files to create documents like my CV and NIH Biosketch.

## Tools

- [`{rentrez}`](https://github.com/ropensci/rentrez) :package: to pull from PubMed.
- [`{rbiorxiv}`](https://github.com/nicholasmfraser/rbiorxiv) :package: to pull from BioRxiv.
- [`{RefManageR}`](https://github.com/ropensci/RefManageR) :package: to convert to Biblatex format.

## TODO

- [ ] Write unit tests
- [ ] 