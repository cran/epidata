# epidata 0.4.0

* fixes CRAN check issue
* added option to suppress citation/notes messages (use `options(epidata.show.citation = FALSE)`)
* switched to {tinytest}

# epidata 0.3.0

* Added a `NEWS.md` file to track changes to the package.
* Added a package user-agent to httr calls
* Changed http: epidata URLS to https:
* Moved DESCRIPTION refs to GitLab
* Fixed CRAN check errors due to new epidata API endpoint
* Added 4 new data sources: "Annual wages by wage group", 
  "Compensation, wages, and benefits", "Minimum wage", "Poverty-level wages"
