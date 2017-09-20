## General comments

This is a minor update to be compatible with qvcalc 0.9-1. The CRAN version of 
BradleyTerry2 (1.0-7) still passes check with qvcalc 0.9-1, but the new version
of qvcalc reveals a bug in BradleyTerry2 1.0-7 which is fixed in this update.

This update depends on qvcalc 0.9-1 exporting the S3 method qvcalc.default, so
qvcalc 0.9-1 must be installed for BradleyTerry2 1.0-8 to pass check.

I could not reproduce the error reported for r-devel-linux-x86_64-debian-clang
in the package check summary, either using R-hub or a docker container. However
it may be related to recent changes in `base::deparse`. I have modified 
`BTabilities` so that it works regardless of whether the `backtick` argument
to `base::deparse` is `TRUE` or `FALSE`.

Similarly I could not reproduce the note reported for r-oldrel-osx-x86_64 on 
R-hub or on a friend's Mac and got no response to a query on R-package-devel. 
The note seems to be associated with having to run BibTex on the vignette .tex 
file, so as a possible solution I have included the contents of the .bbl 
directly in the .Rnw so BibTeX is not needed.

## Test environments

* Local (with qvcalc 0.9-1 installed)
    * Ubuntu 14.04, R 3.4.1
    * Ubuntu 14.04, R-devel (2017-09-20 r73322)
    * Windows 8, R 3.4.1
    
## R CMD check results

Status: OK
