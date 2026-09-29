## General comments

This is a minor update, in particular fixing all issues with CRAN checks:

 * Fix test broken by extra elements from glm.control()
 * Update data/*.R files to replace deprecated special names in structure()
 * Import findbars and nobars from reformulas vs lme4 to avoid warnings in tests

## Test environments

1. (Local) macOS 26.6.2, R 4.6.1 and R-devel (2026-09-28 r90591)
2. (Win-builder) Windows Server 2022, R-devel (2026-09-25 r90590 ucrt)
    
## Check results

No errors, warnings or notes.

## revdepcheck results

Checked 7 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * Saw 0 new problems
 * Failed to check 0 packages