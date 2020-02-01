Changes in BradleyTerry2 1.1-1
==============================

 * improve the way `BTm` finds variables passed to `outcome`, `player1` etc, so that it works when run in a separate environment.
 * convert old tests to unit tests.

Changes in BradleyTerry2 1.1-0
==============================

 * `anova.BTm` now respects `test` and `dispersion` arguments for models that inherit from `glm`.
 * fix bug in `anova.BTmlist` affecting models where ability is modelled by predictors but ability is estimated separately for some players due to missing values. 
 * fix bug in `glmmPQL` affecting models with `.` in formula and either offset or weights specified.
 * standardize tests to use random number generation as in R 2.10 for backwards compatibility.
 
Changes in BradleyTerry2 1.0-9
==============================

 * fix bug in setting contrasts in internal function `Diff()` that gave warning under R-devel.
 * update urls (using https where possible).
 * fix a couple of `if` statements where argument could be > 1.
 
Changes in BradleyTerry2 1.0-8
==============================

 * fix bug in `qvcalc.BTabilities`
 
Changes in BradleyTerry2 1.0-7
==============================

Improvements
------------

 * 	new examples of prediction added, including using `predict.BTm` to estimate 
    abilities with non-player abilities set to non-zero values (for models with 
    a fixed reference category).
 *  `qvcalc.BTabilities` moved over from package **qvcalc**.
 *  package imports rather than depends on **lme4**.

Changes in behaviour
--------------------

 * 	default `level` in `predict.BTm` and `predict.glmmPQL` is 0 if a fixed 
    effects model has been fitted, 1 otherwise.
        
Bug fixes
---------

 * 	BTabilities now works (again) for models where the reference category is
    not the first player. Players are kept in their original order (levels 
    of `player1` and `player2`), but the abilities are returned with the 
    appropriate reference.
        
 *  BTabilities now works when ability is modelled by covariates and some
    parameters are inestimable (e.g. as in `chameleons.model` on `?chameleons`).
        
 *  `predict.BTglmmPQL` now works for models with inestimable parameters
	
Changes in BradleyTerry2 1.0-6
==============================

Changes in behaviour
--------------------

 * 	`BTabilities` now returns `NA` for unidentified abilities

Bug fixes
---------

 * 	BTabilities now respects contrasts argument and contrasts attributes of
    `player1` and `player2` factors. Also handle unidentified coefficients 
	correctly.


Changes in BradleyTerry2 1.0-5
==============================

Bug fixes
---------

 * 	no longer imports from **gnm**, so **gnm** need not be installed.


Changes in BradleyTerry2 1.0-4
==============================

Bug fixes
---------

 * 	depends on **lme4** (>=1.0).


Changes in BradleyTerry2 1.0-3
==============================

New Features
------------

 * 	updated football data to include full 2011-12 season.


Changes in BradleyTerry2 1.0-2
==============================

New Features
------------

 * 	added football example presented at useR! 2013 with generalised
    Davidson model for ties.


Changes in BradleyTerry2 1.0-1
==============================

Bug fixes
---------

 * 	renamed `glmmPQL` object `BTglmmPQL` to avoid conflict with **lme4** 
    (which loads **MASS**).
 * 	fixed `BTm` so that it is able to find variables when called inside
    another function (stackoverflow.com question 14911525).


Changes in BradleyTerry2 1.0-0
==============================

 * 	updated references and CITATION to cite JSS paper on 
    BradleyTerry2	


Changes in BradleyTerry2 0.9-7
==============================

Bug fixes
---------

 * 	fixed `anova.BTmlist` to work for models with random effects

 *  allow models to be specified with no fixed effects


Improvements
------------

 * 	updated vignette, including example of bias-reduction, a new example 
    incorporating random effects and a new example on preparing data for use 
    with package


Changes in BradleyTerry2 0.9-6
==============================

Bug fixes
---------

 * 	fixed `offset` argument to work as documented

 * 	corrected documentation for `citations` data

Improvements
------------

 * 	updated vignette, to provide more explanation of setting up the data


Changes in BradleyTerry2 0.9-5
==============================

 * updated  contact details

Changes in BradleyTerry2 0.9-4
==============================

New Features
------------

 * 	added ice hockey example presented at useR! 2010

Bug fixes
---------

 * 	`predict.BTm` now works for models with no random effects and handles
   	new individuals with missing values in predictors. 


Changes in BradleyTerry2 0.9-3
=============================

New Features
------------

 * 	added predict method for BTm objects.

Bug fixes
---------

 * 	fixed bug in `BTm.setup` causing problems in finding variables when `BTm`
    nested within another function.
