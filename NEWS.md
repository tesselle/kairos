# kairos 2.0.1
## Bugfixes & changes
* Fix support for supplementary rows in `event()`.

# kairos 2.0.0
## New classes and methods
* Add `image()` methods.
* Add `coef()`, `fitted()`, `residuals()`, `sigma()` and `terms()` to extract values from `EventDate` objects.

## Internals
* `MeanDate`, `AoristicSum`, `RateOfChange` and `IncrementTest` classes now inherit from `TimeSeries` (see **aion**).
* Use **tinytest** instead of **testthat**.

## Bugfixes & changes
* Remove previously deprecated methods.

## Breaking changes
* Use **aion** for internal date representation.
* Use **graphics** instead of **ggplot2** to reduce hard dependencies (remove all `autoplot()` methods).
* For consistency, all `mcd()` methods now return a `MeanDate` object.

# kairos 1.2.0
## New classes and methods
* Add `seriate_refine()` method for `PermutationOrder` objects.

## Bugfixes & changes
* Deprecate `refine()`.
* `cutoff` argument of `event()` is defunct (use `rank` instead).

# kairos 1.1.0
## New classes and methods
* Add seriation methods: `seriate_rank()`, `seriate_average()`, `permute()`, `get_order()`.
* Add `SimulationMeanDate` to store mean date estimations from simulated assemblages.
* Add `simulate()` method for `MeanDate` objects.

## Breaking changes
* No longer use classes from **arkhe**.
* Fix `bootstrap()` method for `MeanDate` objects: resample with replacement instead of simulating observations from a multinomial distribution.

# kairos 1.0.1
## Bugfixes & changes
* Fix aoristic sum calculation (#1). `aoristic()` now calculates weights within the time blocks rather than at the break dates between blocks.

# kairos 1.0.0
* First release.
