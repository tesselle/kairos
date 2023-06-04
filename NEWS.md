# kairos 1.2.0.9000
## New classes and methods
* Add `image()` methods.

## Internals
* `MeanDate` and `AoristicSum` classes now inherit from `TimeSeries` (see **aion**).

## Bugfixes & changes
* Remove previously deprecated methods.

## Breaking changes
* For consistency, all `mcd()` methods now return a `MeanDate` object.
* Remove all `autoplot()` methods to reduce dependencies (use **graphics** instead of **ggplot2**).

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
