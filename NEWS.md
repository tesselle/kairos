# kairos 1.1.0.9000

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
