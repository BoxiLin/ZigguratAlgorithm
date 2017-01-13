# ZigguratAlgorithm

Ziggurat algorithm for random variable generation.

The Ziggurat Algorithm is one of the most efficient rejection sampling methods that has been
implemented as the built-in random number generator by many programming languages. It addresses the
problem of high rejection rate and high calculation complicity for every candidate sample point. Ziggurat
Algorithm divided the target area under the density function to several rectangulars and a tail area. Given
number of these rectangulars be 255, only less than 2% of candidate random sample will be rejected.
This continuously increases the efficiency “gap” between other algorithms and Ziggurat as the sample size
increases. Apart from the good performance on generating huge data sets, Ziggurat also performs an
architecture beauty.

In this Project, with the realization of the Ziggurat algorithm, we successfully generated 100,000
numbers from standard normal using 5 seconds. We justified that, theoretically, Ziggurat can efficiently
generate a large set of data with low computational complicity. Initially, we intend to compare Ziggurat
with other sampling methods, like Box-Muller. However, due to technical difficulties, the result don’t
reflect what we expected. A detailed discussion addresses this problem in the conclusion part. We also
provide our own version of generating the initial table for Ziggurat to start in the appendix.
