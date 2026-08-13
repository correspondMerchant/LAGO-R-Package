# LAGOtrials: Learn-As-You-Go Adaptive Trial Optimization

Optimizes interventions for adaptive "Learn-As-you-GO" (LAGO) trials, in
which the intervention package is updated between stages using
accumulating data. Given data from completed stages, lago_optimization()
fits the outcome model (via 'stats' generalized linear models or, for
clustered designs, cluster-robust variance) and computes the
cost-optimal recommended intervention for the next stage that is
expected to reach a target mean outcome and/or a desired statistical
power. get_confidence_set() constructs confidence sets for the optimal
intervention, and visualize_cost() and lago_report() summarize the cost
surface and results. Methods are described in Nevo, Lok and Spiegelman
(2021) [doi:10.1214/20-aos1978](https://doi.org/10.1214/20-aos1978) and
Bing, Spiegelman, Nevo and Lok (2025)
[doi:10.1093/biomtc/ujaf061](https://doi.org/10.1093/biomtc/ujaf061) .

## See also

Useful links:

- <https://correspondmerchant.github.io/LAGO-R-Package/>

- <https://github.com/correspondMerchant/LAGO-R-Package>

- Report bugs at
  <https://github.com/correspondMerchant/LAGO-R-Package/issues>

## Author

**Maintainer**: Ante Bing <abing@bu.edu>

Authors:

- Ante Bing <abing@bu.edu>

- Minh Bui

- Jingyu Cui

Other contributors:

- Mike Bostock (Author of the bundled D3 v7 library,
  inst/js/d3.v7.min.js) \[copyright holder\]
