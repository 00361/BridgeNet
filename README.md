# BridgeNet

Automated bridge hub identification between drug targets and disease core genes.

## Installation

```r
remotes::install_github("yourname/BridgeNet")
```

## Quick start

```r
library(BridgeNet)

result <- run_bridgenet(
  targets = "SwissTargetPrediction.csv",
  core_genes = c("BID"),
  score = 400,
  enrich = FALSE
)

print(result$bridge)
plot_bridge(result$graph, highlight = c("BID", "MCL1", "CTSS"))
```

## Functions

- `read_targets()`: Read drug targets from SwissTargetPrediction CSV
- `build_bridge()`: Build bridge PPI network via STRING API
- `find_bridge_hub()`: Identify bridge hubs using multi-metric centrality
- `plot_bridge()`: Publication-quality network visualization
- `run_bridgenet()`: One-step pipeline

## License

MIT
