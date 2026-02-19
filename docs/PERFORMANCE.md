# Performance & Sampling

DataExplorerPro includes protective sampling and lightweight profiling for large datasets.

- Auto-sampling: When datasets exceed the **Auto-sample threshold**, the app will use a sampled subset for EDA and visualizations to keep interactions fast.
- Thresholds and sample sizes can be configured in Settings → Performance.
- Use "Profile EDA" in Settings → Performance to get a quick measurement of how long EDA generation takes on the current dataset.

Recommendation: Increase sampling size or disable auto-sample only if your machine has sufficient memory and you require exact analyses on the full dataset. For reproducibility, consider running heavy analyses outside the interactive session using the full dataset.
