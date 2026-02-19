# Changelog

All notable changes to DataExplorerPro will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-01-27

### Added

#### Core Features
- Initial release of DataExplorerPro
- AI-powered data exploration using Ollama local models
- Natural language query interface for data analysis
- Comprehensive Auto EDA report generation
- Interactive AI assistant chat

#### Visualization Engine
- **50+ Chart Types** including:
  - Basic: Scatter, line, bar, histogram, box, violin, heatmap, density, pie
  - Advanced: Raincloud, ridgeline, beeswarm, forest, Bland-Altman, Q-Q, volcano, Manhattan
  - Comparison: Error bar, dumbbell, lollipop, diverging bar
  - Time Series: Area with confidence band, stacked area, calendar heatmap
  - Multivariate: Bubble, ternary, Sankey, sunburst, treemap
  - Geographic: Interactive choropleth maps
  - Network: Interactive network graphs with visNetwork

#### AI Integration
- Dynamic Ollama model detection
- Support for multiple models (llama3.2, mistral, codellama, qwen2.5, etc.)
- Configurable temperature and max tokens
- Custom Ollama server URL support

#### User Interface
- 8 theme options: Dark, Light, Blue, Green, Purple, Orange, Red, Monochrome
- Settings modal with Appearance, AI, and Chart Defaults tabs
- Scrollable sidebars in Visualization tab
- Responsive layout with glassmorphism design
- In-app help system

#### Data Handling
- Support for CSV, Excel (.xlsx), RDS, and Parquet files
- Built-in sample datasets (mtcars, iris, diamonds, penguins, gapminder)
- R environment object selection
- Data transformation suite (filter, select, mutate, summarize, etc.)

#### Export Options
- HTML export for interactive charts
- PNG export for static images
- JSON export for chart data
- EDA report download

#### Developer Features
- Comprehensive NAMESPACE with 60+ exported functions
- Unit tests using testthat
- Roxygen2 documentation
- RStudio add-in integration

### Technical Details

#### Dependencies
- shiny (web application framework)
- plotly (interactive visualizations)
- dplyr, tidyr (data manipulation)
- ggplot2 (static visualizations)
- bslib (Bootstrap themes)
- httr, jsonlite (API communication)
- igraph, visNetwork, networkD3 (network visualizations)

#### Performance
- Automatic sampling for large datasets
- Lazy loading for visualizations
- Efficient data handling with data.table backend

### Documentation
- Comprehensive README with feature tables
- Quick tour guide
- Keyboard shortcuts reference
- Performance considerations guide

### Security
- Local-only AI processing via Ollama
- No external data transmission
- Secure file handling

---

## [Unreleased]

### Planned Features
- Multi-dataset comparison mode
- Custom visualization templates
- Collaborative analysis sharing
- Extended geographic visualizations
- Time series forecasting integration
- Statistical test automation
- Report scheduling

---

## Version History

| Version | Date | Highlights |
|---------|------|------------|
| 1.0.0 | 2026-01-27 | Initial release with AI-powered EDA, 50+ chart types |

---

[1.0.0]: https://github.com/DataConceptz/DataExplorerPro/releases/tag/v1.0.0
