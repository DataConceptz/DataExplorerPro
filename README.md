# DataExplorerPro

<div align="center">

**AI-Powered Data Exploration Studio for RStudio**

[![CRAN status](https://img.shields.io/badge/Version-1.0.0-blue.svg)](https://github.com/DataConceptz/DataExplorerPro)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![R Version](https://img.shields.io/badge/R-%3E%3D4.0-orange.svg)](https://www.r-project.org/)
[![Ollama](https://img.shields.io/badge/AI-Ollama-purple.svg)](https://ollama.com)
[![RStudio](https://img.shields.io/badge/IDE-RStudio-blue.svg)](https://posit.co/products/open-source/rstudio/)

[Features](#-features) • [Installation](#-installation) • [Quick Start](#-quick-start) • [Documentation](#-documentation) • [Contributing](#-contributing)

</div>

---

## Overview

DataExplorerPro is a comprehensive RStudio add-in that revolutionizes data exploration by combining the power of AI with interactive visualizations. It automatically generates comprehensive EDA reports, creates intelligent visualizations, and enables natural language queries about your data using local Ollama models.

**Key Highlights:**
- **Privacy-First AI**: All AI processing happens locally via Ollama - your data never leaves your machine
- **50+ Visualization Types**: From basic charts to advanced statistical visualizations
- **Natural Language Interface**: Ask questions in plain English, get R code and visualizations
- **One-Click EDA**: Comprehensive exploratory data analysis with AI-generated insights
- **8 Beautiful Themes**: Dark, Light, Blue, Green, Purple, Orange, Red, Monochrome

---

## Features

### Visualization Studio

#### Basic Charts
| Chart Type | Description | Best For |
|------------|-------------|----------|
| Scatter Plot | Point-by-point comparison | Relationships between two continuous variables |
| Line Chart | Connected data points | Time series, trends |
| Bar Chart | Categorical comparisons | Comparing categories |
| Histogram | Distribution visualization | Understanding data distribution |
| Box Plot | Statistical summary | Outliers, quartiles, spread |
| Violin Plot | Distribution + density | Comparing distributions across categories |
| Heatmap | Color-coded matrix | Correlations, 2D distributions |
| Density Plot | Smoothed distribution | Understanding data shape |
| Pie Chart | Proportional display | Part-to-whole relationships |

#### Advanced Statistical Charts
| Chart Type | Description | Use Case |
|------------|-------------|----------|
| Violin-Box Combo | Hybrid visualization | Detailed distribution analysis |
| Raincloud Plot | Half-violin + box + points | Publication-quality distributions |
| Ridgeline Plot | Overlapping density plots | Comparing multiple distributions |
| Beeswarm Plot | Point distribution avoiding overlap | Showing all data points |
| Forest Plot | Confidence intervals | Meta-analysis, effect sizes |
| Bland-Altman Plot | Method comparison | Agreement between measurements |
| Q-Q Plot | Quantile comparison | Normality testing |
| Volcano Plot | Fold change vs significance | Genomics, differential analysis |
| Manhattan Plot | Genome-wide association | GWAS studies |

#### Comparison Charts
- **Error Bar Plot**: Means with confidence intervals
- **Dumbbell Plot**: Before/after comparisons
- **Lollipop Chart**: Alternative to bar charts
- **Diverging Bar Chart**: Deviation from baseline

#### Time Series Charts
- **Area with Confidence Band**: Trend with uncertainty
- **Stacked Area Chart**: Cumulative time series
- **Calendar Heatmap**: Activity over time

#### Multivariate Charts
- **Bubble Chart**: Three-dimensional scatter
- **Ternary Plot**: Three-component systems
- **Sankey Diagram**: Flow visualization
- **Sunburst Chart**: Hierarchical proportions
- **Treemap**: Hierarchical area display

#### Geographic & Network Visualizations
- **Geographic Maps**: Interactive choropleth maps
- **Network Graphs**: Interactive relationship visualization
- **3D Scatter Plots**: Three-dimensional data exploration
- **Parallel Coordinates**: Multivariate comparison
- **Radar Charts**: Multi-axis comparison

### AI-Powered Features

#### Natural Language Queries
Simply type questions about your data in plain English:

```
"What are the top 10 products by sales?"
"Show me the correlation between age and income"
"Filter rows where status is 'active' and create a bar chart"
"What statistical tests should I run for this data?"
"Find outliers in the price column"
"Compare the distribution of salary across departments"
"Create a time series plot of revenue by month"
```

The AI will:
1. Interpret your question
2. Generate appropriate R/dplyr code
3. Execute the analysis
4. Present results with visualizations

#### Auto EDA Report
One-click comprehensive analysis including:
- **Data Overview**: Dimensions, types, memory usage
- **Summary Statistics**: Mean, median, quartiles, IQR for all numeric columns
- **Missing Value Analysis**: Patterns and recommendations
- **Distribution Analysis**: Histograms, density plots, normality tests
- **Correlation Matrix**: Interactive heatmap with significance
- **Outlier Detection**: IQR-based and Z-score methods
- **Categorical Analysis**: Frequency tables, bar charts
- **AI-Generated Insights**: Key findings and recommendations

#### AI Assistant Chat
- Chat conversationally about your data
- Get explanations for statistical concepts
- Ask for code suggestions and best practices
- Request specific analyses

### Data Transformation Suite

| Operation | Description |
|-----------|-------------|
| Filter | Subset rows by conditions |
| Select | Choose specific columns |
| Rename | Rename columns |
| Mutate | Create new variables |
| Summarize | Aggregate data |
| Group By | Split-apply-combine |
| Arrange | Sort rows |
| Join | Merge datasets |
| Pivot | Reshape data |

### Theme System

Choose from 8 professionally designed themes:
- **Dark**: Sleek dark mode (default)
- **Light**: Clean, bright interface
- **Blue**: Professional blue accent
- **Green**: Nature-inspired green
- **Purple**: Creative purple tones
- **Orange**: Warm, energetic orange
- **Red**: Bold red accent
- **Monochrome**: Minimalist grayscale

---

## Installation

### Prerequisites

1. **R** (version 4.0 or higher)
   ```r
   # Check your R version
   R.version.string
   ```

2. **RStudio** (recommended)
   - Download from: https://posit.co/download/rstudio-desktop/

3. **Ollama** (required for AI features)
   - **Windows**: Download from https://ollama.com/download
   - **macOS**: `brew install ollama`
   - **Linux**: `curl -fsSL https://ollama.com/install.sh | sh`

4. **Pull an Ollama Model**
   ```bash
   ollama pull llama3.2    # Recommended for general use
   ollama pull mistral     # Faster, lighter model
   ollama pull codellama   # Code-focused tasks
   ```

### Install DataExplorerPro

#### Option 1: Install from GitHub
```r
# Install devtools if needed
install.packages("devtools")

# Install DataExplorerPro
devtools::install_github("DataConceptz/DataExplorerPro")
```

#### Option 2: Install from Local Source
```r
# Clone or download this repository, then:
install.packages("path/to/DataExplorerPro", repos = NULL, type = "source")
```

#### Option 3: Use the Installation Script
```r
# Navigate to the package directory and run:
source("install.R")
```

### Verify Installation
```r
library(DataExplorerPro)

# Check if Ollama is running
check_ollama_connection()

# List available models
list_ollama_models()
```

---

## Usage

### Launch the Add-in

#### Option 1: RStudio Addins Menu
1. Open RStudio
2. Go to **Tools** → **Addins** → **Browse Addins**
3. Search for "DataExplorerPro"
4. Click **Execute**

#### Option 2: R Console
```r
library(DataExplorerPro)

# Launch via RStudio addins mechanism
addin()

# Or launch directly (bypasses RStudio)
run_app()
```

#### Option 3: Custom Port
```r
# Run on a specific port
run_app(port = 8080)
```

### Quick Start Guide

#### Step 1: Load Your Data

**Upload a file:**
- Click the **"Upload Data"** button
- Select CSV, Excel (.xlsx), RDS, or Parquet files
- Data preview appears automatically

**Use sample data:**
- Select from built-in datasets: `mtcars`, `iris`, `diamonds`, `penguins`, `gapminder`

**From R environment:**
```r
# Load data into R first
my_data <- read.csv("my_file.csv")

# Then launch DataExplorerPro
run_app()
# Your data will be available in the "R Objects" dropdown
```

#### Step 2: Explore with AI

1. Navigate to the **"Natural Language Query"** tab
2. Type your question in natural language
3. Press **Enter** or click **"Ask"**
4. View the generated code, results, and visualizations

**Example Queries:**
```
"What is the average price by product category?"
"Show me a histogram of customer ages"
"Which variables have the strongest correlation?"
"Detect and visualize outliers in the sales column"
"Compare revenue across regions using a box plot"
```

#### Step 3: Create Visualizations

1. Navigate to the **"Visualization"** tab
2. Select a **Chart Type** from the dropdown
3. Choose variables for X, Y, Color, Size, Facet
4. Customize appearance (theme, colors, labels)
5. Click **"Create Chart"**
6. Export as HTML, PNG, or JSON

#### Step 4: Generate EDA Report

1. Click the **"Auto EDA"** button in the sidebar
2. Wait for analysis to complete
3. Review the comprehensive report
4. Download as HTML for sharing

#### Step 5: Use the AI Assistant

1. Navigate to the **"AI Chat"** tab
2. Ask questions about your data or statistics
3. Get explanations and code suggestions
4. Use **Ctrl+Enter** to send messages

---

## AI Features Deep Dive

### Supported Ollama Models

| Model | Parameters | Best For | Speed |
|-------|------------|----------|-------|
| llama3.2 | 3B | General queries, code generation | Fast |
| llama3.2:1b | 1B | Quick queries, limited resources | Very Fast |
| mistral | 7B | Balanced performance | Medium |
| codellama | 7B | Code-focused tasks | Medium |
| qwen2.5 | 7B | Multilingual support | Medium |
| deepseek-coder | 6.7B | Advanced code tasks | Medium |

### AI Configuration

Access AI settings via **Settings** → **AI Settings**:

- **Model Selection**: Choose from detected Ollama models
- **Temperature**: Control response creativity (0.0-2.0)
- **Max Tokens**: Maximum response length
- **Base URL**: Custom Ollama server (default: http://localhost:11434)

### Programmatic Access

```r
# Check Ollama status
get_ollama_status()

# List available models
models <- list_ollama_models()
print(models)

# Pull a new model
pull_ollama_model("mistral")

# Natural language query
result <- query_data_natural_language(
  data = mtcars,
  query = "What is the average mpg by number of cylinders?"
)

# Generate EDA report
report <- generate_eda_report(iris)
```

---

## Configuration

### Global Options

```r
# Set default Ollama model
options(DataExplorerPro.ollama_model = "llama3.2")

# Set Ollama base URL (for remote servers)
options(DataExplorerPro.ollama_base_url = "http://localhost:11434")

# Set default theme
options(DataExplorerPro.theme = "light")

# Set sampling threshold for large datasets
options(DataExplorerPro.sample_threshold = 100000)
```

### Custom Theme

```r
library(bslib)

# Create custom Bootstrap theme
my_theme <- bs_theme(
  version = 5,
  bootswatch = "cosmo",
  primary = "#3498db",
  secondary = "#2ecc71"
)

# Run with custom theme
run_app(theme = my_theme)
```

---

## Project Structure

```
DataExplorerPro/
├── DESCRIPTION              # Package metadata and dependencies
├── NAMESPACE                # Exported functions
├── LICENSE                  # MIT License
├── README.md                # This file
├── CHANGELOG.md             # Version history
├── CONTRIBUTING.md          # Contribution guidelines
├── CODE_OF_CONDUCT.md       # Community standards
├── SECURITY.md              # Security policy
├── .gitignore               # Git ignore patterns
│
├── R/                       # Core R functions
│   ├── DataExplorerPro.R    # Package initialization
│   ├── run_app.R            # App launcher
│   ├── ollama_integration.R # AI/Ollama functions
│   ├── eda_engine.R         # EDA report generation
│   ├── visualization_engine.R  # Chart creation
│   ├── advanced_charts.R    # Advanced visualizations
│   ├── network_viz.R        # Network graphs
│   ├── geographic_viz.R     # Geographic maps
│   ├── dashboard_engine.R   # Dashboard components
│   └── performance.R        # Performance utilities
│
├── inst/
│   ├── app/                 # Shiny application
│   │   ├── ui.R             # UI definition
│   │   ├── server.R         # Server logic
│   │   └── www/             # Static assets
│   │       └── app.js       # JavaScript
│   └── rstudio/
│       └── addins.dcf       # RStudio addin definition
│
├── tests/                   # Unit tests
│   ├── testthat.R
│   └── testthat/
│       ├── test-dataexplorerpro.R
│       ├── test_visualization_engine.R
│       ├── test_network_viz.R
│       └── test_performance.R
│
├── docs/                    # Documentation
│   ├── QUICK_TOUR.md
│   ├── SHORTCUTS.md
│   └── PERFORMANCE.md
│
└── install.R                # Installation helper
```

---

## Dependencies

### Required Packages

| Package | Purpose |
|---------|---------|
| shiny | Web application framework |
| plotly | Interactive visualizations |
| dplyr | Data manipulation |
| tidyr | Data tidying |
| ggplot2 | Static visualizations |
| DT | Interactive data tables |
| bslib | Bootstrap themes |
| htmltools | HTML utilities |

### AI-Related Packages

| Package | Purpose |
|---------|---------|
| httr | HTTP requests to Ollama |
| jsonlite | JSON parsing |

### Visualization Packages

| Package | Purpose |
|---------|---------|
| igraph | Network analysis |
| visNetwork | Interactive networks |
| networkD3 | D3.js networks |

### Suggested Packages

| Package | Purpose |
|---------|---------|
| testthat | Unit testing |
| knitr | Report generation |
| rmarkdown | R Markdown support |
| webshot2 | Screenshot capture |

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+D` | Launch DataExplorerPro (configurable) |
| `Ctrl+Enter` | Send chat message |
| `Shift+Enter` | New line in chat input |
| `Esc` | Close modal dialogs |
| `?` | Open in-app help |

---

## Testing

```r
# Run all tests
testthat::test_dir("tests/testthat")

# Run specific test
testthat::test_file("tests/testthat/test_visualization_engine.R")
```

---

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

### Development Setup

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Load package for development
devtools::load_all()

# Run tests
devtools::test()

# Generate documentation
devtools::document()

# Check package
devtools::check()
```

### Code Style
- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Use roxygen2 for documentation
- Write tests for new functions

---

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## Acknowledgments

- [Ollama](https://ollama.com) - Local AI inference made easy
- [Posit (RStudio)](https://posit.co) - The best R IDE
- [Plotly](https://plotly.com/r/) - Interactive visualizations
- [Tidyverse](https://www.tidyverse.org/) - Data science tools
- [bslib](https://rstudio.github.io/bslib/) - Beautiful Bootstrap themes

---

## Contact & Support

- **Issues**: [GitHub Issues](https://github.com/DataConceptz/DataExplorerPro/issues)
- **Discussions**: [GitHub Discussions](https://github.com/DataConceptz/DataExplorerPro/discussions)
- **Documentation**: [Package Website](https://DataConceptz.github.io/DataExplorerPro/)

---

<div align="center">

**Made with ❤️ for the R Community**

[⬆ Back to Top](#dataexplorerpro)

</div>