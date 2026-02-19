# Contributing to DataExplorerPro

First off, thank you for considering contributing to DataExplorerPro! It's people like you that make DataExplorerPro such a great tool.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How Can I Contribute?](#how-can-i-contribute)
- [Development Setup](#development-setup)
- [Pull Request Process](#pull-request-process)
- [Coding Standards](#coding-standards)
- [Documentation](#documentation)
- [Testing](#testing)

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code. Please report unacceptable behavior to the project maintainers.

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check the issue list as you might find that you don't need to create one. When you are creating a bug report, please include as many details as possible:

- **Use a clear and descriptive title**
- **Describe the exact steps to reproduce the problem**
- **Provide specific examples** (sample data, code snippets)
- **Describe the behavior you observed** and what you expected
- **Include screenshots** if applicable
- **Include your environment details**:
  - R version (`R.version.string`)
  - RStudio version
  - OS and version
  - Ollama version (if relevant)
  - Package versions (`sessionInfo()`)

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an enhancement suggestion:

- **Use a clear and descriptive title**
- **Provide a detailed description of the suggested enhancement**
- **Explain why this enhancement would be useful** to most users
- **Include mockups or examples** if applicable

### Contributing Code

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Run tests and linting
5. Commit your changes (`git commit -m 'Add amazing feature'`)
6. Push to the branch (`git push origin feature/amazing-feature`)
7. Open a Pull Request

## Development Setup

### Prerequisites

```r
# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2", "styler", "lintr"))
```

### Clone and Load

```bash
git clone https://github.com/DataConceptz/DataExplorerPro.git
cd DataExplorerPro
```

```r
# Load package for development
devtools::load_all()

# Run the app
run_app()
```

### Package Structure

```
DataExplorerPro/
├── R/                    # Core R functions
│   ├── DataExplorerPro.R # Package initialization
│   ├── run_app.R         # App launcher
│   ├── ollama_integration.R
│   ├── eda_engine.R
│   └── visualization_engine.R
├── inst/
│   ├── app/              # Shiny application
│   │   ├── ui.R
│   │   ├── server.R
│   │   └── www/
│   └── rstudio/
│       └── addins.dcf
└── tests/                # Unit tests
```

## Pull Request Process

1. **Update documentation** if you change functionality
2. **Add tests** for new features
3. **Update the CHANGELOG.md** with your changes
4. **Ensure the package checks pass**:
   ```r
   devtools::check()
   ```
5. **Request review** from maintainers

### PR Checklist

- [ ] Code follows the tidyverse style guide
- [ ] All tests pass (`devtools::test()`)
- [ ] Documentation is updated (`devtools::document()`)
- [ ] Package checks pass (`devtools::check()`)
- [ ] CHANGELOG.md is updated
- [ ] Commit messages are clear and descriptive

## Coding Standards

### R Style Guide

We follow the [tidyverse style guide](https://style.tidyverse.org/). Key points:

- Use `<-` for assignment, not `=`
- Use snake_case for function and variable names
- Use PascalCase for S3 classes
- Keep lines under 80 characters
- Use spaces around operators (`x + y`, not `x+y`)
- Use double quotes for strings, not single quotes

### Styling Tools

```r
# Style your code
styler::style_file("R/my_file.R")
styler::style_dir("R/")

# Lint your code
lintr::lint("R/my_file.R")
```

### Documentation

Use roxygen2 for documentation:

```r
#' Short title
#'
#' @description Longer description of the function.
#'
#' @param x Description of parameter x
#' @param y Description of parameter y
#'
#' @return Description of return value
#'
#' @examples
#' my_function(1, 2)
#'
#' @export
my_function <- function(x, y) {
  # function body
}
```

Generate documentation with:
```r
devtools::document()
```

## Documentation

### Function Documentation

All exported functions must have roxygen2 documentation including:
- Title and description
- All parameters documented
- Return value
- At least one example
- Cross-references where appropriate

### Vignettes

For longer-form documentation, create vignettes:

```r
# Create a new vignette
usethis::use_vignette("my-topic")
```

### README

Update README.md when:
- Adding new features
- Changing installation instructions
- Adding new dependencies

## Testing

### Unit Tests

We use testthat for unit testing. All new features should have tests:

```r
# Create a new test file
usethis::use_test("my-function")

# Run all tests
devtools::test()

# Run specific test
testthat::test_file("tests/testthat/test-my-function.R")
```

### Test Structure

```r
test_that("my_function works correctly", {
  expect_equal(my_function(1, 2), 3)
  expect_error(my_function("a", "b"))
  expect_warning(my_function(-1, 0))
})
```

### Code Coverage

Check test coverage:

```r
# Install covr if needed
install.packages("covr")

# Check coverage
covr::package_coverage()

# View coverage report
covr::report()
```

## Release Process

1. Update DESCRIPTION version
2. Update CHANGELOG.md
3. Run full package check
4. Create git tag
5. Submit to CRAN (if applicable)

## Getting Help

- Open an issue for bugs or feature requests
- Start a discussion for questions or ideas
- Check existing documentation and issues first

Thank you for contributing! 🎉
