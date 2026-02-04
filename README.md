# dcelab <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/pmarcowski/dcelab/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pmarcowski/dcelab/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

A framework for creating, configuring, and deploying Discrete Choice Experiments (DCE). Scaffolds experiment directories with configurable Shiny applications, generates D-efficient experimental designs, and handles deployment to shinyapps.io with response cloud data storage.

## Installation

```r
# Install from GitHub
devtools::install_github("pmarcowski/dcelab")
```

## Quick Start

```r
library(dcelab)

# Try it
run_demo()  # Creates and launches a demo experiment

# Or create your own:
create_experiment("my_study")

# 2. Edit the configuration files:
#    - config.yaml    → design parameters, UI settings, storage
#    - attributes.csv → attribute names and levels

# 3. Prepare (validate config & generate D-efficient design)
prepare_experiment("my_study")

# 4. Test locally
run_experiment("my_study")

# 5. Deploy to shinyapps.io
deploy_experiment("my_study", account = "your-account")
```

## Workflow

### 1. Create Experiment

```r
create_experiment("my_savings_study")
```

Creates a directory with:
```
my_savings_study/
├── app.R           # Shiny application
├── utils.R         # App utility functions
├── config.yaml     # Experiment configuration
├── attributes.csv  # Attribute definitions
├── intro.txt       # Introduction text
├── outro.txt       # Closing text
└── www/            # Static assets
```

### 2. Configure Your Experiment

Edit `config.yaml`:

```yaml
exp_id: savings_study

design:
  n_sets: 30           # Total choice sets
  n_total: 15          # Sets per participant
  n_alts: 2            # Alternatives per set
  alternatives:
    - "Plan A"
    - "Plan B"

ui:
  buttons_text: "Which savings plan do you prefer?"
  shuffle_attributes: "participant"
  explicit_choice: true
```

Edit `attributes.csv`:

```csv
Monthly savings,Employer match,Account access
1% of salary,No match,Anytime
2% of salary,1% match,After 1 year
5% of salary,2% match,After 5 years
```

### 3. Prepare & Run

```r
prepare_experiment("my_savings_study")   # Generates D-efficient design
run_experiment("my_savings_study")       # Opens in browser
```

### 4. Deploy

```r
# Deploy to shinyapps.io
deploy_experiment("my_savings_study", account = "your-account")
```

## Data Storage

Configure one or more storage providers in `config.yaml`:

| Provider | Package | Config Fields |
|----------|---------|---------------|
| `local` | (built-in) | `path` |
| `s3` | aws.s3 | `bucket`, `prefix`, `region`, `access_key`, `secret_key` |
| `gsheets` | googlesheets4 | `sheet_id`, `sheet_name` (optional) |
| `dropbox` | rdrop2* | `path` |
| `gcs` | googleCloudStorageR | `bucket`, `prefix` |

*rdrop2 archived on CRAN; install from GitHub: `remotes::install_github("karthik/rdrop2")`

### Example: Local + Google Sheets

```yaml
storage:
  local:
    path: "data/responses"
  gsheets:
    sheet_id: "1abc..."
```

## Custom Attributes

Add custom display functions by creating `custom.R`:

```r
display_image <- function(context) {
  level <- context$choice_set["Product", context$col_index]
  sprintf('<img src="images/%s.png" width="100">', level)
}
```

Then reference in `config.yaml`:

```yaml
custom_attributes:
  product_image:
    function_name: "display_image"
    attribute_label: "Product Image"
```

## API Reference

| Function | Description |
|----------|-------------|
| `create_experiment()` | Scaffold new experiment directory |
| `prepare_experiment()` | Validate config & generate design |
| `run_experiment()` | Run locally for testing |
| `deploy_experiment()` | Deploy to shinyapps.io |
| `validate_config()` | Validate configuration |
| `save_experiment_data()` | Save response data |

## Dependencies

- [idefix](https://CRAN.R-project.org/package=idefix) - D-efficient design generation
- [shiny](https://shiny.rstudio.com/) - Web application framework
- [rsconnect](https://rstudio.github.io/rsconnect/) - Deployment to shinyapps.io
- [aws.s3](https://github.com/cloudyr/aws.s3) - S3 storage integration

## License

This project is licensed under the GNU General Public License v3.0 - see LICENSE for details.

## Authors

- Przemyslaw Marcowski (p.marcowski@gmail.com)
- Jakub Krawiec (krawiecjm@gmail.com)
