# dcelab 0.1.0

Initial release.

## Features

* **Scaffolding**: `create_experiment()` creates complete experiment directories from templates
* **D-efficient designs**: `prepare_experiment()` generates optimal choice designs using idefix
* **Local testing**: `run_experiment()` launches experiments locally
* **Cloud deployment**: `deploy_experiment()` deploys to shinyapps.io
* **Demo mode**: `run_demo()` for instant demonstration
* **Custom attributes**: Support for custom display functions (images, dynamic content)

## Storage Providers

* Local file storage (default)
* AWS S3
* Google Sheets
* Google Cloud Storage
* Dropbox

## Workflow

```r
library(dcelab)
create_experiment("my_study")
# Edit config.yaml and attributes.csv
prepare_experiment("my_study")
run_experiment("my_study")
deploy_experiment("my_study")
```
