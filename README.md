# vet-dahomey
PEPFAR/WAR - FY24 DQA &amp; SIMS in Benin

## Setup 

1. Clone the repo

`cd <path-to-your-github-projects>`

`git clone https://github.com/USAID-OHA-SI/vet-dahomey.git`

`cd vet-dahomey`

2. Configure and load data

`Open the vet-dahomey.Rproj`

Run `glamr::si_setup()`

Load raw data into the `Data` folder and make sure new data file extensions are added to gitignore for exclusion

## Collaboration

Most data munging and analytics code will live within `Scripts` folder. Just make sure filenames start with 2 digits numbers starting with `00` to `99`. With `00_.*` files being used for config and or utility functions.

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
