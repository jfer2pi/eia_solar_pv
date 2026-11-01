# US Solar PV Fleet

Shiny dashboard of utility-scale solar PV in the United States, built on the U.S. EIA's
monthly generator inventory ([Form EIA-860M](https://www.eia.gov/electricity/data/eia860m/)).

- **Bubble map** of every PV site on OpenStreetMap tiles, sized by MWac. Operating sites are
  filled; pipeline sites (under construction, permitted, pending approval, early planning)
  are dashed rings.
- **Capacity trajectory**: annual additions by year online, with the cumulative fleet
  projected stage by stage (+ under construction, + permitted, + full pipeline).
- **Capacity by owner** table with operating and pipeline MWac, exportable to CSV/Excel.
- State and size breakdowns, KPI cards, and a searchable site table that flies the map to a site.
- Linear-style dark UI (bslib).

## Data

The app downloads the latest EIA-860M workbook straight from eia.gov (no API key) and reads
the `Operating` and `Planned` sheets, keeping energy source `SUN` and prime mover `PV`.
The parsed snapshot is cached to `eia860m_solar.rds`. After 24 hours the app checks only
for a newer month; the **Check EIA for a newer month** button does the same on demand.

## Run locally

```r
install.packages(c("shiny", "bslib", "httr2", "readxl", "dplyr", "tidyr",
                   "leaflet", "plotly", "DT", "htmltools"))
shiny::runApp()
```

## Deploy on Posit Connect Cloud

`manifest.json` pins R and package versions. In Connect Cloud choose **Publish → Shiny**,
pick this repo and branch `main`, and set `app.R` as the primary file.

After changing `app.R` or adding packages, regenerate the manifest before pushing,
otherwise the file checksum will not match:

```r
rsconnect::writeManifest(appFiles = "app.R")
```

## Credits

Data: U.S. Energy Information Administration, Form EIA-860M.
Map data © [OpenStreetMap](https://www.openstreetmap.org/copyright) contributors.
