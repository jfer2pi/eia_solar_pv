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
Array details come from the annual [Form EIA-860](https://www.eia.gov/electricity/data/eia860/),
Schedule 3_3 Solar, joined on plant and generator ID: tracking type (single-axis, dual-axis,
fixed tilt, east-west), bifacial, DC capacity and ILR, module material, and tilt/azimuth.
They appear in the site popups, the site table and the owner table (operating MWdc and
tracker share). The annual file lags about a year, so recent plants and the pipeline show
as not yet reported.

The parsed snapshots are cached to `eia860m_solar.rds` (checked for a newer month after
24 hours) and `eia860_solar_arrays.rds` (checked for a newer annual release after 30 days).
The **Check EIA for newer data** button checks both on demand.

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
