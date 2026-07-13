# SCEMS Course and Subject Roadmap

An interactive curriculum visualisation for the **School of Computing, Engineering and Mathematical Sciences (SCEMS)** at La Trobe University.

The application presents active 2026 IT and Engineering courses, majors and specialisations as subject roadmaps. It helps users explore curriculum structure, subject sequencing, prerequisites and post-requisites, while retaining direct links to the official La Trobe University Handbook.

## Live application

[Open the SCEMS Course and Subject Roadmap](https://wdloe.shinyapps.io/scems-roadmap-2026/)

> This application is a planning and visualisation aid. The official [La Trobe University Handbook](https://handbook.latrobe.edu.au/) remains the authoritative source for course rules, subject requirements, availability and enrolment advice.

## Features

### Roadmap

- Searchable course, major/specialisation and subject selectors.
- Majors and specialisations are limited to those available within the selected course.
- Subjects are organised by academic year.
- Toggle between **years as columns** and **years as rows**.
- Prerequisite relationships are displayed as directed arrows.
- Hovering over or selecting a subject highlights its prerequisite and post-requisite pathway.
- Optional filtering to show only related subjects.
- Subject nodes link to their official Handbook pages.
- Generic elective requirements are retained as roadmap placeholders.
- Colour coding distinguishes fundamental, core and elective requirements.
- Major/specialisation-only subjects use a red border.

### Subject Catalogue

- Catalogue of all subjects in the validated dataset.
- Global search and individual column filters.
- Sorting and pagination.
- Copy, CSV and Excel export.
- Direct links to official subject pages.
- Includes subject level, credit points, coordinator, AQF level, elective, exchange, capstone, academic progress review and requisite information where available.

## Dataset scope

The bundled snapshot is restricted to the **2026 academic year** and follows this rule:

> Start from active courses, include only their active areas of study, and collect the respective 2026 subjects.

Current validated dataset:

| Item | Count |
|---|---:|
| Active courses | 43 |
| Bachelor courses | 18 |
| Master courses | 25 |
| Majors and specialisations | 40 |
| Subjects | 300 |

Subject records are stored once in the central catalogue. Course and area-of-study structures reference subject codes, avoiding duplicate subject records across multiple pathways.

## Repository structure

```text
.
├── app.R
├── data/
│   └── handbook-data.json
├── www/
│   └── ltu-logo.png
├── outputs/
│   └── latrobe-it-engineering-2026-active-v2/
├── crawl.py
└── required-courses.json
```

Key files:

- `app.R` — Shiny user interface, roadmap construction and server logic.
- `data/handbook-data.json` — portable dataset used by the local and deployed application.
- `www/ltu-logo.png` — institutional logo displayed by the application.
- `outputs/latrobe-it-engineering-2026-active-v2/` — validated crawl outputs and audit reports.
- `crawl.py` — crawl orchestration entry point.

## Local setup

### Requirements

- R 4.3 or later is recommended.
- A current web browser.
- The following R packages:

```r
install.packages(c(
  "shiny",
  "jsonlite",
  "dplyr",
  "purrr",
  "tibble",
  "visNetwork",
  "DT"
))
```

### Run the application

Open the repository as the working directory in RStudio, then run:

```r
shiny::runApp()
```

Alternatively:

```r
shiny::runApp("/path/to/scems-course-subject-roadmap")
```

The application expects these deployment files to exist:

```text
app.R
data/handbook-data.json
www/ltu-logo.png
```

## Data refresh workflow

For a new academic year:

1. Crawl the new Handbook year into a separate output directory.
2. Exclude courses and areas of study marked as suspended, closed, phasing out or discontinued.
3. Validate all subject references and ownership records.
4. Review unresolved subjects before accepting any fallback records.
5. Preserve the academic year on every course, area of study and subject record.
6. Copy the final validated `handbook-data.json` into `data/`.
7. Update the year labels and version metadata in `app.R` and this README.
8. Test both roadmap orientations and the Subject Catalogue before deployment.

The current `crawl.py` imports a companion module named `crawl_latrobe`. That parser/downloader module must be present in the repository or Python environment before the crawler can run. Do not use the crawl command until that dependency has been restored and validated.

When the complete crawler is available, its intended command format is:

```bash
python3 crawl.py \
  --year 2026 \
  --output outputs/latrobe-it-engineering-2026-active
```

## Deploy to shinyapps.io

Install and configure `rsconnect` locally:

```r
install.packages("rsconnect")
```

Connect the local R session to a shinyapps.io account using the private command generated under **Account → Tokens** in the shinyapps.io dashboard. Never commit the token or secret to this repository.

Deploy only the required runtime files:

```r
rsconnect::deployApp(
  appDir = ".",
  appFiles = c(
    "app.R",
    "data/handbook-data.json",
    "www/ltu-logo.png"
  ),
  appName = "scems-roadmap-2026",
  appTitle = "SCEMS Course and Subject Roadmap"
)
```

The console should report that three files are being bundled.

## Security and repository hygiene

Do not commit:

- shinyapps.io tokens or secrets;
- `.Renviron` files;
- local credentials;
- temporary files or logs;
- `.DS_Store` files;
- `rsconnect/` deployment metadata unless the team intentionally wants to share the deployment record.

## Credits

- **Prepared for:** School of Computing, Engineering and Mathematical Sciences (SCEMS), La Trobe University
- **Developed by:** [W.Lukito@latrobe.edu.au](mailto:W.Lukito@latrobe.edu.au)
- **Primary data source:** [La Trobe University Handbook](https://handbook.latrobe.edu.au/)

## Disclaimer

Handbook content may change after a dataset snapshot is created. Users must verify all academic decisions against the official Handbook and seek formal course or enrolment advice where required.

The La Trobe University name and logo remain the property of La Trobe University and should be used in accordance with applicable institutional branding requirements.

## Licence and reuse

No open-source licence has currently been assigned to this repository. Contact the project owner before reusing, redistributing or adapting the application or bundled data.

