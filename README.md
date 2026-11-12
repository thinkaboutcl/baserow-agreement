# baserow-agreement

This tool measures how well raters agree with each other.
It works on a Baserow table or a CSV file.
Two or more people label the same items, and an AI tool labels them too.
The tool reports Jaccard, precision and recall.

You only need a Baserow token and a table ID.
The tool finds the columns and field types by itself.
A config file is optional.

## Install

You need R 4.3 or newer.

```bash
git clone https://github.com/thinkaboutcl/baserow-agreement.git
cd baserow-agreement
Rscript -e 'install.packages(c("tidyverse", "httr2", "jsonlite", "yaml", "testthat"))'
```

## Web app

Open the web app here:
https://thinkaboutcl.github.io/baserow-agreement/

The first visit takes up to a minute. The page loads R into your browser.
After that it is quick.

How to use it:

1. Enter the Baserow address, the table ID and your token. Click **Connect**.
2. **Check columns** shows what the tool found.
3. **Settings** lets you change prefixes, fields, filters and synonyms.
   You can also upload a settings file (YAML) instead of filling in the form.
4. **Results** runs round 1 or round 2. You can see tables and charts and download everything as a zip file.

### Settings files

You do not need a settings file. The app finds your columns by itself.
A settings file only saves you from filling in the Settings page again.

To make one:

1. Connect to your table and fill in the **Settings** page.
2. Click **Save these settings** at the bottom of that page.
   You get a file called `settings.yml`.

To use it next time:

1. Click **Upload YAML** under *Settings file* and choose your file.
2. Enter the table ID and token, then click **Connect**.
3. The Settings page is filled in for you. You can still change it.

You can share a settings file with your team so everyone uses the same rules.
The file holds settings only. It has no data and no token.
A synonyms CSV is not stored in it, so upload that again in the Settings page.

The results zip also contains `settings_used.yml`. It works the same way.
`config.example.yml` lists every setting with a short note, if you want to edit a file by hand.

### Your data stays with you

The whole app runs inside your browser. There is no server.

* Your token goes from your browser straight to Baserow. It is never saved.
* Your table is read into your browser only. It is not uploaded anywhere.
* A CSV file you choose is opened in your browser. It is not uploaded either.

### Run the web app on your own computer

```bash
Rscript -e 'install.packages(c("shiny", "bslib", "DT", "zip"))'
Rscript -e 'shiny::runApp(launch.browser = TRUE)'
```

### Publish the web app

The site is built with [Shinylive](https://posit-dev.github.io/r-shinylive/).
GitHub Actions builds and publishes it each time `main` changes.
See `.github/workflows/pages.yml`.

To build it by hand:

```bash
Rscript -e 'install.packages("shinylive")'
Rscript build_site.R
```

The site is written to the `site` folder. Only `app.R`, `R` and `www` go into it.

## Command line

The rest of this page explains the command line version.
It does the same work as the web app.

## Set up

### 1. Make a token

In Baserow, click your avatar and open **My settings**.
Go to **Database tokens** and click **Create token**.
Turn on **read** only. Leave create, update and delete off.

### 2. Save the token

Create a file called `.Renviron` in the project folder.
Put this line in it:

```
BASEROW_TOKEN=your_token_here
```

Git ignores this file. Do not share it.

### 3. Find the table ID

Open your table in Baserow and look at the address bar:

```
https://baserow.io/database/111/table/222/333
```

The number after `/table/` is the table ID. Here it is `222`.

## Run

Check what the tool found. This step shows column names and types only. It does not read any cell values.

```bash
Rscript run.R --table 222 --inspect
```

Compare the human raters:

```bash
Rscript run.R --table 222
```

Compare the humans with the AI:

```bash
Rscript run.R --table 222 --round 2
```

Results go to the `output` folder.

## Blinding

Round 1 compares the humans only. It never uses the AI columns. You can run it at any time.

Round 2 shows the AI output. Run it only after every reviewer has finished.
A reviewer who is still working must not see round 2 results.

You can make the tool check this for you. See `completion` in the config section.
With that setting, round 2 stops if any reviewer has not finished.
Add `--force` to skip the check.

## How your table should look

Use one row per item, for example one row per experiment.
Give each rater their own column for each field.
Name each column with the rater prefix and then the field name:

| Name | R1_Pathology | R2_Pathology | AI_Pathology | R1_Age | R2_Age | AI_Age |
|---|---|---|---|---|---|---|

The default prefixes are `R1_`, `R2_` and `AI_`.
You can change them in a config file.

A field is compared only if every rater has a column for it.
So columns like `R1_Notes` are skipped when there is no `AI_Notes`.

Field types come from the Baserow column type:

| Baserow type | Compared as |
|---|---|
| Multiple select, link to table | set |
| Single select, text, boolean, date | category |
| Number, rating | number |
| Long text, file | skipped |

Some tables keep lists in plain text cells, such as `Rice, Wheat`.
Set `text_as: set` in a config file. The tool then splits these cells on commas.

If the table links to another table, such as a Papers table, you also get results per paper.

## Other ways to run

From a CSV file:

```bash
Rscript run.R --csv data/export.csv
```

The CSV uses the same column names.
Cells with several values are split on commas.

From your own Baserow server:

```bash
Rscript run.R --table 222 --url https://baserow.example.org
```

All options:

```
Rscript run.R [config.yml] [--table ID | --csv FILE] [--url URL]
              [--round 1|2] [--inspect] [--force] [--out DIR]
```

The tool only reads from Baserow. It never changes your data.

## Config file

You only need a config file to change the defaults.
Copy the example and edit it:

```bash
cp config.example.yml config.yml
```

If you run the tool with no `--table` or `--csv`, it uses `config.yml`.
You can also name a config file and a table together:

```bash
Rscript run.R my_config.yml --table 222
```

Main settings:

| Setting | Default | What it does |
|---|---|---|
| `raters` | `R1_`, `R2_`, `AI_` | Rater names and their column prefixes |
| `human_raters` | `R1`, `R2` | Raters compared in round 1 |
| `model_rater` | `AI` | Rater compared in round 2 |
| `fields` | `auto` | Fields to compare. You can list them yourself |
| `exclude_fields` | none | Fields to leave out |
| `field_settings` | none | `tolerance` for numbers. `empty_as` fills empty cells |
| `units` | row id, first link column | Which column is a row and which is a paper |
| `row_filter` | none | Only compare rows that all humans marked as valid |
| `completion` | none | Stop round 2 until all humans have finished |
| `reference` | `intersection` | What counts as the right answer in round 2 |
| `empty_both` | `NA` | Score when both sides are empty. `NA` or `1` |
| `unknown_as_empty` | `false` | Treat `Unknown` as no answer |
| `text_as` | `category` | Use `set` to split text cells like `a, b` |
| `synonyms` | none | CSV file that merges different spellings |

You can keep your own settings files anywhere on your computer.
The web app downloads a `settings_used.yml` file with your results.
You can upload it again next time, or use it on the command line:

```bash
Rscript run.R settings_used.yml --table 222
```

## Output

| File | What it contains |
|---|---|
| `summary_by_field.csv` | Main result. Scores per field and for all fields together |
| `metrics_long.csv` | Scores for every item, field and comparison |
| `rows.csv` | Every row, its paper, and whether it was used |
| `plots/*.png` | Score charts for each field |

There are two levels:

* `experiment` compares one row at a time.
* `paper` joins all rows of a paper first, then compares.

There are two comparisons:

* `R1_vs_R2` compares two humans. It reports Jaccard only.
* `ref_vs_AI` compares the reference with the AI. It reports all three scores.

## How the scores work

A is the reference answer. B is the answer being checked.

* Jaccard = |A∩B| / |A∪B|. It shows how much the two answers overlap.
* Precision = |A∩B| / |B|. It shows how much of the AI answer is right.
* Recall = |A∩B| / |A|. It shows how much of the right answer the AI found.

Some special cases:

* If both sides are empty, the score is `empty_both`.
* If only one side is empty, Jaccard is 0.
* Two numbers match if they differ by no more than `tolerance`.

Text is trimmed and made lower case before comparing.

There are two ways to average:

* Macro: score each item, then take the mean.
* Micro: add up the counts from all items, then score once.

## Tests

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

## Files

| File | What it does |
|---|---|
| `app.R` | The web app |
| `www/baserow.js` | Reads Baserow from the browser for the web app |
| `build_site.R` | Builds the web app as a static website |
| `run.R` | Starts the analysis from the command line |
| `R/pipeline.R` | Runs all steps in order |
| `R/config.R` | Reads settings |
| `R/schema.R` | Finds fields and types in the table |
| `R/fetch.R` | Reads data from Baserow or CSV |
| `R/tidy.R` | Reshapes the data |
| `R/normalise.R` | Cleans values |
| `R/metrics.R` | Calculates the scores |
| `R/compare.R` | Runs round 1 and round 2 |
| `R/report.R` | Writes tables and charts |

## Data safety

The repository holds no data, tokens or results.
Git ignores `.Renviron`, `.env`, `config.yml`, `data/` and `output/`.
