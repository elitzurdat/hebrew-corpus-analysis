# Workshop Preparation Guide — Windows
## Hebrew Corpus Analysis with R and Stanza

Welcome! Before the workshop, please follow the steps below carefully.
The whole setup should take about 20–30 minutes, most of which is waiting
for downloads. If anything doesn't work, bring the error message to the
workshop and we'll sort it out together.

You'll need: a working internet connection, and R + RStudio already installed.

---

## Part 1 — Install Python (via Miniconda)

We use Python only as a backend for the NLP library (Stanza). You won't need
to write any Python yourself — R talks to it automatically.

**Step 1.** Download Miniconda from:
https://docs.conda.io/en/latest/miniconda.html

Get the **Windows 64-bit** installer (the `.exe` file).

**Step 2.** Run the installer. When you reach the "Advanced Options" screen,
check **"Add Miniconda to PATH"** — the installer warns against this, but
for our purposes it makes things simpler. Leave all other options as default.

**Step 3.** After installation, open **"Anaconda Prompt"** from the Start Menu
(search for it — it looks like a black terminal window). Use this for all
the commands below, not regular cmd or PowerShell.

---

## Part 2 — Create the Python environment and install Stanza

In the Anaconda Prompt, run these commands one by one.
Wait for each one to finish before running the next.

```bash
conda create -n heb_nlp python=3.10
```
*(When asked "Proceed ([y]/n)?", type `y` and press Enter)*

```bash
conda activate heb_nlp
```

```bash
pip install stanza
```

```bash
pip install torch
```

**Step 4 — Download the Hebrew language models** (~500MB, run once):

```bash
python -c "import stanza; stanza.download('he')"
```

This downloads the trained neural models for Hebrew tokenization, POS tagging,
lemmatization, and dependency parsing. It may take a few minutes.

**Step 5 — Find and save your Python path.** You'll need this for the R script:

```bash
where python
```

This will print something like:
```
C:\Users\YourName\miniconda3\envs\heb_nlp\python.exe
```

**Copy this path and save it somewhere** (a text file, a sticky note, anywhere).

---

## Part 3 — Install R packages

Open **RStudio** and paste this into the Console, then press Enter:

```r
install.packages(c(
  "reticulate",
  "readxl",
  "tidyverse",
  "scales",
  "tidytext",
  "progress",
  "showtext"
))
```

This may take a few minutes. Say yes to any prompts about installing dependencies.

---

## Part 4 — Changes to make in the R script

The script was written on a Mac. Three small things need adjusting for Windows.

### Change 1 — Python path

Find this line near the top of the script:

```r
use_condaenv("heb_nlp")
```

Replace it with your actual Python path from Step 5 above,
using **forward slashes** (not backslashes):

```r
use_python("C:/Users/YourName/miniconda3/envs/heb_nlp/python.exe")
```

> **Important:** `use_python()` must be the very first reticulate call in your
> script — before any `import()` calls. Reticulate locks in the Python version
> on first use and cannot switch mid-session. If something seems wrong, restart
> RStudio and try again.

### Change 2 — Hebrew font rendering

On Windows, ggplot2 sometimes can't render Hebrew text without a little help.
Add these lines near the top of the script, **before any plot code**:

```r
library(showtext)
font_add("Arial", "C:/Windows/Fonts/arial.ttf")
showtext_auto()
```

Then add `text = element_text(family = "Arial")` inside the `theme()` call
of any plot where Hebrew labels appear garbled, for example:

```r
theme_minimal(base_size = 13) +
theme(text = element_text(family = "Arial"))
```

### Change 3 — File path to the data file

Use an explicit path with forward slashes, or set your working directory
at the top of the script so relative paths work:

```r
# Option A: set working directory (then use filename only below)
setwd("C:/Users/YourName/Desktop/workshop")
df <- read_excel("20180613080504.xlsx")

# Option B: full explicit path everywhere
df <- read_excel("C:/Users/YourName/Desktop/workshop/20180613080504.xlsx")
```

---

## Part 5 — Sanity check before the workshop

Run this short script in RStudio to confirm everything is connected correctly.
If it runs without errors, you're all set.

```r
library(reticulate)

# Paste your actual path here
use_python("C:/Users/YourName/miniconda3/envs/heb_nlp/python.exe")

# Should print Python version and path — verify it shows heb_nlp
py_config()

# Should import without error
stanza <- import("stanza")
message("Stanza version: ", stanza$`__version__`)
message("Everything is working — see you at the workshop!")
```

If `py_config()` shows a different Python than your heb_nlp environment,
reticulate picked up the wrong installation. Fix: restart RStudio and make
sure `use_python()` is the very first line you run.

---

## Troubleshooting

| Problem | Likely cause | Fix |
|---|---|---|
| `use_python()` has no effect | Another reticulate call ran first | Restart RStudio, run `use_python()` first |
| `import("stanza")` fails | Wrong Python env | Check `py_config()`, verify path |
| Hebrew text shows as boxes in plots | Font not loaded | Add `showtext` block (Change 2 above) |
| `stanza.download('he')` fails | No internet / firewall | Try on a different network |
| Conda not found in Anaconda Prompt | PATH not set during install | Reinstall Miniconda, check the PATH option |

---

*Prepared for workshop use. Please complete all steps before the session.*
