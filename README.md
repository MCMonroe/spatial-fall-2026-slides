# ECON 895 — Spatial Techniques in Empirical Economics

George Mason University, Fall 2026. Mondays 7:20 to 10:00 PM, Research Hall 201.
Instructor: Noel D. Johnson, njohnsoL@gmu.edu.

This repository holds the code, data, and slides for the course. Everything we run
in class is here, and it is here **before** class. Pull it before every meeting.

## Setting up, once

1. Install R and RStudio, current versions.
2. Install the packages.

   ```r
   install.packages(c("tidyverse", "sf", "terra", "s2", "units", "AER",
                      "sandwich", "lmtest"))
   ```

   `AER` arrived with Week 2, and `sandwich` and `lmtest` with Week 3, where
   the regression discontinuity takes its standard errors from `coeftest()`
   and `vcovHC()`. Those three are the ones that do not arrive as a dependency
   of the others. `haven`, `readxl` and `units` come with the tidyverse and
   `sf`.

3. Clone this repository.

   ```
   git clone https://github.com/noeldjohnson/spatial-fall-2026-slides.git
   ```

4. Open `ECON895.Rproj`. **Always open the project, not the bare script.** That
   sets the working directory to the repository root, which is why every path in
   this repo reads `data/africa_scale.shp` rather than a path from someone else's
   machine. Absolute paths are the single most common
   reason one person's code will not run on another person's computer.

5. Run `code/00_setup_test.R`. It prints seventeen checks and tells you how to fix
   any that fail. If a fix does not work, email me the console output.

## Before every class

```
git pull
```

That is the whole workflow. If you have edited a file and `pull` complains, see
"If git refuses to pull" below.

## What is in here

```
code/00_setup_test.R   check your machine can do what the course needs
code/week01.R          the code from Week 1, in slide order
code/week02.R          the code from Week 2, in slide order
code/week03.R          the code from Week 3, in slide order
data/                  everything the scripts read. See DATA_SOURCES.md
slides/                the lecture PDFs
```

Each script is organised by slide, so you can read the deck and the code side by
side. Blocks marked PREDICT are the ones we voted on in class. Commit to an answer
before you run them.

## If git refuses to pull

You edited a file that I also changed. The safe move is to keep your version under
a new name and take mine.

```
git stash            # set your changes aside
git pull             # take my version
git stash pop        # put yours back, and resolve any conflict
```

Better habit: do your own work in a file the repo does not track, such as
`scratch_yourname.R`. Anything matching `scratch_*` is ignored, so it will never
collide with mine.

## A note on what this repository is for

The course argues that a script is a complete, rerunnable record of every step from
raw data to published table, and that a sequence of clicks is not. Distributing the
course this way rather than as files in a learning management system is that
argument applied to the course itself. You are also welcome to use this structure
as a template for your own paper.
