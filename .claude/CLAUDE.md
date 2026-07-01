# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

The course website for **ETC3550/ETC5550 Applied Forecasting** (Monash University), a Quarto website published to <https://af.numbat.space>. Content is a mix of Markdown/Quarto (`.qmd`) pages and R code that runs at render time to generate schedules, assignment links, and figures. **Deployment is manual/local**: render the site with `make build`, commit the resulting `docs/` folder, and push to `main`. GitHub Pages serves `docs/` on `main` directly (no GitHub Actions / `gh-pages` branch).

The syllabus follows the textbook *Forecasting: Principles and Practice* (3rd ed., <https://OTexts.com/fpp3>), and R content depends on the `fpp3` package suite.

## Commands

- `make build` — render the website into `docs/` (`quarto render --profile noslides --no-clean`). The `noslides` profile excludes the beamer `slides.qmd` / `revision_slides.qmd` decks from the website render.
- `make preview` — live preview (`quarto preview`).
- `make clean` — remove `docs/`, `_freeze/`, and per-week `slides_cache`/`slides_files`. **Caution:** this deletes the committed deploy folder; always `make build` again before committing.
- Deploy: `make build`, then `git add -A && git commit && git push`. Pushing to `main` publishes; there is no build step on GitHub's side.
- Render one page: `quarto render week3/index.qmd`.
- Build a lecture slide deck (produces `weekN/slides.pdf`): `quarto render week3/slides.qmd`. Slides are **not** part of `make build`; they are rendered separately and the resulting PDFs are committed and served over URLs.

R dependencies are pinned with **renv** (`renv.lock`); `.Rprofile` activates it and points CRAN at the Posit package manager. On a fresh checkout run `renv::restore()`. `_dependencies.R` lists packages needed at render time that aren't otherwise imported.

## Architecture

### Central config: `course_info.R`
Almost every page sources this file. It is the single source of truth for the semester and drives page content dynamically:
- Defines `start_semester`, `mid_semester_break`, and builds the 12-week `schedule` tibble (topic, textbook chapter, dates) joined with `assignments.csv` and `quizzes.csv`.
- Provides the helper functions that pages call inside `#| output: asis` chunks: `show_slides(week)`, `show_activity(week)`, `show_assignments(week)`, `show_quiz(week)`, `submit(schedule, assignment)`.
- These helpers are **date-aware**: they compare `Sys.Date()` against the schedule and only reveal slides/activities/assignments once they are within ~1–2 weeks of their due date. Editing dates in `course_info.R` or the CSVs changes what is visible on the live site. When testing, remember output depends on the current date.

### Per-week structure (`week1/` … `week12/`)
Each week folder typically contains:
- `index.qmd` — the student-facing week page. Sets `week <- N`, sources `course_info.R`, then calls the `show_*` helpers.
- `slides.qmd` — beamer lecture deck (`format: presentation-beamer`), sources `setup.R` + `course_info.R`, uses `header.tex` and the local `beamerthemeMonash.sty`. Rendered separately to PDF.
- `activities.qmd` — Tuesday seminar activities.
- `exN-sol.qmd` — exercise solutions.
- Supporting R scripts (`tutorial_code.R`, `solutions.R`, etc.) and a `figs/` directory.

### Render setup
- `setup.R` — sourced by slides; loads `fpp3`, sets knitr chunk defaults (echo on for slides), and the course ggplot2 theme/colour palette (Okabe-Ito discrete, viridis continuous, Fira Sans).
- Website-wide `execute` defaults (in `_quarto.yml`) are the opposite: `echo: false`, warnings/messages off.
- `_quarto.yml` defines the navbar (Schedule menu → each `weekN/index.qmd`; Assessments menu → `assignments/*.qmd`). Adding a week or assessment page means adding it to the navbar here. It also sets `output-dir: docs` and copies `CNAME` + `.nojekyll` into the output via `resources` (both required for GitHub Pages folder deploys — the custom domain and disabling Jekyll respectively).
- The `noslides` profile lives in the **project root** `_quarto-noslides.yml` (Quarto only reads profile files from the root, not subdirectories). It re-lists the base render excludes plus the slide-deck excludes, since it may replace rather than merge the base `render` array.
- `freeze: auto` + the committed `_freeze/` cache mean pages only re-execute when their source changes. If R output looks stale, the freeze cache is why.

### Assessments (`assignments/`)
Assignment/project/competition pages (`A1.qmd`, `A2.qmd`, `A3.qmd`, `Project.qmd`, `competition.qmd`, `weekly_quizzes.qmd`). Due dates and Moodle activity IDs live in `assignments.csv` and `quizzes.csv`. The forecasting competition has its own machinery: `competition.R`, `competition_functions.R`, leaderboard/response `.rds` files, and a `moodlequiz`-based quiz (`competition_quiz.Rmd` → `competition_quiz.xml`).

## Conventions & gotchas

- The rendered site in `docs/` **is committed** — it's the deploy artifact. `.gitignore` ignores render leftovers (`.quarto/`, `*_files/`, `*_cache/`) but has a trailing `!/docs/**` un-ignore so the whole output folder (including its `site_libs/` and `*_files/` figure dirs) is tracked. Committed `slides.pdf` and `_freeze/` are also intentional.
- Reference paths from `index.qmd` files use `here::here("course_info.R")` so they work regardless of render working directory.
- Semester dates are updated each year at the top of `course_info.R`; the schedule and all reveal logic cascade from there.
