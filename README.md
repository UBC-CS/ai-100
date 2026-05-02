# AI 100

The main feature of this course template is a landing page with a schedule table allowing students to navigate to all the course materials.
Website is automatically deployed using GH Actions to GitHub Pages.

## Edit the template

There are a few things you need to do to adapt this template for your course.

1. In `_variables.yml`, you can define variables that are used across the entire Quarto project using the shortcode `{{< var <key> >}}` (reference sub-keys using the dot, `.`, delimiter, e.g.,`code` listed under `course` is referenced as `course.code`; [more details](https://quarto.org/docs/authoring/variables.html#var)).
Replace the placeholders provided for the variables, for example, update `course.code` and `course.title`.
You must update `course.monday-of-the-first-term-week` to the date of the Monday of the first week of the term (format `YYYY-MM-DD`) as it will be used to calculate dates in the schedule table.

2. The `_quarto.yml` file controls [Quarto project settings](https://quarto.org/docs/projects/quarto-projects.html) including [website options (such as navigation)](https://quarto.org/docs/reference/projects/websites.html).

3. To enable the schedule table, you will need to provide week numbers and day of the week when different units of the course will happen in the `/data/schedules.csv` file.

4. In addition, any extra resources that are not Quarto documents, such as links to lecture recordings and to PrairieLearn, are provided in the file `/data/additional-resources.csv`.

## Website notes

The course schedule is dynamically generated from the files the directories `pre-activities`, `activities`, `slides` and `summaries` using R (more specifically, `render_schedule()` in `/R/render-schedule.csv`).
Having documents organized this way allows them to be formatted with `_metadata.yml` files in their directories.

- There are six different types of `<unit>`s: `part`, `week`, `lecture`, `discussion`, `potw` and `exam`.
- There are the following `<types>` of resources: `summaries`, `pre-activities`, `activities`, `slides`, `recording`, `practice` and `link`.
- All resources belonging together have a unique `<id>` consisting of their `<unit>` followed by a two-digit number, e.g., `lecture-01`.
- Files belonging to one unit should be named following the pattern: `<id>_<type>`.
- `id` is a unique identifier to join resources for all related resources to generate the schedule table.
- The titles of `part` documents are used as headings in the course schedule.

## Setup

We recommend developing content locally on your computer using [Positron](https://positron.posit.co/). Project dependencies are managed with the [Nix package manager](https://nixos.org/) and defined in `flake.nix` and `flake.lock`.
You will have to install Nix (we recommend the [Determinate Nix Installer](https://zero-to-nix.com/start/install/)), and [direnv](https://direnv.net/docs/installation.html) on your system including the Open VSX [direnv extension](https://p3m.dev/client/#/repos/openvsx/packages/mkhl.direnv/overview).
When opening the project repository, select **direnv: Open .envrc file** and while open **direnv: Allow this .envrc file** from the Command Palette.

When working on Quarto reveal.js slides, precise layout can be very challenging.
Use the installed Quarto extension [Editable](https://emilhvitfeldt.github.io/quarto-revealjs-editable/) to help. [Slidecrafting](https://slidecrafting-book.com/) is an excellent resource for general tips on how to get the most out of your slides.

Different versions of the website are rendered to different subdirectories by defining Quarto project profiles. To render a profile, it has to be added to the GitHub actions step.

| Target audience                        | Quarto project profile(s) | Website subdirectory   | Content                  | Lesson plans |
| -------------------------------------- | ------------------------- | ---------------------- | ------------------------ | ------------ |
| Student                                | `student`                 | `/`                    | Do not show future weeks | No           |
| Student with accessibility needs       | `student,access`          | `/access`              | Do not show future weeks | No           |
| Instructor                             | `instructor`              | `/instructor`          | All                      | Yes          |
| Teaching assistant (TA)                | `ta`                      | `/ta`                  | All                      | Yes          |
| Coordinator                            | `coordinator`             | `/coordinator`         | All                      | Yes          |

## Attribution

The course template can be found at <https://github.com/stephan-koenig/course-template> and is based on:

- [STA 199 by Mine Çetinkaya-Rundel](https://sta199-s24.github.io/)
- [ESPM 157 by Carl Boettinger](https://espm-157.carlboettiger.info/)
- [STA 112 by Lucy D'Agostino McGowan](https://sta-112-s24.github.io/website/)
- [PMAP 8521 by Andrew Heiss](https://evalsp25.classes.andrewheiss.com/)

Some slides design was adapted from:

- [rstudio::conf-2022 Workshop on Quarto by Tom Mock et al.](https://github.com/rstudio-conf-2022/get-started-quarto)
