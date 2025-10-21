<h1 align="center">
  <br>
KS4 Transition Matrices dashboard
  <br>
</h1>

<p align="center">
  <a href="#introduction">Introduction</a> |
  <a href="#requirements">Requirements</a> |
  <a href="#how-to-use">How to use</a> |
  <a href="#how-to-contribute">How to contribute</a> |
  <a href="#contact">Contact</a>
</p>

---

## Introduction 

The KS4 Transition Matrices (TM) dashboard allows to explore pupil progress from Key stage 2 (KS2) to Key stage 4 (KS4) based on KS2 scaled score and KS4 achievements. 

The dashboard is split into 2 categories: 

  -	<b> Pupil progress in GCSE subjects:</b>  This section explores pupil progress from KS2-KS4 based on number of pupils entering a GCSE subjects’ grades 9-1 and KS2 scaled scores achieved.

  -  <b> Pupil progress in KS4 measures:</b>  The section explores pupil progress from KS2-KS4 based on number of pupils entering EBacc entry, EBacc achievement (9-4), EBacc achievement (9-5), English and maths (9-4), English and maths (9-5) and KS2 scaled scores achieved.

Notes:

  Data can be viewed in the format of a table or chart. It has been broken down by pupil characteristics; disadvantage, English as an additional language (EAL), free school meal eligibility (FSM), special educational needs (SEN). Figures are available at national (England) level only. Includes pupils in state-funded mainstream and special schools, hospital schools and non-maintained special schools.


---

## Requirements

You should list out the software and programming skills needed, as well as any access requirements = e.g.


### i. Software requirements (for running locally)

- Installation of R Studio 1.2.5033 or higher

- Installation of R 3.6.2 or higher

- Installation of RTools40 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R training guide](https://dfe-analytical-services.github.io/r-training-course/)

- Particularly [R Shiny](https://shiny.rstudio.com/)

---

## How to use

You should clearly lay out the steps needed to run your code here - generally, they will be similar to the below for Shiny apps:

### Data updates

#### Pre-update tasks

Around 3-7 days prior to a data update, create a new branch from main and run `renv::update()`. Then add/commit/push and create PR of that branch into main and merge in. This will make the deploy for the data update run much faster on publication day.


### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `Y` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.


### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

### Tests

UI tests have been created using shinytest that test the app loads, that content appears correctly when different inputs are selected, and that tab content displays as expected. More should be added over time as extra features are added.

GitHub Actions provide CI by running the automated tests and checks for code styling. The yaml files for these workflows can be found in the .github/workflows folder.

The tests can be run locally within RStudio by running the command ``shinytest2::test_app()``.

### Deployment

- The app is deployed to the department's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the .github/workflows folder.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function `dfeshiny::tidy_code()` can be used from the RStudio console to tidy code according to tidyverse styling using the styler package. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.

---

## How to contribute

Details on how to contribute to the app should go here, e.g.

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug.

### Merging pull requests

Only members of the Statistics Development team can merge pull requests. Add lauraselby, cjrace and sarahmwong as requested reviewers, and the team will review before merging.

---

## Contact

Email
Attainment.STATISTICS@education.gov.uk
