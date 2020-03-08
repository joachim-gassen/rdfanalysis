# --- Some Code for testing and playing around with the package ----------------

# --- Toy workflow example from the README -------------------------------------

# devtools::install_github("joachim-gassen/rdfanalysis")
library(rdfanalysis)

sim_data <- function(n, effect_size) {
  z <- rnorm(n)
  x <- rnorm(n) + z
  y <- effect_size*x + z + rnorm(n)
  data.frame(x = x, y = y, z = z)
}

est_model <- function(input = NULL, choice = NULL) {
  step_description <- c(
    "## Estimate model",
    "### Content",
    "",
    "This step estimates on OLS model based on simulated data."
  )
  choice_description <- c(
    "### Choice",
    "",
    "A character value `control_for_z` that may take one of the following values:",
    "",
    "- `yes`: control for z",
    "- `no`: do not control for z"
  )
  choice_type <- list(
    list(name = "control_for_z",
         type = "character",
         valid_values = c("yes", "no"))
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)

  # ___ Analysis code starts below ___
  if(choice[[1]] == "yes")
    mod <- lm(y ~ x + z, data = input)
  else     mod <- lm(y ~ x, data = input)
  return(list(
    data = list(
      est = summary(mod)$coefficient[2,1],
      lb = confint(mod)[2,1],
      ub = confint(mod)[2,2]
    ),
    protocol = list(choice)
  ))
}

design <- c("est_model")
prepare_design_documentation(design, output_file = "my_design.pdf")
prepare_design_flow_chart(design, landscape = TRUE)
test_design(design, input = sim_data(100, 0.1), reporter = "minimal")
sim_data(100, 0.1) %>%
  est_model("yes")

power_df <- simulate_design_power(design, protocol = list("yes"),
                                  input_sim_func = sim_data,
                                  range_n = seq(100, 1000, 100),
                                  effect_size = 0.1)

library(tidyverse)
power_df %>%
  group_by(n) %>%
  summarise(power = sum(lb > 0)/n()) %>%
  ggplot(aes(x = n, y = power)) +
  geom_line()

exhaust_design(design, sim_data(100, 0.1))


# --- Using the case study from the vignette -----------------------------------

# devtools::install_github("joachim-gassen/rdfanalysis")
library(rdfanalysis)
library(readr)
library(stargazer)
library(tidyverse)
library(ExPanDaR)

design <- c(
  "read_data",
  "select_idvs",
  "treat_extreme_obs",
  "specify_model",
  "est_model"
)
source_design(design, rel_dir = "vignettes/case_study_code")

data_file <- file.path(tempdir(), "wb_new.csv")
download.file("https://joachim-gassen.github.io/data/wb_new.csv", data_file)

res <-
  read_data(data_file, "yes") %>%
  select_idvs("full") %>%
  treat_extreme_obs(list("win", 0.01)) %>%
  specify_model("level-log") %>%
  est_model(list("ctryyear", "ctryyear"))

stargazer(res$model, type = "text")

weighted_ests <- exhaust_design(design, data_file, weight = TRUE)

plot_rdf_spec_curve(weighted_ests, est = "est", lb = "lb", ub = "ub", pt_size = 2)

shiny_rdf_spec_curve(weighted_ests, list("est", "lb", "ub", pt_size = 2))

shiny_rdf_spec_curve(weighted_ests, list("est", "lb", "ub"),
                     design, "vignettes/case_study_code",
                     start_input = "wb_new.csv", add_files = data_file,
                     regression_cutoff = 8)

# The below takes a while as it will exhaust all 11,264 protocols...
# est <- exhaust_design(design, data_file)
# feel free to just load the final data instead
load(url("https://joachim-gassen.github.io/data/rdf_ests.RData"))

# Plot all 11,264 protocols, highlightening the ones with positive weight
# This will take a while to plot...
df <- weighted_ests[, attr(df, "choices")]
highlight_df <- df[, 1:7]
plot_rdf_spec_curve(ests, est = "est", lb = "lb", ub = "ub",
                    highlight = highlight_df,
                    choice_ind_point = FALSE)

# --- Code for shinyapps.io -------------------------

devtools::install_github("joachim-gassen/rdfanalysis")
library(rdfanalysis)

load(url("https://joachim-gassen.github.io/data/rdf_ests.RData"))
design <- c(
  "read_data",
  "select_idvs",
  "treat_extreme_obs",
  "specify_model",
  "est_model"
)
source_design(design, rel_dir = "vignettes/case_study_code")

data_file <- file.path(tempdir(), "wb_new.csv")
download.file("https://joachim-gassen.github.io/data/wb_new.csv", data_file)

shiny_rdf_spec_curve(
  ests, list("est", "lb", "ub", choice_ind_point = FALSE,
             est_label = "GDP effect [years]"),
  design, "vignettes/case_study_code",
  start_input = "wb_new.csv",
  libs = list("ExPanDaR"), add_files = data_file,
  default_choices = list(na.omit = "no",
                         idvs = "full",
                         outlier_tment_style ="win",
                         outlier_cutoff = 0,
                         model_type = "level-log",
                         feffect = "ctryyear",
                         cluster = "ctryyear"),
  title = "A Shiny Specification Curve for the Link of GDP with Life Expectancy",
  abstract = paste(
    "This display reports findings for the association of national income" ,
    "with life expectancy. Data is as provided by the",
    "<a href=https://data.worldbank.org> World Bank</a>.",
    "This association has become known as the",
    "<a href=https://en.wikipedia.org/wiki/Preston_curve> Preston Curve</a>",
    "(<a href=https://www.tandfonline.com/doi/abs/10.1080/00324728.1975.10410201>Preston, Pop Studies, 1975</a>).",
    "It is generally perceived to be robustly positive. In this display,",
    "country year panel data ranging from 1960 to 2016 is being used in",
    "a regression setup with life expectancy in years at birth ('lifeexpectancy')",
    "as dependent variable and national income per inhabitant ('gdp_capita') as",
    "independent variable of interest. Potential control variables are",
    "the mean years of schooling ('mn_yrs_school') and the unemployment rate",
    "('unemployment').<br><br>",
    "The specification curve design is inspired by the visuals reported by",
    "<a href=https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2694998>",
    "Simonsohn, Simmons and Nelson (2019)</a>.<br><br>",
    "Use the choices in the sidebar to explore how the coefficient estimate of",
    "'gdp_capita' changes when you vary the specifications of the analysis.",
    "When the specification curve visual is displayed instead of regression",
    "results the coefficients are standardized to measure the 'effect' of a 10%",
    "increase in GDP on life expectancy in years to ease comparison across",
    "various model setups. Results are not meant to be interpreted as causal.<br>"
  ),
  choice_labels = c(
    "Omit observations with missing data",
    "Independent variables to include",
    "Outlier treatment method",
    "Outlier identification percentile",
    "Regression model (log or level values for y-x)",
    "Regression fixed effects",
    "Clustering of standard errors"
  )
)
