# --- Code for https://jgassen.shinyapps.io/shiny_rdf_spec_curve/ --------------

# devtools::install_github("joachim-gassen/rdfanalysis")
library(rdfanalysis)

load(url("https://joachim-gassen.github.io/data/rdf_ests.RData"))
data_file <- file.path(tempdir(), "wb_new.csv")
download.file("https://joachim-gassen.github.io/data/wb_new.csv", data_file)

design <- c(
  "read_data",
  "select_idvs",
  "treat_extreme_obs",
  "specify_model",
  "est_model"
)
source_design(design, rel_dir = "vignettes/case_study_code")

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
  title = "A Shiny Specification Curve to Visualize Researcher Degrees of Freedom",
  abstract = paste(
    "This display demonstrates how shiny can be used to interactively explore",
    "the robustness of empirical evidence. It allows you to explore the",
    "researcher degrees of freedom that are inherit in typical inferential",
    "research designs, starting from one regression output to a specification",
    "curve that summarizes the findings from 11,264 regressions.",
    "The specification curve design is inspired by the visuals reported by",
    "<a href=https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2694998>",
    "Simonsohn, Simmons and Nelson (2019)</a>.<br><br>",
    "As a use case it reports findings for the association of national income" ,
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
