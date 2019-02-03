library(tidyverse)
library(ExPanDaR)
library(rdfanalysis)
library(ggdag)

read_data <- function(input = NULL, choice = NULL) {
  read.csv("https://joachim-gassen.github.io/data/wb_condensed.csv") %>%
    select(country, year,
           lifeexpectancy, gdp_capita,
           resdevelop_gdp, unemployment) %>%
    na.omit()
}


sim_data <- function(countries = 75, es = 0.5, years = 15) {
  n <- countries * years
  df <- expand.grid(country = as.factor(1:countries), year = as.ordered((2016 - years + 1):2016))

  mm_gdp_capita <- 18000
  sd_gdp_capita <- 20000
  location <- log(mm_gdp_capita^2 / sqrt(sd_gdp_capita^2 + mm_gdp_capita^2))
  shape <- sqrt(log(1 + (sd_gdp_capita^2 / mm_gdp_capita^2)))

  base_gdp_capita <- rlnorm(countries, location, shape)
  base_unemp <- rbeta(countries, 2, 20)*100
  base_rd_gdp <- rbeta(countries, 1.5, 150)*100
  base_le <- rnorm(countries, 70, 10)
  base_le[base_le > 70] <- 70 + 0.6 * (base_le[base_le > 70] - 70)

  for (yr in (2016 - years + 1):2016) {
    if (yr == 2016 - years + 1) df$gdp_capita[df$year == yr] <- base_gdp_capita
    else df$gdp_capita[df$year == yr] <- df$gdp_capita[df$year == yr - 1] * (1 + rnorm(countries, 0.02, 0.02))
  }
  df$resdevelop_gdp <- pmax(rep(base_rd_gdp, years) + 0.1 * (log(df$gdp_capita) - mean(log(df$gdp_capita))) + rnorm(n, 0.10, 0.05), 0)
  df$unemployment <- pmax(rep(base_unemp, years) - 0.2 * (log(df$gdp_capita) - mean(log(df$gdp_capita))) + rnorm(n, 0.2, 0.2), 0)
  df$lifeexpectancy <- rep(base_le, years) + df$resdevelop_gdp - 0.5 * df$unemployment +
    es * (log(df$gdp_capita) - mean(log(df$gdp_capita))) + rnorm(n, 0.5, 0.5)

  df
}


coords <- tribble(
  ~name,               ~x,   ~y,
  "gdp_capita",         1,    2,
  "lifeexpectancy",     3,    2,
  "resdevelop_gdp",     2,    3,
  "unemployment",       2,    1
)

naive_dag <- dagify(lifeexpectancy ~ gdp_capita +
                      resdevelop_gdp +
                      unemployment,
                    resdevelop_gdp ~ gdp_capita,
                    unemployment ~ gdp_capita,
                    exposure = "gdp_capita",
                    outcome = "lifeexpectancy",
                    coords = coords)

plot(naive_dag)

design <- define_design(steps = c("define_vars", "select_vars", "outlier_treatment", "est_model"),
                        rel_dir = "case_study_code")
source_design(design, "case_study_code")
test_design(design, input = sim_data())
prepare_design_flow_chart(design, landscape = TRUE)
prepare_design_documentation(design, output_file = "case_study_design.pdf")

sim_data() %>%
  define_vars("yes") %>%
  select_vars("full") %>%
  outlier_treatment(list("win", 0.01)) %>%
  est_model(list("ctryyear", "ctryyear"))


power_df <- simulate_design_power(design, protocol = list("yes", "full",
                                                          list("win", 0.01),
                                                          list("ctryyear", "ctryyear")),
                                  input_sim_func = sim_data,
                                  range_n = seq(30, 100, 10),
                                  effect_size = 0.1)

power_df %>%
  group_by(n) %>%
  summarise(power = sum(lb > 0)/n()) %>%
  ggplot(aes(x = n, y = power)) +
  geom_line() +
  theme_minimal()

df <- exhaust_design(design, sim_data(1000, 0.1))
```
