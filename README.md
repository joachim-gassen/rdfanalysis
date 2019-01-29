Joachim Gassen

Researcher Degree of Freedom Analysis
=====================================

A package to explore and document your degrees of freedom
---------------------------------------------------------

This experimental in development package provides a set of functions that support researchers to develop code so that it systematically documents researcher degrees of freedom. The resulting code base is self-documenting, supports unit testing and power simulations based on simulated data. The documented researcher degrees of freedom can be exhausted to generate a distribution of outcome estimates.

### First: Define your research design by a series of functions

``` r
sim_data <- function(input = NULL, choice = NULL) {
  step_description <- doc(
    "## Simulates Data",
    "### Content",
    "",
    "This step simulates some data.",
    "The used equation is $y = x + z + \\epsilon$",
    "with $z$ being N(0,1) distributed and $x$ being potentially endogenous to $z$ (see choices below)"
  )
  choice_description <- doc(
    "### Choice",
    "",
    "A list containing a character value `make_x_endogenous` and a numerical value `noise_y`.", 
    "`make_x_endogenous` may take one of the following values:",
    "",
    "- `yes`: x will depend on z",
    "- `no`: x will not depend on z",
    "",
    "`noise_y` sets the standard deviation of the error term affecting y",
    "and may take any value within [1, 2]"
  )
  choice_type <- list(
    list(name = "make_x_endogenous", 
         type = "character", 
         valid_values = c("yes", "no")),
    list(name = "noise_y", 
         type = "double", 
         valid_min = 1, valid_max = 2)
  )
  if (is.null(choice)) return(list(
    step_description = step_description,
    choice_description = choice_description,
    choice_type = choice_type
  )) else check_choice(choice, choice_type)

  z <- rnorm(1000)
  x <- rnorm(1000) 
  if (choice[[1]] == "yes") x <- x + z
  y <- x + z + rnorm(1000, sd = choice[[2]]) 
  return(list(
    data = data.frame(x = x, y = y, z = z),
    protocol = choice
  ))
}


est_model <- function(input = NULL, choice = NULL) {
  step_description <- doc(
    "## Estimate model",
    "### Content",
    "",
    "This step estimates on OLS model based on the simulated data of the last step"
  )
  choice_description <- doc(
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

  if(choice[[1]] == "yes") 
    mod <- lm(y ~ x + z, data = input$data)
  else     mod <- lm(y ~ x, data = input$data)
  return(list(
    data = list(
      est = summary(mod)$coefficient[2,1],
      lb = confint(mod)[2,1],
      up = confint(mod)[2,2]
    ),
    protocol = c(input$protocol, choice)
  ))  
} 

design <- c("sim_data", "est_model")
```

Second: Display your design and verify it by testing it
-------------------------------------------------------

``` r
prepare_design_flow_chart(design, landscape = TRUE)
```

![](README_files/figure-markdown_github/test_design-1.png)

``` r
test_design(design, reporter = "minimal")
```

    ## ......................................................

Third: Run a single protocol
============================

``` r
sim_data(NULL, list("no", 2)) %>%
  est_model("no")
```

    ## $data
    ## $data$est
    ## [1] 0.9616477
    ## 
    ## $data$lb
    ## [1] 0.8248209
    ## 
    ## $data$up
    ## [1] 1.098475
    ## 
    ## 
    ## $protocol
    ## $protocol[[1]]
    ## [1] "no"
    ## 
    ## $protocol[[2]]
    ## [1] 2
    ## 
    ## $protocol[[3]]
    ## [1] "no"

Fourth: Exhaust your research degrees of freedom
================================================

``` r
ggplot(data = df) + geom_histogram(aes(x = est))
```

![](README_files/figure-markdown_github/display_rdf-1.png)

``` r
kable(df)
```

|           | make\_x\_endogenous |  noise\_y| control\_for\_z |        est|         lb|         up|
|-----------|:--------------------|---------:|:----------------|----------:|----------:|----------:|
| result.1  | yes                 |       1.0| yes             |  1.0048866|  0.9424485|  1.0673246|
| result.2  | no                  |       1.0| yes             |  1.0383417|  0.9767014|  1.0999820|
| result.3  | yes                 |       1.1| yes             |  0.9658920|  0.8985497|  1.0332342|
| result.4  | no                  |       1.1| yes             |  1.0157834|  0.9449030|  1.0866638|
| result.5  | yes                 |       1.2| yes             |  1.0016160|  0.9230360|  1.0801961|
| result.6  | no                  |       1.2| yes             |  0.9897247|  0.9156679|  1.0637814|
| result.7  | yes                 |       1.3| yes             |  0.9831030|  0.8962750|  1.0699311|
| result.8  | no                  |       1.3| yes             |  0.9627626|  0.8820381|  1.0434870|
| result.9  | yes                 |       1.4| yes             |  1.1336476|  1.0478035|  1.2194916|
| result.10 | no                  |       1.4| yes             |  1.0119615|  0.9277144|  1.0962087|
| result.11 | yes                 |       1.5| yes             |  0.9299387|  0.8344423|  1.0254351|
| result.12 | no                  |       1.5| yes             |  1.0191141|  0.9225308|  1.1156974|
| result.13 | yes                 |       1.6| yes             |  1.1683995|  1.0711180|  1.2656809|
| result.14 | no                  |       1.6| yes             |  0.9697603|  0.8696797|  1.0698408|
| result.15 | yes                 |       1.7| yes             |  0.9969275|  0.8882535|  1.1056015|
| result.16 | no                  |       1.7| yes             |  0.9761950|  0.8696204|  1.0827697|
| result.17 | yes                 |       1.8| yes             |  0.9549287|  0.8456491|  1.0642083|
| result.18 | no                  |       1.8| yes             |  1.0440939|  0.9351197|  1.1530680|
| result.19 | yes                 |       1.9| yes             |  0.9334816|  0.8174954|  1.0494678|
| result.20 | no                  |       1.9| yes             |  0.8756892|  0.7592815|  0.9920969|
| result.21 | yes                 |       2.0| yes             |  0.9353306|  0.8112624|  1.0593989|
| result.22 | no                  |       2.0| yes             |  0.9730361|  0.8515470|  1.0945252|
| result.23 | yes                 |       1.0| no              |  1.5336397|  1.4774740|  1.5898055|
| result.24 | no                  |       1.0| no              |  0.9650359|  0.8784563|  1.0516154|
| result.25 | yes                 |       1.1| no              |  1.4607692|  1.4039432|  1.5175952|
| result.26 | no                  |       1.1| no              |  1.0578664|  0.9648209|  1.1509118|
| result.27 | yes                 |       1.2| no              |  1.4964793|  1.4349079|  1.5580506|
| result.28 | no                  |       1.2| no              |  0.9993233|  0.9058838|  1.0927628|
| result.29 | yes                 |       1.3| no              |  1.4827808|  1.4154856|  1.5500760|
| result.30 | no                  |       1.3| no              |  0.9098132|  0.8067051|  1.0129212|
| result.31 | yes                 |       1.4| no              |  1.5635726|  1.4912941|  1.6358512|
| result.32 | no                  |       1.4| no              |  0.9779360|  0.8678000|  1.0880720|
| result.33 | yes                 |       1.5| no              |  1.5000315|  1.4266130|  1.5734500|
| result.34 | no                  |       1.5| no              |  1.0246369|  0.9154209|  1.1338529|
| result.35 | yes                 |       1.6| no              |  1.4214367|  1.3419436|  1.5009299|
| result.36 | no                  |       1.6| no              |  0.9538750|  0.8312962|  1.0764538|
| result.37 | yes                 |       1.7| no              |  1.4798936|  1.3988312|  1.5609560|
| result.38 | no                  |       1.7| no              |  1.0369726|  0.9186370|  1.1553083|
| result.39 | yes                 |       1.8| no              |  1.5330719|  1.4473517|  1.6187922|
| result.40 | no                  |       1.8| no              |  1.0232446|  0.9000230|  1.1464662|
| result.41 | yes                 |       1.9| no              |  1.4592049|  1.3726319|  1.5457780|
| result.42 | no                  |       1.9| no              |  1.0629130|  0.9404191|  1.1854068|
| result.43 | yes                 |       2.0| no              |  1.5042567|  1.4078030|  1.6007104|
| result.44 | no                  |       2.0| no              |  1.0880088|  0.9518836|  1.2241340|
