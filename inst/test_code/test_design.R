# --- Header -------------------------------------------------------------------
# (C) Joachim Gassen 2019, gassen@wiwi.hu-berlin.de
# See LICENSE file for details
#
# This is testthat code to test the user-specified design
# ------------------------------------------------------------------------------

for (step in d) {
  ret <- get(step)()
  context(sprintf("Testing design step %s for correctly specified step_info", step))

  test_that("step_info has the right members", {
    expect_is(ret, "list")
    expect_equal(length(ret), 3)
    expect_equal(names(ret), c("step_description",
                               "choice_description",
                               "choice_type"))
  })

  test_that("step_character and choice_description are character values", {
    for (i in 1:2) expect_is(ret[[i]], "character")
  })

  ct <- ret[[3]]

  test_that("choice_type is a list", {
    expect_is(ct, "list")
    expect_true(length(ct) >= 1)
  })

  for (i in 1:length(ct)) {
    cti <- ct[[i]]

    test_that(sprintf("choice_type[[%d]] is a list with valid members", i), {
      expect_is(cti, "list")
      expect_true(all(c("name", "type") %in% names(cti)))
      expect_is(cti$name, "character")
      expect_equal(length(cti$name), 1)
      expect_identical(make.names(cti$name), cti$name)
      expect_is(cti$type, "character")
      expect_equal(length(cti$type), 1)
      expect_true(any(c("character", "double") %in% cti$type))
    })

    if(cti$type == "character") {
      test_that(sprintf("discrete choice %s has correctly specified range of valid values", cti$name), {
        expect_true("valid_values" %in% names(cti))
        expect_is(cti$valid_values, "character")
        expect_true(length(cti$valid_values) > 1)
      })

      weighted <- "weights" %in% names(cti)
      if (weighted) {
        test_that(sprintf("weight parameter of discrete choice %s is correctly specified", cti$name), {
          expect_is(cti$weights, "numeric")
          expect_equal(length(cti$weights), length(cti$valid_values))
          expect_equal(sum(cti$weights), 1)
          expect_true(all(cti$weights >= 0))
        })
      }
    } else {
      test_that(sprintf("continous choice %s has correctly specified range of valid values", cti$name), {
        expect_true(all(c("valid_min", "valid_max") %in% names(cti)))
        expect_is(cti$valid_min, "numeric")
        expect_equal(length(cti$valid_min), 1)
        expect_is(cti$valid_max, "numeric")
        expect_equal(length(cti$valid_max), 1)
        expect_true(cti$valid_max > cti$valid_min)
      })

      weighted <- "weight_sample" %in% names(cti)
      if (weighted) {
        test_that(sprintf("weight parameter of continous choice %s is correctly specified", cti$name), {
          expect_is(cti$weight_sample, "numeric")
          expect_true(length(cti$weight_sample) >= 1)
          expect_true(all(cti$weight_sample >= cti$valid_min))
          expect_true(all(cti$weight_sample <= cti$valid_max))
        })
      }
    }
  }
}


if(exists("input")) {
  setwd(wd)
  if (!is.null(input_test_code)) {
    context(sprintf("Testing whether input data for first step %s is valid", d[1]))
    source(input_test_code, encoding = "UTF-8", local = TRUE)
  }

  for (step in d) {
    context(sprintf("Testing whether step %s produces output for each choice without warnings or errors", step))
    if (match(c(step), d) != 1) input <- output
    ct <- get(step)()$choice_type
    step_cl <- vector("list", length(ct))
    i = 1
    for (c in ct) {
      if(c$type == "character") {
        cvals <- c$valid_values
      } else cvals <- seq(from = c$valid_min, to = c$valid_max,
                          by = (c$valid_max - c$valid_min)/10)
      step_cl[[i]] <- cvals
      i = i +1
    }
    cdf <- expand.grid(step_cl, stringsAsFactors = FALSE)
    for (i in 1:nrow(cdf)) {
      params <- cdf[i, ]
      test_that(sprintf("params: %s do not generate warnings or errors", paste(params, collapse = ",")), {
        expect_warning(do.call(get(step), list(input, params)), NA)
        expect_error(do.call(get(step), list(input, params)), NA)
      })
      output <- do.call(get(step), list(input, params))
      test_that(sprintf("params: %s do not create NULL output", paste(params, sep = ",", collapse = ",")), {
        expect_false(is.null(output))
      })

      if (!is.null(output_test_code) && output_test_code[match(c(step), d)] != "")
        source(output_test_code[match(c(step), d)], encoding = "UTF-8", local = TRUE)
    }
  }
}
