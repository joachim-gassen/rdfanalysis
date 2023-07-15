#' Exhaust all Researcher Degrees of Freedom
#'
#' Parse through a research design, iterating over all possible choices, thereby
#' exhausting all documented researcher degrees of freedom. Provides the option
#' for parallel processing. Returns a data frame containing the
#' results for all choice combinations. Alternatively, you can provide a
#' database handle. In this case, only choice combinations that are not
#' present in the database are estimated
#'
#' @param d A character vector of the research design steps function names
#' @param start_input The input data for the first step.
#' @param cl Either the return value of \code{parallel::make_cluster()} or
#'   the number of cores that you want the function to start. If
#'   \code{NULL} (the default) no parallel processing is used and choices are
#'   evaluated sequentially.
#' @param db_conn_func A function that will return a \link[DBI]{DBIConnection-class}
#'   object. if not \code{NULL} the data is written to this
#'   connection and choice combinations are only estimated when they are not
#'   already present in the data. If \code{NULL} (the default) the evaluated
#'   choice combinations are returned as a data frame.
#' @param db_table A character string providing the table name to use or
#'   create under the database connection provided with \code{conn}. Ignored
#'   if \code{conn} is \code{NULL} or has exactly one table.
#' @param libs The libraries that the design steps rely on.
#' @param export The members of the environment that you want to export to the
#'   parallel cores. Defaults to all members of the global environment.
#' @param choice_df A dataframe containing the design choices that should be
#'  explored. If \code{NULL} (the default), it will be created using the
#'  parameters below.
#' @param weight Whether each step's choices should be weighted by their user
#'   assigned weights as included in the \code{choice_type}. Protocols with zero
#'   weight are excluded from the analysis. Defaults to \code{FALSE}.
#' @param est_by_cchoice Each continuous choice will be evaluated by
#'   \code{est_by_choice} equally spaced steps, staring at \code{valid_min} and
#'   ending at \code{valid_max}.
#' @param verbose Set to \code{TRUE} for some additional diagnostic output.
#'   Useful for large designs that take a while to process.
#' @return A data frame containing results for all feasible choice permutations.
#' @details See the vignette of the package for further details.
#' @examples
#' \dontrun{
#'   print("Sorry. No examples yet.")
#' }
#' @export

exhaust_design <- function(
    d, start_input, cl = NULL,
    db_conn_func = NULL, db_table = "exhausted_designs",
    libs = NULL, export = ls(globalenv()),
    choice_df = NULL, weight = FALSE, est_by_cchoice = 10,
    verbose = FALSE
) {
  i <- NULL # to make devtools:check() happy

  if (is.null(choice_df)) {
    choice_df <- generate_choice_df(d, weight, est_by_cchoice, verbose)
  }
  if (!weight) choices <- 1:ncol(choice_df)
  else choices <- 1:(ncol(choice_df) - 1)

  if (!is.null(cl)) {
    if (is.numeric(cl)) {
      if (verbose) message(
        sprintf("%s: Setting up %d clusters...", Sys.time(), cl), appendLF = FALSE
      )
      cl <- parallel::makeCluster(cl)
      if (verbose) message(" done!")
      started_clusters <- TRUE
    } else started_clusters <- FALSE

    if (verbose) message(
      sprintf(
        "%s: Preparing environment for %d clusters...", Sys.time(), length(cl)
      ),
      appendLF = FALSE
    )
    invisible({
      parallel::clusterExport(cl = cl, varlist = unique(c(export, d)))
      parallel::clusterExport(cl = cl, varlist = c("db_conn_func", "db_table"), envir = environment())
      parallel::clusterExport(cl = cl, varlist = c("libs"), envir = environment())
      parallel::clusterEvalQ(cl = cl, {
        lapply(libs, library, character.only = TRUE)
      })
    })
    if (verbose) message(" done!")
  } else started_clusters <- FALSE

  if (!is.null(db_conn_func)) {
    conn <- db_conn_func()
    if(!inherits(conn, "DBIConnection")) {
      stop("conn is not NULL but does not seem to be a DBI Connection-")
    }
    db_tnames <- DBI::dbListTables(conn)
    if (length(db_tnames) == 0) {
      if (verbose) message(sprintf(
        "%s: No table present in database. Will create table '%s'",
        Sys.time(), db_table)
      )
    } else if (length(db_tnames) == 1) {
      db_table <- db_tnames
    } else {
      if (!db_table %in% db_tnames) stop(sprintf(
        "Multiple tables found in database but table '%s' is not among them.",
        db_table
      ))
    }
    if (length(db_tnames) > 0) {
      exhausted_designs_in_db <- DBI::dbReadTable(conn, db_table)

      if (verbose) message(
        sprintf(paste(
          "%s: Writing to database table '%s'. %d choices are already present",
          "in the data and will not be re-estimated."
        ), Sys.time(), db_table, nrow(exhausted_designs_in_db))
      )
    }
    DBI::dbDisconnect(conn)
  }

  is_choice_comb_in_db <- function(i) {
    if (length(db_tnames) == 0) return(FALSE)
    sql_str <- sprintf(
      'select count(*) from "%s" t where %s',
      db_table,
      paste(
        sprintf(
          't."%s" = \'%s\'', names(choice_df)[choices], choice_df[i, choices]
        ), collapse = " and "
      )
    )
    conn <- db_conn_func()
    rv <- DBI::dbGetQuery(conn, sql_str)[1, 1]
    DBI::dbDisconnect(conn)
    rv > 0
  }


  cl_job <- function(i) {
    if (!is.null(db_conn_func)) {
      sprintf("conn inherits from DBI: %s", inherits(conn, "DBIConnection"))
      if (is_choice_comb_in_db(i)) return(NULL)
    }
    for (step in d) {
      vars <- unlist(get(step)()$choice_type)[names(unlist(get(step)()$choice_type)) %in% "name"]
      params <- choice_df[i, vars]
      if (match(c(step), d) == 1)
        input <- do.call(get(step), list(start_input, params))
      else input <- do.call(get(step), list(input, params))
    }
    if (!is.null(db_conn_func)) {
      res_row <- dplyr::bind_cols(choice_df[i,], input$data)
      conn <- db_conn_func()
      DBI::dbWriteTable(conn, db_table, res_row, append = TRUE)
      DBI::dbDisconnect(conn)
      if(length(db_tnames) == 0) db_tnames <- db_table
      return(NULL)
    }
    return(unlist(input$data))
  }

  if(exists("exhausted_designs_in_db")) choice_df <- dplyr::anti_join(
    choice_df, exhausted_designs_in_db, by = names(choice_df)[choices]
  )
  if (nrow(choice_df) > 0) {
    if (verbose) message(
      sprintf(
        "%s: Running %s researcher degrees of freedom...", Sys.time(),
        format(nrow(choice_df), big.mark = ",")
      )
    )

    results <- dplyr::bind_rows(
      pbapply::pblapply(1:nrow(choice_df), cl_job, cl = cl)
    )
    rownames(results) <- NULL
  } else if (verbose) message(
    sprintf(
      "%s: All researcher degrees of freedom are already estimated.", Sys.time()
    )
  )

  if (started_clusters) parallel::stopCluster(cl)

  if (!is.null(db_conn_func)) {
    conn <- db_conn_func()
    choice_df <- DBI::dbReadTable(conn, db_table)
    DBI::dbDisconnect(conn)
  } else  choice_df <- cbind(choice_df, results)

  attr(choice_df, "choices") <- choices
  rownames(choice_df) <- NULL
  choice_df
}
