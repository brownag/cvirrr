# define process method
#' Process a CVIR Script
#' @param x a CVIRScript object
#'
#' @export
#' @aliases process
#' @rdname process
process.CVIRScript <- function(x) {
  j <- 1
  buf <- vector('list', length(x))
  for (i in 1:length(x)) {
    # print(j)

    instr <- res[[i]]
    i_n <- length(instr)

    res_numeric  <- pop(res, c(NUMERIC = 84), n = sum(instr == 84))
    res_variable <- pop(res, c(VARIABLE = 97), n = sum(instr == 97))
    res_string   <- pop(res, c(STRING_LITERAL = 220), n = sum(instr == 220))

    mode <- 0

    sql_expr_buffer <- character(0)
    informix_define <- character(0)
    informix_expr_buffer <- character(0)
    for (k in instr) {
      if (k != 0) {
        if (k == 95) {# base
          mode <- 1
        } else if (k == 96) {# table
          if (mode == 1)
            mode <- 2
        } else if (k == 97) {# variable
          if (mode == 2) { # setting base table
            x <- pop(x, k)
            base_table <- attr(x, 'data')

            if (is_NASIS_table(base_table)) {
              message("The base table is: ", base_table)
              attr(x, 'CVIR_BASE_TABLE') <- base_table
            }
            mode <- 0
          } else if (mode == 5) { # SQL select columns
            x <- pop(x, k)
            sql_column <- attr(x, 'data')

            message("Added SQL column to script data: ", sql_column)
            attr(x, 'CVIR_SQL_COLUMNS') <- c(attr(x, 'CVIR_SQL_COLUMNS'), sql_column)
          } else if (mode == 6) { # SQL FROM tables
            x <- pop(x, k)
            sql_table <- attr(x, 'data')

            base_table <- attr(x, 'CVIR_BASE_TABLE')
            if (is.null(base_table))
              base_table <- ""
            if (sql_table != base_table)
              message("Added SQL table to script data sources: ", sql_table)

            attr(x, 'CVIR_SQL_TABLES') <- c(attr(x, 'CVIR_SQL_TABLES'), sql_table)
          } else if (mode == 7) { # SQL WHERE clause
            x <- pop(x, k)
            sql_expr_buffer <- c(sql_expr_buffer, attr(x, 'data'))
          } else if (mode == 8) { # SQL JOIN/TO tables
            x <- pop(x, k)
            sql_table <- attr(x, 'data')

            base_table <- attr(x, 'CVIR_BASE_TABLE')
            if (is.null(base_table))
              base_table <- ""
            if (sql_table != base_table)
              message("Joining SQL table to base table: ", sql_table)

            attr(x, 'CVIR_SQL_JOIN_TABLES') <- c(attr(x, 'CVIR_SQL_JOIN_TABLES'), sql_table)
          } else if (mode == 10) { # informix sort by columns
            x <- pop(x, k)
            attr(x, 'CVIR_INFORMIX_SORT_BY') <- c(attr(x, 'CVIR_INFORMIX_SORT_BY'), attr(x, 'data'))
          } else if (mode == 11) { # informix aggregate columns
            x <- pop(x, k)
            attr(x, 'CVIR_INFORMIX_AGGREGATE') <- c(attr(x, 'CVIR_INFORMIX_AGGREGATE'), attr(x, 'data'))
          } else if (mode == 12) { # informix derived columns
            x <- pop(x, k)
            attr(x, 'CVIR_INFORMIX_DERIVE') <- c(attr(x, 'CVIR_INFORMIX_DERIVE'), attr(x, 'data'))
          } else if (mode == 13) { # informix derived columns
            x <- pop(x, k)
            attr(x, 'CVIR_INFORMIX_DERIVE_FROM') <- c(attr(x, 'CVIR_INFORMIX_DERIVE_FROM'), attr(x, 'data'))
          } else if (mode == 16) { # define custom columns
            x <- pop(x, k)
            new_column <- attr(x, 'data')
            attr(x, 'CVIR_INFORMIX_DEFINE') <- c(attr(x, 'CVIR_INFORMIX_DEFINE'), new_column)
            informix_define <- new_column
            mode <- 17
          } else if (mode == 17) { # defined with expressions involving other data
            x <- pop(x, k)
            informix_expr_buffer <- c(informix_expr_buffer, attr(x, 'data'))
          }

        } else if (k == 84) {# numeric
          x <- pop(x, k)
          num <- attr(x, 'numeric')
          if (mode == 7) { # SQL WHERE clause
            sql_expr_buffer <- c(sql_expr_buffer, num)
          } else if (mode == 17) {
            informix_expr_buffer <- c(informix_expr_buffer, num)
          }
        } else if (k == 220) {# string
          x <- pop(x, k)
          string <- attr(x, 'string')

          if (mode == 7) { # SQL WHERE clause
            sql_expr_buffer <- c(sql_expr_buffer, string)
          } else if (mode == 14) { # informix derived nasis group
            attr(x, 'CVIR_INFORMIX_DERIVE_NASIS_GROUP') <- c(attr(x, 'CVIR_INFORMIX_DERIVE_NASIS_GROUP'), string)
          } else if (mode == 15) { # informix derived nasis property script
            attr(x, 'CVIR_INFORMIX_DERIVE_PROPERTY_SCRIPT') <- c(attr(x, 'CVIR_INFORMIX_DERIVE_PROPERTY_SCRIPT'), string)
          } else if (mode == 17) { # informix define expression
            informix_expr_buffer <- c(informix_expr_buffer, string)
          }
        } else if (k %in% 156:167) {# NOT/ANY/ALL/ISNULL/EQ/NE/GT/LT/GTE/LTE/MATCHES/IMATCHES
          if (mode == 7) { # SQL WHERE clause
            sql_expr_buffer <- c(sql_expr_buffer, id_to_token(k))
          } else if (mode == 17) {
            informix_expr_buffer <- c(informix_expr_buffer, id_to_token(k))
          }
        } else if (k == 104) {# EXEC
          mode <- 3
        } else if (k == 105) {# SQL
          if (mode == 3)
            mode <- 4
        } else if (k == 14) { # SELECT
          if (mode == 4)
            mode <- 5
        } else if (k == 94) { # FROM
          if (mode == 5) { # SQL SELECT
            mode <- 6
          } else if (mode == 12) { # INFORMIX DERIVE
            mode <- 13
          }
        } else if (k == 16) { # WHERE
          if (mode == 6)
            mode <- 7
        } else if (k == 15) { # JOIN/TO
          if (mode == 7)
            mode <- 8
        } else if (k == 154 | k == 155 | k == 252 | k == 65) { # AND/OR/;/.
          if (mode == 8) {
            mode <- 7
          }
          if (mode == 7) {
            attr(x, 'CVIR_SQL_WHERE') <- c(attr(x, 'CVIR_SQL_WHERE'), sql_expr_buffer, id_to_token(k))
            sql_expr_buffer <- character(0)
          } else if (mode == 17) {
            expr <- attr(x, 'INFORMIX_DEFINE_EXPRESSION')
            initial <- character(0)
            if (length(informix_define) > 0)
              initial <- expr[[informix_define]]
            expr[[informix_define]] <- c(initial, informix_expr_buffer, id_to_token(k))
            attr(x, 'INFORMIX_DEFINE_EXPRESSION') <- expr
            informix_expr_buffer <- character(0)
          }
          if (k == 252 | k == 65) { # semicolon denotes end of SQL; . is end of expression
            informix_define <- character(0)
            mode <- 0
          }
        } else if (k == 109) { # informix sort
          if (mode == 0) {
            mode <- 9
          }
        } else if (k == 110) { # informix by
          if (mode == 9) {
            mode <- 10
          }
        } else if (k == 121) { # informix aggregate
          if (mode == 0 | mode == 10) {
            mode <- 11
          }
        } else if (k == 124) { # informix aggregate method
          if (mode == 11) { # informix aggregate columns
            agg_method <- id_to_token(k)

            attr(x, 'CVIR_INFORMIX_AGGREGATE') <- c(attr(x, 'CVIR_INFORMIX_AGGREGATE'), agg_method)
          }
        } else if (k == 141) { # derive informix method
          if (mode == 0) {
            mode <- 12
          }
        } else if (k == 142) { # derive informix using
          if (mode == 13) {
            mode <- 14
          }
        } else if (k == 101) {# COLON
          if (mode == 14) { # informix derive
            mode <- 15
          } else if (mode == 17) {
            informix_expr_buffer <- c(informix_expr_buffer, id_to_token(k))
          }
        } else if (k == 146 | k == 147) {# DEFINE/ASSIGN
          if (mode == 0) {
            mode <- 16
          }
        } else if (k %in% 137:138 | k == 150) { # (/)/?
          if (mode == 7) { # sql where
            attr(x, 'CVIR_SQL_WHERE') <- c(attr(x, 'CVIR_SQL_WHERE'), sql_expr_buffer, id_to_token(k))
            sql_expr_buffer <- character(0)
          } else if (mode == 17) { # informix define
            informix_expr_buffer <- c(informix_expr_buffer, id_to_token(k))
          }
        } else {
          if (mode == 17) {
            informix_expr_buffer <- c(informix_expr_buffer, id_to_token(k))
          }
        }

      }
    }

    buf[i] <- paste0(id_to_token(instr[instr != 65 & instr != 0][1:2]), collapse = " ")

    j <- j + i_n
  }
  #TODO
  debug = TRUE
  if (!debug) {
    attr(x, 'data') <- NULL
    attr(x, 'numeric') <- NULL
    attr(x, 'string') <- NULL
  }
  # attr(x, 'result') <- buf
  x
}

#' @export
#' @rdname process
process <- function(x) UseMethod("process", x)


#' "pop" data off the CVIRScript stack
#'
#' @param x a CVIRScript object
#' @param instruction one of: `"NUMERIC"`, `"VARIABLE"`, `"STRING_LITERAL"` or their bytecode equivalents (`84`, `97`, `220`)
#' @param n number of items to pop off the stack
#' @param what values to pop off the stack (default `"value"`)
#'
#' @export
#' @aliases pop
#' @rdname pop
pop.CVIRScript <- function(x, instruction, n = 1, what = "value") {
  att <- switch(as.character(instruction),
                `84` = "numeric_literals",
                `NUMERIC` = "numeric_literals",
                `97` = "data_identifiers",
                `VARIABLE` = "data_identifiers",
                `220` = "string_literals",
                `STRING_LITERAL` = "string_literals")
  ax <- attr(x, att)
  res <- head(ax, n)
  if (n >= 1) {
    ax <- ax[-(1:n),]
  }
  attr(x, att) <- ax
  attr(x, gsub("(.*)_.*","\\1", att)) <- res[[what]]
  x
}

#' @export
#' @rdname pop
pop <- function(x, instruction, n = 1, what = "value") {
  UseMethod("pop", x)
}


CVIRData.default <- function(x) UseMethod("CVIRData", x)
CVIRData.CVIRScript <- function(x) {
  attr(x, "data_identifiers")
}
CVIRString.default <- function(x) UseMethod("CVIRString", x)
CVIRString.CVIRScript <- function(x) {
  attr(x, "string_literals")
}
CVIRNumeric.default <- function(x) UseMethod("CVIRNumeric", x)
CVIRNumeric.CVIRScript <- function(x) {
  attr(x, "numeric_literals")
}
