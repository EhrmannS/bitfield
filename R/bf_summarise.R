#' Build a bit flag from the summary of ...
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains a column
#'   with the residuals.
#' @param sum [`character(1)`][character]\cr the column in \code{x} that ...
#' @param ... description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
#' @importFrom checkmate assertDataFrame assertSubset
#' @export

bf_summarise <- function(x, sum, ..., pos = NULL, na.val = NULL,
                         description = NULL, registry = NULL){

  # assertDataFrame(x = x)
  # assertSubset(x = sum, choices = names(x))
  # assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  # assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  # assertCharacter(x = description, len = , null.ok = TRUE)
  # assertClass(x = registry, classes = "registry", null.ok = TRUE)
  #
  # if(is.null(registry)){
  #   registry <- bf_registry(name = "new_registry")
  # }
  #
  # thisName <- paste0("sum_", source)
  #
  # # out <-
  #
  # # determine floating point encoding
  # enc <- .determineEncoding(x = out, ...)
  #
  # # replace NA values
  # if(any(is.na(out))){
  #   if(is.null(na.val)) stop("there are NA values in the bit representation, please define 'na.val'.")
  #   out[is.na(out)] <- na.val
  #   naProv <- paste0("substituteValue: NA->", na.val)
  # } else {
  #   naProv <- NULL
  # }
  #
  # # update position if it's not set
  # if(is.null(pos)){
  #   pos <- registry@width + 1L
  # } else {
  #   # include test that checks whether sufficient positions are set, and give an error if not
  # }
  #
  # # update the registry
  # registry@width <- registry@width + 1L
  # if(registry@length == 0L){
  #   registry@length <- length(out)
  # } else {
  #   if(registry@length != length(out)){
  #     stop(paste0("this flag doesn't have as many items, as there are observations in the bitfield."))
  #   }
  # }
  #
  # # update flag metadata ...
  # if(is.null(description)){
  #   description <- paste0("the bits encode the ... [", enc$sign, ".", enc$exponent, ".", enc$mantissa, "].")
  # }
  #
  # prov <- list(wasDerivedFrom = ,
  #              wasGeneratedBy = c(naProv, paste0("encodingAsBinary: ", enc$sign, ".", enc$exponent, ".", enc$mantissa, "/", enc$bias)))
  #
  # # ... and store everything in the registry
  # temp <- list(description = description,
  #              position = pos,
  #              encoding = enc,
  #              provenance = prov)
  #
  # registry@flags[[thisName]] <- temp
  #
  # # assign tentative flags values into the current environment
  # env_bind(.env = bf_env, !!thisName := out)
  #
  # return(registry)
}
