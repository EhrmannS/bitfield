#' Build a bit flag from a (probability density) distribution
#'
#' @param x [`data.frame(1)`][data.frame]\cr the table that contains columns
#'   with the distribution parameters.
#' @param ... bla
#' @param family [`character(1)`][character]\cr the family of the density
#'   distribution.
#' @param ... description
#' @param pos [`integerish(.)`][integer]\cr the position(s) in the bitfield that
#'   should be set.
#' @param na.val description
#' @param description description
#' @param registry description
#' @importFrom checkmate assertDataFrame assertCharacter

bf_distribution <- function(x, mean, sd, family = NULL, ..., pos = NULL,
                            na.val = NULL, description = NULL, registry = NULL){

  # https://en.wikipedia.org/wiki/Probability_density_function

  # assertDataFrame(x = x)
  # assertCharacter(x = family, len = 1, any.missing = FALSE)
  # # assertChoice(x = family, choices = c(""))
  # assertIntegerish(x = pos, lower = 1, min.len = 1, unique = TRUE, null.ok = TRUE)
  # assertIntegerish(x = na.val, lower = 0, len = 1, null.ok = TRUE)
  # assertCharacter(x = description, len = , null.ok = TRUE)
  # assertClass(x = registry, classes = "registry", null.ok = TRUE)
  #
  # if(is.null(registry)){
  #   registry <- bf_registry(name = "new_registry")
  # }
  #
  # thisName <- paste0("dist_", source)
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



  # newRegistry <- newRegistry %>%
  #   bf_grow(flags = bf_numeric(x = input, source = x, digits = 0),
  #           name = "normal_distribution.mean",
  #           desc = "the bit-representation decodes to a numeric value in column 'soandso' that represents the mean of a normal distribution",
  #           pos = 1:8, registry = .) %>%
  #   bf_grow(flags = bf_numeric(x = input, source = x, digits = 0),
  #           name = "normal_distribution.standard_deviation",
  #           desc = "the bit-representation decodes to a numeric value in column 'thisandthat' that represents the standard deviation of a normal distribution",
  #           pos = 9:16, registry = .)


}
