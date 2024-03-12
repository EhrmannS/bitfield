#' Build a bitfield from a bit registry
#'
#' @param registry [`registry(1)`][registry]\cr the bit registry that should be
#'   combined into a bitfield.
#'
#' @importFrom checkmate assertClass assertCharacter assertLogical
#' @importFrom tibble tibble
#' @importFrom purrr map map_int
#' @importFrom dplyr bind_rows arrange bind_cols select
#' @importFrom stringr str_split str_split_i str_sub
#' @export

bf_encode <- function(registry){

  assertClass(x = registry, classes = "registry")

  # open the registry
  theBitfield <- tibble(.rows = registry@length)

  # arrange them by position of the bit ...
  theBits <- bind_rows(map(seq_along(registry@flags), function(ix){
    tibble(pos = registry@flags[[ix]]$position,
           name = names(registry@flags)[ix])
  }))
  theBits <- arrange(theBits, pos)

  # ... and write into the registry
  for(i in seq_along(theBits$name)){

    if(i != 1){
      if(theBits$name[i] == theName){
        next
      }
    }

    theName <- theBits$name[i]
    theFlag <- registry@flags[[theName]]
    theVals <- bf_env[[theName]]

    temp <- .makeBits(x = theVals, encoding = theFlag$encoding)
    theBitfield <- bind_cols(theBitfield, temp, .name_repair = "minimal")

  }

  # build the integer representation
  out <- map_int(1:dim(theBitfield)[1], function(ix){

    temp <- paste0(theBitfield[ix, ], collapse = "")
    int <- str_split(temp, "")[[1]]
    sum(+(int == "1") * 2^(seq(int)-1))

  })

  return(out)

}