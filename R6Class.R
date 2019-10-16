MapNode <- R6::R6Class(
    classname = "MapNode",
    public = list(
        position = c(NA,NA),
        g = 0,
        h = 0,
        f = 0,
        parent = NA,
        initialize = function(position = NA,parent = NA){
            if(!any(is.na(position))){
                self$position <- position
            }
            self$parent <- parent
            invisible(self)
        },
        setValue = function(g,h){
            self$g <- g
            self$h <- h
            self$f <- g + h
            invisible(self)
        },
        print = function(){
            str <- glue::glue("
                     Position: x:{self$position[1]},y:{self$position[2]}
                     Values: f:{self$f}; g:{self$g}; h:{self$h}
                     Parent: {private$parentInfo()}")
            cat(str)
        }
    ),
    private = list(
        parentInfo = function(){
            parent = self$parent
            if (R6::is.R6(parent)) {
                return(
                    sprintf("x: %s; y: %s; f: %s",
                            parent$position[1],
                            parent$position[2],
                            parent$f)
                )
            } else {
                return(NA)
            }
        }
    )
)