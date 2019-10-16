
ListR6 <- R6::R6Class(
    classname = "ListR6",
    public = list(
        elements = list(),
        initialize = function(...){
            elements <- list(...)
            for(e in elements){
                self$elements <- append(self$elements,list(e))
            }
            invisible(self)
        },
        append = function(...){
            elements <- list(...)
            stopifnot(length(elements) > 0)
            for (e in elements) {
                self$elements <- append(self$elements,list(e))
            }
            invisible(self)
        },
        pop = function(i = NULL){
            if(is.null(i)){
                pop_index <- length(self$elements)
            } else if (is.integer(i)){
                pop_index <- i
            } else if (is.numeric(i)){
                pop_index <- round(i)
            } else if (is.list(i)){
                pop_index <- round(unlist(i))
            } else {
                stop(paste(deparse(substitute(i)),"must not be class of", class(i)))
            }
            
            pop_elements <- self$elements[pop_index]
            self$elements <- self$elements[-pop_index]
            
            
            if(length(pop_index) == 1) {
                # if pop single element, don't pop it as R6
                return(pop_elements[[1]])
            } else {
                # if pop multiple elements, pop as R6
                return_list <- ListR6$new()
                for(e in pop_elements){
                    return_list$append(e)
                }
                return(return_list)
            }

        },
        insert = function(i,x,.before = TRUE){
            stopifnot(is.numeric(i))
            i <- ifelse(is.integer(i),i,round(i))
            if(!.before) i <- i + 1
            if(i == 1){
                self$elements <- append(list(x), self$elements)
            } else if ( i == 1 + length(self$elements)){
                self$elements <- append(self$elements,list(x))
            } else if (i > 1 & i <= length(self$elements)) {
                section1 <- self$elements[1:(i-1)]
                section2 <- list(x)
                section3 <- self$elements[i:length(self$elements)]
                self$elements <- append(append(section1,section2),section3)
            } else {
                stop("subscript out of bounds")
            }
            invisible(self)
        },
        `[` = function(i){
            elements <- self$elements[i]
            return_list <- ListR6$new()
            for(e in elements){
                return_list$append(e)
            }
            return(return_list)
        },
        `[<-` = function(i,e){
            self$elements[i] <- e
            invisible(self)
        },
        `[[` = function(i){
            stopifnot(length(i) == 1)
            return(self$elements[[i]])
        },
        print = function(){
            cat("ListR6:\n")
            print(self$elements)
        }
    )
)

is.ListR6 <- function(x) "ListR6" %in% class(x)
`[.ListR6` <- function(x,i) x$`[`(i) 
`[<-.ListR6` <- function(x, i,e) x$`[<-`(i,e) 
`[[.ListR6` <- function(x,i) x$`[[`(i) 