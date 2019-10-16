
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
        extend = function(x){
            stopifnot(is.ListR6(x))
            private$flattenAppend(x$elements)
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
                return(private$flattenNew(pop_elements))
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
        reverse = function(){
            self$elements <- rev(self$elements)
            invisible(self)
        },
        `[` = function(i){
            return(private$flattenNew(self$elements[i]))
        },
        `[<-` = function(i,value){
            self$elements[i] <- value
            invisible(self)
        },
        `[[` = function(i){
            stopifnot(length(i) == 1)
            return(self$elements[[i]])
        },
        `[[<-` = function(i,value){
            stopifnot(length(i) == 1)
            self$elements[[i]] <- value
            invisible(self)
        },
        toList = function(){
            return(self$elements)
        },
        print = function(){
            cat("ListR6:\n")
            print(self$elements)
        }
    ),
    private = list(
        # flatten a list
        flattenNew = function(l){
            return_list <- ListR6$new()
            for(e in l){
                return_list$append(e)
            }
            return(return_list)
        },
        flattenAppend = function(l){
            for(e in l){
                self$append(e)
            }
            invisible(self)
        }
    ),
    active = list(
        length = function(){
            return(length(self$elements))  
        }
    )
)


# add S3 methods for ListR6 which makes it compatible with built-in functions

is.ListR6 <- function(x) "ListR6" %in% class(x)
`[.ListR6` <- function(x,i) x$`[`(i) 
`[<-.ListR6` <- function(x,i,value) x$`[<-`(i,value)
`[[<-.ListR6` <- function(x,i,value) x$`[[<-`(i,value)
`[[.ListR6` <- function(x,i) x$`[[`(i)
as.list.ListR6 <- function(x) x$elements
length.ListR6 <- function(x) x$length
names.ListR6 <- function(x) names(x$elements)