source("R6Class.R")
source("ListR6.R")

test_that("List creation, append, pop method works",{
    testList <- ListR6$new(list(x=1,2),2)
    testList$append("a","b",list(y=2))
    # append works
    expect_identical(testList$elements,list(list(x=1,2),2,"a","b",list(y=2)))
    
    # pop works when popping single element
    pop_element <- testList$pop()
    # popped element is not a ListR6
    expect_true(!is.ListR6(pop_element))
    # element is removed from list
    expect_identical(testList$elements,list(list(x=1,2),2,"a","b"))
    # pop element itself if only pop one element
    expect_identical(pop_element,list(y=2))
    
    # pop works when popping out multiple elements
    pop_elements <- testList$pop(list(1,2))
    # elements are removed
    expect_identical(testList$elements,list("a","b"))
    # popped elements is a ListR6
    expect_true(is.ListR6(pop_elements))
    expect_identical(pop_elements$elements,ListR6$new(list(x=1,2),2)$elements)
    
    # insert before first position works
    testList$insert(1,list(x=5))
    expect_identical(testList$elements[[1]], list(x=5))
    # insert between works
    testList$insert(2,list(y=6))
    expect_identical(testList$elements[[2]], list(y=6))
    # insert after last element works
    testList$insert(4,1,.before = FALSE)
    expect_identical(testList$elements[[5]],1)
    # insert outside of bounds cause error
    expect_error(testList$insert(7,"position6"))
})

testthat::test_that("List subsetting, element extraction works",{
    # subsetting works
    testList <- ListR6$new(list(x=1,2),2,3,4,"five")
    list_subset <- testList[c(1,5)]
    expect_identical(list_subset$elements,list(list(x=1,2),"five"))
    
    # extracting element works
    expect_identical(testList[[2]],2)
    
    # assi
})
