
# ----------------------------------------------------------------------------------------------------------------------------
book.total_volumes <- function(book) {

    b <- book$bid
    a <- book$ask
    b1 <- sum(b[,"size"])
    a1 <- sum(a[,"size"])
    total <- list("bid" = b1, "ask" = a1 )
    return(total)
}

# ----------------------------------------------------------------------------------------------------------------------------
book.best_prices <- function(book) {
    
    b <- book$bid
    a <- book$ask
    bestB <- b[1,"price"]
    bestA <- a[1,"price"]
    c <- 1
    #Gets Best bid price
    while(c <= nrow(b)){
        if (b[c,"price"] > bestB){
            bestB = b[c,"price"] 
        }
        c = c + 1
    }
    c = 1
    #Gets Best ask price
    while(c <= nrow(a)){
        if (a[c,"price"] < bestA){
            bestA = a[c,"price"] 
        }
        c = c + 1
    }
    best <- list("bid" = bestB, "ask" = bestA )
    return(best)
}

# ----------------------------------------------------------------------------------------------------------------------------
book.midprice <- function(book) {
    
    bestP <- book.best_prices(book)
    b1 <- bestP$bid 
    a1 <- bestP$ask
    midprice = (a1 + b1)/2
    return(midprice)
}

# ----------------------------------------------------------------------------------------------------------------------------
book.spread <- function(book) {

    bestP <- book.best_prices(book)
    b1 <- bestP$bid 
    a1 <- bestP$ask
    spread = a1 - b1
    return(spread)
}

# ----------------------------------------------------------------------------------------------------------------------------
book.add <- function(book, message) {
    
    book = book.sort(book)
    id <- message$oid
    side <- message$side
    price = as.numeric(message$price)
    size =  as.numeric(message$size)
  
    while(size > 0){
        #Adding to Ask
        if (side == "S"){
            # if no bids available ----> Then sell request is added to ask.
            if (nrow(book$bid) == 0){
                book$ask[nrow(book$ask) + 1,] = c(id,price,size)
                size = 0
                book$ask$size = as.numeric(book$ask$size)
                book$ask$price = as.numeric(book$ask$price)
                book$bid$size = as.numeric(book$bid$size)
                book$bid$price = as.numeric(book$bid$price)
                book = book.sort(book)
                return(book)
            }
            b <- book$bid
            # if the top of bid has a price greater than or equal to the sell request's rate ----> Then sale can occur.
            if (head(b[,"price"],n=1) >= price){
                
                # if the top of bid has more volumes than the sell request's volumes ----> Then the top Buy price's volumes are decreased.
                if((head(b[,"size"],n=1)) > size){
                    reduceList = list("oid" = head(b[,"oid"],n=1),"amount" = size)
                    book = book.reduce(book,reduceList)
                    size = 0
                }
                # if the top of bid has the same volumes as the sell request's volume ----> Then sell request is not added and top of buy is removed.
                else if((head(b[,"size"],n=1)) == size){
                    reduceList = list("oid" = head(b[,"oid"],n=1),"amount" = size)
                    book = book.reduce(book,reduceList)
                    size = 0
                }
                # if the top of bid has less volumes than the sell request's volumes ----> Then the amount bought is reduced to sell request's size 
                # and counter is incremented meaning that the next top of bid can be compared and old top is removed.
                else if((head(b[,"size"],n=1)) < size){
                    reduceList = list("oid" = head(b[,"oid"],n=1),"amount" = size)
                    book = book.reduce(book,reduceList)
                    size = size - head(b[,"size"],n=1)
                }
            }
            # if the top of bid has a price less than to the sell request's rate ----> Then sale cannot occur and sell request is added to ask.
            else if (head(b[,"price"],n=1) < price) {
                book$ask[nrow(book$ask) + 1,] = c(id,price,size)
                size = 0
            }
        }
        #Adding to Bid
        if (side == "B"){
            if (nrow(book$ask) == 0){
                book$bid[nrow(book$bid) + 1,] = c(id,price,size)
                size = 0
                #print("askEmpty")
                book$ask$size = as.numeric(book$ask$size)
                book$ask$price = as.numeric(book$ask$price)
                book$bid$size = as.numeric(book$bid$size)
                book$bid$price = as.numeric(book$bid$price)
                book = book.sort(book)
                return(book)
            }
            a <- book$ask
            # if the bottom of ask has a price less than or equal to buy request's price ----> Then a Buy can occur
            if (a[1,"price"] <= price){
                
                # if the bottom of ask has less volumes than the buy request's volumes ----> then the bottom of asked is removed 
                # and the next cheapest ask priced is compared to the message price.
                if (a[1,"size"] < size){
                    reduceList = list("oid" = a[1,"oid"],"amount" = size)
                    book = book.reduce(book,reduceList)
                    size = size - (a[1,"size"])
                }
                # if the bottom of ask has the same volumes as the buy request's volumes ----> then the bottom of ask is removed
                else if (a[1,"size"] == size){
                    reduceList = list("oid" = a[1,"oid"],"amount" = size)
                    book = book.reduce(book,reduceList)
                    #print("requested size == bottom")
                    size = 0
                }
                # if the bottom of ask has more volumes than the buy request's volumes ----> Then the bottom of ask is reduced
                else if (a[1,"size"] > size){
                    reduceList = list("oid" = a[1,"oid"],"amount" = size)
                    book = book.reduce(book,reduceList)
                    #print("requested size smaller than bottom")
                    size = 0
                    
                }
            }
            # if the bottom of ask has a price greater than to the buy request's rate ----> Then sale cannot occur and buy request is added to Bid.
            else if (a[1,"price"] > price){
                book$bid[nrow(book$bid) + 1,] = c(id,price,size)
                #print("Buy request added to Bid")
                size = 0
            }
        }
    }
    book$ask$size = as.numeric(book$ask$size)
    book$ask$price = as.numeric(book$ask$price)
    book$bid$size = as.numeric(book$bid$size)
    book$bid$price = as.numeric(book$bid$price)
    book = book.sort(book)
    return(book)
    
}

# ----------------------------------------------------------------------------------------------------------------------------
book.reduce <- function(book, message) {

    b <- book$bid
    a <- book$ask
    id <- message$oid
    amount <- message$amount
    c <- 1
    # Checks if ID is in the Bid list
    while(c <= nrow(b)){
        if (b[c,"oid"] == id){
            b[c,"size"] = b[c,"size"] - amount
            if (b[c,"size"] <= 0){
                b <- b[-c(c), ]
            }
            c = nrow(b) + 1
        }
        c = c + 1
    }
    c = 1
    # Checks if ID is in the Ask list
    while (c <= nrow(a)){
        if (a[c,"oid"] == id){
            a[c,"size"] = a[c,"size"] - amount
            if (a[c,"size"] <= 0){
                a <- a[-c(c), ]
            }
            c = nrow(a) + 1
        }
        c = c + 1
    }
    book$bid = b
    book$ask = a
    return(book)
}

# ----------------------------------------------------------------------------------------------------------------------------
##############################################################################################################################################################

book.extra1 <- function(book, size) {
  
  ask <- book$ask
  askPrices <- book$ask[, "price"]
  askSize <- book$ask[, "size"]
  
  totalAsk <- sum(askSize)
  cumulativeAsk <- cumsum(askSize)

  probsList <- askSize / totalAsk
 
  randomPrice <- ask(askPrices, size = 1, prob = probsList)

  sizeBelowRandom <- sum(askSize[askPrices <= randomPrice])
 
  midprice <- (book$ask[1, "price"] + book$bid[1, "price"]) / 2
  
  midpriceEnd <- (totalAsk - size) * midprice / totalAsk
  
  if (sizeBelowRandom  >= size) {
    midpriceEnd = randomPrice
  } 
  
  else if (sizeBelowRandom  > 0) {
    sizeAbovePrice = size - sizeBelowRandom 
    topPrice = sizeAbovePrice / (totalAsk - sizeBelowRandom)
    bottomPrice = 1 - topPrice
    minprice =  book$ask[which.min(askPrices > randomPrice),"price"]
    midpriceEnd = (bottomPrice * randomPrice) + (topPrice * minprice)
  }
  return(midpriceEnd)
}
# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
book.extra2 <- function(book, size) {
    # See handout for instructions
}

book.extra3 <- function(book) {
    # See handout for instructions
}

book.extra4 <- function(book, k) {
    # See handout for instructions
}
