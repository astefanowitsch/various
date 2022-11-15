ptb2synframe <- function(pstr) {

# initialize variables

# how many open parentheses (corresponds to parse level)

open_cnt <- 0

# what position in the parse string (needed for the loop to keep track of which element it is currently dealing with)

pstr_no <-0

# index of the current node (including terminals)

node_no <- 0

ptable <- data.frame(Index=character(),Name=character(),Node=character(),Level=character(),Terminal=character(),Mother=character())

dlist <- list("L0" = c("ROOT"))

# add parentheses around the terminal nodes (makes processing easier because it is more uniform)

pstr <- gsub(" ([^() ]+)","(\\1)",pstr)

# convert string to vector and remove empty elements

pstr <- unlist(strsplit(pstr, "(?=[ ()])", perl = TRUE))

pstr <- pstr[pstr != " "]

# start processing

for (sym in pstr) {

  if (sym == "(") {

    open_cnt = open_cnt +1

    pstr_no = pstr_no +1

  }else if (sym == ")"){
    
    level_name <- paste("L",open_cnt,sep="")

    dlist[[level_name]] <- dlist[[level_name]][-1]

    open_cnt = open_cnt -1

    pstr_no = pstr_no +1

  }else {
 
    node_no <- node_no + 1

    pstr_no <- pstr_no +1
    
    node_w_index <- paste(sym,node_no,sep="-")
    
    term_node <- ifelse(identical(pstr[pstr_no+1],")"),1,0)
    
    level_name <- paste("L",open_cnt,sep="")

    dlist[[level_name]] = c(node_w_index,dlist[[level_name]])
    
    mother_list <- paste("L",open_cnt-1,sep="")

    mother_node <- dlist[[mother_list]][1]

    node <- data.frame(Index=node_no, Name=sym, Node=node_w_index, Level=open_cnt, Terminal=term_node,Mother=mother_node)

    ptable <- rbind(ptable,node)
    
    }
  
  }
  
# print the result

ptable 

}