# Project:  CS 330
# Program:  Pathfinding
# Purpose:  Provide example implementations of Dijkstra's and A* pathfinding algorithms.
# Author:   Mikel D. Petty, Ph.D., 256-824-6140, pettym@uah.edu
# Modified by Jay Sebastian, adjunct lecturer, jms0147@uah.edu
# Created:  2019-3-3
# Modified: 2022-10-16
# Source:   I. Millington, Artificial Intelligence for Games, Third Edition,
#           CRC Press, Boca Raton FL, 2019.

# Modificiations for v21 include adding options to show Astar iterations and to only run Astar for paths shown in Lecture 15. 

rm(list = ls())      # Clear workspace
options(scipen=999)  # Suppress scientific notation

#----------------------------------------------------------------

# Set run control parameters.

run.Dijkstras    <- FALSE  # Run Dijkstra's pathfinding
run.Astar        <- FALSE  # Run A* pathfinding - will run every possible path between all nodes
plot.before      <- FALSE  # Plot graph after initialization, before pathfinding
compare.paths    <- FALSE  # Compare paths produced by Dijkstra's and A*; ignored if run.Dijkstras = FALSE or run.Astar = FALSE
plot.single      <- FALSE  # Plot a single path after pathfinding
run.AstarExample <- TRUE   # Run A* pathfinding for only those paths in Lecture 15
showAstarIterations <- FALSE # Flag for showing every iteration of Astar
path.to.plot     <- 1     # Ignored if plot.path = FALSE

# Select scenario.

scenario      <- 4  # 1=Wikipedia 1, 2=Wikipedia 2a, 3=Wikipedia 2b, 4=Adventure Bay, 5=Adventure Bay Tactical
scenario.name <- c("Wikipedia W1", "Wikipedia W2a", "Wikipedia W2b", "Adventure Bay AB", "Adventure Bay Tactical ABT")[scenario]

# Define support functions.

write.text <- function(textfile, msg, first=FALSE) { write(msg, textfile, ncolumns=1, append=!first) }
num.width  <- function(x, left, right) { format(round(x, right),  nsmall=right, width=(left + right + ifelse(right > 0, 1, 0))) }

# Initialize file path and trace file; report program begin to trace file.

computer   <- 2  # 1=Alfa, 2=Bravo
file.path  <- c("C:/Users/mpetty/Desktop/Working, CS 330/", "Z:/UAH/CS330 Fall 2022/JMS Fall 2022/Programming Assignment 3 - JMS/")[computer]
trace.file <- paste(file.path, "CS 330, Pathfinding, Trace ", scenario.name, " ", Sys.Date(), ".txt", sep="")

write.text(trace.file, paste("CS 330, Pathfinding, Begin", Sys.time(), "\n"), TRUE)

#----------------------------------------------------------------

# Initialize constants used by pathfinding algorithms.

INFINITY  <- Inf
UNDEFINED <- 0
UNVISITED <- 1
OPEN      <- 2
CLOSED    <- 3

# Initialize constants used for indexes in graph nodes data structure.
# Constants start at 3 because first two columns loaded from input nodes file are not used by algorithms.

STATUS    <-  3  # Status of node; UNVISITED, OPEN, or CLOSED
COSTSOFAR <-  4  # Cost of shortest path found so far to this node
HEURISTIC <-  5  # Estimated heuristic cost
TOTAL     <-  6  # Estimated total cost
PREVIOUS  <-  7  # Previous node in path from start to this node
LOC.X     <-  8  # Location (position) x coordinate
LOC.Z     <-  9  # Location (position) z coordinate
PLOT.NBR  <- 10  # Plotting position for node number (1=below, 2=left, 3=above, 4=right)
PLOT.POS  <- 11  # Plotting position for node name (1=below, 2=left, 3=above, 4=right)
PLOT.NAME <- 12  # Node name

# Initialize constants used for indexes in graph connections data structure.
# Constants start at 3 because first two columns loaded from input nodes file are not used by algorithms.

FROM.NODE <-  3  # Connection from node number
TO.NODE   <-  4  # Connection to node number
COST      <-  5  # Connection cost
COST.POS  <-  6  # Estimated total cost
TYPE      <-  7  # Previous node in path from start to this node

#----------------------------------------------------------------

# Find shortest paths from first node to all other nodes using Dijkstra's algorithm.

Dijkstra.find.paths <- function(graph, first) {
  write.text(trace.file, paste("\n", "Dijkstra's from ", first, " to all", sep=""))
  for (i in 1:graph$n) {
    graph$nodes[i, STATUS]    <- UNVISITED
    graph$nodes[i, COSTSOFAR] <- INFINITY
    graph$nodes[i, PREVIOUS]  <- UNDEFINED
  }
  graph$nodes[first, STATUS]    <- OPEN
  graph$nodes[first, COSTSOFAR] <- 0
  open.list                     <- first
  
  while (length(open.list) > 0) {
    smallest.node <- NA
    smallest.cost <- INFINITY
    for (i in open.list) {
      if (graph$nodes[i, COSTSOFAR] < smallest.cost) {
        smallest.node <- i
        smallest.cost <- graph$nodes[i, COSTSOFAR]
      }
    }
    current.node <- smallest.node
    open.list    <- open.list[open.list != current.node]

    current.connections <- numeric(0)
    for (i in 1:graph$m) {
      if (graph$connections[i, FROM.NODE] == current.node) {
        current.connections <- c(current.connections, i)
      }
    }

    for (connection in current.connections) {
      to.node      <- graph$connections[connection, TO.NODE]
      current.cost <- graph$nodes[current.node, COSTSOFAR] + graph$connections[connection, COST]
      if (current.cost < graph$nodes[to.node, COSTSOFAR]) {
        graph$nodes[to.node, STATUS]    <- OPEN
        graph$nodes[to.node, COSTSOFAR] <- current.cost
        graph$nodes[to.node, PREVIOUS]  <- current.node
        open.list <- union(open.list, to.node)
      }
    }

    graph$nodes[current.node, STATUS] <- CLOSED
  }

  return(graph)
}

#----------------------------------------------------------------

# Find shortest path from first node to last node using A* algorithm.
# This implementation uses Node Array A*, described in [Millington, 2019] section 4.3.7 pp. 228-230.

# Find the OPEN node with the lowest total cost.
# If more than one open node has the lowest total cost, the lowest numbered node is returned.

Astar.find.lowest <- function(graph, open.nodes) {
  lowest.total   <- min(graph$nodes[open.nodes, TOTAL])                    # Determine lowest total cost of all open nodes
  result.indexes <- which(graph$nodes[open.nodes, TOTAL] == lowest.total)  # Find indexes of all open nodes with lowest total cost
  result         <- open.nodes[min(result.indexes)]                        # Find node number of lowest total cost open node with lowest index
  return(result)
}

# Calculate heuristic value using standard Euclidean 2D distance.

Astar.heuristic <- function(graph, node.1, node.2) {
  distance <- sqrt((graph$nodes[node.2, LOC.X] - graph$nodes[node.1, LOC.X])^2 +
                   (graph$nodes[node.2, LOC.Z] - graph$nodes[node.1, LOC.Z])^2)
  return(distance)
}

# Get all outgoing connections for current.node.

Astar.get.connections <- function(graph, current.node) {
  result <- which(graph$connections[, FROM.NODE] == current.node)
  return(result)
}

# Display status of current iteration.

Astar.show.iteration.status <- function(graph, iteration, current=NULL) {
  fill.symbols <- c(".", "O", "X")
  fill.display <- rep("?", graph$n)
  fill.display[1:graph$n] <- fill.symbols[graph$nodes[1:graph$n, STATUS]]
  if (!is.null(current)) { fill.display[current] <- "C" }

  write.text(trace.file, paste(
    "    ",   num.width(iteration, 2, 0),
    "     ",  num.width(length(which(graph$node[, STATUS] == UNVISITED)), 2, 0),
    "     ",  num.width(length(which(graph$node[, STATUS] == OPEN)),      2, 0),
    "     ",  num.width(length(which(graph$node[, STATUS] == CLOSED)),    2, 0), 
    "     ",  paste(fill.display, collapse=""), sep=""))
}

# Find path from start node (first) to goal node (last) using A*.

Astar.find.path <- function(graph, first, last) {
  if (showAstarIterations == TRUE) {
    write.text(trace.file, paste("\n", "A* from ", first, " to ", last, sep=""))
    write.text(trace.file, paste("  itera  unvis   open  closed"))
  }

  # Initialize node array.

  for (i in 1:graph$n) {
    graph$nodes[i, STATUS]    <- UNVISITED
    graph$nodes[i, COSTSOFAR] <- INFINITY
    graph$nodes[i, PREVIOUS]  <- UNDEFINED
  }

  # Initialize start node (first) and show initial status, before first iteration. 

  graph$nodes[first, STATUS]    <- OPEN
  graph$nodes[first, COSTSOFAR] <- 0
  iteration                     <- 0
  open.nodes                    <- first  # List (actually an R vector) of nodes currently OPEN.

  if (showAstarIterations == TRUE) {
    Astar.show.iteration.status(graph, iteration)
  }

  # Main loop; execute once for each node, or until path is found.

  while (length(open.nodes) > 0) {
    iteration <- iteration + 1

    # Select current node; end main loop if path has been found.

    current.node <- Astar.find.lowest(graph, open.nodes)
    if (current.node == last) {
      if (showAstarIterations == TRUE) {
      Astar.show.iteration.status(graph, iteration, current.node)  # Show status with goal node as current.
      }
      break                                                        # Goal node reached, end main loop.
    }

    # Get outgoing connections (as indexes into connections array) for current node.

    current.connections <- Astar.get.connections(graph, current.node)

    # Inner loop; execute once for each outgoing connection of current node.

    for (connection in current.connections) {
	  # to.node is node at other end of connection from current node (which is from node of the connection).
	  # to.cost is sum of COSTSOFAR, cost from start node to to current node,  plus COST, cost of current connection.
	  
      to.node <- graph$connections[connection, TO.NODE]
      to.cost <- graph$nodes[current.node, COSTSOFAR] + graph$connections[connection, COST]
      
      # If the path to the to node via the current node is lower cost than the previous lowest cost path to the to node,
      # then update the to node's fields to reflect the newly found lower cost path.

      if (to.cost < graph$nodes[to.node, COSTSOFAR]) {
        graph$nodes[to.node, STATUS]    <- OPEN
        graph$nodes[to.node, COSTSOFAR] <- to.cost
        graph$nodes[to.node, HEURISTIC] <- Astar.heuristic(graph, to.node, last)
        graph$nodes[to.node, TOTAL]     <- graph$nodes[to.node, COSTSOFAR] + graph$nodes[to.node, HEURISTIC]
        graph$nodes[to.node, PREVIOUS]  <- current.node
        open.nodes                      <- union(open.nodes, to.node)
      }
    }

    # Show iteration status and close current node.
    if (showAstarIterations == TRUE) {
      Astar.show.iteration.status(graph, iteration, current.node)  # Show status after current iteration.
    }
    graph$nodes[current.node, STATUS] <- CLOSED                  # Close current node.
    open.nodes <- setdiff(open.nodes, current.node)              # Remove current node from open list.
  }

  return(graph)
}

#----------------------------------------------------------------

# Retrieve path nodes from first to last found by either Dijkstra's algorithm or the A* algorithm.

retrieve.path <- function(graph, first, last) {
  path    <- numeric(0)
  current <- last

  while ((current != first) && (current != UNDEFINED)) {
    path    <- c(current, path)
    current <- graph$nodes[current, PREVIOUS]
  }

  if (current == first) {
    path <- c(first, path)
    write.text(trace.file, paste("Path from", first, "to", last, "path=", paste(path, collapse=" "), "cost=", graph$nodes[last, COSTSOFAR]))
  } else {
    path <- numeric(0)
    write.text(trace.file, paste ("Path from", first, "to", last, "not found"))
  }
  return(path)
}

#----------------------------------------------------------------

# Plot graph using information in graph specification.

plot.graph <- function(graph, scenario.name, plot.cex, plot.axis.ticks, plot.axis.lims, plot.legend, plot.arrows, plot.numbers, path=NA) {
  filename  <- paste("CS 330, Pathfinding, Plot ", scenario.name, " ", Sys.Date(), ".png", sep="")
  plotfile  <- paste(file.path, filename, sep="")
  main.text <- paste("Pathfinding", scenario.name)

  plot.cex        <- list(c(0.75, 1.00, 0.75), c(0.75, 1.00, 0.75), c(0.75, 1.00, 0.75), c(0.35, 0.50, 0.35), c(0.35, 0.50, 0.35))[[scenario]]
  plot.axis.ticks <- list(c(-2, 2, 0.5), c(0, 200, 25), c(0, 200, 25), c(0, 200, 25), c(0, 200, 25))[[scenario]]
  plot.axis.lims  <- list(c(-2.1, 2.1), c(-5, 205), c(-5, 205), c(-5, 205), c(-5, 205))[[scenario]]
  plot.legend     <- c(TRUE,  FALSE,  FALSE, TRUE , TRUE )[scenario]
  plot.arrows     <- c(FALSE, TRUE,   TRUE,  FALSE, FALSE)[scenario]
  plot.numbers    <- c(FALSE, TRUE,   TRUE,  TRUE , TRUE )[scenario]

  png(file=plotfile, width=1350, height=1350, res=300)  # Open plot file
  par(mar=c(2.5, 3.0, 1.5, 1.0))                        # Set margins bottom, left, top, right

  plot(NULL, xlim=plot.axis.lims, ylim=plot.axis.lims, xaxt="n", xaxt="n", yaxt="n", xlab="", ylab="", main="")
  plot.axis.ticks <- seq(from=plot.axis.ticks[1], to=plot.axis.ticks[2], by=plot.axis.ticks[3])
  axis(side=1, at=plot.axis.ticks, labels=plot.axis.ticks, cex.axis=0.60, mgp=c(3, 0.50, 0))
  axis(side=2, at=plot.axis.ticks, labels=plot.axis.ticks, cex.axis=0.60, mgp=c(3, 0.75, 0), las=1)
  title(main=main.text, line=0.50, cex.main=0.80)

  connection.name       <- c("road",  "walkway", "sand",  "dock", "other",  "congestion", "no traffic", "path"     )
  connection.color      <- c("black", "gray64",  "gold3", "tan4", "purple", "red",        "green",      "palegreen")
  connection.cost.color <- c("red",   "red",     "red",   "red",  "red",    "red",        "green",      "green"    )
  connection.lwd        <- c(1.00,    1.00,      1.00,    1.00,    1.00,    1.00,         1.00,         4.00       )
  connection.lty        <- c("solid", "solid",   "solid", "solid", "solid", "solid",      "solid",      "solid"    )

  # Plot legend.

  if (plot.legend) {
    legend.indexes <- 1:5
    if (scenario == 5)   { legend.indexes <- c(legend.indexes, 6:7) }
    if (!is.na(path[1])) { legend.indexes <- c(legend.indexes, 8)   }  # Path reverse intentionally omitted from legend

    legend(x=-10, y=215,                                            # legend location
      legend=connection.name[legend.indexes], cex=0.50,             # legend words
      col=connection.color[legend.indexes], lty="solid", lwd=1.50,  # legend lines
      bty="n", box.lty="solid", box.lwd=0.75, box.col="black")      # legend box
  }

  # Plot connections.

  for (i in 1:graph$m) {
    from.node    <- graph$connections[i, FROM.NODE]
    to.node      <- graph$connections[i, TO.NODE]
    con.type     <- graph$connections[i, TYPE]
    from.x       <- graph$nodes[from.node, LOC.X]
    from.y       <- graph$nodes[from.node, LOC.Z]
    to.x         <- graph$nodes[to.node, LOC.X]
    to.y         <- graph$nodes[to.node, LOC.Z]

    con.col      <- connection.color[con.type]
    con.lty      <- connection.lty[con.type]
    con.lwd      <- connection.lwd[con.type]
    con.cost.col <- connection.cost.color[con.type]

    # If connection is on path or reverse on path, plot connection highlight.
    
    if (!is.na(path[1]) && (length(path) > 1)) {
      for (j in 1:(length(path) - 1)) {
        if (((path[j] == from.node) && (path[j + 1] == to.node))   ||  # Connection on path
            ((path[j] == to.node)   && (path[j + 1] == from.node))) {  # Connection reverse on path
          lines(type="l", x=c(from.x, to.x), y=c(from.y, to.y),
            col=connection.color[8], lty=connection.lty[8], lwd=connection.lwd[8])
        }
      }
    }

    # Plot connection as line or arrow.

    if (plot.arrows) {
      arrows(from.x, from.y, to.x, to.y, length=0.08, angle=20, col=con.col, lty=con.lty, lwd=con.lwd)
    } else {
      lines(type="l", x=c(from.x, to.x), y=c(from.y, to.y), col=con.col, lty=con.lty, lwd=con.lwd)
    }

    # Plot connection cost.

    cost.pos      <- graph$connections[i, COST.POS]
    if (cost.pos != 0) {
      cost.x      <- (from.x + to.x) / 2
      cost.y      <- (from.y + to.y) / 2
      cost.offset <- c(0.2, 0.2, 0.2, 0.2)[cost.pos]  # pos 1=below, 2=left, 3=above, 4=right
      cost.cex    <- plot.cex[1]
      text(x=cost.x, y=cost.y, graph$connections[i, COST], col=con.cost.col, cex=cost.cex, pos=cost.pos, offset=cost.offset)
    }
  }

  # Plot nodes.

  for (i in 1:graph$n) {
    node.x   <- graph$nodes[i, LOC.X]
    node.y   <- graph$nodes[i, LOC.Z]
    name.pos <- graph$nodes[i, PLOT.POS]
    name     <- graph$nodes[i, PLOT.NAME]
    name     <- gsub("\\n", "\n", name, fixed = TRUE)  # Convert \n newline in input string to print properly in plot.
    name.offset <- c(0.3, 0.2, 0.3, 0.2)[name.pos]  # pos 1=below, 2=left, 3=above, 4=right
    name.cex    <- plot.cex[2]
    points(x=node.x, y=node.y, pch=20, col="black")
    text(x=node.x, y=node.y, name, col="black", cex=name.cex, pos=name.pos, offset=name.offset)

    if (plot.numbers) {
      number.pos    <- graph$nodes[i, PLOT.NBR]
      number.offset <- c(0.3, 0.2, 0.3, 0.2)[number.pos]
      number.cex    <- plot.cex[3]
      text(x=node.x, y=node.y, i, col="black", cex=number.cex, pos=number.pos, offset=number.offset)
    }
  }

  dev.off() # Close plot file.
}

#----------------------------------------------------------------

# Load and verify pathfinding graph.

# Load pathfinding graph nodes for selected scenario into a table,
# with one row per node and one column for each field of the node.

nodes.name     <- c("CS 330, Pathfinding, Graph W1 Nodes v3.txt",
                    "CS 330, Pathfinding, Graph W2a Nodes v3.txt",
                    "CS 330, Pathfinding, Graph W2b Nodes v3.txt",
                    "CS 330, Pathfinding, Graph AB Nodes v3.txt",
                    "CS 330, Pathfinding, Graph AB Nodes v3.txt")[scenario]
nodes.expected <- c(6, 5, 5, 66, 66)[scenario]
nodes.name.max <- c(1, 1, 1, 28, 28)[scenario]

connections.name     <- c("CS 330, Pathfinding, Graph W1 Connections v3.txt",
                          "CS 330, Pathfinding, Graph W2a Connections v3.txt",
                          "CS 330, Pathfinding, Graph W2b Connections v3.txt",
                          "CS 330, Pathfinding, Graph AB Connections v3.txt",
                          "CS 330, Pathfinding, Graph AB Connections v3.txt")[scenario]
connections.expected <- c(18, 10, 10, 153, 153)[scenario]

nodes.in <- paste(file.path, nodes.name, sep="")
nodes    <- read.csv(file=nodes.in, header=FALSE, sep=",", comment.char="#", strip.white=TRUE,
            colClasses=c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
                         "numeric",   "numeric", "numeric", "numeric", "numeric", "character"))

# Verify values of loaded data for each node.
						   
all.nodes.OK <- TRUE
for (i in 1:nodes.expected) {
  error.field <- NA
  if (mode(nodes[i,  1]) != "character")    { error.field <-  1 }
  if (mode(nodes[i,  2]) != "numeric")      { error.field <-  2 }
  if (mode(nodes[i,  3]) != "numeric")      { error.field <-  3 }
  if (mode(nodes[i,  4]) != "numeric")      { error.field <-  4 }
  if (mode(nodes[i,  5]) != "numeric")      { error.field <-  5 }
  if (mode(nodes[i,  6]) != "numeric")      { error.field <-  6 }
  if (mode(nodes[i,  7]) != "numeric")      { error.field <-  7 }
  if (mode(nodes[i,  8]) != "numeric")      { error.field <-  8 }
  if (mode(nodes[i,  9]) != "numeric")      { error.field <-  9 }
  if (mode(nodes[i, 10]) != "numeric")      { error.field <- 10 }
  if (mode(nodes[i, 11]) != "numeric")      { error.field <- 11 }
  if (mode(nodes[i, 12]) != "character")    { error.field <- 12 }
  if (nodes[i, 1] != "N")                   { error.field <-  1 }
  if (nodes[i, 2] != i)                     { error.field <-  2 }
  if (nodes[i, 3] != 0)                     { error.field <-  3 }
  if (nodes[i, 4] != 0)                     { error.field <-  4 }
  if (nodes[i, 5] != 0)                     { error.field <-  5 }
  if (nodes[i, 6] != 0)                     { error.field <-  6 }
  if (nodes[i, 7] != 0)                     { error.field <-  7 }
  if (!is.element(nodes[i, 10], 1:4))       { error.field <- 10 }
  if (!is.element(nodes[i, 11], 1:4))       { error.field <- 11 }
  if (nchar(nodes[i, 12]) > nodes.name.max) { error.field <- 12 }
  if (!is.na(error.field)) {
    cat("Error in nodes, i=", i, "error.field=", error.field, "\n")
    all.nodes.OK <- FALSE
  }
}

# Load connections for selected scenario into a table,
# with one row per connection and one column for each field of the connection.

connections.in <- paste(file.path, connections.name, sep="")
connections    <- read.csv(file=connections.in, header=FALSE, sep=",", comment.char="#", strip.white=TRUE,
                  colClasses=c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

# Verify values of loaded data for each connection.
					  
all.connections.OK <- TRUE
for (i in 1:connections.expected) {
  error.field <- NA
  if (mode(connections[i,  1]) != "character")          { error.field <- 1 }
  if (mode(connections[i,  2]) != "numeric")            { error.field <- 2 }
  if (mode(connections[i,  3]) != "numeric")            { error.field <- 3 }
  if (mode(connections[i,  4]) != "numeric")            { error.field <- 4 }
  if (mode(connections[i,  5]) != "numeric")            { error.field <- 5 }
  if (mode(connections[i,  6]) != "numeric")            { error.field <- 6 }
  if (mode(connections[i,  7]) != "numeric")            { error.field <- 7 }
  if (connections[i, 1] != "C")                         { error.field <- 1 }
  if (connections[i, 2] != i)                           { error.field <- 2 }
  if (!is.element(connections[i, 3], 1:nodes.expected)) { error.field <- 3 }
  if (!is.element(connections[i, 4], 1:nodes.expected)) { error.field <- 4 }
  if (connections[i, 5] <= 0)                           { error.field <- 5 }
  if (!is.element(connections[i, 6], 0:4))              { error.field <- 6 }
  if (!is.element(connections[i, 7], 1:5))              { error.field <- 7 }
  if (!is.na(error.field)) {
    cat("Error in connections, i=", i, "error.field=", error.field, "\n")
    all.connections.OK <- FALSE
  }
}

# Adjust cost of selected connections for scenario 5, Adventure Bay Tactical.

if (scenario == 5) {
  congestion                    <- c( 68,  70,  72,  74,  76,  78,  66, 128,  80, 132, 130,  64, 126,  46,  35,  33,
                                      69,  71,  73,  75,  77,  79,  67, 129,  81, 133, 131,  65, 127,  47,  36,  34)
  congestion.weight             <- rep(1.0, length(connections[, 1]))
  congestion.weight[congestion] <- 3.0

  no.traffic                    <- c(106, 108, 110,
                                     107, 109, 111)
  no.traffic.weight             <- rep(1.0, length(connections[, 1]))
  no.traffic.weight[no.traffic] <- 0.5

  for (i in 1:length(connections[, 1])) {
    connections[i, 5] <- connections[i, 5] * (congestion.weight[i] * no.traffic.weight[i])
  }

  connections[congestion, 7] <- 6
  connections[no.traffic, 7] <- 7
}

# Combine the nodes table, the connections table, and two size variables into a list.

graph <- list(nodes=nodes, connections=connections, n=length(nodes[, 1]), m=length(connections[, 1]))

# Write loaded nodes and connections to trace file.

write.text(trace.file, paste("Loaded scenario", scenario.name, "\n"))
write.text(trace.file, "Nodes")
for (i in 1:graph$n) {
  write.text(trace.file, paste(graph$nodes[i, ], collapse=" "))
}
write.text(trace.file, " ")
write.text(trace.file, "Connections")
for (i in 1:graph$m) {
  write.text(trace.file, paste(graph$connections[i, ], collapse=" "))
}
write.text(trace.file, " ")

# Report results of file loading and verification to console.

cat(paste("Loading nodes:      ", ifelse(all.nodes.OK,       "OK", "Error(s)")), "\n")
cat(paste("Loading connections:", ifelse(all.connections.OK, "OK", "Error(s)")), "\n")


#----------------------------------------------------------------

# Plot graph after initialize and before pathfinding.

if (plot.before) {
  plot.graph(graph, scenario.name, plot.cex, plot.axis.ticks, plot.axis.lims, plot.legend, plot.arrows, plot.numbers)
}

#----------------------------------------------------------------

# Generate all possible paths in graph using Dijkstra's.

if (run.Dijkstras) {
  write.text(trace.file, paste("Dijkstra's ", scenario.name, sep=""))

  # Initialize empty list to hold the generated paths.

  paths.Dijkstras <- list()
  for (i in 1:graph$n) { paths.Dijkstras[[i]] <- list() }

  # Call Dijsktra's for each node.

  cat("Dijkstra's: ")
  for (i in 1:graph$n) {
    cat(paste(i, " ", sep=""))
    graph.out <- Dijkstra.find.paths(graph, i)
    for (j in 1:graph$n) {
      path <- retrieve.path(graph.out, i, j)
      paths.Dijkstras[[i]][[j]] <- path
    }
  }
  cat("\n")
}

#----------------------------------------------------------------

# Generate all possible paths in graph using A*.

if (run.Astar) {
  write.text(trace.file, paste("Astar ", scenario.name, sep=""))

  # Initialize empty list to hold the generated paths.

  paths.Astar <- list()
  for (i in 1:graph$n) { paths.Astar[[i]] <- list() }

  # Call A* for each pair of nodes.

  cat("A*: ")
  for (i in 1:graph$n) {
    cat(paste(i, " ", sep=""))
    for (j in 1:graph$n) {
      graph.out <- Astar.find.path(graph, i, j)
      path      <- retrieve.path(graph.out, i, j)
      paths.Astar[[i]][[j]] <- path
    }
  }
  cat("\n")
}

#----------------------------------------------------------------

# Compare paths produced by Dijkstra's and A*, if both run.

if (compare.paths && run.Dijkstras && run.Astar) {

  # For each pair of nodes, compare the path found by Dijsktra's to the path found by A*.

  cat("Compare   : ")
  errors <- 0
  for (i in 1:graph$n) {
    cat(paste(i, " ", sep=""))
    for (j in 1:graph$n) {
      path.Dijkstras <- paths.Dijkstras[[i]][[j]]
      path.Astar     <- paths.Astar[[i]][[j]]
      same <- (length(path.Dijkstras) == length(path.Astar)) && all(path.Dijkstras == path.Astar)
      if (!same) {
        write.text(trace.file, paste(
          "Error from",  i, "to",          j, "\n",
          "  Dijsktra",  paste(path.Dijkstras, collapse=" "), "\n", 
          "  A*",        paste(path.Astar,     collapse=" ")))
        errors <- errors + 1
      }
    }
  }
  cat("\n")
  write.text(trace.file, paste("\n", "Paths ", graph$n * graph$n, " Errors ", errors, "\n", sep=""))
}

#----------------------------------------------------------------

# Generate and plot a single path.

if (plot.single) {
  start.node <- c( 1, 45, 60)[path.to.plot]
  goal.node  <- c(59, 66,  9)[path.to.plot]

  graph.out <- Astar.find.path(graph, start.node, goal.node)
  path      <- retrieve.path(graph.out, start.node, goal.node)
  plot.graph(graph, paste(scenario.name, "from", start.node, "to", goal.node),
    plot.cex, plot.axis.ticks, plot.axis.lims, plot.legend, plot.arrows, plot.numbers, path=path)
}

#----------------------------------------------------------------

#----------------------------------------------------------------

# Generate and plot a single path.

if (run.AstarExample) {
 # start.node <- c( 1, 45, 60)[path.to.plot]
 # goal.node  <- c(59, 66,  9)[path.to.plot]

 # graph.out <- Astar.find.path(graph, 1, 59)
 # path      <- retrieve.path(graph.out, 1, 59)
 # graph.out <- Astar.find.path(graph, 45, 66)
 # path      <- retrieve.path(graph.out, 45, 66)
 # graph.out <- Astar.find.path(graph, 60, 9)
 # path      <- retrieve.path(graph.out, 60, 9)
   graph.out <- Astar.find.path(graph, 1, 29)
   path      <- retrieve.path(graph.out, 1, 29)
   graph.out <- Astar.find.path(graph, 1, 38)
   path      <- retrieve.path(graph.out, 1, 38)
   graph.out <- Astar.find.path(graph, 11, 1)
   path      <- retrieve.path(graph.out, 11, 1)
   graph.out <- Astar.find.path(graph, 33, 66)
   path      <- retrieve.path(graph.out, 33, 66)
   graph.out <- Astar.find.path(graph, 58, 43)
   path      <- retrieve.path(graph.out, 58, 43)   


  #plot.graph(graph, paste(scenario.name, "from", start.node, "to", goal.node),
  #  plot.cex, plot.axis.ticks, plot.axis.lims, plot.legend, plot.arrows, plot.numbers, path=path)
}

#----------------------------------------------------------------

# Report program end to trace file.

write.text(trace.file, paste("\nCS 330, Pathfinding, End", Sys.time(), "\n"))

# End of program
