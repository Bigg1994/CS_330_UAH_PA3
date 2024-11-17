#########################################################
# Pathfinding Algorithm Implementation
# Author: Garett Sutton
# Team Members: Garett Sutton
# Reference Credit: Parker Clark
# Due Date: 11/17/2024
# Program Name: Sutton_PA3.py
# Python Version: 3.12
#########################################################

# Dependencies
import math

# File Paths for Input and Output
OUTPUT_FILE = "Pathfinding_Output_GS.txt"
NODES_FILE = "CS 330, Pathfinding, Graph AB Nodes v3.txt"
CONNECTIONS_FILE = "CS 330, Pathfinding, Graph AB Connections v3.txt"

class Graph:

    def __init__(self, nodeFilePath, connectionFilePath):
        # Open the node file and read all lines into a list
        with open(nodeFilePath, 'r') as file:
            nodeData = file.readlines()

        # Create a list of nodes, starting with a dummy node at index 0
        self.nodes = [Node()]

        # Iterate through the node data and create Node instances
        for line in nodeData:
            if line[0] == '#':  # Skip comment lines
                continue
            rowData = line.split(',')
            for i in range(1, 9):  # Populate nodes with data from the file
                self.nodes.append(Node("N", float(rowData[i])))

        # Load the connection file
        with open(connectionFilePath, 'r') as file:
            connectionData = file.readlines()

        # Initialize the connection list, starting with a placeholder connection
        self.connections = [Connection()]

        # Process each connection line
        for line in connectionData:
            if line[0] == '#':  # Skip comment lines
                continue
            rowData = line.split(',')
            self.connections.append(Connection("C", int(rowData[1]), int(rowData[2]),
                                               int(rowData[3]), float(rowData[4]),
                                               int(rowData[6])))

class Node:

    def __init__(self, id="N", nodeNum=0, status=0, currentCost=0, heuristic=0, total=0, pre=0, X=0, Y=0):
        # Constructor for creating a new pathfinding node
        self.id = id
        self.nodeNum = nodeNum
        self.status = status
        self.currentCost = currentCost
        self.heuristic = heuristic
        self.total = total
        self.predecessor = pre
        self.X = X
        self.Y = Y

    def heuristicDistance(self, otherNode):
        # Calculate the Euclidean distance between this node and another
        return math.sqrt((self.X - otherNode.X)**2 + (self.Y - otherNode.Y)**2)

class Connection:

    def __init__(self, id="C", connection=0, fromNode=0, toNode=0, cost=0, totalCost=0, type=0):
        # Constructor for creating a connection between nodes
        self.id = id
        self.connection = connection
        self.fromNode = fromNode
        self.toNode = toNode
        self.cost = cost
        self.totalCost = totalCost
        self.type = type

class AStarAlgorithm:

    def __init__(self, graph: Graph):
        # Initialize A* algorithm with a graph
        self.graph = graph
        self.finalCost = None

    def getConnections(self, nodeIndex):
        # Retrieve all connections from a specific node
        connections = []
        for connection in self.graph.connections:
            if connection.fromNode == nodeIndex:
                connections.append(connection)
        return connections

    def findLowest(self, openList):
        # Find the node with the lowest total cost from the open list
        lowestCost = float('inf')
        lowestNode = None
        for nodeIndex in openList:
            node = self.graph.nodes[nodeIndex]
            if node.total < lowestCost:
                lowestCost = node.total
                lowestNode = nodeIndex
        return lowestNode

    def retrievePath(self, startIndex, goalIndex):
        # Reconstruct the path from the start node to the goal node
        path = []
        currentIndex = goalIndex

        # Backtrack to the start node using the predecessor pointer
        while currentIndex != startIndex and currentIndex is not None:
            path.append(currentIndex)
            currentIndex = self.graph.nodes[currentIndex].predecessor

        if currentIndex == startIndex:
            path.append(startIndex)

        # Return the path from start to goal (reverse the list to correct the order)
        return path[::-1]

    def findPath(self, startIndex, goalIndex):
        # Reset the graph nodes for a new search
        for node in self.graph.nodes[1:]:  # Skip the dummy node at index 0
            node.status = 'unvisited'
            node.currentCost = float('inf')
            node.predecessor = None

        # Initialize the starting node
        startNode = self.graph.nodes[startIndex]
        startNode.status = 'open'
        startNode.currentCost = 0

        openNodes = [startIndex]

        while openNodes:
            # Select the node with the lowest total cost
            currentIndex = self.findLowest(openNodes)
            currentNode = self.graph.nodes[currentIndex]

            # If the goal is reached, stop the loop
            if currentIndex == goalIndex:
                break

            # Process each connection from the current node
            for connection in self.getConnections(currentIndex):
                nextNodeIndex = connection.toNode
                newCost = currentNode.currentCost + connection.cost

                # If a better path is found, update the node
                if newCost < self.graph.nodes[nextNodeIndex].currentCost:
                    nextNode = self.graph.nodes[nextNodeIndex]
                    nextNode.status = 'open'
                    nextNode.predecessor = currentIndex
                    nextNode.currentCost = newCost
                    nextNode.heuristic = self.graph.nodes[nextNodeIndex].heuristicDistance(self.graph.nodes[goalIndex])
                    nextNode.total = nextNode.currentCost + nextNode.heuristic

                    if nextNodeIndex not in openNodes:
                        openNodes.append(nextNodeIndex)

            # Mark the current node as visited and remove it from the open list
            currentNode.status = 'visited'
            openNodes.remove(currentIndex)

        # Store the total cost of the path to the goal
        self.finalCost = self.graph.nodes[goalIndex].currentCost

# Open the output file and log results
with open(OUTPUT_FILE, "w") as outFile:
    outFile.write("Pathfinding Output by Garett Sutton\nCS330 Fall 2024\n\n")

    # Write node data to the output file
    with open(NODES_FILE, "r") as inputFile:
        for line in inputFile:
            if not line.startswith("#"):
                outFile.write(line)

    # Write connection data to the output file
    with open(CONNECTIONS_FILE, "r") as inputFile:
        for line in inputFile:
            if not line.startswith("#"):
                outFile.write(line)

    outFile.write("\n\n")

    # Define test cases for pathfinding (start, goal)
    testCases = [
        [1, 29],
        [1, 38],
        [11, 1],
        [33, 66],
        [58, 43]
    ]

    # Initialize graph and A* algorithm instance
    graph = Graph(NODES_FILE, CONNECTIONS_FILE)
    aStarAlgorithm = AStarAlgorithm(graph)

    # Process each test case
    for start, end in testCases:
        aStarAlgorithm.findPath(start, end)
        path = aStarAlgorithm.retrievePath(start, end)

        # Log the path and total cost to the output file
        outFile.write(f"Path from {start} to {end}: {path}\n")
        outFile.write(f"Total Cost: {aStarAlgorithm.finalCost}\n\n")