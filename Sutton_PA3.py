#########################################################
# Pathfinding Algorithm Implementation
# Author: Garett Sutton
# Team Members: Garett Sutton
# Due Date: 11/17/2024
# Program Name: Sutton_PA3.py
# Python Version: 3.12
#########################################################

# Required Libraries
import math

# File Paths for Input and Output
OUTPUT_FILE = "Pathfinding_Output_GS.txt"
NODES_FILE = "CS 330, Pathfinding, Graph AB Nodes v3.txt"
CONNECTIONS_FILE = "CS 330, Pathfinding, Graph AB Connections v3.txt"

class Graph:

    def __init__(self, nodeFilePath, connectionFilePath):
        # Open the node file and load all lines into a list
        with open(nodeFilePath, 'r') as file:
            nodeData = file.readlines()

        # Create a list for storing nodes, starting with a placeholder node at index 0
        self.currentNodes = [Node()]

        # Process each line of the node data
        for line in nodeData:
            # Skip lines that are comments
            if line[0] == '#':
                continue

            # Split the line by commas and process the values
            rowData = line.split(',')

            # Add a new node to the list with the data from the current line
            for i in range(1, 9):
                self.currentNodes.append(Node("N", float(rowData[i])))

        # Load the connection file into a list of lines
        with open(connectionFilePath, 'r') as file:
            connectionData = file.readlines()

        # Initialize a list for storing connections, with a placeholder connection at index 0
        self.currentConnections = [Connection()]

        # Process each connection line
        for line in connectionData:
            # Skip comment lines
            if line[0] == '#':
                continue

            # Split the line and extract data for the connection
            rowData = line.split(',')

            # Add a new connection object to the list
            self.currentConnections.append(Connection("C", int(rowData[1]), int(rowData[2]), 
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
        self.pre = pre
        self.X = X
        self.Y = Y

    def heuristicDistance(self, otherNode):
        # Calculate the Euclidean distance from this node to another node
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
        # Constructor for A* algorithm
        self.graph = graph
        self.finalCost = None

    def getConnections(self, currentNodeIndex):
        # Retrieve all connections from a given node
        connections = []
        for connection in self.graph.currentConnections:
            if connection.fromNode == currentNodeIndex:
                connections.append(connection)
        return connections

    def findLowest(self, openList):
        # Identify the node with the lowest total cost from the open list
        lowestCost = float('inf')
        lowestNode = None
        for nodeIndex in openList:
            node = self.graph.currentNodes[nodeIndex]
            if node.total < lowestCost:
                lowestCost = node.total
                lowestNode = nodeIndex
        return lowestNode

    def retrievePath(self, startNodeIndex, endNodeIndex):
        # Reconstruct the path by following the parent pointers
        path = []
        currentNodeIndex = endNodeIndex

        # Backtrack from the goal node to the start node
        while currentNodeIndex != startNodeIndex and currentNodeIndex is not None:
            path.append(currentNodeIndex)
            currentNodeIndex = self.graph.currentNodes[currentNodeIndex].pre

        # If the start node was found, add it to the path
        if currentNodeIndex == startNodeIndex:
            path.append(startNodeIndex)

        # Return the path in the correct order (start -> end)
        return path[::-1]

    def findPath(self, startNodeIndex, goalNodeIndex):
        # Reset all nodes in the graph for a new search
        for node in self.graph.currentNodes[1:]:  # Skip dummy node at index 0
            node.status = 'unvisited'
            node.currentCost = float('inf')
            node.pre = None

        # Initialize the start node
        startNode = self.graph.currentNodes[startNodeIndex]
        startNode.status = 'open'
        startNode.currentCost = 0

        # List of open nodes, initially just the start node
        openNodes = [startNodeIndex]

        while openNodes:
            # Select the node with the lowest total cost
            currentNodeIndex = self.findLowest(openNodes)
            currentNode = self.graph.currentNodes[currentNodeIndex]

            # If we've reached the goal, break the loop
            if currentNodeIndex == goalNodeIndex:
                break

            # Process all the neighbors of the current node
            for connection in self.getConnections(currentNodeIndex):
                nextNodeIndex = connection.toNode
                newCost = currentNode.currentCost + connection.cost

                # If a cheaper path to the next node is found, update it
                if newCost < self.graph.currentNodes[nextNodeIndex].currentCost:
                    nextNode = self.graph.currentNodes[nextNodeIndex]
                    nextNode.status = 'open'
                    nextNode.pre = currentNodeIndex
                    nextNode.currentCost = newCost
                    nextNode.heuristic = self.graph.currentNodes[nextNodeIndex].heuristicDistance(self.graph.currentNodes[goalNodeIndex])
                    nextNode.total = nextNode.currentCost + nextNode.heuristic

                    # If the next node is not in the open list, add it
                    if nextNodeIndex not in openNodes:
                        openNodes.append(nextNodeIndex)

            # Mark the current node as visited and remove it from the open list
            currentNode.status = 'visited'
            openNodes.remove(currentNodeIndex)

        # Store the total cost to the goal
        self.finalCost = self.graph.currentNodes[goalNodeIndex].currentCost

# Open the output file to log the results
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
        # Find the path and total cost
        aStarAlgorithm.findPath(start, end)
        path = aStarAlgorithm.retrievePath(start, end)

        # Log the path and cost to the output file
        outFile.write(f"Path from {start} to {end}: {path}\n")
        outFile.write(f"Total Cost: {aStarAlgorithm.finalCost}\n\n")