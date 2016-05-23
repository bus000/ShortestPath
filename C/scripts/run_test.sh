#!/usr/bin/env bash

# Run tabular profiling.
echo "Running tabular time profiling"
../bin/diff --tabular ../data/graph_100.txt > ./output/tabular_100.tim
../bin/diff --tabular ../data/graph_200.txt > ./output/tabular_200.tim
../bin/diff --tabular ../data/graph_500.txt > ./output/tabular_500.tim
../bin/diff --tabular ../data/graph_1000.txt > ./output/tabular_1000.tim
../bin/diff --tabular ../data/graph_10000.txt > ./output/tabular_10000.tim

echo "Running tabular space profiling"
valgrind --tool=massif --massif-out-file=./output/tabular_100.mem ../bin/diff --tabular ../data/graph_100.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_200.mem ../bin/diff --tabular ../data/graph_200.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_500.mem ../bin/diff --tabular ../data/graph_500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_1000.mem ../bin/diff --tabular ../data/graph_1000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_10000.mem ../bin/diff --tabular ../data/graph_10000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_100000.mem ../bin/diff --tabular ../data/graph_100000.txt 2> /dev/null > /dev/null

# Run dijkstra profiling.
echo "Running dijkstra time profiling"
../bin/diff --dijkstra ../data/graph_100.txt > ./output/dijkstra_100.tim
../bin/diff --dijkstra ../data/graph_200.txt > ./output/dijkstra_200.tim
../bin/diff --dijkstra ../data/graph_500.txt > ./output/dijkstra_500.tim
../bin/diff --dijkstra ../data/graph_1000.txt > ./output/dijkstra_1000.tim
../bin/diff --dijkstra ../data/graph_10000.txt > ./output/dijkstra_10000.tim

echo "Running dijkstra space profiling"
valgrind --tool=massif --massif-out-file=./output/dijkstra_100.mem ../bin/diff --dijkstra ../data/graph_100.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_200.mem ../bin/diff --dijkstra ../data/graph_200.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_500.mem ../bin/diff --dijkstra ../data/graph_500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_1000.mem ../bin/diff --dijkstra ../data/graph_1000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_10000.mem ../bin/diff --dijkstra ../data/graph_10000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_100000.mem ../bin/diff --dijkstra ../data/graph_100000.txt 2> /dev/null > /dev/null

# Run DFS profiling.
echo "Running DFS time profiling"
../bin/diff --DFS ../data/graph_100.txt > ./output/DFS_100.tim
../bin/diff --DFS ../data/graph_200.txt > ./output/DFS_200.tim
../bin/diff --DFS ../data/graph_500.txt > ./output/DFS_500.tim
../bin/diff --DFS ../data/graph_1000.txt > ./output/DFS_1000.tim
../bin/diff --DFS ../data/graph_10000.txt > ./output/DFS_10000.tim

echo "Running DFS space profiling"
valgrind --tool=massif --massif-out-file=./output/DFS_100.mem ../bin/diff --DFS ../data/graph_100.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_200.mem ../bin/diff --DFS ../data/graph_200.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_500.mem ../bin/diff --DFS ../data/graph_500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_1000.mem ../bin/diff --DFS ../data/graph_1000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_10000.mem ../bin/diff --DFS ../data/graph_10000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_100000.mem ../bin/diff --DFS ../data/graph_100000.txt 2> /dev/null > /dev/null
