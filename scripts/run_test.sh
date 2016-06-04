#!/usr/bin/env bash

# Run tabular profiling.
echo "Running tabular time profiling"
../bin/diff --tabular ../data/graph_500.txt   > ./output/tabular_500.tim
../bin/diff --tabular ../data/graph_1000.txt  > ./output/tabular_1000.tim
../bin/diff --tabular ../data/graph_1500.txt  > ./output/tabular_1500.tim
../bin/diff --tabular ../data/graph_2000.txt  > ./output/tabular_2000.tim
../bin/diff --tabular ../data/graph_2500.txt  > ./output/tabular_2500.tim
../bin/diff --tabular ../data/graph_3000.txt  > ./output/tabular_3000.tim
../bin/diff --tabular ../data/graph_3500.txt  > ./output/tabular_3500.tim
../bin/diff --tabular ../data/graph_4000.txt  > ./output/tabular_4000.tim
../bin/diff --tabular ../data/graph_4500.txt  > ./output/tabular_4500.tim
../bin/diff --tabular ../data/graph_5000.txt  > ./output/tabular_5000.tim
../bin/diff --tabular ../data/graph_5500.txt  > ./output/tabular_5500.tim
../bin/diff --tabular ../data/graph_6000.txt  > ./output/tabular_6000.tim
../bin/diff --tabular ../data/graph_6500.txt  > ./output/tabular_6500.tim
../bin/diff --tabular ../data/graph_7000.txt  > ./output/tabular_7000.tim
../bin/diff --tabular ../data/graph_7500.txt  > ./output/tabular_7500.tim
../bin/diff --tabular ../data/graph_8000.txt  > ./output/tabular_8000.tim
../bin/diff --tabular ../data/graph_8500.txt  > ./output/tabular_8500.tim
../bin/diff --tabular ../data/graph_9000.txt  > ./output/tabular_9000.tim
../bin/diff --tabular ../data/graph_9500.txt  > ./output/tabular_9500.tim
../bin/diff --tabular ../data/graph_10000.txt > ./output/tabular_10000.tim

echo "Running tabular space profiling"
valgrind --tool=massif --massif-out-file=./output/tabular_500.mem   ../bin/diff --tabular ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_1000.mem  ../bin/diff --tabular ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_1500.mem  ../bin/diff --tabular ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_2000.mem  ../bin/diff --tabular ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_2500.mem  ../bin/diff --tabular ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_3000.mem  ../bin/diff --tabular ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_3500.mem  ../bin/diff --tabular ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_4000.mem  ../bin/diff --tabular ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_4500.mem  ../bin/diff --tabular ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_5000.mem  ../bin/diff --tabular ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_5500.mem  ../bin/diff --tabular ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_6000.mem  ../bin/diff --tabular ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_6500.mem  ../bin/diff --tabular ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_7000.mem  ../bin/diff --tabular ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_7500.mem  ../bin/diff --tabular ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_8000.mem  ../bin/diff --tabular ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_8500.mem  ../bin/diff --tabular ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_9000.mem  ../bin/diff --tabular ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_9500.mem  ../bin/diff --tabular ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_10000.mem ../bin/diff --tabular ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run Dijkstra profiling.
echo "Running dijkstra time profiling"
../bin/diff --dijkstra ../data/graph_500.txt   > ./output/dijkstra_500.tim
../bin/diff --dijkstra ../data/graph_1000.txt  > ./output/dijkstra_1000.tim
../bin/diff --dijkstra ../data/graph_1500.txt  > ./output/dijkstra_1500.tim
../bin/diff --dijkstra ../data/graph_2000.txt  > ./output/dijkstra_2000.tim
../bin/diff --dijkstra ../data/graph_2500.txt  > ./output/dijkstra_2500.tim
../bin/diff --dijkstra ../data/graph_3000.txt  > ./output/dijkstra_3000.tim
../bin/diff --dijkstra ../data/graph_3500.txt  > ./output/dijkstra_3500.tim
../bin/diff --dijkstra ../data/graph_4000.txt  > ./output/dijkstra_4000.tim
../bin/diff --dijkstra ../data/graph_4500.txt  > ./output/dijkstra_4500.tim
../bin/diff --dijkstra ../data/graph_5000.txt  > ./output/dijkstra_5000.tim
../bin/diff --dijkstra ../data/graph_5500.txt  > ./output/dijkstra_5500.tim
../bin/diff --dijkstra ../data/graph_6000.txt  > ./output/dijkstra_6000.tim
../bin/diff --dijkstra ../data/graph_6500.txt  > ./output/dijkstra_6500.tim
../bin/diff --dijkstra ../data/graph_7000.txt  > ./output/dijkstra_7000.tim
../bin/diff --dijkstra ../data/graph_7500.txt  > ./output/dijkstra_7500.tim
../bin/diff --dijkstra ../data/graph_8000.txt  > ./output/dijkstra_8000.tim
../bin/diff --dijkstra ../data/graph_8500.txt  > ./output/dijkstra_8500.tim
../bin/diff --dijkstra ../data/graph_9000.txt  > ./output/dijkstra_9000.tim
../bin/diff --dijkstra ../data/graph_9500.txt  > ./output/dijkstra_9500.tim
../bin/diff --dijkstra ../data/graph_10000.txt > ./output/dijkstra_10000.tim

echo "Running dijkstra space profiling"
valgrind --tool=massif --massif-out-file=./output/dijkstra_500.mem   ../bin/diff --dijkstra ../data/graph_500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_1000.mem  ../bin/diff --dijkstra ../data/graph_1000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_1500.mem  ../bin/diff --dijkstra ../data/graph_1500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_2000.mem  ../bin/diff --dijkstra ../data/graph_2000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_2500.mem  ../bin/diff --dijkstra ../data/graph_2500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_3000.mem  ../bin/diff --dijkstra ../data/graph_3000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_3500.mem  ../bin/diff --dijkstra ../data/graph_3500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_4000.mem  ../bin/diff --dijkstra ../data/graph_4000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_4500.mem  ../bin/diff --dijkstra ../data/graph_4500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_5000.mem  ../bin/diff --dijkstra ../data/graph_5000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_5500.mem  ../bin/diff --dijkstra ../data/graph_5500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_6000.mem  ../bin/diff --dijkstra ../data/graph_6000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_6500.mem  ../bin/diff --dijkstra ../data/graph_6500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_7000.mem  ../bin/diff --dijkstra ../data/graph_7000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_7500.mem  ../bin/diff --dijkstra ../data/graph_7500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_8000.mem  ../bin/diff --dijkstra ../data/graph_8000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_8500.mem  ../bin/diff --dijkstra ../data/graph_8500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_9000.mem  ../bin/diff --dijkstra ../data/graph_9000.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_9500.mem  ../bin/diff --dijkstra ../data/graph_9500.txt 2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_10000.mem ../bin/diff --dijkstra ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run DFS profiling.
echo "Running DFS time profiling"
../bin/diff --DFS ../data/graph_500.txt   > ./output/DFS_500.tim
../bin/diff --DFS ../data/graph_1000.txt  > ./output/DFS_1000.tim
../bin/diff --DFS ../data/graph_1500.txt  > ./output/DFS_1500.tim
../bin/diff --DFS ../data/graph_2000.txt  > ./output/DFS_2000.tim
../bin/diff --DFS ../data/graph_2500.txt  > ./output/DFS_2500.tim
../bin/diff --DFS ../data/graph_3000.txt  > ./output/DFS_3000.tim
../bin/diff --DFS ../data/graph_3500.txt  > ./output/DFS_3500.tim
../bin/diff --DFS ../data/graph_4000.txt  > ./output/DFS_4000.tim
../bin/diff --DFS ../data/graph_4500.txt  > ./output/DFS_4500.tim
../bin/diff --DFS ../data/graph_5000.txt  > ./output/DFS_5000.tim
../bin/diff --DFS ../data/graph_5500.txt  > ./output/DFS_5500.tim
../bin/diff --DFS ../data/graph_6000.txt  > ./output/DFS_6000.tim
../bin/diff --DFS ../data/graph_6500.txt  > ./output/DFS_6500.tim
../bin/diff --DFS ../data/graph_7000.txt  > ./output/DFS_7000.tim
../bin/diff --DFS ../data/graph_7500.txt  > ./output/DFS_7500.tim
../bin/diff --DFS ../data/graph_8000.txt  > ./output/DFS_8000.tim
../bin/diff --DFS ../data/graph_8500.txt  > ./output/DFS_8500.tim
../bin/diff --DFS ../data/graph_9000.txt  > ./output/DFS_9000.tim
../bin/diff --DFS ../data/graph_9500.txt  > ./output/DFS_9500.tim
../bin/diff --DFS ../data/graph_10000.txt > ./output/DFS_10000.tim

echo "Running DFS space profiling"
valgrind --tool=massif --massif-out-file=./output/DFS_500.mem   ../bin/diff --DFS ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_1000.mem  ../bin/diff --DFS ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_1500.mem  ../bin/diff --DFS ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_2000.mem  ../bin/diff --DFS ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_2500.mem  ../bin/diff --DFS ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_3000.mem  ../bin/diff --DFS ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_3500.mem  ../bin/diff --DFS ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_4000.mem  ../bin/diff --DFS ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_4500.mem  ../bin/diff --DFS ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_5000.mem  ../bin/diff --DFS ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_5500.mem  ../bin/diff --DFS ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_6000.mem  ../bin/diff --DFS ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_6500.mem  ../bin/diff --DFS ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_7000.mem  ../bin/diff --DFS ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_7500.mem  ../bin/diff --DFS ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_8000.mem  ../bin/diff --DFS ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_8500.mem  ../bin/diff --DFS ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_9000.mem  ../bin/diff --DFS ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_9500.mem  ../bin/diff --DFS ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_10000.mem ../bin/diff --DFS ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run layering profiling.
echo "Running layer time profiling"
../bin/diff --layer ../data/graph_500.txt   > ./output/layer_500.tim
../bin/diff --layer ../data/graph_1000.txt  > ./output/layer_1000.tim
../bin/diff --layer ../data/graph_1500.txt  > ./output/layer_1500.tim
../bin/diff --layer ../data/graph_2000.txt  > ./output/layer_2000.tim
../bin/diff --layer ../data/graph_2500.txt  > ./output/layer_2500.tim
../bin/diff --layer ../data/graph_3000.txt  > ./output/layer_3000.tim
../bin/diff --layer ../data/graph_3500.txt  > ./output/layer_3500.tim
../bin/diff --layer ../data/graph_4000.txt  > ./output/layer_4000.tim
../bin/diff --layer ../data/graph_4500.txt  > ./output/layer_4500.tim
../bin/diff --layer ../data/graph_5000.txt  > ./output/layer_5000.tim
../bin/diff --layer ../data/graph_5500.txt  > ./output/layer_5500.tim
../bin/diff --layer ../data/graph_6000.txt  > ./output/layer_6000.tim
../bin/diff --layer ../data/graph_6500.txt  > ./output/layer_6500.tim
../bin/diff --layer ../data/graph_7000.txt  > ./output/layer_7000.tim
../bin/diff --layer ../data/graph_7500.txt  > ./output/layer_7500.tim
../bin/diff --layer ../data/graph_8000.txt  > ./output/layer_8000.tim
../bin/diff --layer ../data/graph_8500.txt  > ./output/layer_8500.tim
../bin/diff --layer ../data/graph_9000.txt  > ./output/layer_9000.tim
../bin/diff --layer ../data/graph_9500.txt  > ./output/layer_9500.tim
../bin/diff --layer ../data/graph_10000.txt > ./output/layer_10000.tim

echo "Running layer space profiling"
valgrind --tool=massif --massif-out-file=./output/layer_500.mem   ../bin/diff --layer ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_1000.mem  ../bin/diff --layer ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_1500.mem  ../bin/diff --layer ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_2000.mem  ../bin/diff --layer ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_2500.mem  ../bin/diff --layer ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_3000.mem  ../bin/diff --layer ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_3500.mem  ../bin/diff --layer ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_4000.mem  ../bin/diff --layer ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_4500.mem  ../bin/diff --layer ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_5000.mem  ../bin/diff --layer ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_5500.mem  ../bin/diff --layer ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_6000.mem  ../bin/diff --layer ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_6500.mem  ../bin/diff --layer ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_7000.mem  ../bin/diff --layer ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_7500.mem  ../bin/diff --layer ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_8000.mem  ../bin/diff --layer ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_8500.mem  ../bin/diff --layer ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_9000.mem  ../bin/diff --layer ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_9500.mem  ../bin/diff --layer ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_10000.mem ../bin/diff --layer ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run tabular SS profiling.
echo "Running tabular SS time profiling"
../bin/diff --tabular -ss ../data/graph_500.txt   > ./output/tabular_ss_500.tim
../bin/diff --tabular -ss ../data/graph_1000.txt  > ./output/tabular_ss_1000.tim
../bin/diff --tabular -ss ../data/graph_1500.txt  > ./output/tabular_ss_1500.tim
../bin/diff --tabular -ss ../data/graph_2000.txt  > ./output/tabular_ss_2000.tim
../bin/diff --tabular -ss ../data/graph_2500.txt  > ./output/tabular_ss_2500.tim
../bin/diff --tabular -ss ../data/graph_3000.txt  > ./output/tabular_ss_3000.tim
../bin/diff --tabular -ss ../data/graph_3500.txt  > ./output/tabular_ss_3500.tim
../bin/diff --tabular -ss ../data/graph_4000.txt  > ./output/tabular_ss_4000.tim
../bin/diff --tabular -ss ../data/graph_4500.txt  > ./output/tabular_ss_4500.tim
../bin/diff --tabular -ss ../data/graph_5000.txt  > ./output/tabular_ss_5000.tim
../bin/diff --tabular -ss ../data/graph_5500.txt  > ./output/tabular_ss_5500.tim
../bin/diff --tabular -ss ../data/graph_6000.txt  > ./output/tabular_ss_6000.tim
../bin/diff --tabular -ss ../data/graph_6500.txt  > ./output/tabular_ss_6500.tim
../bin/diff --tabular -ss ../data/graph_7000.txt  > ./output/tabular_ss_7000.tim
../bin/diff --tabular -ss ../data/graph_7500.txt  > ./output/tabular_ss_7500.tim
../bin/diff --tabular -ss ../data/graph_8000.txt  > ./output/tabular_ss_8000.tim
../bin/diff --tabular -ss ../data/graph_8500.txt  > ./output/tabular_ss_8500.tim
../bin/diff --tabular -ss ../data/graph_9000.txt  > ./output/tabular_ss_9000.tim
../bin/diff --tabular -ss ../data/graph_9500.txt  > ./output/tabular_ss_9500.tim
../bin/diff --tabular -ss ../data/graph_10000.txt > ./output/tabular_ss_10000.tim

echo "Running tabular SS space profiling"
valgrind --tool=massif --massif-out-file=./output/tabular_ss_500.mem   ../bin/diff -ss --tabular ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_1000.mem  ../bin/diff -ss --tabular ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_1500.mem  ../bin/diff -ss --tabular ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_2000.mem  ../bin/diff -ss --tabular ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_2500.mem  ../bin/diff -ss --tabular ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_3000.mem  ../bin/diff -ss --tabular ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_3500.mem  ../bin/diff -ss --tabular ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_4000.mem  ../bin/diff -ss --tabular ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_4500.mem  ../bin/diff -ss --tabular ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_5000.mem  ../bin/diff -ss --tabular ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_5500.mem  ../bin/diff -ss --tabular ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_6000.mem  ../bin/diff -ss --tabular ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_6500.mem  ../bin/diff -ss --tabular ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_7000.mem  ../bin/diff -ss --tabular ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_7500.mem  ../bin/diff -ss --tabular ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_8000.mem  ../bin/diff -ss --tabular ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_8500.mem  ../bin/diff -ss --tabular ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_9000.mem  ../bin/diff -ss --tabular ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_9500.mem  ../bin/diff -ss --tabular ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/tabular_ss_10000.mem ../bin/diff -ss --tabular ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run Dijkstra SS profiling.
echo "Running dijkstra SS time profiling"
../bin/diff --dijkstra -ss ../data/graph_500.txt   > ./output/dijkstra_ss_500.tim
../bin/diff --dijkstra -ss ../data/graph_1000.txt  > ./output/dijkstra_ss_1000.tim
../bin/diff --dijkstra -ss ../data/graph_1500.txt  > ./output/dijkstra_ss_1500.tim
../bin/diff --dijkstra -ss ../data/graph_2000.txt  > ./output/dijkstra_ss_2000.tim
../bin/diff --dijkstra -ss ../data/graph_2500.txt  > ./output/dijkstra_ss_2500.tim
../bin/diff --dijkstra -ss ../data/graph_3000.txt  > ./output/dijkstra_ss_3000.tim
../bin/diff --dijkstra -ss ../data/graph_3500.txt  > ./output/dijkstra_ss_3500.tim
../bin/diff --dijkstra -ss ../data/graph_4000.txt  > ./output/dijkstra_ss_4000.tim
../bin/diff --dijkstra -ss ../data/graph_4500.txt  > ./output/dijkstra_ss_4500.tim
../bin/diff --dijkstra -ss ../data/graph_5000.txt  > ./output/dijkstra_ss_5000.tim
../bin/diff --dijkstra -ss ../data/graph_5500.txt  > ./output/dijkstra_ss_5500.tim
../bin/diff --dijkstra -ss ../data/graph_6000.txt  > ./output/dijkstra_ss_6000.tim
../bin/diff --dijkstra -ss ../data/graph_6500.txt  > ./output/dijkstra_ss_6500.tim
../bin/diff --dijkstra -ss ../data/graph_7000.txt  > ./output/dijkstra_ss_7000.tim
../bin/diff --dijkstra -ss ../data/graph_7500.txt  > ./output/dijkstra_ss_7500.tim
../bin/diff --dijkstra -ss ../data/graph_8000.txt  > ./output/dijkstra_ss_8000.tim
../bin/diff --dijkstra -ss ../data/graph_8500.txt  > ./output/dijkstra_ss_8500.tim
../bin/diff --dijkstra -ss ../data/graph_9000.txt  > ./output/dijkstra_ss_9000.tim
../bin/diff --dijkstra -ss ../data/graph_9500.txt  > ./output/dijkstra_ss_9500.tim
../bin/diff --dijkstra -ss ../data/graph_10000.txt > ./output/dijkstra_ss_10000.tim

echo "Running dijkstra SS space profiling"
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_500.mem   ../bin/diff --dijkstra -ss ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_1000.mem  ../bin/diff --dijkstra -ss ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_1500.mem  ../bin/diff --dijkstra -ss ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_2000.mem  ../bin/diff --dijkstra -ss ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_2500.mem  ../bin/diff --dijkstra -ss ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_3000.mem  ../bin/diff --dijkstra -ss ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_3500.mem  ../bin/diff --dijkstra -ss ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_4000.mem  ../bin/diff --dijkstra -ss ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_4500.mem  ../bin/diff --dijkstra -ss ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_5000.mem  ../bin/diff --dijkstra -ss ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_5500.mem  ../bin/diff --dijkstra -ss ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_6000.mem  ../bin/diff --dijkstra -ss ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_6500.mem  ../bin/diff --dijkstra -ss ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_7000.mem  ../bin/diff --dijkstra -ss ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_7500.mem  ../bin/diff --dijkstra -ss ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_8000.mem  ../bin/diff --dijkstra -ss ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_8500.mem  ../bin/diff --dijkstra -ss ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_9000.mem  ../bin/diff --dijkstra -ss ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_9500.mem  ../bin/diff --dijkstra -ss ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/dijkstra_ss_10000.mem ../bin/diff --dijkstra -ss ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run DFS SS profiling.
echo "Running DFS SS time profiling"
../bin/diff --DFS -ss ../data/graph_500.txt   > ./output/DFS_ss_500.tim
../bin/diff --DFS -ss ../data/graph_1000.txt  > ./output/DFS_ss_1000.tim
../bin/diff --DFS -ss ../data/graph_1500.txt  > ./output/DFS_ss_1500.tim
../bin/diff --DFS -ss ../data/graph_2000.txt  > ./output/DFS_ss_2000.tim
../bin/diff --DFS -ss ../data/graph_2500.txt  > ./output/DFS_ss_2500.tim
../bin/diff --DFS -ss ../data/graph_3000.txt  > ./output/DFS_ss_3000.tim
../bin/diff --DFS -ss ../data/graph_3500.txt  > ./output/DFS_ss_3500.tim
../bin/diff --DFS -ss ../data/graph_4000.txt  > ./output/DFS_ss_4000.tim
../bin/diff --DFS -ss ../data/graph_4500.txt  > ./output/DFS_ss_4500.tim
../bin/diff --DFS -ss ../data/graph_5000.txt  > ./output/DFS_ss_5000.tim
../bin/diff --DFS -ss ../data/graph_5500.txt  > ./output/DFS_ss_5500.tim
../bin/diff --DFS -ss ../data/graph_6000.txt  > ./output/DFS_ss_6000.tim
../bin/diff --DFS -ss ../data/graph_6500.txt  > ./output/DFS_ss_6500.tim
../bin/diff --DFS -ss ../data/graph_7000.txt  > ./output/DFS_ss_7000.tim
../bin/diff --DFS -ss ../data/graph_7500.txt  > ./output/DFS_ss_7500.tim
../bin/diff --DFS -ss ../data/graph_8000.txt  > ./output/DFS_ss_8000.tim
../bin/diff --DFS -ss ../data/graph_8500.txt  > ./output/DFS_ss_8500.tim
../bin/diff --DFS -ss ../data/graph_9000.txt  > ./output/DFS_ss_9000.tim
../bin/diff --DFS -ss ../data/graph_9500.txt  > ./output/DFS_ss_9500.tim
../bin/diff --DFS -ss ../data/graph_10000.txt > ./output/DFS_ss_10000.tim

echo "Running DFS SS space profiling"
valgrind --tool=massif --massif-out-file=./output/DFS_ss_500.mem   ../bin/diff -ss --DFS ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_1000.mem  ../bin/diff -ss --DFS ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_1500.mem  ../bin/diff -ss --DFS ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_2000.mem  ../bin/diff -ss --DFS ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_2500.mem  ../bin/diff -ss --DFS ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_3000.mem  ../bin/diff -ss --DFS ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_3500.mem  ../bin/diff -ss --DFS ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_4000.mem  ../bin/diff -ss --DFS ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_4500.mem  ../bin/diff -ss --DFS ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_5000.mem  ../bin/diff -ss --DFS ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_5500.mem  ../bin/diff -ss --DFS ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_6000.mem  ../bin/diff -ss --DFS ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_6500.mem  ../bin/diff -ss --DFS ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_7000.mem  ../bin/diff -ss --DFS ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_7500.mem  ../bin/diff -ss --DFS ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_8000.mem  ../bin/diff -ss --DFS ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_8500.mem  ../bin/diff -ss --DFS ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_9000.mem  ../bin/diff -ss --DFS ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_9500.mem  ../bin/diff -ss --DFS ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/DFS_ss_10000.mem ../bin/diff -ss --DFS ../data/graph_10000.txt 2> /dev/null > /dev/null

# Run layering profiling.
echo "Running layer SS time profiling"
../bin/diff --layer -ss ../data/graph_ss_500.txt   > ./output/layer_500.tim
../bin/diff --layer -ss ../data/graph_ss_1000.txt  > ./output/layer_1000.tim
../bin/diff --layer -ss ../data/graph_ss_1500.txt  > ./output/layer_1500.tim
../bin/diff --layer -ss ../data/graph_ss_2000.txt  > ./output/layer_2000.tim
../bin/diff --layer -ss ../data/graph_ss_2500.txt  > ./output/layer_2500.tim
../bin/diff --layer -ss ../data/graph_ss_3000.txt  > ./output/layer_3000.tim
../bin/diff --layer -ss ../data/graph_ss_3500.txt  > ./output/layer_3500.tim
../bin/diff --layer -ss ../data/graph_ss_4000.txt  > ./output/layer_4000.tim
../bin/diff --layer -ss ../data/graph_ss_4500.txt  > ./output/layer_4500.tim
../bin/diff --layer -ss ../data/graph_ss_5000.txt  > ./output/layer_5000.tim
../bin/diff --layer -ss ../data/graph_ss_5500.txt  > ./output/layer_5500.tim
../bin/diff --layer -ss ../data/graph_ss_6000.txt  > ./output/layer_6000.tim
../bin/diff --layer -ss ../data/graph_ss_6500.txt  > ./output/layer_6500.tim
../bin/diff --layer -ss ../data/graph_ss_7000.txt  > ./output/layer_7000.tim
../bin/diff --layer -ss ../data/graph_ss_7500.txt  > ./output/layer_7500.tim
../bin/diff --layer -ss ../data/graph_ss_8000.txt  > ./output/layer_8000.tim
../bin/diff --layer -ss ../data/graph_ss_8500.txt  > ./output/layer_8500.tim
../bin/diff --layer -ss ../data/graph_ss_9000.txt  > ./output/layer_9000.tim
../bin/diff --layer -ss ../data/graph_ss_9500.txt  > ./output/layer_9500.tim
../bin/diff --layer -ss ../data/graph_ss_10000.txt > ./output/layer_10000.tim

echo "Running layer space profiling"
valgrind --tool=massif --massif-out-file=./output/layer_ss_500.mem   ../bin/diff -ss --layer ../data/graph_500.txt   2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_1000.mem  ../bin/diff -ss --layer ../data/graph_1000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_1500.mem  ../bin/diff -ss --layer ../data/graph_1500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_2000.mem  ../bin/diff -ss --layer ../data/graph_2000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_2500.mem  ../bin/diff -ss --layer ../data/graph_2500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_3000.mem  ../bin/diff -ss --layer ../data/graph_3000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_3500.mem  ../bin/diff -ss --layer ../data/graph_3500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_4000.mem  ../bin/diff -ss --layer ../data/graph_4000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_4500.mem  ../bin/diff -ss --layer ../data/graph_4500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_5000.mem  ../bin/diff -ss --layer ../data/graph_5000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_5500.mem  ../bin/diff -ss --layer ../data/graph_5500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_6000.mem  ../bin/diff -ss --layer ../data/graph_6000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_6500.mem  ../bin/diff -ss --layer ../data/graph_6500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_7000.mem  ../bin/diff -ss --layer ../data/graph_7000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_7500.mem  ../bin/diff -ss --layer ../data/graph_7500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_8000.mem  ../bin/diff -ss --layer ../data/graph_8000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_8500.mem  ../bin/diff -ss --layer ../data/graph_8500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_9000.mem  ../bin/diff -ss --layer ../data/graph_9000.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_9500.mem  ../bin/diff -ss --layer ../data/graph_9500.txt  2> /dev/null > /dev/null
valgrind --tool=massif --massif-out-file=./output/layer_ss_10000.mem ../bin/diff -ss --layer ../data/graph_10000.txt 2> /dev/null > /dev/null
