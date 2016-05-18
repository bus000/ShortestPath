#!/usr/bin/env bash

valgrind --tool=massif ../bin/diff --tabular ../data/roadNet-CA.txt

time ../bin/diff --tabular ../data/roadNet-CA.txt
