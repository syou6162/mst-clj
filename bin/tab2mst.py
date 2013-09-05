#! /usr/bin/python

# Usage:
# cat ~/Desktop/PENN_TREEBANK3/PARSED/MRG/WSJ/{00,01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23}/*.tab > train_tmp.txt
# python bin/tab2mst.py train_tmp.txt > train.txt
# cat ~/Desktop/PENN_TREEBANK3/PARSED/MRG/WSJ/24/*.tab > test_tmp.txt
# python bin/tab2mst.py test_tmp.txt > test.txt

import sys;

# Open File
f = open(sys.argv[1],'rt');

wrds = ""; pos = ""; labs = ""; par = "";

for line in f:
    sent = line.split();

    if len(sent) > 0:
        wrds += sent[0] + "\t";
        pos += sent[1] + "\t";
        labs += sent[3] + "\t";
        par += sent[2] + "\t";
    else:
        print wrds; wrds = "";
        print pos; pos = "";
        print labs; labs = "";
        print par; par = "";
        print "";

f.close();
