#!/bin/bash

dest=$1
mes=$2
dir=$3
clients_dyn=100

touch dest
echo Begin merge
#users var tes
for (( i=10; i <= $clients_dyn; i=i+10 ))
do
 T="${i}_${mes}"
 cat $dir/$T.txt >> $dest
done

echo 10000,0.0 >> $dest
