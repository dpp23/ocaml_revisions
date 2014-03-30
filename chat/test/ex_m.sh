#!/bin/bash

dest=$1
users=$2
dir=$3
clients_dyn=1000

touch dest
echo Begin merge
#users var tes
for (( i=100; i <= $clients_dyn; i=i+100 ))
do
 T="${users}_${i}"
 cat $dir/$T.txt >> $dest
done

echo 10000,0.0 >> $dest
