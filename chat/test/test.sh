#!/bin/bash

clients_fix=10
mes_fix=1000

clients_dyn=100
mes_dyn=1000

destination="test_results"
d1=$destination/var_clients
d2=$destination/var_messages

mkdir $destination
mkdir $d1
mkdir $d2

echo Begin user var tests
#users var tes
for (( i=10; i < $clients_dyn; i=i+10 ))
do
 echo Users: $i Messages: $mes_fix
 bash test_cl.sh $d1 $mes_fix $i
done

echo Begin message var tests
#messages var tes
for (( i=10; i < $mes_dyn; i=i+10 ))
do
 echo Users: $clients_fix Messages: $i
 bash test_cl.sh $d2 $i $clients_fix
done

echo Test Finished

