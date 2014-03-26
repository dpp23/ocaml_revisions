#!/bin/bash

DESTINATION=$1
FOLDER="temp"
MESSAGES=$2
USERS=$3
CLIENT="./client.native"
SERVER="./server_maps_pipe_par.native"
LINES="$(( $MESSAGES * 2 ))"
TEST="$3_$2"
mkdir $FOLDER
rm pipe*

gnome-terminal -e "bash test_server.sh $FOLDER" &
bash test_resource.sh $FOLDER &

sleep 1

#start clients
for (( i=0; i < $USERS; i++ ))
do
   touch $FOLDER/$i.txt 
   (bash test_client_check.sh $FOLDER $i $MESSAGES) | (./client.native) > $FOLDER/$i.txt &
done

#check if clients finished
while true; do
    for (( i=0; i < $USERS; i++ ))
    do
       read LINE < <(cat $FOLDER/$i.txt | wc -l | grep '[0-9]*')
       if (( $LINE < $LINES )); 
         then 
           sleep 30
           break;
       elif i==$USERS-1; then break 2;
       fi 
    done
done

#kill clients
killall client.native
killall $SERVER

touch $FOLDER/$TEST.txt
for (( i=0; i < $USERS; i++ ))
do
  cat $FOLDER/$i.txt | ./test_post_process.native -um $USERS >> $FOLDER/$TEST.txt
done  

mv  $FOLDER/$TEST.txt $DESTINATION/$TEST.txt

zip -j $FOLDER/$TEST.zip $FOLDER/*.txt

mv  $FOLDER/$TEST.zip $DESTINATION/$TEST.zip

rm -r $FOLDER

exit















  

