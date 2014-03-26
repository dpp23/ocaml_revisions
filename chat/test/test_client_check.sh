#!/bin/bash

FOLDER=$1
INDEX=$2
MESSAGES=$3
PIPE=pipe$INDEX
ROOM=3

#reg
echo $INDEX

#create/enter room
if [ $INDEX == 0 ]
then
  echo "create $ROOM"
fi

sleep 1
echo "enter $ROOM" 

sleep 20

#send messages
for (( i=0; i < $MESSAGES; i++ ))
do
   echo "send $ROOM $i"
done
