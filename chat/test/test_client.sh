#!/bin/bash

FOLDER=$1
INDEX=$2
MESSAGES=$3
PIPE=pipe$INDEX
ROOM=3

mkfifo $PIPE
touch $FOLDER/$INDEX.txt
./client.native < $PIPE > $FOLDER/$INDEX.txt &

#reg
echo $INDEX >> $PIPE

#create/enter room
if [ $INDEX == 0 ]
then
  echo "create $ROOM" >> $PIPE
fi

sleep 1
echo "enter $ROOM" >> $PIPE

sleep 20

#send messages
for (( i=0; i < $MESSAGES; i++ ))
do
   echo "send $ROOM $i" >> $PIPE
done
