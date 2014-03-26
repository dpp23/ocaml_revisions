#!/bin/bash
#

PROCESSNAME=server_maps_pipe_par.native
FOLDER=$1
OUTPUTFILE=server_resources.txt

touch $FOLDER/$OUTPUTFILE

while true; do
  ps -C $PROCESSNAME -o "%cpu %mem" --no-headers | awk -v file="$FOLDER/$OUTPUTFILE" '{a[$1] = $1; b[$1] += $2; c[$1] += $3}END{for (i in a) printf "%0.1f, %0.1f\n", b[i], c [i] >> file }'
  sleep 1
done

