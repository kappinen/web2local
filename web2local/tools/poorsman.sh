#!/bin/bash

ifnull() {
  if [ -z "$1" ]; then
    echo "$2"
  else
    echo "$1"
  fi
}

start(){
   echo "starting to send quit signals to $1 and sleep for $2"
   while(true); do
     kill -quit $1
     sleep $2
   done
}

report(){ 
  cat | awk 'BEGIN { FS="\"" ; RS="\n\n"} { if(match($0, /'$1'/)) { print $0} }' 
}

count(){
  echo "$@" | sort | uniq -c | sort -nr 
}


case "$1" in
  start)
     start $(ifnull $2 `pgrep java`) $(ifnull $3 0.5s)
     ;;
   threads)
     cat | awk 'BEGIN { FS="\"" ; RS="\n\n"} {print $0}' | grep "\"" | sort | uniq -c | sort -nr
     ;;
   report)
     count "`report "$2"`"
     ;;
   *) echo "cmds: start [pid sleep_time] || threads || report [pattern]" ;;
esac
   
