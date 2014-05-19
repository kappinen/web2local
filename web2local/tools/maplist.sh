#!/bin/bash 
set -o pipefail

export PROCESSING_LIST="${HOME}/.blist/.list"
export WINDOW_FRAME=0

#set -x
f() {
  eval $(echo "${1}() { ${*:2} }")
  export -f "$1"
}

suffix_opener() {
  case   `echo "${1##*.}"| awk '{print tolower($0)}'` in
   "txt" | "c" | "cpp" | "hpp" | "scala" | "sh" ) vi "$@";;
   "jpeg" | "jpg" | "gif" | "tif") geeqie "$@";;
   "mpeg" | "mpg" | "avi" | "flv") mplayer "$@";;
   "doc" | "docx") libreoffice "$@";;
   "r") rstudio "$@";;
   "pdf") evince "$@";;
   *)  pico "$@";;
  esac

}

f 'redirect_fds' 'exec 0>&-; exec 0<>/proc/self/fd/1;'
f 'max_window' 'echo $(($(tput lines) - 4));'
f 'max' 'echo $(($1>$2?$1:$2));'
f 'min' 'echo $(($1<$2?$1:$2));'
f 'abs' 'echo $(($1>0?$1:-$1));'
f 'safe_position' 'echo $(min $(max $1 $3) $2);'
f 'fdirect' 'echo $(($1 - $(safe_position 0 $WINDOW_LEN $1)));'
f 'fast_read' 'sed -n -e $1,$2p -e $(($2 + 1))q $3;'

f 'next_frame' 'echo ${PROCESSING_LIST}.$(($WINDOW_FRAME + 1));'
f 'previous_frame' 'echo ${PROCESSING_LIST}.$(max 0 $(($WINDOW_FRAME - 1)));'
f 'current_frame' 'echo ${PROCESSING_LIST}.${WINDOW_FRAME};'

f 'delete_line' 'sed $(max 1 $(($WINDOW_POS + $FILE_START - 1)))d $(current_frame) > $(next_frame); WINDOW_FRAME=$(($WINDOW_FRAME + 1)); refresh_view;'
f 'undo' 'WINDOW_FRAME=$(max 0 $(($WINDOW_FRAME - 1))); refresh_view;'

mapXargs() {
 xargs -P 4 -I LINE bash -c 'mapFunc LINE' 
}

flines() { 
  echo $(wc -l $(current_frame) | awk '{split($0,a," "); print a[1]}') 
}

nth_line() { lineNo=$1; cat|sed -n "${lineNo}{p;q;}"; }
print_content() { echo -e "`cat|sed $(max 1 $1)'s/^/\\\033[1m->/'| sed $(max 1 $1)'s/$/\\\033[0m/'`" ; }

exec_line() {
  line=$(echo "$VIEW"|nth_line $2)
  eval $1 \"$line\"
}

read_line() {
  stty sane
  read -ep $1 TEMPINPUT
  echo $TEMPINPUT
  stty -echo
}

keys(){
  bind '"\e[21~":"clear;ls -lsa\n"'
  bind '"\e[24~":"exit\n"'
  bind '"\e[B":"export WINDOW_POS=$(($WINDOW_POS + 1));shown\n"'
  bind '"\e[A":"export WINDOW_POS=$(($WINDOW_POS - 1));shown\n"'
  bind '"\e[5~":"export WINDOW_POS=$(($WINDOW_POS - $WINDOW_LEN));shown\n"'
  bind '"\e[6~":"export WINDOW_POS=$(($WINDOW_POS + $WINDOW_LEN));shown\n"'
  bind '"\e[H":"export WINDOW_POS=$((-1 * $FILE_LEN));shown\n"'
  bind '"\e[F":"export WINDOW_POS=$FILE_LEN;shown\n"'

  bind '"\ep":"exec_line pico $WINDOW_POS\n"'
  bind '"\ed":"delete_line;shown\n"'
  bind '"\eu":"undo;shown\n"'
  bind '"\ee":"egrep $(read_line regexp:) $(current_frame) > $(next_frame);WINDOW_FRAME=$(($WINDOW_FRAME + 1));  refresh_view;shown;\n"'
  bind '"\eg":"grep $(read_line string:)  $(current_frame) > $(next_frame);WINDOW_FRAME=$(($WINDOW_FRAME + 1));  refresh_view;shown;\n"'
  bind '"\ev":"grep -v $(read_line string:) $(current_frame) > $(next_frame);WINDOW_FRAME=$(($WINDOW_FRAME + 1));  refresh_view;shown;\n"'
  bind '"\em":" f mapFunc $(read_line func:);  cat $(current_frame) | mapXargs > $(next_frame);  WINDOW_FRAME=$(($WINDOW_FRAME + 1));  refresh_view;shown;\n"'
  bind '"\eo":"exec_line suffix_opener $WINDOW_POS\n"'
}

init() {
  export PS1="$PS1[\$WINDOW_POS,\$fchange,\$WINDOW_LEN]:[\$FILE_START,\$FILE_END,\$FILE_LEN] [\$WINDOW_FRAME]"
  export WINDOW_POS=0
  export FILE_END=$(wc -l $(current_frame) | awk '{split($0,a," "); print a[1]}')
  export FILE_LEN=$FILE_END
  export WINDOW_LEN=$(echo $(($(tput lines) - 2)))
  export WINDOW_FRAME=0

  trap "stty sane" EXIT
  stty -echo
}

refresh_view() {
  export FILE_END=$(wc -l $(current_frame) | awk '{split($0,a," "); print a[1]}')
  export FILE_LEN=$FILE_END

  export FILE_START=$(safe_position 1 $(abs $(($FILE_LEN-$WINDOW_LEN))) $(($FILE_START + $fchange)))
  export FILE_END=$(($FILE_START + $WINDOW_LEN))

  export VIEW=`fast_read $FILE_START $FILE_END $(current_frame)`
}

shown() {
  fchange=$(fdirect $WINDOW_POS)
#  echo "file start:$FILE_START file end:$FILE_END pos: $WINDOW_POS fchange:$fchange"
  export WINDOW_POS=$(safe_position 0 $(min $FILE_LEN $(($WINDOW_LEN + 1))) $WINDOW_POS)

  if [ $fchange -ne 0 -o $WINDOW_POS -eq 0 ]; then
    export FILE_START=$(safe_position 1 $(abs $(($FILE_LEN-$WINDOW_LEN))) $(($FILE_START + $fchange)))
    export FILE_END=$(($FILE_START + $WINDOW_LEN))
    
    export VIEW=`fast_read $FILE_START $FILE_END $(current_frame)`
  fi

  clear
  print_content $WINDOW_POS <<< "$VIEW"

}
export -f f shown print_content keys nth_line flines exec_line init refresh_view mapXargs suffix_opener read_line 

(cat)>$(current_frame)
redirect_fds
bash --rcfile <(echo "init;keys;shown") -i 
