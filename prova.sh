while getopts ":ht" opt; do
  case ${opt} in
    h ) # process option a
      echo "help"
      ;;
    t ) # process option t
      ;;
    \? ) echo "Usage: cmd [-h] [-t]"
      ;;
  esac
done