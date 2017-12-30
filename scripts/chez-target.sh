CONFIG_UNAME=`uname || echo Windows`

case "${CONFIG_UNAME}" in
  Linux)
    echo ta6le
    ;;
  Darwin)
    echo ta6osx
    ;;
  Windows)
    echo ta6nt
    ;;
esac
