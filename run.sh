set -e

gcc -Werror -I./ -c *.c
gcc -o main *.o
./main
rm main *.o
