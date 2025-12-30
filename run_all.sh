clisp run-all-tests.lsp

gcc -std=c90 -pedantic -I./ -o cle *.c
./cle run-all-tests.lsp
rm cle

g++ -I./ -o cle *.c
./cle run-all-tests.lsp
rm cle
