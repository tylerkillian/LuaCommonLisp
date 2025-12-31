set -e

clisp run-all-tests.lsp

COMPILERS=(
	"gcc -std=c90"
       	"g++" 
	"clang -std=c90" 
	"clang++ -x c"
)
for compiler in "${COMPILERS[@]}"
do
	$compiler -pedantic -Wall -Wextra -Werror -I./ -o runAllTests *.c
	./runAllTests
	rm runAllTests
done
