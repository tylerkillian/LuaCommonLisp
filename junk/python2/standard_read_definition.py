import read
import read_macro
import symbol

read_macro.read = read.read

symbol.is_macro_character = read_macro.is_macro_character

read.is_macro_character = read_macro.is_macro_character
read.get_macro_reader = read_macro.get_macro_reader
read.read_symbol = symbol.read_symbol

read = read.read
