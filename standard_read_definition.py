import read
import read_macro

read_macro.read = read.read

read.is_macro_character = read_macro.is_macro_character
read.get_macro_reader = read_macro.get_macro_reader

read = read.read
