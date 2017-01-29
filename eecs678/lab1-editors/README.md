# Lab 1 - Editors (EMACS)

Objective: Review some of the features of text editors designed for programmers.

1. Open a file with some C source code. Copy the first 12 lines of text from
this file. Create three new files named a.c, b.c, and c.c and paste this text
at the top of each new file. Save each new file.

  * `emacs simple.c` to open the file
  * `C-SPC C-u 12 C-e M-w` to copy the first 12 lines
  * `C-x C-f a.c` to create a new file called `a.c`
  * `C-y` to paste and `C-x C-s` to save the file
  * Do the same for `b.c` and `c.c`

2. Open two different source files for editing, ensuring both are visible on
screen simultaneously, and switch between editing each of these two files and
issuing commands to a terminal you have open.

  * We are currently in the `simple.c` buffer
  * `C-x 4 f a.c` will open the `a.c` buffer in another window
  * `C-x o` to switch between windows  

3. As you are reading the code for a large C program (with multiple source
files spanned across multiple directories), you come across a call to an
unknown function. Find the definition of this function. Go back to the
calling context where you started.

  * Emacs doesn't know about function declarations for C/C++ by default, we
  need to use the `etags` command to generate a `TAGS` file that contains
  a list of references to the function definitions
  * See how `etags` work by running `etags --help`
  * `etags --declarations *.h *.c` will generate the list of declarations
  tags from any `.h` and `.c` files
  * `M-.` or `M-x find-tag <function_name>` to begin the search
  * `M-x find-tag-other-window` opens a new buffer window
  * `C-x <left-arrow-key>` to go back to the previous buffer  

4. Given a file with a million lines of text, remove the whitespace (spaces,
tabs, and newlines) from the beginning of every line. That is, when you have
finished, each line should start with a non-whitespace character.

  * `M-x replace-regexp ^[[:space:]]*` will remove any leading whitespace from
  each line
    * `^` indicates the start of a line
    * `[...]` indicates a grouping of characters
    * `[:space:]` indicates the syntax class for any whitespace character
    * `*` indicates repetition of previous character 0 or more times

5. Find and replace every occurrence of the string 'Bill Self' in your source
file with the string 'basketball genius Bill Self' (assume that case matters).
After you're done, reformat your file so that each line adheres to an 80
character text width. If you are using vim, you may assume that your vimrc has
the appropriate textwidth and formatoptions settings so that lines are formatted
correctly when the formatting command is used.

  * `M-x replace-string` will replace `Bill Self` with `basketball genius Bill
  Self`
  * `C-u 80 C-x f` to set the column width to 80 characters if it wasn't the
  default already
  * Move the cursor to the beginning of the paragraph needing formatting,
  `M-q` to reformat a paragraph to the set column width, in this case,
  80 characters
  * `M-x auto-fill-mode` toggles auto-wrapping as you type


##### Resources

[Emacs cheatsheet](http://www.rgrjr.com/emacs/emacs_cheat.html)  
[Emacs multiple windows](https://www.fnal.gov/docs/products/emacs/emacs/emacs_20.html#SEC156)  
[Emacs finding function declarations](http://stackoverflow.com/questions/4759211/what-is-the-short-cut-key-to-jump-to-declaration-in-emacs)  
[Emacs replacing commands](https://www.gnu.org/software/emacs/manual/html_node/emacs/Replace.html)
