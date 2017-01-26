# Lab 1 - Editors (EMACS)

Objective: Review some of the features of text editors designed for programmers.

I have never worked with emacs before so I have chosen to complete this lab using 
emacs!

1. Open a file with some C source code. Copy the first 12 lines of text from
this file. Create three new files named a.c, b.c, and c.c and paste this text
at the top of each new file. Save each new file.

  * `emacs simple.c` to open the file
  * `C-SPC C-u 12 C-e M-w` to copy the first 12 lines
  * `C-x C-f a.c` to create a new file called "a.c"
  * `C-y` to paste and `C-x C-s` to save the file
  * Do the same for "b.c" and "c.c"

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

4. Given a file with a million lines of text, remove the whitespace (spaces,
tabs, and newlines) from the beginning of every line. That is, when you have
finished, each line should start with a non-whitespace character.

5. Find and replace every occurrence of the string 'Bill Self' in your source
file with the string 'basketball genius Bill Self' (assume that case matters).
After you're done, reformat your file so that each line adheres to an 80
character text width. If you are using vim, you may assume that your vimrc has
the appropriate textwidth and formatoptions settings so that lines are formatted
correctly when the formatting command is used.
