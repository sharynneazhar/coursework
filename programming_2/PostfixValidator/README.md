### Task: Stack-based postfix expression validator

##### Your program will prompt for and read a series of strings from the console and test whether each represents a valid postfix expression. The strings will have no embedded blanks and will contain some combination of the standard operators (+, -, \*, /) and identifiers (single alphabetic characters that are to be treated, like C++ and Java, as being case-sensitive). If you detect any other character during parsing, reject the string as syntactically invalid. Continue prompting for strings until the user enters a single # at the prompt. A sample session with your program might look like (user input shown in bold italic):

##### Enter a string: AB/cw+*
##### You entered a valid postfix string. The equivalent infix: ((A/B)\*(c+w))
##### Enter a string: XYZ-
##### You entered an invalid postfix string: it is missing one or more operators
##### Enter a string: XY-*
##### You entered an invalid postfix string: it is missing operands
##### Enter a string XY*BC+C3*++
##### You entered an invalid postfix string: it contains the illegal character '3'
##### Enter a string XY*BC+CD*++
##### You entered a valid postfix string. The equivalent infix: ((X*Y)+((B+C)+(C*D)))
##### Enter a string: #
