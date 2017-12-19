A Scala compiler for the  [Forth language](https://www.forth.com/resources/forth-programming-language/#12_Philosophy_and_Goals), a stack-based language that has been used by NASA in [spacecraft](https://en.wikipedia.org/wiki/Forth_(programming_language)#cite_ref-2)

My original intent was to keep this close in design to assembler code I pair-programmed with at Recurse, but then I veered off, using it as an excuse to investigate : rolling my own state monad, then using cats, and exploring the use of a free monad/interpreter pattern (but I couldn't work out how to compose State with Either), and lenses

Test cases pass for most functionality. Interactive code at [scalafiddle.io](https://scalafiddle.io/sf/20fLRS9/4)
