      ***************************************************************
      *                                                             *
      * This COBOL program was written by Frank Swarbrick (based on *
      * C code found here:                                          *
      * http://stackoverflow.com/questions/1085083/                 *
      *regular-expressions-in-c-examples)                           *
      * with some enhancement suggested by Peter Farley.            *
      * This code demonstrates how one can write a simple regex     *
      * functionality without using PCRE. However, it relies on the *
      * IBM supplied, C standard, C run-time library and its regex, *
      * Posix compliant functions with all the limitations of that  *
      * standard                                                    *
      *                                                             *
      ***************************************************************
      * This code is published here with permission from Mr. Frank  *
      * Swarbrick and under the same BSD licesnce as the rest of the*
      * package.                                                    *
      ***************************************************************
      * One my enhance this program by adding some PARM=            *
      * manipulation to accept pattern and input string from the    *
      * JCL                                                         *
      ***************************************************************
      ****process nodynam pgmname(mixed)
      *process codepage(1047)
       identification division.
       program-id.  'REGEX1'.
       data division.
       working-storage section.
       01  regex.
           05  re-nsub         comp-5    pic s9(8).
           05  re-comp         pointer.
           05  re-cflags       comp-5    pic s9(8).
           05  re-erroff       comp-5    pic s9(8).
           05  re-len          comp-5    pic s9(8).
           05  re-ucoll        comp-5    pic s9(4)  occurs 2.
           05  re-lsub         pointer              occurs 10.
           05  re-esub         pointer              occurs 10.
           05  re-map          display   pic x(256).
           05  re-shift        comp-5    pic s9(4).
           05  re-dbcs         comp-5    pic s9(4).
       77  reti                comp-5    pic s9(8).
       77  msgbuf              display   pic x(100).
      *77  lmsgbuf             comp      pic s9(8).
       procedure division.
           call 'regcomp' using regex
                                content z'¬a[[:alnum:]]'
                                value 0
                returning reti
           if reti is not equal to zero
               display 'Could not compile regex'
               stop run
           end-if
           call 'regexec' using regex
                                content z'abc'
                                value 0 0 0
                returning reti
           perform check-reti
           call 'regexec' using regex
                                content z'qxp'
                                value 0 0 0
                returning reti
           perform check-reti
           call 'regfree' using regex
           goback.

       check-reti.
           evaluate reti
           when zero
               display 'match'
           when 1
               display 'no match'
           when other
      *        move length of msgbuf to lmsgbuf
               call 'regerror' using regex
                                                 msgbuf
                      value length of msgbuf
                    returning reti
               display 'Regex match failed: ' msgbuf
               stop run
           end-evaluate
           .

       end program 'REGEX1'.
