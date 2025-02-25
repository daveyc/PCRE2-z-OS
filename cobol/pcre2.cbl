      **************************************************
      *       Perl-Compatible Regular Expressions      *
      **************************************************

      * This is a port of the public header (pcre2.h) file for the
      * PCRE library, second API, to COBOL.  It is to be COPIED by
      * applications that call the PCRE functions.
      * Version 0.3
      * Contributed by:   Ze'ev Atlas  2013.
      * Copyright (c) 2013-2017 Ze'ev Atlas.
      * All rights reserved.

      *---------------------------------------------------------------
      *Redistribution and use in source and binary forms, with or
      *without modification, are permitted provided that the following
      *conditions are met:

      * 1. Redistributions of source code must retain the above
      * copyright notice, this list of conditions and the following
      * disclaimer.

      * 2. Redistributions in binary form must reproduce the above
      * copyright notice, this list of conditions and the following
      * disclaimer in the documentation and/or other materials
      * provided with the distribution.

      * 3. Neither the name of the University of Cambridge nor the
      * names of its contributors may be used to endorse or promote
      * products derived from this software without specific prior
      * written permission.

      *THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
      *CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
      *INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
      *MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
      *DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
      *CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
      *SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
      *NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
      *LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
      *HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
      *CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
      *OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
      *EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
      *---------------------------------------------------------------

      * The current PCRE version information.

       01 PCRE2-MAJOR       PIC S9(9) COMP VALUE  10.
       01 PCRE2-MINOR       PIC S9(9) COMP VALUE  41.
       01 PCRE2-PRERELEASE  PIC X(4)  VALUE  '    '.
       01 PCRE2-DATE        PIC X(10) VALUE  '2022-12-06'.

      * The following option bits can be passed to pcre2_compile(),
      * pcre2_match(), or pcre2_dfa_match(). PCRE2_NO_UTF_CHECK
      * affects only the function to which it is passed. Put these
      * bits at the most significant end of the options word so
      * others can be added next to them

       01 PCRE2-ANCHORED-x          PIC X(4) VALUE    x'80000000'.
       01 PCRE2-ANCHORED
             REDEFINES PCRE2-ANCHORED-x PIC 9(9) COMP.
       01 PCRE2-NO-UTF-CHECK-x      PIC X(4) VALUE    x'40000000'.
       01 PCRE2-NO-UTF-CHECK
             REDEFINES PCRE2-NO-UTF-CHECK-x PIC 9(9) COMP.
       01 PCRE2-ENDANCHORED-x       PIC X(4) VALUE    x'20000000'.
       01 PCRE2-ENDANCHORED
             REDEFINES PCRE2-ENDANCHORED-x PIC 9(9) COMP.

      * The following option bits can be passed only to
      * pcre2_compile(). However, they may affect compilation, JIT
      * compilation, and/or interpretive execution.
      * The following tags indicate which:

      * C   alters what is compiled by pcre2_compile()
      * J   alters what is compiled by pcre2_jit_compile()
      * M   is inspected during pcre2_match() execution
      * D   is inspected during pcre2_dfa_match() execution

       01 PCRE2-ALLOW-EMPTY-CLASS-x PIC X(4) VALUE    x'00000001'.
       01 PCRE2-ALLOW-EMPTY-CLASS
             REDEFINES PCRE2-ALLOW-EMPTY-CLASS-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-ALT-BSUX-x          PIC X(4) VALUE    x'00000002'.
       01 PCRE2-ALT-BSUX
             REDEFINES PCRE2-ALT-BSUX-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-AUTO-CALLOUT-x      PIC X(4) VALUE    x'00000004'.
       01 PCRE2-AUTO-CALLOUT
             REDEFINES PCRE2-AUTO-CALLOUT-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-CASELESS-x          PIC X(4) VALUE    x'00000008'.
       01 PCRE2-CASELESS
             REDEFINES PCRE2-CASELESS-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-DOLLAR-ENDONLY-x    PIC X(4) VALUE    x'00000010'.
       01 PCRE2-DOLLAR-ENDONLY
             REDEFINES PCRE2-DOLLAR-ENDONLY-x PIC 9(9) COMP.
      *   J M D */
       01 PCRE2-DOTALL-x            PIC X(4) VALUE    x'00000020'.
       01 PCRE2-DOTALL
             REDEFINES PCRE2-DOTALL-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-DUPNAMES-x          PIC X(4) VALUE    x'00000040'.
       01 PCRE2-DUPNAMES
             REDEFINES PCRE2-DUPNAMES-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-EXTENDED-x          PIC X(4) VALUE    x'00000080'.
       01 PCRE2-EXTENDED
             REDEFINES PCRE2-EXTENDED-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-FIRSTLINE-x         PIC X(4) VALUE    x'00000100'.
       01 PCRE2-FIRSTLINE
             REDEFINES PCRE2-FIRSTLINE-x PIC 9(9) COMP.
      *   J M D */
       01 PCRE2-MATCH-UNSET-BACKREF-x pIC X(4) VALUE  x'00000200'.
       01 PCRE2-MATCH-UNSET-BACKREF
             REDEFINES PCRE2-MATCH-UNSET-BACKREF-x PIC 9(9) COMP.
      * C J M   */
       01 PCRE2-MULTILINE-x         PIC X(4) VALUE    x'00000400'.
       01 PCRE2-MULTILINE
             REDEFINES PCRE2-MULTILINE-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-NEVER-UCP-x         PIC X(4) VALUE    x'00000800'.
       01 PCRE2-NEVER-UCP
             REDEFINES PCRE2-NEVER-UCP-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-NEVER-UTF-x         PIC X(4) VALUE    x'00001000'.
       01 PCRE2-NEVER-UTF
             REDEFINES PCRE2-NEVER-UTF-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-NO-AUTO-CAPTURE-x   PIC X(4) VALUE    x'00002000'.
       01 PCRE2-NO-AUTO-CAPTURE
             REDEFINES PCRE2-NO-AUTO-CAPTURE-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-NO-AUTO-POSSESS-x   PIC X(4) VALUE    x'00004000'.
       01 PCRE2-NO-AUTO-POSSESS
             REDEFINES PCRE2-NO-AUTO-POSSESS-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-NO-DOTSTAR-ANCHOR-x PIC X(4) VALUE    x'00008000'.
       01 PCRE2-NO-DOTSTAR-ANCHOR
             REDEFINES PCRE2-NO-DOTSTAR-ANCHOR-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-NO-START-OPTIMIZE-x PIC X(4) VALUE    x'00010000'.
       01 PCRE2-NO-START-OPTIMIZE
             REDEFINES PCRE2-NO-START-OPTIMIZE-x PIC 9(9) COMP.
      *   J M D */
       01 PCRE2-UCP-x               PIC X(4) VALUE    x'00020000'.
       01 PCRE2-UCP
             REDEFINES PCRE2-UCP-x PIC 9(9) COMP.
      * C J M D */
       01 PCRE2-UNGREEDY-x          PIC X(4) VALUE    x'00040000'.
       01 PCRE2-UNGREEDY
             REDEFINES PCRE2-UNGREEDY-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-UTF-x               PIC X(4) VALUE    x'00080000'.
       01 PCRE2-UTF
             REDEFINES PCRE2-UTF-x PIC 9(9) COMP.
      * C J M D */
       01 PCRE2-NEVER-BACKSLASH-C-x PIC X(4) VALUE    x'00100000'.
       01 PCRE2-NEVER-BACKSLASH-C
             REDEFINES PCRE2-NEVER-BACKSLASH-C-x PIC 9(9) COMP.
      * C       */
       01 PCRE2-ALT-CIRCUMFLEX-x    PIC X(4) VALUE    x'00200000'.
       01 PCRE2-ALT-CIRCUMFLEX
             REDEFINES PCRE2-ALT-CIRCUMFLEX-x PIC 9(9) COMP.
      *   J M D */
       01 PCRE2-ALT-VERBNAMES-x    PIC X(4)  VALUE    x'00400000'.
       01 PCRE2-ALT-VERBNAMES
             REDEFINES PCRE2-ALT-VERBNAMES-x  PIC 9(9) COMP.
      * C       */
       01 PCRE2-USE-OFFSET-LIMIT-x PIC X(4)  VALUE    x'00800000'.
       01 PCRE2-USE-OFFSET-LIMIT
             REDEFINES PCRE2-USE-OFFSET-LIMIT-x  PIC 9(9) COMP.
      *   J M D */
       01 PCRE2-EXTENDED-MORE-x    PIC X(4)  VALUE    x'01000000'.
       01 PCRE2-EXTENDED-MORE
             REDEFINES PCRE2-EXTENDED-MORE-x  PIC 9(9) COMP.
      * C       */
       01 PCRE2-LITERAL-x          PIC X(4)  VALUE    x'02000000'.
       01 PCRE2-LITERAL
             REDEFINES PCRE2-LITERAL-x  PIC 9(9) COMP.
      * C       */
       01 PCRE2-MATCH-INVALID-UTF-x  PIC X(4)  VALUE  x'04000000'.
      *   J M D */
       01 PCRE2-MATCH-INVALID-UTF
             REDEFINES PCRE2-MATCH-INVALID-UTF-x  PIC 9(9) COMP.


      * An additional compile options word is available in the compile
      * context. */

       01 PCRE2-EXTRA-ALLOW-SURRGT-ESC-x PIC x(4) VALUE x'00000001'.
       01 PCRE2-EXTRA-ALLOW-SURRGT-ESC
             REDEFINES PCRE2-EXTRA-ALLOW-SURRGT-ESC-x
                                        PIC 9(9) COMP.
      * C */
       01 PCRE2-EXTRA-BAD-ESC-IS-LITRL-x PIC x(4) VALUE x'00000002'.
       01 PCRE2-EXTRA-BAD-ESC-IS-LITRL
             REDEFINES PCRE2-EXTRA-BAD-ESC-IS-LITRL-x
                                        PIC 9(9) COMP.
      * C */
       01 PCRE2-EXTRA-MATCH-WORD-x     PIC x(4) VALUE x'00000004'.
       01 PCRE2-EXTRA-MATCH-WORD
             REDEFINES PCRE2-EXTRA-MATCH-WORD-x PIC 9(9) COMP.
      * C */
       01 PCRE2-EXTRA-MATCH-LINE-x     PIC x(4) VALUE x'00000008'.
       01 PCRE2-EXTRA-MATCH-WORD
             REDEFINES PCRE2-EXTRA-MATCH-LINE-x  PIC 9(9) COMP.
      * C */
       01 PCRE2-EXTRA-ESCAPED-CR-IS-LF-x PIC x(4) VALUE x'00000010'.
       01 PCRE2-EXTRA-ESCAPED-CR-IS-LF
             REDEFINES PCRE2-EXTRA-ESCAPED-CR-IS-LF-x
                                        PIC 9(9) COMP.
      * C */
       01 PCRE2-EXTRA-ALT-BSUX-x       PIC x(4) VALUE x'00000020'.
       01 PCRE2-EXTRA-ALT-BSUX
             REDEFINES PCRE2-EXTRA-ALT-BSUX-x  PIC 9(9) COMP.
      * C */
       01 PCRE2-EXTRA-ALLOW-LKARND-BSK-x PIC x(4) VALUE x'00000040'.
       01 PCRE2-EXTRA-ALLOW-LKARND-BSK
             REDEFINES PCRE2-EXTRA-ALLOW-LKARND-BSK-x
			                           PIC 9(9) COMP.

      * These are for pcre2_jit_compile(). */

       01 PCRE2-JIT-COMPLETE-x      PIC X(4) VALUE    x'00000001'.
       01 PCRE2-JIT-COMPLETE
             REDEFINES PCRE2-JIT-COMPLETE-x PIC 9(9) COMP.
      * For full matching */
       01 PCRE2-JIT-PARTIAL-SOFT-x  PIC X(4) VALUE    x'00000002'.
       01 PCRE2-JIT-PARTIAL-SOFT
             REDEFINES PCRE2-JIT-PARTIAL-SOFT-x PIC 9(9) COMP.
       01 PCRE2-JIT-PARTIAL-HARD-x  PIC X(4) VALUE    x'00000004'.
       01 PCRE2-JIT-PARTIAL-HARD
             REDEFINES PCRE2-JIT-PARTIAL-HARD-x PIC 9(9) COMP.
       01 PCRE2-JIT-INVALID-UTF-x   PIC X(4) VALUE    x'00000100'.
       01 PCRE2-JIT-INVALID-UTF
             REDEFINES PCRE2-JIT-INVALID-UTF-x PIC 9(9) COMP.

      * These are for pcre2_match(), pcre2_dfa_match(), and
      * pcre2_jit_match(). Note that PCRE2_ANCHORED and
      * PCRE2_NO_UTF_CHECK can also be passed to these functions
      * (though pcre2_jit_match() ignores the latter since it
      * bypasses all sanity checks).

       01 PCRE2-NOTBOL-x            PIC X(4) VALUE    x'00000001'.
       01 PCRE2-NOTBOL
             REDEFINES PCRE2-NOTBOL-x PIC 9(9) COMP.
       01 PCRE2-NOTEOL-x            PIC X(4) VALUE    x'00000002'.
       01 PCRE2-NOTEOL
             REDEFINES PCRE2-NOTEOL-x PIC 9(9) COMP.
       01 PCRE2-NOTEMPTY-x          PIC X(4) VALUE    x'00000004'.
       01 PCRE2-NOTEMPTY
             REDEFINES PCRE2-NOTEMPTY-x PIC 9(9) COMP.
      * ) These two must be kept */
       01 PCRE2-NOTEMPTY-ATSTART-x  PIC X(4) VALUE    x'00000008'.
       01 PCRE2-NOTEMPTY-ATSTART
             REDEFINES PCRE2-NOTEMPTY-ATSTART-x PIC 9(9) COMP.

      * ) adjacent to each other. */
       01 PCRE2-PARTIAL-SOFT-x      PIC X(4) VALUE    x'00000010'.
       01 PCRE2-PARTIAL-SOFT
             REDEFINES PCRE2-PARTIAL-SOFT-x PIC 9(9) COMP.
       01 PCRE2-PARTIAL-HARD-x      PIC X(4) VALUE    x'00000020'.
       01 PCRE2-PARTIAL-HARD
             REDEFINES PCRE2-PARTIAL-HARD-x PIC 9(9) COMP.

      * These are additional options for pcre2_dfa_match(). */

       01 PCRE2-DFA-RESTART-x       PIC X(4) VALUE    x'00000040'.
       01 PCRE2-DFA-RESTART
             REDEFINES PCRE2-DFA-RESTART-x PIC 9(9) COMP.
       01 PCRE2-DFA-SHORTEST-x      PIC X(4) VALUE    x'00000080'.
       01 PCRE2-DFA-SHORTEST
             REDEFINES PCRE2-DFA-SHORTEST-x PIC 9(9) COMP.

      * This is an additional option for pcre2_substitute(). */

       01 PCRE2-SUBSTITUTE-GLOBAL-x PIC X(4) VALUE    x'00000100'.
       01 PCRE2-SUBSTITUTE-GLOBAL
             REDEFINES PCRE2-SUBSTITUTE-GLOBAL-x PIC 9(9) COMP.
       01 PCRE2-SUBSTITUTE-EXTENDED-x PIC X(4)   VALUE x'00000200'.
       01 PCRE2-SUBSTITUTE-EXTENDED
             REDEFINES PCRE2-SUBSTITUTE-EXTENDED-x PIC 9(9) COMP.
       01 PCRE2-SUBSTITUTE-UNSET-EMPTY-x PIC X(4) VALUE x'00000400'.
       01 PCRE2-SUBSTITUTE-UNSET-EMPTY
             REDEFINES PCRE2-SUBSTITUTE-UNSET-EMPTY-x PIC 9(9) COMP.
       01 PCRE2-SUBSTITUTE-UNKNOWN-UNS-x PIC X(4) VALUE x'00000800'.
       01 PCRE2-SUBSTITUTE-UNKNOWN-UNSET
             REDEFINES PCRE2-SUBSTITUTE-UNKNOWN-UNS-x PIC 9(9) COMP.
       01 PCRE2-SUBSTITUTE-OVERFLOW-LN-x PIC X(4) VALUE x'00001000'.
       01 PCRE2-SUBSTITUTE-OVERFLOW-LEN
             REDEFINES PCRE2-SUBSTITUTE-OVERFLOW-LN-x PIC 9(9) COMP.
      * A further option for pcre2_match(), not allowed for
      * pcre2_dfa_match(), ignored for pcre2_jit_match().
       01 PCRE2-NO-JIT-x                 PIC X(4) VALUE x'00002000'.
       01 PCRE2-NO-JIT
             REDEFINES PCRE2-NO-JIT-x    PIC 9(9) COMP.
       01 PCRE2-COPY-MATCHED-SUBJECT-x   PIC X(4) VALUE x'00004000'.
       01 PCRE2-COPY-MATCHED-SUBJECT
             REDEFINES PCRE2-COPY-MATCHED-SUBJECT-x   PIC 9(9) COMP.
       01 PCRE2-SUBSTITUTE-LITERAL-x     PIC X(4) VALUE x'00008000'.
       01 PPCRE2-SUBSTITUTE-LITERAL
             REDEFINES PCRE2-SUBSTITUTE-LITERAL-x     PIC 9(9) COMP.
      * pcre2_substitute() only */
       01 PCRE2-SUBSTITUTE-MATCHED-x     PIC X(4) VALUE x'00010000'.
       01 PCRE2-SUBSTITUTE-MATCHED
             REDEFINES PCRE2-SUBSTITUTE-MATCHED-x     PIC 9(9) COMP.
      * pcre2_substitute() only */
       01 PCRE2-SUBSTITUTE-REPLCMENT-O-x PIC X(4) VALUE x'00020000'.
       01 PCRE2-SUBSTITUTE-REPLCMENT-O
             REDEFINES PCRE2-SUBSTITUTE-REPLCMENT-O-x PIC 9(9) COMP.
      * pcre2_substitute() only */

      * Options for pcre2_pattern_convert(). */

       01 PCRE2-CONVERT-UTF-x             PIC X(4) VALUE x'00000001'.
       01 PCRE2-CONVERT-UTF
             REDEFINES PCRE2-CONVERT-UTF-x PIC 9(9) COMP.
       01 PCRE2-CONVERT-NO-UTF-CHECK-x    PIC X(4) VALUE x'00000002'.
       01 PCRE2-CONVERT-NO-UTF-CHECK
             REDEFINES PCRE2-CONVERT-NO-UTF-CHECK-x PIC 9(9) COMP.
       01 PCRE2-CONVERT-POSIX-BASIC-x     PIC X(4) VALUE x'00000004'.
       01 PCRE2-CONVERT-POSIX-BASIC
             REDEFINES PCRE2-CONVERT-POSIX-BASIC-x PIC 9(9) COMP.
       01 PCRE2-CONVERT-POSIX-EXTENDED-x  PIC X(4) VALUE x'00000008'.
       01 PCRE2-CONVERT-POSIX-EXTENDED
             REDEFINES PCRE2-CONVERT-POSIX-EXTENDED-x PIC 9(9) COMP.
       01 PCRE2-CONVERT-GLOB-x            PIC X(4) VALUE x'00000010'.
       01 PCRE2-CONVERT-GLOB
             REDEFINES PCRE2-CONVERT-GLOB-x PIC 9(9) COMP.
       01 PCRE2-CONVERT-GLOB-NOWLD-SPR-x  PIC X(4) VALUE x'00000030'.
       01 PCRE2-CONVERT-GLOB-NOWLD-SPR
             REDEFINES PCRE2-CONVERT-GLOB-NOWLD-SPR-x PIC 9(9) COMP.
       01 PCRE2-CONVERT-GLOB-NO-STRSTR-x  PIC X(4) VALUE x'00000050'.
       01 PCRE2-CONVERT-GLOB-NO-STRSTR
             REDEFINES PCRE2-CONVERT-GLOB-NO-STRSTR-x PIC 9(9) COMP.

      * Newline and \R settings, for use in compile contexts. The
      * newline values must be kept in step with values set in
      * config.h and both sets must all be greater than zero.

       01 PCRE2-NEWLINE-CR          PIC S9(9) COMP VALUE 1.
       01 PCRE2-NEWLINE-LF          PIC S9(9) COMP VALUE 2.
       01 PCRE2-NEWLINE-CRLF        PIC S9(9) COMP VALUE 3.
       01 PCRE2-NEWLINE-ANY         PIC S9(9) COMP VALUE 4.
       01 PCRE2-NEWLINE-ANYCRLF     PIC S9(9) COMP VALUE 5.
       01 PCRE2-NEWLINE-NUL         PIC S9(9) COMP VALUE 6.

       01 PCRE2-BSR-UNICODE         PIC S9(9) COMP VALUE 1.
       01 PCRE2-BSR-ANYCRLF         PIC S9(9) COMP VALUE 2.

       01 PCRE2-COB-ERROR-CODES.

      * Error codes for pcre2_compile(). Some of these are also used by
      * pcre2_pattern_convert().

           05 RE2ER-END-BACKSLASH
                                    PIC s9(9) COMP VALUE 101.
           05 RE2ER-END-BACKSLASH-C
                                    PIC s9(9) COMP VALUE 102.
           05 RE2ER-UNKNOWN-ESCAPE
                                    PIC s9(9) COMP VALUE 103.
           05 RE2ER-QUANTIFIER-OUT-OF-ORDER
                                    PIC s9(9) COMP VALUE 104.
           05 RE2ER-QUANTIFIER-TOO-BIG
                                    PIC s9(9) COMP VALUE 105.
           05 RE2ER-MISSING-SQUARE-BRACKET
                                    PIC s9(9) COMP VALUE 106.
           05 RE2ER-ESCAPE-INVALID-IN-CLASS
                                    PIC s9(9) COMP VALUE 107.
           05 RE2ER-CLASS-RANGE-ORDER
                                    PIC s9(9) COMP VALUE 108.
           05 RE2ER-QUANTIFIER-INVALID
                                    PIC s9(9) COMP VALUE 109.
           05 RE2ER-INTERNAL-UNEXPECTED-REPT
                                    PIC s9(9) COMP VALUE 110.
           05 RE2ER-INVALID-AFTER-PARENS-QRY
                                    PIC s9(9) COMP VALUE 111.
           05 RE2ER-POSIX-CLASS-NOT-IN-CLASS
                                    PIC s9(9) COMP VALUE 112.
           05 RE2ER-POSIX-NO-SUPPORT-COLLATI
                                    PIC s9(9) COMP VALUE 113.
           05 RE2ER-MISSING-CLOSING-PARENTHE
                                    PIC s9(9) COMP VALUE 114.
           05 RE2ER-BAD-SUBPATTERN-REFERENCE
                                    PIC s9(9) COMP VALUE 115.
           05 RE2ER-NULL-PATTERN
                                    PIC s9(9) COMP VALUE 116.
           05 RE2ER-BAD-OPTIONS
                                    PIC s9(9) COMP VALUE 117.
           05 RE2ER-MISSING-COMMENT-CLOSING
                                    PIC s9(9) COMP VALUE 118.
           05 RE2ER-PARENTHESES-NEST-TOO-DP
                                    PIC s9(9) COMP VALUE 119.
           05 RE2ER-PATTERN-TOO-LARGE
                                    PIC s9(9) COMP VALUE 120.
           05 RE2ER-HEAP-FAILED
                                    PIC s9(9) COMP VALUE 121.
           05 RE2ER-UNMATCHED-CLOSING-PAREN
                                    PIC s9(9) COMP VALUE 122.
           05 RE2ER-INTERNAL-CODE-OVERFLOW
                                    PIC s9(9) COMP VALUE 123.
           05 RE2ER-MISSING-CONDITION-CLOSIN
                                    PIC s9(9) COMP VALUE 124.
           05 RE2ER-LOOKBEHIND-NOT-FIXED-LEN
                                    PIC s9(9) COMP VALUE 125.
           05 RE2ER-ZERO-RELATIVE-REFERENCE
                                    PIC s9(9) COMP VALUE 126.
           05 RE2ER-TOO-MANY-CONDITION-BRANC
                                    PIC s9(9) COMP VALUE 127.
           05 RE2ER-CONDITION-ASSERTION-EXPC
                                    PIC s9(9) COMP VALUE 128.
           05 RE2ER-BAD-RELATIVE-REFERENCE
                                    PIC s9(9) COMP VALUE 129.
           05 RE2ER-UNKNOWN-POSIX-CLASS
                                    PIC s9(9) COMP VALUE 130.
           05 RE2ER-INTERNAL-STUDY-ERROR
                                    PIC s9(9) COMP VALUE 131.
           05 RE2ER-UNICODE-NOT-SUPPORTED
                                    PIC s9(9) COMP VALUE 132.
           05 RE2ER-PARENTHESES-STACK-CHECK
                                    PIC s9(9) COMP VALUE 133.
           05 RE2ER-CODE-POINT-TOO-BIG
                                    PIC s9(9) COMP VALUE 134.
           05 RE2ER-LOOKBEHIND-TOO-COMPLICAT
                                    PIC s9(9) COMP VALUE 135.
           05 RE2ER-LOOKBEHIND-INVALID-BKS-C
                                    PIC s9(9) COMP VALUE 136.
           05 RE2ER-UNSUPPORTED-ESCAPE-SEQUE
                                    PIC s9(9) COMP VALUE 137.
           05 RE2ER-CALLOUT-NUMBER-TOO-BIG
                                    PIC s9(9) COMP VALUE 138.
           05 RE2ER-MISSING-CALLOUT-CLOSING
                                    PIC s9(9) COMP VALUE 139.
           05 RE2ER-ESCAPE-INVALID-IN-VERB
                                    PIC s9(9) COMP VALUE 140.
           05 RE2ER-UNRECOGNIZED-AFTER-QUERY
                                    PIC s9(9) COMP VALUE 141.
           05 RE2ER-MISSING-NAME-TERMINATOR
                                    PIC s9(9) COMP VALUE 142.
           05 RE2ER-DUPLICATE-SUBPATTERN-NAM
                                    PIC s9(9) COMP VALUE 143.
           05 RE2ER-INVALID-SUBPATTERN-NAME
                                    PIC s9(9) COMP VALUE 144.
           05 RE2ER-UNICODE-PROPERTIES-UNAVA
                                    PIC s9(9) COMP VALUE 145.
           05 RE2ER-MALFORMED-UNICODE-PROPER
                                    PIC s9(9) COMP VALUE 146.
           05 RE2ER-UNKNOWN-UNICODE-PROPERTY
                                    PIC s9(9) COMP VALUE 147.
           05 RE2ER-SUBPATTERN-NAME-TOO-LONG
                                    PIC s9(9) COMP VALUE 148.
           05 RE2ER-TOO-MANY-NAMED-SUBPATTER
                                    PIC s9(9) COMP VALUE 149.
           05 RE2ER-CLASS-INVALID-RANGE
                                    PIC s9(9) COMP VALUE 150.
           05 RE2ER-OCTAL-BYTE-TOO-BIG
                                    PIC s9(9) COMP VALUE 151.
           05 RE2ER-INTERNAL-OVERRAN-WORKSPA
                                    PIC s9(9) COMP VALUE 152.
           05 RE2ER-INTERNAL-MISSING-SUBPATT
                                    PIC s9(9) COMP VALUE 153.
           05 RE2ER-DEFINE-TOO-MANY-BRANCHES
                                    PIC s9(9) COMP VALUE 154.
           05 RE2ER-BACKSLASH-O-MISSING-BRAC
                                    PIC s9(9) COMP VALUE 155.
           05 RE2ER-INTERNAL-UNKNOWN-NEWLINE
                                    PIC s9(9) COMP VALUE 156.
           05 RE2ER-BACKSLASH-G-SYNTAX
                                    PIC s9(9) COMP VALUE 157.
           05 RE2ER-PARENS-QUERY-R-MISSING-C
                                    PIC s9(9) COMP VALUE 158.
      * Error 159 is obsolete and should now never occur */
           05 RE2ER-VERB-ARGUMENT-NOT-ALLOWE
                                    PIC s9(9) COMP VALUE 159.
           05 RE2ER-VERB-UNKNOWN
                                    PIC s9(9) COMP VALUE 160.
           05 RE2ER-SUBPATTERN-NUMBER-TOO-BI
                                    PIC s9(9) COMP VALUE 161.
           05 RE2ER-SUBPATTERN-NAME-EXPECTED
                                    PIC s9(9) COMP VALUE 162.
           05 RE2ER-INTERNAL-PARSED-OVERFLOW
                                    PIC s9(9) COMP VALUE 163.
           05 RE2ER-INVALID-OCTAL
                                    PIC s9(9) COMP VALUE 164.
           05 RE2ER-SUBPATTERN-NAMES-MISMATC
                                    PIC s9(9) COMP VALUE 165.
           05 RE2ER-MARK-MISSING-ARGUMENT
                                    PIC s9(9) COMP VALUE 166.
           05 RE2ER-INVALID-HEXADECIMAL
                                    PIC s9(9) COMP VALUE 167.
           05 RE2ER-BACKSLASH-C-SYNTAX
                                    PIC s9(9) COMP VALUE 168.
           05 RE2ER-BACKSLASH-K-SYNTAX
                                    PIC s9(9) COMP VALUE 169.
           05 RE2ER-INTERNAL-BAD-CODE-LOOKBE
                                    PIC s9(9) COMP VALUE 170.
           05 RE2ER-BACKSLASH-N-IN-CLASS
                                    PIC s9(9) COMP VALUE 171.
           05 RE2ER-CALLOUT-STRING-TOO-LONG
                                    PIC s9(9) COMP VALUE 172.
           05 RE2ER-UNICODE-DISALLOWED-CODE
                                    PIC s9(9) COMP VALUE 173.
           05 RE2ER-UTF-IS-DISABLED
                                    PIC s9(9) COMP VALUE 174.
           05 RE2ER-UCP-IS-DISABLED
                                    PIC s9(9) COMP VALUE 175.
           05 RE2ER-VERB-NAME-TOO-LONG
                                    PIC s9(9) COMP VALUE 176.
           05 RE2ER-BACKSLASH-U-CDPT-TOO-BI
                                    PIC s9(9) COMP VALUE 177.
           05 RE2ER-MISSING-OCTAL-OR-HEX-DI
                                    PIC s9(9) COMP VALUE 178.
           05 RE2ER-VERSION-CONDITION-SYNTA
                                    PIC s9(9) COMP VALUE 179.
           05 RE2ER-INTERNAL-BAD-AUTO-POSSE
                                    PIC s9(9) COMP VALUE 180.
           05 RE2ER-CALLOUT-NO-STRING-DELIM
                                    PIC s9(9) COMP VALUE 181.
           05 RE2ER-CALLOUT-BAD-STRING-DELI
                                    PIC s9(9) COMP VALUE 182.
           05 RE2ER-BACKSLASH-C-CALLER-DISA
                                    PIC s9(9) COMP VALUE 183.
           05 RE2ER-QUERY-BARJX-NEST-TOO-DE
                                    PIC s9(9) COMP VALUE 184.
           05 RE2ER-BACKSLASH-C-LIBRARY-DIS
                                    PIC s9(9) COMP VALUE 185.
           05 RE2ER-PATTERN-TOO-COMPLICATED
                                    PIC s9(9) COMP VALUE 186.
           05 RE2ER-LOOKBEHIND-TOO-LONG
                                    PIC s9(9) COMP VALUE 187.
           05 RE2ER-PATTERN-STRING-TOO-LONG
                                    PIC s9(9) COMP VALUE 188.
           05 RE2ER-INTERNAL-BAD-CODE
                                    PIC s9(9) COMP VALUE 189.
           05 RE2ER-INTERNAL-BAD-CODE-IN-SKI
                                    PIC s9(9) COMP VALUE 190.
           05 RE2ER-NO-SURROGATES-IN-UTF16
                                    PIC s9(9) COMP VALUE 191.
           05 RE2ER-BAD-LITERAL-OPTIONS
                                    PIC s9(9) COMP VALUE 192.
           05 RE2ER-SUPPORTED-ONLY-IN-UNICOD
                                    PIC s9(9) COMP VALUE 193.
           05 RE2ER-INVALID-HYPHEN-IN-OPTION
                                    PIC s9(9) COMP VALUE 194.
           05 PCRE2-ERROR-ALPHA-ASSERTIONUNK
                                    PIC s9(9) COMP VALUE 195.
           05 PCRE2-ERROR-SCRIPTRUNNOTAVAILA
                                    PIC s9(9) COMP VALUE 196.
           05 PCRE2-ERROR-TOO-MANY-CAPTURES
                                    PIC s9(9) COMP VALUE 197.
           05 PCRE2-ERROR-CONDATOMASRTEXPECT
                                    PIC s9(9) COMP VALUE 198.

      * "Expected" matching error codes: no match and partial match.

           05 PCRE2-ERROR-NOMATCH         PIC s9(9) COMP VALUE -1.
           05 PCRE2-ERROR-PARTIAL         PIC s9(9) COMP VALUE -2.

      * Error codes for UTF-8 validity checks

           05 PCRE2-ERROR-UTF8-ERR1       PIC s9(9) COMP VALUE -3.
           05 PCRE2-ERROR-UTF8-ERR2       PIC s9(9) COMP VALUE -4.
           05 PCRE2-ERROR-UTF8-ERR3       PIC s9(9) COMP VALUE -5.
           05 PCRE2-ERROR-UTF8-ERR4       PIC s9(9) COMP VALUE -6.
           05 PCRE2-ERROR-UTF8-ERR5       PIC s9(9) COMP VALUE -7.
           05 PCRE2-ERROR-UTF8-ERR6       PIC s9(9) COMP VALUE -8.
           05 PCRE2-ERROR-UTF8-ERR7       PIC s9(9) COMP VALUE -9.
           05 PCRE2-ERROR-UTF8-ERR8      PIC s9(9) COMP VALUE -10.
           05 PCRE2-ERROR-UTF8-ERR9      PIC s9(9) COMP VALUE -11.
           05 PCRE2-ERROR-UTF8-ERR10     PIC s9(9) COMP VALUE -12.
           05 PCRE2-ERROR-UTF8-ERR11     PIC s9(9) COMP VALUE -13.
           05 PCRE2-ERROR-UTF8-ERR12     PIC s9(9) COMP VALUE -14.
           05 PCRE2-ERROR-UTF8-ERR13     PIC s9(9) COMP VALUE -15.
           05 PCRE2-ERROR-UTF8-ERR14     PIC s9(9) COMP VALUE -16.
           05 PCRE2-ERROR-UTF8-ERR15     PIC s9(9) COMP VALUE -17.
           05 PCRE2-ERROR-UTF8-ERR16     PIC s9(9) COMP VALUE -18.
           05 PCRE2-ERROR-UTF8-ERR17     PIC s9(9) COMP VALUE -19.
           05 PCRE2-ERROR-UTF8-ERR18     PIC s9(9) COMP VALUE -20.
           05 PCRE2-ERROR-UTF8-ERR19     PIC s9(9) COMP VALUE -21.
           05 PCRE2-ERROR-UTF8-ERR20     PIC s9(9) COMP VALUE -22.
           05 PCRE2-ERROR-UTF8-ERR21     PIC s9(9) COMP VALUE -23.

      * Error codes for UTF-16 validity checks

           05 PCRE2-ERROR-UTF16-ERR1     PIC s9(9) COMP VALUE -24.
           05 PCRE2-ERROR-UTF16-ERR2     PIC s9(9) COMP VALUE -25.
           05 PCRE2-ERROR-UTF16-ERR3     PIC s9(9) COMP VALUE -26.

      * Error codes for UTF-32 validity checks

           05 PCRE2-ERROR-UTF32-ERR1     PIC s9(9) COMP VALUE -27.
           05 PCRE2-ERROR-UTF32-ERR2     PIC s9(9) COMP VALUE -28.

      * Miscellaneous error codes for pcre2[_dfa]_match(), substring
      * extraction functions, context functions, and serializing
      * functions. They are in numerical order. Originally they were
      * in alphabetical order too, but now that PCRE2 is released,
      * the numbers must not be changed.

           05 PCRE2-ERROR-BADDATA          PIC s9(9) COMP VALUE -29.
           05 PCRE2-ERROR-MIXEDTABLES      PIC s9(9) COMP VALUE -30.
      * Name was changed */
           05 PCRE2-ERROR-BADMAGIC         PIC s9(9) COMP VALUE -31.
           05 PCRE2-ERROR-BADMODE          PIC s9(9) COMP VALUE -32.
           05 PCRE2-ERROR-BADOFFSET        PIC s9(9) COMP VALUE -33.
           05 PCRE2-ERROR-BADOPTION        PIC s9(9) COMP VALUE -34.
           05 PCRE2-ERROR-BADREPLACEMENT   PIC s9(9) COMP VALUE -35.
           05 PCRE2-ERROR-BADUTFOFFSET     PIC s9(9) COMP VALUE -36.
           05 PCRE2-ERROR-CALLOUT          PIC s9(9) COMP VALUE -37.
      * Never used by PCRE2 itself */
           05 PCRE2-ERROR-DFA-BADRESTART   PIC s9(9) COMP VALUE -38.
           05 PCRE2-ERROR-DFA-RECURSE      PIC s9(9) COMP VALUE -39.
           05 PCRE2-ERROR-DFA-UCOND        PIC s9(9) COMP VALUE -40.
           05 PCRE2-ERROR-DFA-UFUNC        PIC s9(9) COMP VALUE -41.
           05 PCRE2-ERROR-DFA-UITEM        PIC s9(9) COMP VALUE -42.
           05 PCRE2-ERROR-DFA-WSSIZE       PIC s9(9) COMP VALUE -43.
           05 PCRE2-ERROR-INTERNAL         PIC s9(9) COMP VALUE -44.
           05 PCRE2-ERROR-JIT-BADOPTION    PIC s9(9) COMP VALUE -45.
           05 PCRE2-ERROR-JIT-STACKLIMIT   PIC s9(9) COMP VALUE -46.
           05 PCRE2-ERROR-MATCHLIMIT       PIC s9(9) COMP VALUE -47.
           05 PCRE2-ERROR-NOMEMORY         PIC s9(9) COMP VALUE -48.
           05 PCRE2-ERROR-NOSUBSTRING      PIC s9(9) COMP VALUE -49.
           05 PCRE2-ERROR-NOUNIQUESUBSTRING PIC s9(9) COMP VALUE -50.
           05 PCRE2-ERROR-NULL             PIC s9(9) COMP VALUE -51.
           05 PCRE2-ERROR-RECURSELOOP      PIC s9(9) COMP VALUE -52.
           05 PCRE2-ERROR-DEPTHLIMIT       PIC s9(9) COMP VALUE -53.
      * Obsolete synonym */
           05 PCRE2-ERROR-RECURSIONLIMIT   PIC s9(9) COMP VALUE -53.
           05 PCRE2-ERROR-UNAVAILABLE      PIC s9(9) COMP VALUE -54.
           05 PCRE2-ERROR-UNSET            PIC s9(9) COMP VALUE -55.
           05 PCRE2-ERROR-BADOFFSETLIMIT   PIC s9(9) COMP VALUE -56.
           05 PCRE2-ERROR-BADREPESCAPE     PIC s9(9) COMP VALUE -57.
           05 PCRE2-ERROR-REPMISSINGBRACE  PIC s9(9) COMP VALUE -58.
           05 PCRE2-ERROR-BADSUBSTITUTION  PIC s9(9) COMP VALUE -59.
           05 PCRE2-ERROR-BADSUBSPATTERN   PIC s9(9) COMP VALUE -60.
           05 PCRE2-ERROR-TOOMANYREPLACE   PIC s9(9) COMP VALUE -61.
           05 PCRE2-ERROR-BADSERIALIZEDDATA PIC s9(9) COMP VALUE -62.
           05 PCRE2-ERROR-HEAPLIMIT        PIC s9(9) COMP VALUE -63.
           05 PCRE2-ERROR-CONVERT-SYNTAX   PIC s9(9) COMP VALUE -64.
           05 PCRE2-ERROR-INTERNAL-DUPMATCH PIC s9(9) COMP VALUE -65.
       01 PCRE2-COB-INFO-CODES.
      * Request types for pcre2_pattern_info() */

           05 PCRE2-INFO-ALLOPTIONS         PIC s9(9) COMP VALUE  0.
           05 PCRE2-INFO-ARGOPTIONS         PIC s9(9) COMP VALUE  1.
           05 PCRE2-INFO-BACKREFMAX         PIC s9(9) COMP VALUE  2.
           05 PCRE2-INFO-BSR                PIC s9(9) COMP VALUE  3.
           05 PCRE2-INFO-CAPTURECOUNT       PIC s9(9) COMP VALUE  4.
           05 PCRE2-INFO-FIRSTCODEUNIT      PIC s9(9) COMP VALUE  5.
           05 PCRE2-INFO-FIRSTCODETYPE      PIC s9(9) COMP VALUE  6.
           05 PCRE2-INFO-FIRSTBITMAP        PIC s9(9) COMP VALUE  7.
           05 PCRE2-INFO-HASCRORLF          PIC s9(9) COMP VALUE  8.
           05 PCRE2-INFO-JCHANGED           PIC s9(9) COMP VALUE  9.
           05 PCRE2-INFO-JITSIZE            PIC s9(9) COMP VALUE 10.
           05 PCRE2-INFO-LASTCODEUNIT       PIC s9(9) COMP VALUE 11.
           05 PCRE2-INFO-LASTCODETYPE       PIC s9(9) COMP VALUE 12.
           05 PCRE2-INFO-MATCHEMPTY         PIC s9(9) COMP VALUE 13.
           05 PCRE2-INFO-MATCHLIMIT         PIC s9(9) COMP VALUE 14.
           05 PCRE2-INFO-MAXLOOKBEHIND      PIC s9(9) COMP VALUE 15.
           05 PCRE2-INFO-MINLENGTH          PIC s9(9) COMP VALUE 16.
           05 PCRE2-INFO-NAMECOUNT          PIC s9(9) COMP VALUE 17.
           05 PCRE2-INFO-NAMEENTRYSIZE      PIC s9(9) COMP VALUE 18.
           05 PCRE2-INFO-NAMETABLE          PIC s9(9) COMP VALUE 19.
           05 PCRE2-INFO-NEWLINE            PIC s9(9) COMP VALUE 20.
           05 PCRE2-INFO-DEPTHLIMIT         PIC s9(9) COMP VALUE 21.
      * Obsolete synonym */
           05 PCRE2-INFO-RECURSIONLIMIT     PIC s9(9) COMP VALUE 21.
           05 PCRE2-INFO-SIZE               PIC s9(9) COMP VALUE 22.
           05 PCRE2-INFO-HASBACKSLASHC      PIC s9(9) COMP VALUE 23.
           05 PCRE2-INFO-FRAMESIZE          PIC s9(9) COMP VALUE 24.
           05 PCRE2-INFO-HEAPLIMIT          PIC s9(9) COMP VALUE 25.
           05 PCRE2-INFO-EXTRAOPTIONS       PIC s9(9) COMP VALUE 26.

       01 PCRE2-COB-CONFIG-CODES.
      * Request types for pcre2_config(). */

           05 PCRE2-CONFIG-BSR               PIC s9(9) COMP VALUE  0.
           05 PCRE2-CONFIG-JIT               PIC s9(9) COMP VALUE  1.
           05 PCRE2-CONFIG-JITTARGET         PIC s9(9) COMP VALUE  2.
           05 PCRE2-CONFIG-LINKSIZE          PIC s9(9) COMP VALUE  3.
           05 PCRE2-CONFIG-MATCHLIMIT        PIC s9(9) COMP VALUE  4.
           05 PCRE2-CONFIG-NEWLINE           PIC s9(9) COMP VALUE  5.
           05 PCRE2-CONFIG-PARENSLIMIT       PIC s9(9) COMP VALUE  6.
           05 PCRE2-CONFIG-DEPTHLIMIT        PIC s9(9) COMP VALUE  7.
      * Obsolete synonym */
           05 PCRE2-CONFIG-RECURSIONLIMIT    PIC s9(9) COMP VALUE  7.
      * Obsolete synonym */
           05 PCRE2-CONFIG-STACKRECURSE      PIC s9(9) COMP VALUE  8.
           05 PCRE2-CONFIG-UNICODE           PIC s9(9) COMP VALUE  9.
           05 PCRE2-CONFIG-UNICODE-VERSION   PIC s9(9) COMP VALUE 10.
           05 PCRE2-CONFIG-VERSION           PIC s9(9) COMP VALUE 11.
           05 PCRE2-CONFIG-HEAPLIMIT         PIC s9(9) COMP VALUE 12.
           05 PCRE2-CONFIG-NEVER-BACKSLASH-C PIC s9(9) COMP VALUE 13.
           05 PCRE2-CONFIG-COMPILED-WIDTHS   PIC s9(9) COMP VALUE 14.
           05 PCRE2-CONFIG-TABLES-LENGTH     PIC s9(9) COMP VALUE 15.

      * The PCRE2_SIZE type is used for all string lengths and offsets
      * in PCRE2, including pattern offsets for errors and subject
      * offsets after a match. We define special values to indicate
      * zero-terminated strings and unset offsets in the offset vector
      * (ovector). */

       01 PCRE2-SIZE            PIC 9(9) COMP.
       01 PCRE2-SIZE-MAX        PIC 9(9) COMP.
      *   defining PCRE2_SIZE_MAX to SIZE_MAX is irrelevant. IBM COBOL
      *   is always 32 bits
       01 PCRE2-ZERO-TERMINATED PIC S9(9) COMP VALUE -1.
       01 PCRE2-UNSET           PIC S9(9) COMP VALUE -1.

      * Types *
      *  typedef unsigned char pcre_uint8 => X.
      *  typedef unsigned int pcre_uint32 => 9(9) BINARY.
      *  typedef int pcre_int32 => S9(9) Binary.
      *  typedef unsigned short pcre_uint16 => 9(4) BINARY.
      *  typedef short pcre_int16 => S9(4) BINARY.
      * If PCRE is compiled with 32 bit character support, PCRE_UCHAR32
      * must contain a 32 bit wide signed data type. Otherwise it can
      * be a dummy data type since pcre32 functions are not
      * implemented. There is a check for this in pcre_internal.h.

      * When PCRE is compiled as a C++ library, the subject pointer
      * type can be replaced with a custom type. For conventional use,
      * the public interface is a const char *.
       01 PCRE2-SPTR                            USAGE POINTER.

      * COBOL COPYBOOK DOES NOT CONTAIN FUNCTION DEFINITIONS
      * structure types delegated to specific copybooks

      * Private z/OS definitions for the PCRZ functions

       01   PCRZ-DEFINITIONS.
            05   PCRZ-CHAR-NULL               PIC X VALUE X'00'.
            05   PCRZ-NULL-TERMINATED         PIC S9(4) VALUE -1.
            05   PCRZ-SPACE-TERMINATED        PIC S9(4) VALUE -2.
            05   PCRZ-LENGTH-TERMINATED       PIC S9(4) VALUE -3.

      * error codes migrated to a header file in 10.42 */
      *  pcrz_codeset_init errors           */
            05   PCRZERR-ZCSSTRCT-ALLOC       PIC S9(4) VALUE -1.
            05   PCRZERR-CODESET-NAME-LEN2 PIC S9(4) VALUE -2.
            05   PCRZERR-CODESET-NAME-LEN3 PIC S9(4) VALUE -3.
            05   PCRZERR-CODESET-NAME-LEN4 PIC S9(4) VALUE -4.
            05   PCRZERR-OPEN-CONVETER5       PIC S9(4) VALUE -5.
            05   PCRZERR-PATTERN-ALLOC        PIC S9(4) VALUE -6.
            05   PCRZERR-SUBJECT-ALLOC        PIC S9(4) VALUE -7.
            05   PCRZERR-OPEN-CONVETER8       PIC S9(4) VALUE -8.

      *  pcrz_codeset_init_substitute errors  */
            05   PCRZERR-REPLACEMENT-ALLOC PIC S9(4) VALUE -10.
            05   PCRZERR-SUBSTITUTE-ALLOC PIC S9(4) VALUE -11.
            05   PCRZERR-SUBSTITUTE-BACK-ALLOC PIC S9(4) VALUE -12.

      *  pcrz_codeset_convert_errors          */
            05   PCRZERR-TARGET-LEN-ALLOC     PIC S9(4) VALUE -21.
            05   PCRZERR-ICONV-ERROR          PIC S9(4) VALUE -22.
            05   PCRZERR-UNKNOWN-FUNCTION     PIC S9(4) VALUE -23.

            05   PCRZ-MAX-PATTERN-SIZE        PIC S9(4) VALUE 2048.
            05   PCRZ-MAX-SUBJECT-SIZE        PIC S9(4) VALUE 8192.
            05   PCRZ-MAX-REPLACEMENT-SIZE    PIC S9(4) VALUE 1024.
            05   PCRZ-MAX-SUBSTITUTE-SIZE     PIC S9(9) VALUE 16384.
            05   PCRZ-MAX-CODESET-SIZE        PIC S9(4) VALUE 16.
