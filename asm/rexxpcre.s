 TITLE 'REXXPCRE - REXX FUNCTION FOR PERL COMPATIBLE REGEX'
 PRINT GEN
REXXPCRE PRGDEF FSEG=MAIN_BIT,REXX=Y,RMODE=24
*______________________________________________________________________
*
* This is the PEXXPCRE interface between the Rexx language on z/OS
* and the PCRE2 - regex processing library on same platform.
* Version 0.3
* Contributed by:   John Gateley  January 2017.
* Amended     by:   John Gateley  June 2017 (use sub-module for stem)
* Amended     by:   John Gateley  Nov. 2017 (add code page argument
*                                            and swap result values
*                                            1 = match
*                                            2 = no match)
* Copyright (c) 2017, John Gateley.
* All rights reserved.
*
*______________________________________________________________________
*
* Redistribution and use in source and binary forms, with or
* without modification, are permitted provided that the following
* conditions are met:
*
*  1. Redistributions of source code must retain the above
*  copyright notice, this list of conditions and the following
*  disclaimer.
*
*  2. Redistributions in binary form must reproduce the above
*  copyright notice, this list of conditions and the following
*  disclaimer in the documentation and/or other materials
*  provided with the distribution.
*
*  3. Neither the name of the University of Cambridge nor the
*  names of its contributors may be used to endorse or promote
*  products derived from this software without specific prior
*  written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
* CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
* INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
* NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
* HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
* OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
* EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*______________________________________________________________________
*
* This program acts as a REXX function and is designed to be used
* with the z/OS port of 'PCRE2 - Perl Compatible Regular Expressions'
* which was created by Ze'ev Atlas.
*
* This program was written by John Gateley in January 2017
*
* To assemble this program you will need to supplied macros and will
* also need to assemble module STRINGIT which is used in STRING macro.
*
* An article "Calling C functions from Assembler - revisited"
* by A Rudd printed in issue 208 of Xephon magazine of January 2004
* was very useful when I wrote this program.
* http://www.cbttape.org/xephon/xephonm/mvs0401.pdf
*
*    INPUT ARGUMENTS
*      1, the PERL expression
*      2, the string to be worked on
*      3, a STEM name which will be populated with the output
*      4, an optional OPTION
*                     'g'  -  match all
*                     'i'  -  ignore case
*                     'x'  -  exclude white space
*      5, an optional code-page - e.g.  'IBM-285'
*         if omitted the default local page is used
*      6, 'debug' if debugging required. Needs PCREDUMP DD name.
*
*    OUTPUT
*         if   successfull
*              RC=1    matched  - stem contains output
*              RC=0    no match - stem.0 will be '0'
*         else
*              RC='error message text'
*         endif
*
*    STEM variables output
*         stem.0             count of stem variables
*         stem.STRING.?      matched substrings
*         stem.POS.?         position and length of matches e.g. 3,4
*         stem.NAME.?        named substrings ' ' if none
*
*    DEBUGGING can be done by specifying 'debug' as argument 6.
*              You will also need to allocate the DD name PCREDUMP
*              with DISP=MOD if more than one call to REXXPCRE
*              because each call is a separate unit of work which
*              includes opening and closing the dataset.
*______________________________________________________________________
*
* EXAMPLE USAGE (please rely on the latest code in the accompanied
* Rexx program TRYPCRE1)
*
* /*REXX*/
*
*   "ISPEXEC LIBDEF ISPLLIB DATASET ID ('MY.LOAD.LIBRARY') STACK"
*
*   /*  test 1                                                  */
*
*   reg_ex  = "(?<char>A)\g<char>"
*   the_str = "AN_AARDWARK_JAKE_AND_aaRDWARK_JACK"
*   my_stem = "WANG"
*   opt_ion = "gi"
*   code_pg = "IBM-285'
*
*   say the_str
*
*   drop wang.
*
*   re_sponse = REXXPCRE(reg_ex,the_str,my_stem,opt_ion)
*
*   select
*      when re_sponse = 1 then do
*         say 'wang.0  is  ' wang.0
*         do lo_op_r = 1 to wang.0
*            say  wang.string.lo_op_r
*            say  wang.pos.lo_op_r  '       '  wang.name.lo_op_r
*         end
*        end
*      when re_sponse = 0 then
*         say 'no matches found'
*      Otherwise
*         say 'Error :' re_sponse
*   end
*
*   drop wang.
*
*   say '  '
*
*   /*  test 2                                                  */
*
*   the_str = "The quick brown fox jumps over the lazy dog."
*   the_Str = the_str||"The quick brown fox jumps over the"
*   reg_ex  = "(quick|jump)"
*   my_stem = "WANG"
*
*   say the_str
*
*   /* no option specified but use debug mode                   */
*
*   re_sponse = REXXPCRE(reg_ex,the_str,my_stem,,,'debug')
*
*   select
*      when re_sponse = 1 then do
*         say 'wang.0  is  ' wang.0
*         do lo_op_r = 1 to wang.0
*            say  wang.string.lo_op_r
*            say  wang.pos.lo_op_r  '       '  wang.name.lo_op_r
*         end
*        end
*      when re_sponse = 0 then
*         say 'no matches found'
*      Otherwise
*         say 'Error :' re_sponse
*   end
*
*   drop wang.
*
*   say '  '
*
*   /*  test 3                                                  */
*
*   the_str = "A fat cat doesn't eat oat but a rat eats bats."
*   reg_ex  = "(at)"
*   my_stem = "WANG"
*   opt_ion = "g"
*
*   say the_str
*
*   re_sponse = REXXPCRE(reg_ex,the_str,my_stem,opt_ion)
*
*   select
*      when re_sponse = 1 then do
*         say 'wang.0  is  ' wang.0
*         do lo_op_r = 1 to wang.0
*            say  wang.string.lo_op_r
*            say  wang.pos.lo_op_r  '       '  wang.name.lo_op_r
*         end
*        end
*      when re_sponse = 0 then
*         say 'no matches found'
*      Otherwise
*         say 'Error :' re_sponse
*   end
*
*   drop wang.
*
*   /*  test 4                                                  */
*
*   the_str = "He_jumps_brown_fox"
*   reg_ex  = "(?<first>quick|jumps_+(?<second>brown|over)_+(fox|the))"
*   my_stem = "WANG"
*
*   say the_str
*
*   re_sponse = REXXPCRE(reg_ex,the_str,my_stem,'g')
*
*   select
*      when re_sponse = 1 then do
*         say 'wang.0  is  ' wang.0
*         do lo_op_r = 1 to wang.0
*            say  wang.string.lo_op_r
*            say  wang.pos.lo_op_r  '       '  wang.name.lo_op_r
*         end
*        end
*      when re_sponse = 0 then
*         say 'no matches found'
*      Otherwise
*         say 'Error :' re_sponse
*   end
*
*   /*  test 5                                                  */
*   /*                                                          */
*   /*  this test uses the dollar symbol which is code page     */
*   /*  dependant in EBCDIC. In IBM-037 the dollar is x'5B'     */
*   /*  but in IBM-285 (UK) this is the currency symbol         */
*   /*  for GBP and the dollar is at x'4A' which is the cent    */
*   /*  symbol in IBM-037. Confusing!                           */
*   /*  Internally PCRE2 uses IBM-1047 so to use the dollar     */
*   /*  symbol in the regex we pass a fifth argument which      */
*   /*  is the code page name we are actually using.            */
*   /*  This causes PCRE to convert the regex from IBM-285      */
*   /*  to IBM-1047 before it is compiled, similarly the        */
*   /*  string is converted before the match is done.           */
*
*   /*  NOTE - this argument is only required if you use        */
*   /*         a code page in your terminal emulator which      */
*   /*         is NOT the same as the default local code page.  */
*   /*         Normal processing by PCRE is to convert the      */
*   /*         REGEX and string from the default local code     */
*   /*         page to IBM-1047.                                */
*   /*         IBM-1047 is used by the mainframe C compiler.    */
*
*   pa_rm    = "INDSN(DSNAME(MEMBER))"
*
*   reg_ex  = "\((.*)\)¢"
*   opt_ion = "g"
*
*   say pa_rm
*   say reg_ex
*
*   re_sponse = REXXPCRE(reg_ex,pa_rm,'wang',opt_ion,'IBM-285')
*
*   select
*      when re_sponse = 1 then do
*         say 'wang.0  is  ' wang.0
*         do lo_op_r = 1 to wang.0
*            var_1  =  substr(wang_string.lo_op_r,1,30)
*            var_2  =  substr(wang_pos.lo_op_r,1,10)
*            var_3  =  wang_name.lo_op_r
*            say  var_1 var_2 var_3
*         end
*        end
*      when re_sponse = 0 then
*         say 'no matches found'
*      Otherwise
*         say 'Error :' re_sponse
*   end
*
*   drop wang.
*
*   "ispexec libdef ispllib "
*
*   exit
*______________________________________________________________________
*
*  REENTRANT STORAGE
*
REG_EX_ADDR          DS    F           details of supplied REGEX
REG_EX_LEN           DS    F
REG_EX_END           DS    F
THE_STR_ADDR         DS    F           details of supplied string
THE_STR_LENGTH       DS    F
THE_STR_END          DS    F
CODEPAGE_PTR         DS    F
CODEPAGE_LEN         DS    F
CODEPAGE_RES         DS    F
CONV_REGEX_ADDR      DS    F
CONV_REGEX_LEN       DS    F
CONV_SUBJECT_PTR     DS    F
CONV_SUBJECT_LEN     DS    F
*
PCREWS_ERRORNUMBER   DS    F           PCRE working storage
PCREWS_ERROROFFSET   DS    F
PCREWS_RE_PTR        DS    F
PCREWS_MATCH_DATA    DS    F
PCREWS_NULL_PTR      DS    F
PCREWS_PATTERN_PTR   DS    F
PCREWS_SUBJECT_PTR   DS    F
PCREWS_SUBJECT_LEN   DS    F
PCREWS_RC            DS    F
PCREWS_OVECTOR       DS    F
PCREWS_NAMECOUNT     DS    F
PCREWS_NAME_TABLE    DS    F
PCREWS_NAME_ENTSZ    DS    F
PCRE2_INFO_NAMECOUNT EQU   17
PCRE2_INFO_NAM_ENTSZ EQU   18
PCRE2_INFO_NAMETABLE EQU   19
*
HIGHEST_MATCH        DS    F           address of highest match
CURRENT_ADDRESS      DS    F
CURRENT_COUNT        DS    F
*
HANDLE               DS    A           address of the C environment
FUNC_NAME            DS    A           C function to be called
CONVERT_HANDLE       DS    A           address of code-page converter
*
                     PRGSTEM PREF=ROOT       for  STEM.0
                     PRGSTEM PREF=POS             STEM.POS.?
                     PRGSTEM PREF=NAMED           STEM.NAME.?
                     PRGSTEM PREF=STRING          STEM.STRING.?
*
*    The parameter area used to call IRXEXCOM (shared variable access)
IX_PARM              DS    4F
*
VAR_TEXT             DC    CL8' '
VAR_POINT            DC    H'0'
FIRST_SET_MATCH      DC    PL4'0'      stem count at start of search
*
ERR_NO               DS    CL1         some indicators
REPEAT_SRCH          DS    CL1
IGNORE_CASE          DS    CL1
EXCLUDE_WHITE        DS    CL1
C_ENV_SETUP          DS    CL1
CONV_SETUP           DS    CL1
REGX_COMPILED        DS    CL1
MATCH_DONE           DS    CL1
DEBUG_MODE           DS    CL1
PCREDUMP_OPEN        DS    CL1
WORK_32              DS    CL32        32 bytes used when debugging
CALL_STEM_MESS       DS    CL26        error message for PROCSTEM call
ERR_BUFFER_LEN       DS    F           length of error message
ERR_BUFFER           DS    CL256       area for error message
*
STEM_POS             DS    F           position in string
STEM_LEN             DS    F           length of substring
MAX_STEM_LEN         EQU   2048        max length of a stem variable
STEM_TEXT            DS    CL(MAX_STEM_LEN)   storage for stem value
*
        PRGEDEF
*______________________________________________________________________
*
 SEGS MAIN_BIT
*
   ZAP   FIRST_SET_MATCH,=P'0'         initialise
*
   SEGDO GET_ARGUMENTS
*
   SEGDO INIT_STEM_VARIABLES
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     OPEN  (PCREDUMP,OUTPUT)           open output file
     IF (LTR,15,R15,NZ)                error on open
       MVI   ERR_NO,16
       SEGDO NOT_VALID
     ENDIF
     MVI   PCREDUMP_OPEN,C'Y'          flag as open
     SEGDO WRITE_PCREDUMP              write blank line
     MVC   MESS_TXT(22),=C'Rexx function starting'
     SEGDO WRITE_PCREDUMP              write MESS_TXT to PCREDUMP
     MVC   MESS_TXT(12),=C'Repeat srch='
     MVC   MESS_TXT+12(1),REPEAT_SRCH
     SEGDO WRITE_PCREDUMP
     MVC   MESS_TXT(12),=C'Ignore case='
     MVC   MESS_TXT+12(1),IGNORE_CASE
     SEGDO WRITE_PCREDUMP
     MVC   MESS_TXT(14),=C'Exclude white='
     MVC   MESS_TXT+14(1),EXCLUDE_WHITE
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   SEGDO SET_C_ENVIRONMENT
*
   MVC   PCREWS_PATTERN_PTR,REG_EX_ADDR
   MVC   PCREWS_SUBJECT_PTR,THE_STR_ADDR
   MVC   PCREWS_SUBJECT_LEN,THE_STR_LENGTH
*
   SEGDO COMPILE_REGEX
*
   IF (CLC,PCREWS_RE_PTR,EQ,=F'0')
     SEGDO COMPILE_ERROR
   ENDIF
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(14),=C'REGEX compiled'
     SEGDO WRITE_PCREDUMP
   ENDIF
   MVI   REGX_COMPILED,C'Y'            flag as compiled
*
   SEGDO CREATE_MATCH_BLOCK
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(19),=C'Match block created'
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   MVC   FUNC_NAME,=V(ZCSCONVS)        set C function required
   L     R14,CONVERT_HANDLE            code page conversion handle
   L     R15,PCREWS_SUBJECT_PTR        string to convert
   L     R0,PCREWS_SUBJECT_LEN         length of string
   LA    R1,CONV_SUBJECT_PTR           field for converted address
   LA    R2,CONV_SUBJECT_LEN           field for converted length
   STM   R14,R2,MY_P_3                 save parameters
   SEGDO CALL_C_FUNCTION               go and compile it
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'STRING converted'
     XUNPK CONV_SUBJECT_PTR,4,MESS_TXT+20
     XUNPK CONV_SUBJECT_LEN,4,MESS_TXT+30
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   SEGDO DO_FIRST_MATCH
*
   IF (CLI,REPEAT_SRCH,EQ,C'Y')
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(15),=C'Trying for more'
       SEGDO WRITE_PCREDUMP
     ENDIF
     DO INF
     DOEXIT (CLC,HIGHEST_MATCH,EQ,=F'0')
       SEGDO TRY_FOR_MORE
     ENDDO
   ENDIF
*
   SEGDO END_C_ENVIRONMENT
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=C'Rexx function ending'
     SEGDO WRITE_PCREDUMP
     CLOSE PCREDUMP                    close output file
   ENDIF
*
 SEGE MAIN_BIT
*______________________________________________________________________
*
 SEGS COMPILE_REGEX
*
*  convert the REGEX to code-page 1047
*
   MVC   FUNC_NAME,=V(ZCSCONVP)        set C function required
   L     R14,CONVERT_HANDLE
   L     R15,REG_EX_ADDR
   L     R0,REG_EX_LEN
   LA    R1,CONV_REGEX_ADDR
   LA    R2,CONV_REGEX_LEN
   STM   R14,R2,MY_P_3                 save parameters
   SEGDO CALL_C_FUNCTION               go and compile it
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'REGEX converted'
     XUNPK CONV_REGEX_ADDR,4,MESS_TXT+20
     XUNPK CONV_REGEX_LEN,4,MESS_TXT+30
     SEGDO WRITE_PCREDUMP
   ENDIF
*
*  compile the REGEX
*
   MVC   FUNC_NAME,=V(COMPILE2)        set C function required
*  L     R14,REG_EX_ADDR               address of REGEX
*  LHI   R15,-1                        -1
   L     R14,CONV_REGEX_ADDR           address of REGEX
   L     R15,CONV_REGEX_LEN            length of REGEX
   LA    R0,0                          0 = no options
   IF (CLI,IGNORE_CASE,EQ,C'Y')
     A     R0,PCRE2_CASELESS           add ignore case value
   ENDIF
   IF (CLI,EXCLUDE_WHITE,EQ,C'Y')
     A     R0,PCRE2_EXTENDED           add exclude white space
   ENDIF
   LA    R1,PCREWS_ERRORNUMBER         error number field
   LA    R2,PCREWS_ERROROFFSET         error offfset field
   LA    R3,0                          0
   STM   R14,R3,MY_P_3                 save parameters
   SEGDO CALL_C_FUNCTION               go and compile it
   ST    R15,PCREWS_RE_PTR             save returned address
*
 SEGE COMPILE_REGEX
*______________________________________________________________________
*
 SEGS CREATE_MATCH_BLOCK
*
*  create a block to hold results of the REGEX
*
   MVC   FUNC_NAME,=V(MATCDCFP)        set C function
   L     R14,PCREWS_RE_PTR
   L     R15,PCREWS_NULL_PTR
   STM   R14,R15,MY_P_3                save parameters
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_MATCH_DATA         save returned address
*
 SEGE CREATE_MATCH_BLOCK
*______________________________________________________________________
*
 SEGS DO_FIRST_MATCH
*
*  see if there is a match for the REGEX in the supplied string
*
   MVI   MATCH_DONE,C'Y'               flag matching
   MVC   FUNC_NAME,=V(MATCH2)          set C function
   L     R14,PCREWS_RE_PTR
   L     R15,PCREWS_SUBJECT_PTR
   L     R0,PCREWS_SUBJECT_LEN
   LA    R1,0
   LA    R2,0
   L     R3,PCREWS_MATCH_DATA
   L     R4,PCREWS_NULL_PTR
   STM   R14,R4,MY_P_3                 save parameters
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_RC                 save returned value
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     STRING ('First match return was ',C,PCREWS_RC,X)
     SEGDO WRITE_PCREDUMP
   ENDIF
   L     R15,PCREWS_RC                 reload RC (write changes R15)
   IF (LTR,R15,R15,M)                  < 0
     IF (CHI,R15,EQ,-1)
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         MVC   MESS_TXT(8),=C'No match'
         SEGDO WRITE_PCREDUMP
       ENDIF
     ELSE
       STRING (ERR_13,C,PCREWS_RC,I)
       MVC   EVALBLOCK_EVDATA(80),MESS_TXT
       LA    R1,80
       ST    R1,EVALBLOCK_EVLEN        return 'error message'
     ENDIF
     SEGQUIT                           and quit this segment
   ENDIF
*
   MVC   FUNC_NAME,=V(GETOVEPT)        set C function
   L     R1,PCREWS_MATCH_DATA
   ST    R1,MY_P_3                     save parameter
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_OVECTOR
*
*  See if output vector wasn't big enough. This should not happen,
*  because we used pcre2_match_data_create_from_pattern() above.
*
   IF (CLC,PCREWS_RC,EQ,=F'0')
     MVC   MESS_TXT(L'ERR_17),ERR_17
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       SEGDO WRITE_PCREDUMP
     ELSE
       SEGDO CALL_IRXSAY
     ENDIF
   ENDIF
*
   SETRC RC=1                          return '1' when something found
*
   SEGDO OUTPUT_MATCH_RESULTS
*
   SEGDO TRY_NAMED_SUBSTRING
*
 SEGE DO_FIRST_MATCH
*______________________________________________________________________
*
 SEGS TRY_FOR_MORE
*
*  g option specified so try for more matches
*
   MVC   FUNC_NAME,=V(MATCH2)          set C function
   L     R14,PCREWS_RE_PTR
   L     R15,PCREWS_SUBJECT_PTR
   L     R0,PCREWS_SUBJECT_LEN
   L     R1,HIGHEST_MATCH
   LA    R2,0
   L     R6,PCREWS_OVECTOR             point to output vector
   IF (CLC,0(4,R6),EQ,4(R6))           empty string ?
     L     R2,PCRE2_ANCHORED           set options in R2
     A     R2,PCRE2_NOT_EMPTY
   ENDIF
   L     R3,PCREWS_MATCH_DATA
   L     R4,PCREWS_NULL_PTR
   STM   R14,R4,MY_P_3                 save parameters
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_RC
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     STRING ('Next match return was ',C,PCREWS_RC,X)
     SEGDO WRITE_PCREDUMP
   ENDIF
   L     R15,PCREWS_RC
   IF (LTR,R15,R15,NP)                 < 1
     XC    HIGHEST_MATCH,HIGHEST_MATCH   flag no more matches
     IF (CHI,R15,EQ,-1)
       SEGQUIT                         found all the matches
     ELSE
       STRING (ERR_13,C,PCREWS_RC,I)
       MVC   EVALBLOCK_EVDATA(80),MESS_TXT
       LA    R1,80
       ST    R1,EVALBLOCK_EVLEN        return 'error message'
     ENDIF
     SEGQUIT                           and quit this segment
   ENDIF
*
   MVC   FUNC_NAME,=V(GETOVEPT)        set C function
   L     R1,PCREWS_MATCH_DATA
   ST    R1,MY_P_3                     save parameter
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_OVECTOR
*
*  See if output vector wasn't big enough. This should not happen,
*  because we used pcre2_match_data_create_from_pattern() above.
*
   IF (CLC,PCREWS_RC,EQ,=F'0')
     MVC   MESS_TXT(22),=C'Ovector not big enough'
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       SEGDO WRITE_PCREDUMP
     ELSE
       SEGDO CALL_IRXSAY
     ENDIF
   ENDIF
*
   SEGDO OUTPUT_MATCH_RESULTS
*
   SEGDO TRY_NAMED_SUBSTRING
*
 SEGE TRY_FOR_MORE
*______________________________________________________________________
*
 SEGS OUTPUT_MATCH_RESULTS
*
*  output the results of the REGEX match
*
*  When bytes 4 and 5 match the expression the values returned are
*  3 and 5. So length of match is (5-3).
*
   XC    HIGHEST_MATCH,HIGHEST_MATCH   set to zero
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     L     R6,PCREWS_OVECTOR           point to output vector
     L     R3,PCREWS_RC                count of values
     DO FROM=(R3)
       LM    R4,R5,0(R6)               offset and end of match
       STRING ('Ovector',C,(R4),X,(R4),I,(R5),X,(R5),I),PADB=Y
       SEGDO WRITE_PCREDUMP
       LA    R6,8(,R6)                 point past this pair of values
     ENDDO
   ENDIF
*
   ZAP   FIRST_SET_MATCH,POS_ZERO_COUNT  save current variable count
*
   L     R6,PCREWS_OVECTOR             point to output vector
   L     R3,PCREWS_RC                  count of values
   DO FROM=(R3)
     LM    R4,R5,0(R6)                 offset and end of match
     IF (C,R5,GT,HIGHEST_MATCH)        keep highest position
       ST    R5,HIGHEST_MATCH
     ENDIF
     IF (C,R5,GE,THE_STR_LENGTH)       at end of string
       XC    HIGHEST_MATCH,HIGHEST_MATCH   set to zero
     ENDIF
     IF (CR,R5,EQ,R6)                  match is empty string
       XC    HIGHEST_MATCH,HIGHEST_MATCH   set to zero
     ENDIF
     SR    R5,R4                       get length
     IF (CFI,R5,GT,MAX_STEM_LEN)       string to long
       MVI   ERR_NO,11
       SEGDO NOT_VALID
     ENDIF
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       STRING ('Match succeeded at offset ',C,(R4),I,                  /
               ' length ',C,(R5),I)
       SEGDO WRITE_PCREDUMP
     ENDIF
     LA    R4,1(,R4)                   convert offset to position
     ST    R4,STEM_POS                 save position
     BCTR  R4,0                        back to offset
     ST    R5,STEM_LEN                 save length
     L     R1,THE_STR_ADDR             get address of string
     LA    R4,0(R1,R4)                 point to first matching byte
     LA    R14,STEM_TEXT               area for stem value
     LR    R15,R5                      copy length
     MVCL  R14,R4                      copy fragment to output area
     ST    R6,CURRENT_ADDRESS          save current position
     ST    R3,CURRENT_COUNT            save current count
     SEGDO STEM_LINE                   go output stem line
     L     R6,CURRENT_ADDRESS          restore position
     L     R3,CURRENT_COUNT            restore count
     LA    R6,8(,R6)                   point past this pair of values
   ENDDO
*
 SEGE OUTPUT_MATCH_RESULTS
*______________________________________________________________________
*
 SEGS TRY_NAMED_SUBSTRING
*
*  see if there is a named substring available in this match
*
   MVC   FUNC_NAME,=V(PATTERN2)
   L     R14,PCREWS_RE_PTR
   LA    R15,PCRE2_INFO_NAMECOUNT
   LA    R0,PCREWS_NAMECOUNT
   STM   R14,R0,MY_P_3
   SEGDO CALL_C_FUNCTION
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     STRING   ('Name count is ',C,PCREWS_NAMECOUNT,I)
     SEGDO WRITE_PCREDUMP
   ENDIF
   IF (CLC,PCREWS_NAMECOUNT,GT,=F'0')  got some names?
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(16),=C'Named substrings'
       SEGDO WRITE_PCREDUMP
     ENDIF
     MVC   FUNC_NAME,=V(PATTERN2)
     L     R14,PCREWS_RE_PTR
     LA    R15,PCRE2_INFO_NAMETABLE
     LA    R0,PCREWS_NAME_TABLE
     STM   R14,R0,MY_P_3
     SEGDO CALL_C_FUNCTION
     MVC   FUNC_NAME,=V(PATTERN2)
     L     R14,PCREWS_RE_PTR
     LA    R15,PCRE2_INFO_NAM_ENTSZ
     LA    R0,PCREWS_NAME_ENTSZ
     STM   R14,R0,MY_P_3
     SEGDO CALL_C_FUNCTION
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       STRING   ('Name size is  ',C,PCREWS_NAME_ENTSZ,I)
       SEGDO WRITE_PCREDUMP
     ENDIF
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       L     R4,PCREWS_NAME_TABLE
       MVC   WORK_32,0(R4)
       STRING   ('Name table is ',C,WORK_32,X)
       SEGDO WRITE_PCREDUMP
     ENDIF
     L     R5,PCREWS_NAMECOUNT         get count of names
     L     R4,PCREWS_NAME_TABLE        point to name table
     DO FROM=(R5)                      loop counter
       LH    R1,0(R4)                  get array offset
       CVD   R1,DOUB_WORD              convert to decimal
       AP    DOUB_WORD,FIRST_SET_MATCH add start of this set results
       L     R1,PCREWS_NAME_ENTSZ      length of name entry
       AHI   R1,-2                     -2 for halfword variable number
       AHI   R1,-1                     -1 for EX (and the null)
       ST    R1,STEM_LEN               save length of text
       EX    R1,COPY_NAME_TEXT         copy text could end in nulls
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         STRING (STEM_LEN,I)
         MVC   MESS_TXT+14(32),STEM_TEXT
         SEGDO WRITE_PCREDUMP
       ENDIF
       L     R1,STEM_LEN               restore in case debug
       LA    R14,STEM_TEXT             point at output area
       LA    R14,0(R1,R14)             point after last output byte
       BCTR  R14,0                     now pointing at last
       DO FROM=(R1)                    loop count on output length
       DOEXIT (CLI,0(R14),GT,X'00')    quit when not null
         BCTR  R14,0                   point back a byte
       ENDDO
       ST    R1,STEM_LEN               save new length of text
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         STRING (STEM_LEN,I)
         MVC   MESS_TXT+14(32),STEM_TEXT
         SEGDO WRITE_PCREDUMP
       ENDIF
       SEGDO STEM_LINE_NAME
       A     R4,PCREWS_NAME_ENTSZ      next entry in name table
     ENDDO
   ENDIF
*
 SEGE TRY_NAMED_SUBSTRING
*______________________________________________________________________
*
 SEGS COMPILE_ERROR
*
*  error occured compiling the REGEX - output message
*
   MVC   FUNC_NAME,=V(GETERROR)        set C function
   L     R14,PCREWS_ERRORNUMBER        get error number
   LA    R15,ERR_BUFFER                boint to buffer
   LHI   R0,256                        length is 356 bytes
   STM   R14,R0,MY_P_3                 save in parameter list
   SEGDO CALL_C_FUNCTION
   STRING (ERR_14,C,PCREWS_ERROROFFSET,I)
   MVC   EVALBLOCK_EVDATA(60),MESS_TXT set return value
   MVC   EVALBLOCK_EVDATA+61(190),ERR_BUFFER
   LA    R1,250                        which is length 250
   ST    R1,EVALBLOCK_EVLEN            set length of returned value
   SEGDO END_C_ENVIRONMENT
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     CLOSE PCREDUMP
   ENDIF
   PRGQUIT                             quit the program
*
 SEGE COMPILE_ERROR
*______________________________________________________________________
*
 SEGS GET_ARGUMENTS
*
*  reads the arguments provided to this rexx function
*
   MVI   REPEAT_SRCH,C'N'              indicate option not set
   MVI   IGNORE_CASE,C'N'              indicate option not set
   MVI   EXCLUDE_WHITE,C'N'            indicate option not set
*
*  first argument should be the REGEX
*
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,1
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,2
     SEGDO NOT_VALID
   ENDIF
   MVC   REG_EX_ADDR,ARGTABLE_ARGSTRING_PTR    address of argument
   MVC   REG_EX_LEN,ARGTABLE_ARGSTRING_LENGTH  length or argument
*
*  second argument should be the string to work on
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for second argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,3
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,4
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR
   L     R15,ARGTABLE_ARGSTRING_LENGTH
   LA    R0,0(R14,R15)                 point after last byte
   BCTR  R0,0                          back one byte
   STM   R14,R0,THE_STR_ADDR           set address, length and end
*
*  third argument should be the STEM name to populate
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for third argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,5                    must have stem name
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,6                    zero length argument
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,7                    arbitary max length
     SEGDO NOT_VALID
   ENDIF
*
   MVC   ROOT_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   ROOT_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
   MVC   POS_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   POS_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
   MVC   NAMED_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   NAMED_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
   MVC   STRING_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   STRING_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
*
*  fourth argument is optional and can be 0 length
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for fourth argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     SEGQUIT                           * not provided so carry on
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,8                    arbitary max length
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')  not 0 length
     L     R14,ARGTABLE_ARGSTRING_PTR  address of argument
     L     R15,ARGTABLE_ARGSTRING_LENGTH length or argument
*
*  about to look for options currently  G/g  I/i  X/x
*
     DO FROM=(R15)                     R15 has length of argument
       IF (CLI,0(R14),EQ,C'G')         check for repeat search
         MVI   REPEAT_SRCH,C'Y'        flag as found
       ELSEIF (CLI,0(R14),EQ,C'g')     check for repeat search
         MVI   REPEAT_SRCH,C'Y'        flag as found
       ELSEIF (CLI,0(R14),EQ,C'I')     check for ignore case
         MVI   IGNORE_CASE,C'Y'        flag as found
       ELSEIF (CLI,0(R14),EQ,C'i')     check for ignore case
         MVI   IGNORE_CASE,C'Y'        flag as found
       ELSEIF (CLI,0(R14),EQ,C'X')     check for exclude white space
         MVI   EXCLUDE_WHITE,C'Y'      flag as found
       ELSEIF (CLI,0(R14),EQ,C'x')     check for exclude white space
         MVI   EXCLUDE_WHITE,C'Y'      flag as found
       ELSE
         MVI   ERR_NO,9                unknown option value
         SEGDO NOT_VALID
       ENDIF
       LA    R14,1(,R14)               next byte in argument
     ENDDO
   ENDIF
*
*  fifth argument is optional and can be 0 length
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for fifth argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     SEGQUIT                           * not provided so carry on
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')  not 0 length
     MVC   CODEPAGE_PTR,ARGTABLE_ARGSTRING_PTR     address of argument
     MVC   CODEPAGE_LEN,ARGTABLE_ARGSTRING_LENGTH  length oF argument
   ENDIF
*
*  sixth argument is optional but must be 'debug'
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for fifth argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,NE,=8X'FF')
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')
       IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'5')
         MVI   ERR_NO,10                 fifth argument <> 5 bytes
         SEGDO NOT_VALID
       ENDIF
       L     R14,ARGTABLE_ARGSTRING_PTR  address of argument
       MVC   WORK_32(5),0(R14)           copy argument
       OC    WORK_32(5),=C'     '        make upper case
       IF (CLC,=C'DEBUG',EQ,WORK_32)     check for DEBUG
         MVI   DEBUG_MODE,C'Y'           flag debug mode
       ENDIF
       MVC   WORK_32(5),=C'     '        clear to spaces
     ENDIF
   ENDIF
*
 SEGE GET_ARGUMENTS
*______________________________________________________________________
*
 SEGS INIT_STEM_VARIABLES
*
   MVI   ROOT_TRIM_VAR,C'N'            do not trim spaces from var
   MVHHI ROOT_FUNCTION,1               init STEM.0
   LA    R1,ROOT_AREA                  point to STEM. area
   SEGDO CALL_PROCSTEM
*
   MVI   POS_TRIM_VAR,C'Y'             do trim spaces from var
   MVC   POS_MID_NAME,=CL20'POS'       specify required .MIDDLE.
   MVHHI POS_FUNCTION,1                init STEM.POS.0
   LA    R1,POS_AREA                   point to STEM.POS. area
   SEGDO CALL_PROCSTEM
*
   MVI   NAMED_TRIM_VAR,C'N'           do not trim spaces from var
   MVC   NAMED_MID_NAME,=CL20'NAME'
   MVHHI NAMED_FUNCTION,1              init STEM.NAME.0
   LA    R1,NAMED_AREA                 point to STEM.NAME. area
   SEGDO CALL_PROCSTEM
*
   MVI   STRING_TRIM_VAR,C'N'          do not trim spaces from var
   MVC   STRING_MID_NAME,=CL20'STRING'
   MVHHI STRING_FUNCTION,1             init STEM.STRING.0
   LA    R1,STRING_AREA                point to STEM.STRING. area
   SEGDO CALL_PROCSTEM
*
 SEGE INIT_STEM_VARIABLES
*______________________________________________________________________
*
 SEGS STEM_LINE
*
*  sets stem.STRING.? to the substring of the input string
*  sets stem.NAME.?   to ' '
*  sets stem.POS.?    to 'x,y'         position,length
*
*  output the stem.STRING.? variable
*
   LA    R1,STEM_TEXT                  address of stem text
   ST    R1,STRING_TEXT_ADR            store address
   MVC   STRING_TEXT_LEN,STEM_LEN      copy length
   MVHHI STRING_FUNCTION,2             set STEM.STRING.? value
   LA    R1,STRING_AREA                point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in stem.string line'
   SEGDO CALL_PROCSTEM
*
*  output the stem.POS.? variable
*
   LA    R1,STEM_TEXT                  address of stem text
   ST    R1,POS_TEXT_ADR               store address
   STRING (STEM_POS,I,',',C,STEM_LEN,I),OUT=STEM_TEXT
*                                      output from string is 23 bytes
*                                      '         4+,         2+'
   LA    R1,22                         required length
   MVI   STEM_TEXT+10,C' '             make middle '+' into ' '
   ST    R1,POS_TEXT_LEN               set length
   MVHHI POS_FUNCTION,2                set STEM.POS.? value
   LA    R1,POS_AREA                   point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in stem.pos line'
   SEGDO CALL_PROCSTEM
*
*  output the stem.NAME.? variable as just ' ' for now
*
   LA    R1,STEM_TEXT                  point to text
   MVI   STEM_TEXT,C' '                make blank
   ST    R1,NAMED_TEXT_ADR             store address
   LA    R1,1                          required length
   ST    R1,NAMED_TEXT_LEN             set length
   MVHHI NAMED_FUNCTION,2              set STEM value
   LA    R1,NAMED_AREA                 point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in stem.name 1 line'
   SEGDO CALL_PROCSTEM
*
*  output the stem.? variable as just ' '
*
   LA    R1,STEM_TEXT                  point to text
   MVI   STEM_TEXT,C' '                make blank
   ST    R1,ROOT_TEXT_ADR              store address
   LA    R1,1                          required length
   ST    R1,ROOT_TEXT_LEN              set length
   MVHHI ROOT_FUNCTION,2               set STEM value
   LA    R1,ROOT_AREA                  point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in stem.     line'
   SEGDO CALL_PROCSTEM
*
*  set stem.0 to the current count
*
   SEGDO STEM_ZERO                     update stem.0 with new value
*
 SEGE STEM_LINE
*______________________________________________________________________
*
 SEGS STEM_LINE_NAME
*
*  previously created stem.NAME.? variable as ' '
*  now it is time to put in the real value
*
   LA    R1,STEM_TEXT                  point to text
   ST    R1,NAMED_TEXT_ADR             store address
   MVC   NAMED_TEXT_LEN,STEM_LEN       copy length
   MVHHI NAMED_FUNCTION,2              set STEM value
   ZAP   NAMED_ZERO_COUNT,DOUB_WORD+4(4)  put in variable number
   LA    R1,NAMED_AREA                 point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in stem.name 2 line'
   SEGDO CALL_PROCSTEM
*
   ZAP   NAMED_ZERO_COUNT,POS_ZERO_COUNT   put back MAX value
*
 SEGE STEM_LINE_NAME
*______________________________________________________________________
*
 SEGS STEM_ZERO
*
*  populate stem.pos.0 with current stem count
*
   MVHHI POS_FUNCTION,3                set STEM.POS.0 to total lines
   LA    R1,POS_AREA
   MVC   CALL_STEM_MESS,=CL26'Error in stem.pos  zero'
   SEGDO CALL_PROCSTEM
*
*  keep all the other STEMs in line
*
   ZAP   ROOT_ZERO_COUNT,POS_ZERO_COUNT
   MVHHI ROOT_FUNCTION,3               set STEM.0 to total lines
   LA    R1,ROOT_AREA
   MVC   CALL_STEM_MESS,=CL26'Error in stem.     zero'
   SEGDO CALL_PROCSTEM
*
   ZAP   NAMED_ZERO_COUNT,POS_ZERO_COUNT
   MVHHI NAMED_FUNCTION,3              set STEM.NAME.0 to total lines
   LA    R1,NAMED_AREA
   MVC   CALL_STEM_MESS,=CL26'Error in stem.name zero'
   SEGDO CALL_PROCSTEM
*
   ZAP   STRING_ZERO_COUNT,POS_ZERO_COUNT
   MVHHI STRING_FUNCTION,3             set STEM.STRING.0 to total lines
   LA    R1,STRING_AREA
   MVC   CALL_STEM_MESS,=CL26'Error in stem.string zero'
   SEGDO CALL_PROCSTEM
*
 SEGE STEM_ZERO
*______________________________________________________________________
*
 SEGS CALL_PROCSTEM
*
*  R1 should already point to the required PRGSTEM macro expansion
   ST    R1,MY_P_1                     save as first parm
   LA    R1,MY_PARM                    point to parameter list
   L     R15,=V(PROCSTEM)              get program address
   BASR  R14,R15                       go and set stem value
   IF (LTR,R15,R15,NZ)                 error ?
     ST    R15,R_C                     save return code
     STRING (CALL_STEM_MESS,C,R_C,I)
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       SEGDO WRITE_PCREDUMP
     ELSE
       SEGDO CALL_IRXSAY
     ENDIF
     SEGDO END_C_ENVIRONMENT
     IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
       CLOSE PCREDUMP
     ENDIF
     PRGQUIT                           get out of program
   ENDIF
*
 SEGE CALL_PROCSTEM
*______________________________________________________________________
*
 SEGS NOT_VALID
*
*  an error occured so set error message as output from REXX function
*
   MVC   EVALBLOCK_EVLEN,=F'30'        set length of returned value
   IF (CLI,ERR_NO,EQ,1)
     MVC   EVALBLOCK_EVDATA(L'ERR_1),ERR_1
     LA    R1,L'ERR_1
   ELSEIF (CLI,ERR_NO,EQ,2)
     MVC   EVALBLOCK_EVDATA(L'ERR_2),ERR_2
     LA    R1,L'ERR_2
   ELSEIF (CLI,ERR_NO,EQ,3)
     MVC   EVALBLOCK_EVDATA(L'ERR_3),ERR_3
     LA    R1,L'ERR_3
   ELSEIF (CLI,ERR_NO,EQ,4)
     MVC   EVALBLOCK_EVDATA(L'ERR_4),ERR_4
     LA    R1,L'ERR_4
   ELSEIF (CLI,ERR_NO,EQ,5)
     MVC   EVALBLOCK_EVDATA(L'ERR_5),ERR_5
     LA    R1,L'ERR_5
   ELSEIF (CLI,ERR_NO,EQ,6)
     MVC   EVALBLOCK_EVDATA(L'ERR_6),ERR_6
     LA    R1,L'ERR_6
   ELSEIF (CLI,ERR_NO,EQ,7)
     MVC   EVALBLOCK_EVDATA(L'ERR_7),ERR_7
     LA    R1,L'ERR_7
   ELSEIF (CLI,ERR_NO,EQ,8)
     MVC   EVALBLOCK_EVDATA(L'ERR_8),ERR_8
     LA    R1,L'ERR_8
   ELSEIF (CLI,ERR_NO,EQ,9)
     MVC   EVALBLOCK_EVDATA(L'ERR_9),ERR_9
     LA    R1,L'ERR_9
   ELSEIF (CLI,ERR_NO,EQ,10)
     MVC   EVALBLOCK_EVDATA(L'ERR_10),ERR_10
     LA    R1,L'ERR_10
   ELSEIF (CLI,ERR_NO,EQ,11)
     MVC   EVALBLOCK_EVDATA(L'ERR_11),ERR_11
     LA    R1,L'ERR_11
   ELSEIF (CLI,ERR_NO,EQ,16)
     MVC   EVALBLOCK_EVDATA(L'ERR_16),ERR_16
     LA    R1,L'ERR_16
   ELSE    this should never happen
     MVC   EVALBLOCK_EVDATA(30),=CL30'Unknown error, RC='
     XR    R1,R1
     IC    R1,ERR_NO
     CVD   R1,DOUB_WORD
     OI    DOUB_WORD+7,X'0F'
     UNPK  EVALBLOCK_EVDATA+18(4),DOUB_WORD
     XC    R_C,R_C
     LA    R1,30
   ENDIF
   ST    R1,EVALBLOCK_EVLEN            Set length of returned value
*
   IF (CLI,C_ENV_SETUP,EQ,C'Y')
     SEGDO END_C_ENVIRONMENT
   ENDIF
*
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     CLOSE PCREDUMP
   ENDIF
   PRGQUIT                             get out of program
*
 SEGE NOT_VALID
*______________________________________________________________________
*
 SEGS SET_C_ENVIRONMENT
*
*  set up C environment with librarian functionality
*
   LA    R1,HANDLE                     address for returned handle
   ST    R1,MY_P_1
   LA    R1,STKSIZE                    stack size
   ST    R1,MY_P_2
   LA    R1,STKLOC                     stack location
   ST    R1,MY_P_3
   OI    MY_P_3,X'80'                  set VL flag
   LA    R1,MY_PARM                    address of argument list
   L     R15,=V(EDCXHOTL)
   BASR  R14,R15                       go set up C environment
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(22),=C'C environment obtained'
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   MVC   FUNC_NAME,=V(ZCSINIT)
   LA    R14,CONVERT_HANDLE            return address of code pages
   L     R15,CODEPAGE_PTR              may be 0 if not specified
*                                      0 means current code page
   L     R0,CODEPAGE_LEN               length of code page name
   LHI   R1,2048                       max size of pattern
   LHI   R2,8192                       max size of string
   STM   R14,R1,MY_P_3                 save in parameter list
   LHI   R14,0                         max size of string  - default
   LA    R15,ERR_BUFFER                point to error message buffer
   STM   R14,R15,MY_P_7                save in parameter list
   SEGDO CALL_C_FUNCTION
   ST    R15,CODEPAGE_RES
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'init convert routine'
     XUNPK (R15),4,MESS_TXT+25
     XUNPK CONVERT_HANDLE,4,MESS_TXT+35
     IF (CLC,CODEPAGE_RES,NE,=F'0')
       MVC   MESS_TXT+45(8),=C'- FAILED'
     ENDIF
     L     R1,CODEPAGE_PTR
     IF (LTR,R1,R1,NZ)
       L     R2,CODEPAGE_LEN
       IF (CHI,R2,GT,30)               allow max of 30 characters
         LA    R2,30
       ENDIF
       BCTR  R2,0
       EX    R2,COPY_CODEPAGE
     ELSE
       MVC   MESS_TXT+55(7),=C'DEFAULT'
     ENDIF
     SEGDO WRITE_PCREDUMP
   ENDIF
   IF (CLC,CODEPAGE_RES,NE,=F'0')
     IF (CLC,ERR_BUFFER(40),GT,SPACES)
       SEGDO CALL_IRXSAY_ERROR_MESS
     ENDIF
     SEGDO END_C_ENVIRONMENT
     IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
       CLOSE PCREDUMP
     ENDIF
     MVC   EVALBLOCK_EVDATA(L'ERR_15),ERR_15
     LA    R1,L'ERR_15
     ST    R1,EVALBLOCK_EVLEN            set length of returned value
     PRGQUIT                             quit the program
   ENDIF
   MVI   CONV_SETUP,C'Y'
*
 SEGE SET_C_ENVIRONMENT
*______________________________________________________________________
*
 SEGS END_C_ENVIRONMENT
*
*  terminate C environment after cleaning up
*
   IF (CLI,MATCH_DONE,EQ,C'Y')
     MVC   FUNC_NAME,=V(MATCHDFR)      free any matches
     L     R1,PCREWS_MATCH_DATA
     ST    R1,MY_P_3
     SEGDO CALL_C_FUNCTION
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(15),=C'Called MATCHDFR'
       SEGDO WRITE_PCREDUMP
     ENDIF
   ENDIF
*
   IF (CLI,REGX_COMPILED,EQ,C'Y')
     MVC   FUNC_NAME,=V(CODEFREE)      free compiled code
     L     R1,PCREWS_RE_PTR
     ST    R1,MY_P_3
     SEGDO CALL_C_FUNCTION
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(15),=C'Called CODEFREE'
       SEGDO WRITE_PCREDUMP
     ENDIF
   ENDIF
   IF (CLI,CONV_SETUP,EQ,C'Y')
     MVI   CONV_SETUP,0
     MVC   FUNC_NAME,=V(ZCSRLSE)
     L     R14,CONVERT_HANDLE            return address of code pages
     ST    R14,MY_P_3                    save in parameter list
     SEGDO CALL_C_FUNCTION
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(30),=CL30'release convert routine'
       SEGDO WRITE_PCREDUMP
     ENDIF
   ENDIF
*
*  terminate C environment with librarian functionality
*
   LA    R1,HANDLE
   ST    R1,MY_P_1
   OI    MY_P_1,X'80'                  set VL flag
   LA    R1,MY_PARM
   L     R15,=V(EDCXHOTT)
   BASR  R14,R15                       terminate C environment
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(22),=C'C environment released'
     SEGDO WRITE_PCREDUMP
   ENDIF
*
 SEGE END_C_ENVIRONMENT
*______________________________________________________________________
*
 SEGS CALL_C_FUNCTION
*
*  parameters from 3 onward are set so just add handle and function
*
   LA    R1,HANDLE                     address of C environment
   ST    R1,MY_P_1
   LA    R1,FUNC_NAME                  function name
   ST    R1,MY_P_2
   LA    R1,MY_PARM                    point to parameters
   L     R15,=V(EDCXHOTU)
   BASR  R14,R15
   MVI   C_ENV_SETUP,C'Y'
*
 SEGE CALL_C_FUNCTION
*______________________________________________________________________
*
 SEGS CALL_IRXSAY
*
   LA    R1,=CL8'WRITE'
   ST    R1,MY_P_1
   LA    R1,MESS_TXT
   ST    R1,MY_P_8
   LA    R1,MY_P_8
   ST    R1,MY_P_2
   LA    R1,L'MESS_TXT
   ST    R1,MY_P_9
   LA    R1,MY_P_9
   ST    R1,MY_P_3
   OI    MY_P_3,X'80'
   L     R0,#SAV_REX+12                rexx environment block addr
   LA    R1,MY_PARM
   USING IRXEXTE,15
   L     15,ENVBLOCK_IRXEXTE
   L     15,IRXSAY
   DROP  15
   BASR  14,15
   IF (LTR,R15,R15,NZ)
     ST    R15,R_C
     SEGDO WRITE_LOG                   write original message
     MVC   MESS_TXT(L'ERR_12),ERR_12
     XUNPK R_C,4,MESS_TXT+19
     SEGDO WRITE_LOG                   then this error message
   ENDIF
   MVI   MESS_TXT,C' '
   MVC   MESS_TXT+1(L'MESS_TXT-1),MESS_TXT
*
 SEGE CALL_IRXSAY
*______________________________________________________________________
*
 SEGS CALL_IRXSAY_ERROR_MESS
*
   LA    R14,ERR_BUFFER
   XR    R15,R15
   DO FROM=(R1,256)
   DOEXIT (CLI,0(R14),EQ,0)
     LA    R14,1(,R14)
     LA    R15,1(,R15)
   ENDDO
   IF (CFI,R15,EQ,0)
     SEGQUIT
   ENDIF
   ST    R15,ERR_BUFFER_LEN
*
   LA    R1,=CL8'WRITE'
   ST    R1,MY_P_1
   LA    R1,ERR_BUFFER
   ST    R1,MY_P_8
   LA    R1,MY_P_8
   ST    R1,MY_P_2
   LA    R1,ERR_BUFFER_LEN
   ST    R1,MY_P_3
   OI    MY_P_3,X'80'
   L     R0,#SAV_REX+12                rexx environment block addr
   LA    R1,MY_PARM
   USING IRXEXTE,15
   L     15,ENVBLOCK_IRXEXTE
   L     15,IRXSAY
   DROP  15
   BASR  14,15
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   IF (LTR,R15,R15,NZ)
     ST    R15,R_C
     MVC   MESS_TXT,ERR_BUFFER         copy error message
     SEGDO WRITE_LOG                   write original message
     MVC   MESS_TXT(L'ERR_12),ERR_12
     XUNPK R_C,4,MESS_TXT+19
     SEGDO WRITE_LOG                   then this error message
   ENDIF
*
 SEGE CALL_IRXSAY_ERROR_MESS
*______________________________________________________________________
*
 SEGS WRITE_PCREDUMP
*
   PUT   PCREDUMP,MESS_TXT             output text to PCREDUMP
   MVI   MESS_TXT,C' '                 clear message line
   MVC   MESS_TXT+1(L'MESS_TXT-1),MESS_TXT
*
 SEGE WRITE_PCREDUMP
*______________________________________________________________________
*
               PRGSTAT
*
PCREDUMP DCB   DSORG=PS,DDNAME=PCREDUMP,LRECL=80,MACRF=(PM),RECFM=FB
*
SPACES DC  CL40' '
ERR_1  DC  C'REXXPCRE-001 - REGEX (argument 1) not supplied'
ERR_2  DC  C'REXXPCRE-002 - REGEX (argument 1) was zero length'
ERR_3  DC  C'REXXPCRE-003 - STRING (argument 2) not supplied'
ERR_4  DC  C'REXXPCRE-004 - STRING (argument 2) was zero length'
ERR_5  DC  C'REXXPCRE-005 - STEM NAME (argument 3) not supplied'
ERR_6  DC  C'REXXPCRE-006 - STEM NAME (argument 3) was zero length'
ERR_7  DC  C'REXXPCRE-007 - STEM NAME (argument 3) > 20 bytes'
ERR_8  DC  C'REXXPCRE-008 - OPTIONS (argument 4) > 20 bytes'
ERR_9  DC  C'REXXPCRE-009 - OPTIONS (argument 4) not recognised'
ERR_10 DC  C'REXXPCRE-010 - DEBUG (argument 6) <> 5 bytes'
ERR_11 DC  C'REXXPCRE-011 - Output value longer than 2048'
ERR_12 DC  C'REXXPCRE-012 - Return code on SAY='
ERR_13 DC  C'REXXPCRE-013 - Matching error '
ERR_14 DC  C'REXXPCRE-014 - PCRE2 compilation failed at offset '
ERR_15 DC  C'REXXPCRE-015 - Code page failure (argument 5)'
ERR_16 DC  C'REXXPCRE-016 - Debug specified but DD name PCREDUMP not al/
               located'
ERR_17 DC  C'REXXPCRE-017 - Ovector not big enough'
*
STKSIZE        DC    F'4096'     stack size
STKLOC         DC    F'1'        stack location (0 < 16MB, 1 >= 16MB)
*
COPY_NAME_TEXT MVC   STEM_TEXT(1),2(R4)
DO_MOVE        MVC   0(1,R3),0(R1)
COPY_CODEPAGE  MVC   MESS_TXT+55(0),0(R1)
*
                     DS    0F          ensure fullword aligned
PCRE2_CASELESS       DC    X'00000008'
PCRE2_EXTENDED       DC    X'00000080'
PCRE2_NOT_EMPTY      DC    X'00000008'
PCRE2_ANCHORED       DC    X'80000000'
*
               LTORG
*
               PRGESTAT
               PRGEND
               END
