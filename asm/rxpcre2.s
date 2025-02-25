 TITLE 'RXPCRE2 - REXX FUNCTION FOR PERL COMPATIBLE REGEX'
 PRINT GEN
RXPCRE2  PRGDEF FSEG=MAIN_BIT,REXX=Y,RMODE=24
*______________________________________________________________________
*
* This is the RXPCRE2 interface between the Rexx language on z/OS
* and the PCRE2 - regex processing library on same platform.
*
* Version 0.1
* Contributed by:   John Gateley  September 2017.
* Copyright (c) 2017, John Gateley.
* All rights reserved.
* 2019-dec      :   add substitute function
*               :   move PCREDUMP DCB to RXPCRE2 to allow RMODE=31
*                   for load module RXPCRE2A
* 2020-nov      :   add m and s switches
* 2022-aug      :   add option to send messages to rexx stem variable
*                   RXPCRE2_ERROR_MESS instead of using IRXSAY
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
* This program was written by John Gateley in September 2017.
*
* To assemble these programs you will need to supplied macros and will
* also need to assemble modules STRINGIT which is used in STRING macro
* and TRIMIT which is used by the TRIM macro.
*______________________________________________________________________
*
* How it works.
*
* This program RXPCRE2 is a rexx function and RXPCRE2A is a helper.
* On the 'connect' call RXPCRE2A is loaded into memory and the
* address of the program is returned, this enables the program to
* be found on subsequent calls.
* The 'disconnect' function terminates the C environment and deletes
* RXPCRE2A from memory.
*
*
* There are 6 calls to the function
*
*
* 1, CONNECT
*    Establish the C environment with or without debug.
*    If OK returns an eight byte environment handle which should
*    be used in subsequent calls to the function.
*    An optional third parameter is the code page that is being used,
*    this defaults to a z/OS environment variable but can be set
*    by the user.
*    If DEBUG is specified then the DDNAME PCREDUMP must be allocated
*    with DISP=MOD as the dataset will be opened and closed on
*    every call.
*    By default error messages are issued using IRXSAY which cannot
*    be trapped, so there is an additional option to send messages
*    to the REXX variable RXPCRE2_ERROR_MESS. This is done by
*    adding a fifth parameter of 'nosay'.
*
*    Returns 0 if OK
*            8 if failed - an error message will be writen using IRXSAY
*                          or in RXPCRE2_ERROR_MESS if it is specified
*
*      r_c     = RXPCRE2('connect','pcre_env')
*      r_c     = RXPCRE2('connect','pcre_env',,'debug')
*      r_c     = RXPCRE2('connect','pcre_env')
*      r_c     = RXPCRE2('connect','pcre_env',IBM-285,'debug','nosay')
*
*      if  r_c  > 0 then exit
*
*
* 2, COMPILE
*    Compile the PCRE regular expression.
*    This returns an nineteen byte compile handle which should
*    be used in subsequent calls to the function.
*    Note that the reg_ex must be null terminated.
*    Options can be specified
*                 'g'  repeat search
*                 'i'  ignore case
*                 'x'  exclude white space
*                 'm'  multiline
*                 's'  dotall
*
*    Returns 0 if OK
*            8 if failed - an error message will be writen using IRXSAY
*
*      reg_ex  = "(?<char>A)\g<char>"||'00'x
*
*      r_c     = RXPCRE2('compile',pcre_env,reg_ex,'pcre_comp','g')
*
*      if  r_c  > 0 then exit
*
*
* 3, MATCH - alias EXEC and EXECUTE
*    Process the compiled expression, repeat this call as required.
*    The STEM name is used as the first part of the output
*    'WANG' will give  WANG.0         the count of matches
*                      WANG_STRING.?  the sub-strings
*                      WANG_POS.?     the positions within the string
*                      WANG_NAME.?    named sub-strings (blank if none)
*
*    returns  0     no match - stem.string.0 will be '0'
*             1     matched  - stem.string.? contains output
*             8     an error message will be writen using IRXSAY
*
*      the_str   = "AN_AARDWARK_JAKE_AND_AARDWARK_JACK"
*      my_stem   = "WANG"
*
*      r_c = RXPCRE2('execute',pcre_env,pcre_comp,the_str,my_stem)
*
*    This call can be repeated as many times as required.
*
*
* 4, SUBSTITUTE - alias SUBS
*    This is similar to EXECUTE but uses the routine SUBSTIT2
*    instead of MATCH2 and requires an additional input containing
*    the substitute string.
*    The variable name is used to contain the output string
*
*    returns  0     no match - rexx variable not set
*             1     matched  - rexx variable contains output string
*             8     an error message will be writen using IRXSAY
*
*      reg_ex      "cat|dog"
*      the_str     "the dog sat on the cat's dog"
*      subs_str    "horse"
*      my_var    = "WANG"
*
*      r_c = RXPCRE2('substitute',pcre_env,pcre_comp,the_str,,
*                    my_var,subs_str)
*
*    This call can be repeated as many times as required.
*
*
* 5, RELEASE
*    Release the compile storage
*    Returns 0 if successfull.
*            8 if failed - an error message will be writen using IRXSAY
*
*      re_lease  = RXPCRE2('release',pcre_env,pcre_comp)
*
*
* 6, DISCONNECT
*    Terminate the C environment.
*    Returns 0 if successfull.
*            8 if failed - an error message will be writen using IRXSAY
*
*      r_c     = RXPCRE2('disconnect',pcre_env)
*
*
* NOTE if any call fails then the C environment will be terminated.
*______________________________________________________________________
*
*    STEM variables output
*         stem.0             count of stem variables
*         stem_STRING.?      matched substrings
*         stem_POS.?         position and length of matches e.g. 3,4
*         stem_NAME.?        named substrings ' ' if none
*
*    these stem values will contain the same as stem.0
*         stem_STRING.0
*         stem_POS.0
*         stem_NAME.0
*______________________________________________________________________
*
*         PCRELIB
*
*  When running in batch the LOAD macro will try to find the requested
*  module/program object in the STEPLIB or JOBLIB concatenation
*  which will contain the PDSE library containing RXPCRE2A. This works.
*
*  In ISPF the library would be concatenated to ISPLLIB which will
*  allow RXPCRE2 to be loaded, however, when this tries to load
*  RXPCRE2A the STEPLIB would be used and would fail.
*  It should be possible to put the program library in the ISPLLIB
*  concatenation and amend all references to PCRELIB to ISPLLIB
*  but this will not work when ISPLLIB is modified using LIBDEF as
*  the library is not added to the actual ISPLLIB but to another
*  DDNAME which is logically concatenated by ISPF.
*
*  For the above reasons PCRELIB is used. If it is not present in the
*  task IO table the program will not attempt to use it.
*
*      /* allow RXPCRE2 to be loaded by REXX */
*      "ISPEXEC LIBDEF ISPLLIB DATASET ID ('SDJRG.LOADLIB.POBJ')"
*      if rc /= 0 then do
*         say 'allocation to ispllib failed.'
*         exit
*      end
*      /* allow RXPCRE2A to be loaded by RXPCRE2A */
*      "ALLOC FI(PCRELIB)  DA('?????.LOADLIB.POBJ') SHR"
*      if rc /= 0 then do
*         say 'allocation to PCRELIB failed.'
*         exit
*      end
*
*      ...
*
*      "FREE FILE(PCRELIB)"
*      "ISPEXEC LIBDEF ISPLLIB "
*______________________________________________________________________
*
*  REENTRANT STORAGE
*
LOAD_ADDRESS         DS    F
LOAD_ADDR_CHAR       DS    CL8
NO_SAY_COPY          DS    CL1
*
COMPILE_CHAR         DS    CL21
                     DS    0F
COMPILE_DATA         DS    CL13
                     ORG   COMPILE_DATA
PCREWS_RE_PTR        DS    F
PCREWS_MATCH_DATA    DS    F
REPEAT_SRCH          DS    CL1         flags
IGNORE_CASE          DS    CL1
EXCLUDE_WHITE        DS    CL1
MULTI_LINE           DS    CL1
DOT_ALL              DS    CL1
                     ORG
*
REG_EX_ADDR          DS    F           details of supplied REGEX
REG_EX_LENGTH        DS    F
REG_EX_END           DS    F
THE_STR_ADDR         DS    F           details of supplied string
THE_STR_LENGTH       DS    F
THE_STR_END          DS    F
SUBS_STR_ADDR        DS    F           details of substitute string
SUBS_STR_LENGTH      DS    F
SUBS_STR_END         DS    F
STEM_NAME_ADR        DS    F           rexx stem or variable name
STEM_NAME_LEN        DS    F
CODEPAGE_PTR         DS    F
CODEPAGE_LEN         DS    F
OPTION_WORD          DS    F
ENV_H_LEN            DS    F
ENV_H_NAME           DS    CL20
COMP_LEN             DS    F
COMP_NAME            DS    CL20
*
THE_COMMAND          DS    CL20        input command
NO_SAY               DS    CL1
SAV_COMMAND          DS    CL20        save input command
WORK_32              DS    CL32        32 bytes of work area
CALL_STEM_MESS       DS    CL26        error message for PROCSTEM call
*
FREE_C_NEEDED        DS    CL1         flag if C environment exists
ERR_NO               DS    CL1         some indicators
*
*                    the parameter area used to call irxexcom
IX_PARM              DS    4F
PARM_AREA            DS    (SHVBLEN)XL1
*
        PRGEDEF
*
        USING SHVBLOCK,R6
*______________________________________________________________________
*
 SEGS MAIN_BIT
*
   XC    OPTION_WORD,OPTION_WORD       clear options
   SEGDO SET_STEM_ZERO                 set RXPCRE2_ERROR_MESS.0 to '0'
*
   SEGDO GET_FIRST_ARGUMENT            into THE_COMMAND
*
*  some commands have alternatives so set the actual command here
*
   IF (CLC,=C'COMP ',EQ,THE_COMMAND)         alias for COMPILE
     MVC   THE_COMMAND,=CL20'COMPILE'
   ELSEIF (CLC,=C'EXECUTE ',EQ,THE_COMMAND)  alias for MATCH
     MVC   THE_COMMAND,=CL20'MATCH'
   ELSEIF (CLC,=C'EXEC ',EQ,THE_COMMAND)     alias for MATCH
     MVC   THE_COMMAND,=CL20'MATCH'
   ELSEIF (CLC,=C'SUBS ',EQ,THE_COMMAND)     alias for SUBSTITUTE
     MVC   THE_COMMAND,=CL20'SUBSTITUTE'
   ENDIF
*
   IF (CLC,=C'CONNECT ',EQ,THE_COMMAND)
     SEGDO GET_CONNECT_ARGUMENTS
     SEGDO CONNECT_PROCESS
   ELSEIF (CLC,=C'COMPILE ',EQ,THE_COMMAND)
     MVI   FREE_C_NEEDED,C'Y'
     SEGDO GET_COMPILE_ARGUMENTS
     SEGDO COMPILE_PROCESS
   ELSEIF (CLC,=C'MATCH ',EQ,THE_COMMAND)
*    MVI   THE_COMMAND,C'X'            this would force an error
     MVI   FREE_C_NEEDED,C'Y'
     SEGDO GET_MATCH_ARGUMENTS
     SEGDO MATCH_PROCESS
   ELSEIF (CLC,=C'SUBSTITUTE ',EQ,THE_COMMAND)
     MVI   FREE_C_NEEDED,C'Y'
     SEGDO GET_SUBSTITUTE_ARGUMENTS
     SEGDO SUBSTITUTE_PROCESS
   ELSEIF (CLC,=C'RELEASE ',EQ,THE_COMMAND)
     MVI   FREE_C_NEEDED,C'Y'
     SEGDO GET_RELEASE_ARGUMENTS
     SEGDO RELEASE_PROCESS
   ELSEIF (CLC,=C'DISCONNECT ',EQ,THE_COMMAND)
     SEGDO DISCONNECT_PROCESS
     SEGDO DELETE_RXPCRE2A
   ELSE
     MVI   ERR_NO,1                    invalid command
     SEGDO NOT_VALID
   ENDIF
*
 SEGE MAIN_BIT
*______________________________________________________________________
*
 SEGS GET_FIRST_ARGUMENT
*
*  first argument should be the command
*
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,2
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,3
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,4                    arbitary max length
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   L     R15,ARGTABLE_ARGSTRING_LENGTH length or argument
   BCTR  R15,0                         length less one
   MVC   THE_COMMAND,SPACES            initialise
   EX    R15,COPY_COMMAND              copy the command
   OC    THE_COMMAND,SPACES            make uppercase
*
   IF (CLC,=C'CONNECT ',NE,THE_COMMAND)
*
*    next argument should be the environment which is 8 bytes normally
*    but could be 9 if nosay was specified
*
     LA    ARG_POINT,ARGTABLE_NEXT     check for environment
     IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
       MVI   ERR_NO,5
       SEGDO NOT_VALID
     ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'8'),AND,              /
               (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'9')
       MVI   ERR_NO,6
       SEGDO NOT_VALID
     ENDIF
     L     R14,ARGTABLE_ARGSTRING_PTR  address of argument
     MVC   LOAD_ADDR_CHAR,0(R14)       copy it
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'9')
       MVC   NO_SAY,8(R14)
     ENDIF
     XPACK LOAD_ADDR_CHAR,8,LOAD_ADDRESS   pack into 4 bytes
   ENDIF
*
 SEGE GET_FIRST_ARGUMENT
*______________________________________________________________________
*
 SEGS GET_CONNECT_ARGUMENTS
*
   MVC   SAV_COMMAND,THE_COMMAND
   MVC   THE_COMMAND,SPACES            initialise
   DO FROM=(R5,4)
     LA    ARG_POINT,ARGTABLE_NEXT       point to next argument
   DOEXIT (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')  no more
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')
       L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
       L     R15,ARGTABLE_ARGSTRING_LENGTH length or argument
       BCTR  R15,0                         length less one
       MVC   THE_COMMAND,SPACES            initialise
       EX    R15,COPY_COMMAND              copy the command
       OC    THE_COMMAND,SPACES            make uppercase
     ENDIF
   ENDDO
   IF (CLC,=C'NOSAY ',EQ,THE_COMMAND)
     MVI   NO_SAY,C'Y'                 flag nosay mode
   ENDIF
   MVC   THE_COMMAND,SAV_COMMAND       put real command back
*
*  first argument should be the name for the environment handle
*
   L     ARG_POINT,EFPLARG             point to first argument again
   LA    ARG_POINT,ARGTABLE_NEXT       check for environment name
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,51                   must have variable name
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,52                   zero length argument
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,53                   arbitary max length
     SEGDO NOT_VALID
   ENDIF
*
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   L     R15,ARGTABLE_ARGSTRING_LENGTH
   ST    R15,ENV_H_LEN                 save length
   MVC   ENV_H_NAME,SPACES             set to spaces
   BCTR  R15,0                         minus 1 for execute
   EX    R15,COPY_ENV_H
   OC    ENV_H_NAME,SPACES             make uppercase
   IF (CLC,=C'DEBUG',EQ,ENV_H_NAME)    check not DEBUG
     MVI   ERR_NO,54                   cannot use debug for handle
     SEGDO NOT_VALID
   ELSEIF (CLC,=C'NOSAY',EQ,ENV_H_NAME)  check not NOSAY
     MVI   ERR_NO,54                   cannot use debug for handle
     SEGDO NOT_VALID
   ENDIF
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for additional argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')  none - so quit
     SEGQUIT
   ENDIF
*
*  second argument is the optional code-page can be omitted.
*
   IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')
     MVC   CODEPAGE_PTR,ARGTABLE_ARGSTRING_PTR
     MVC   CODEPAGE_LEN,ARGTABLE_ARGSTRING_LENGTH
   ENDIF
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for additional argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')  none - so quit
     SEGQUIT
   ENDIF
*
*  third argument is the optional debug command
*
   IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'5')
       MVI   ERR_NO,7                  third argument <> 5 bytes
       SEGDO NOT_VALID
     ENDIF
     L     R14,ARGTABLE_ARGSTRING_PTR  address of argument
     MVC   WORK_32(5),0(R14)           copy argument
     OC    WORK_32(5),SPACES           make upper case
     IF (CLC,=C'DEBUG',EQ,WORK_32)     check for DEBUG
       MVI   THE_COMMAND+7,C'D'        flag debug mode
     ELSE
       MVI   ERR_NO,7                  third argument <> 5 bytes
       SEGDO NOT_VALID
     ENDIF
     MVC   WORK_32(5),=C'     '        clear to spaces
   ENDIF
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for additional argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')  none - so quit
     SEGQUIT
   ENDIF
*
*  fourth argument is the optional nosay command
*
   IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'0')
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'5')
       MVI   ERR_NO,81                 fourth argument <> 5 bytes
       SEGDO NOT_VALID
     ENDIF
     L     R14,ARGTABLE_ARGSTRING_PTR  address of argument
     MVC   WORK_32(5),0(R14)           copy argument
     OC    WORK_32(5),SPACES           make upper case
     IF (CLC,=C'NOSAY',EQ,WORK_32)     check for NOSAY
       MVI   NO_SAY,C'Y'               flag nosay mode
     ELSE
       MVI   ERR_NO,81                 fourth argument <> 5 bytes
       SEGDO NOT_VALID
     ENDIF
     MVC   WORK_32(5),=C'     '        clear to spaces
   ENDIF
*
 SEGE GET_CONNECT_ARGUMENTS
*______________________________________________________________________
*
 SEGS GET_COMPILE_ARGUMENTS
*
   MVI   REPEAT_SRCH,C'N'              indicate option not set
   MVI   IGNORE_CASE,C'N'              indicate option not set
   MVI   EXCLUDE_WHITE,C'N'            indicate option not set
   MVI   MULTI_LINE,C'N'               indicate option not set
   MVI   DOT_ALL,C'N'                  indicate option not set
*
*  first argument should be the REGEX
*
   LA    ARG_POINT,ARGTABLE_NEXT
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,8
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,9
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   L     R15,ARGTABLE_ARGSTRING_LENGTH length or argument
   LA    R0,0(R14,R15)                 point after last byte
   BCTR  R0,0                          back one byte
   STM   R14,R0,REG_EX_ADDR            set address, length and end
   LR    R1,R0                         copy address of last byte
*
*  second argument should be the name for the compile handle
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for compile name
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,61                   must have variable name
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,62                   zero length argument
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,63                   arbitary max length
     SEGDO NOT_VALID
   ENDIF
*
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   L     R15,ARGTABLE_ARGSTRING_LENGTH
   ST    R15,COMP_LEN                  save length
   MVC   COMP_NAME,SPACES              set to spaces
   BCTR  R15,0                         minus 1 for execute
   EX    R15,COPY_COMP
   OC    COMP_NAME,SPACES              make uppercase
   IF (CLC,=C'DEBUG',EQ,COMP_NAME)     check not DEBUG
     MVI   ERR_NO,64                   cannot use debug for compile
     SEGDO NOT_VALID
   ENDIF
*
*  third argument is the compile options
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for fourth argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
*    NOP                               nothing there
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,11                   arbitary max length
     SEGDO NOT_VALID
   ELSE
     L     R14,ARGTABLE_ARGSTRING_PTR  address of argument
     L     R15,ARGTABLE_ARGSTRING_LENGTH length or argument
     LA    R1,0(R14,R15)               point after last byte
     BCTR  R1,0                        back one byte
     IF (CLI,0(R1),EQ,X'00')           NULL terminated
       MVI   ERR_NO,12                 yes so error
       SEGDO NOT_VALID
     ENDIF
*
*  about to look for options currently  G/g  I/i  M/m  S/s  X/x
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
       ELSEIF (CLI,0(R14),EQ,C'm')     check for multiline
         MVI   MULTI_LINE,C'Y'         flag as found
       ELSEIF (CLI,0(R14),EQ,C'M')     check for multiline
         MVI   MULTI_LINE,C'Y'         flag as found
       ELSEIF (CLI,0(R14),EQ,C's')     check for dotall
         MVI   DOT_ALL,C'Y'            flag as found
       ELSEIF (CLI,0(R14),EQ,C'S')     check for dotall
         MVI   DOT_ALL,C'Y'            flag as found
       ELSE
         MVI   ERR_NO,13               unknown option value
         SEGDO NOT_VALID
       ENDIF
       LA    R14,1(,R14)               next byte in argument
     ENDDO
*
*  fourth argument is the optional OPTIONS word in hex display
*
     LA    ARG_POINT,ARGTABLE_NEXT       check for option argument
     IF (CLC,ARGTABLE_ARGSTRING_PTR,NE,=8X'FF')
       IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'8')
         MVI   ERR_NO,80
         SEGDO NOT_VALID
       ELSE
         L     R3,ARGTABLE_ARGSTRING_PTR
         XPACK 0(R3),8,OPTION_WORD
       ENDIF
     ENDIF
   ENDIF
*
 SEGE GET_COMPILE_ARGUMENTS
*______________________________________________________________________
*
 SEGS GET_MATCH_ARGUMENTS
*
*  first argument should be the compile information
*
   LA    ARG_POINT,ARGTABLE_NEXT
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,14
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'21')
     MVI   ERR_NO,15
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   MVC   COMPILE_CHAR,0(R14)           copy it
   XPACK COMPILE_CHAR,16,COMPILE_DATA
   MVC   COMPILE_DATA+8(5),COMPILE_CHAR+16
*
*  second argument should be the string to work on
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for second argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,16
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,17
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
     MVI   ERR_NO,18                   must have stem name
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,19                   zero length argument
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,20                   arbitary max length
     SEGDO NOT_VALID
   ENDIF
*
   MVC   STEM_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   STEM_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
*
*  fourth argument is the optional OPTIONS word in hex display
*
   LA    ARG_POINT,ARGTABLE_NEXT
   IF (CLC,ARGTABLE_ARGSTRING_PTR,NE,=8X'FF')
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'8')
       MVI   ERR_NO,80
       SEGDO NOT_VALID
     ELSE
       L     R3,ARGTABLE_ARGSTRING_PTR
       MVC   MESS_TXT(20),=CL20'MATCH   INPUT'
       MVC   MESS_TXT+20(8),0(R3)
       XPACK 0(R3),8,OPTION_WORD
     ENDIF
   ENDIF
*
 SEGE GET_MATCH_ARGUMENTS
*______________________________________________________________________
*
 SEGS GET_SUBSTITUTE_ARGUMENTS
*
*  first argument should be the compile information
*
   LA    ARG_POINT,ARGTABLE_NEXT
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,71
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'21')
     MVI   ERR_NO,72
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   MVC   COMPILE_CHAR,0(R14)           copy it
   XPACK COMPILE_CHAR,16,COMPILE_DATA
   MVC   COMPILE_DATA+8(5),COMPILE_CHAR+16
*
*  second argument should be the string to work on
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for second argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,73
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,74
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR
   L     R15,ARGTABLE_ARGSTRING_LENGTH
   LA    R0,0(R14,R15)                 point after last byte
   BCTR  R0,0                          back one byte
   STM   R14,R0,THE_STR_ADDR           set address, length and end
*
*  third argument should be the variable name to populate
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for third argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,75                   must have variable name
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,76                   zero length argument
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,77                   arbitary max length
     SEGDO NOT_VALID
   ENDIF
   MVC   STEM_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   STEM_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
*
*  fourth argument should be the substitute string
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for fourth argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,78                   must have substitute string
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,79                   zero length argument
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR
   L     R15,ARGTABLE_ARGSTRING_LENGTH
   LA    R0,0(R14,R15)                 point after last byte
   BCTR  R0,0                          back one byte
   STM   R14,R0,SUBS_STR_ADDR          set address, length and end
*
*  fifth argument is the optional OPTIONS word in hex display
*
   LA    ARG_POINT,ARGTABLE_NEXT
   IF (CLC,ARGTABLE_ARGSTRING_PTR,NE,=8X'FF')
     IF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'8')
       MVI   ERR_NO,80
       SEGDO NOT_VALID
     ELSE
       L     R3,ARGTABLE_ARGSTRING_PTR
       MVC   MESS_TXT(20),=CL20'SUBST   INPUT'
       MVC   MESS_TXT+20(8),0(R3)
       XPACK 0(R3),8,OPTION_WORD
     ENDIF
   ENDIF
*
 SEGE GET_SUBSTITUTE_ARGUMENTS
*______________________________________________________________________
*
 SEGS GET_RELEASE_ARGUMENTS
*
*  first argument should be the compile information
*
   LA    ARG_POINT,ARGTABLE_NEXT
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,21
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'21')
     MVI   ERR_NO,22
     SEGDO NOT_VALID
   ENDIF
   L     R14,ARGTABLE_ARGSTRING_PTR    address of argument
   MVC   COMPILE_CHAR,0(R14)           copy it
   XPACK COMPILE_CHAR,16,COMPILE_DATA
*
 SEGE GET_RELEASE_ARGUMENTS
*______________________________________________________________________
*
 SEGS CONNECT_PROCESS
*
   SEGDO CHECK_FOR_PCRELIB
   IF (CLI,PCRELIB_THERE,EQ,C'Y')      DDNAME is available
     OPEN  (PCRELIB,INPUT)             open the load library
     IF (LTR,R15,R15,Z)
       LOAD  EP=RXPCRE2A,DCB=PCRELIB,ERRET=*+4
       IF (LTR,R15,R15,NZ)
         MVC   MESS_TXT(L'C_P_MS1),C_P_MS1
         SEGDO CALL_IRXSAY             output error message
         CLOSE PCRELIB                 close the load library
         PRGQUIT RC=8                  quit RC=8
       ENDIF
       ST    R0,LOAD_ADDRESS           save address of program
       CLOSE PCRELIB                   close the load library
     ELSE
       MVC   MESS_TXT(L'C_P_MS2),C_P_MS2
       SEGDO CALL_IRXSAY               output error message
       PRGQUIT RC=8                    quit RC=8
     ENDIF
   ELSE                                load library was not there
     LOAD  EP=RXPCRE2A                 so just try normal load
     IF (LTR,R15,R15,NZ)
       MVC   MESS_TXT(L'C_P_MS3),C_P_MS3
       SEGDO CALL_IRXSAY               output error message
       PRGQUIT RC=8                    quit RC=8
     ENDIF
     ST    R0,LOAD_ADDRESS             save address of program
   ENDIF
   XUNPK LOAD_ADDRESS,4,LOAD_ADDR_CHAR   convert address to character
*
   LA    R1,THE_COMMAND            point to the command
   ST    R1,MY_P_1                 save in parm-list
   LA    R1,CODEPAGE_PTR           get code page value (if any)
   ST    R1,MY_P_2                 save in parm-list
   LA    R1,PCREDUMP               address of DCB
   ST    R1,MY_P_20                save it
   LA    R1,NOSAY_AREA             address of DCB
   ST    R1,MY_P_21                save it
   L     R15,LOAD_ADDRESS          get program address
   LA    R1,MY_PARM                point to parm-list
   BASR  R14,R15                   call RXPCRE2A
   IF (LTR,R15,R15,NZ)
     SEGDO DELETE_RXPCRE2A
     PRGQUIT RC=8
   ENDIF
*
   SEGDO SET_ENV_HANDLE
*
 SEGE CONNECT_PROCESS
*______________________________________________________________________
*
 SEGS COMPILE_PROCESS
*
   LA    R1,THE_COMMAND            point to the command
   ST    R1,MY_P_1
   LA    R1,REG_EX_ADDR            point to the regex
   ST    R1,MY_P_2
   LA    R1,COMPILE_DATA           point to output compile address
   ST    R1,MY_P_3
   LA    R1,OPTION_WORD            get options value (if any)
   ST    R1,MY_P_4                 save in parm-list
   LA    R1,PCREDUMP               address of DCB
   ST    R1,MY_P_20                save it
   LA    R1,NOSAY_AREA             address of DCB
   ST    R1,MY_P_21                save it
   L     R15,LOAD_ADDRESS          get program address
   LA    R1,MY_PARM
   BASR  R14,R15                   call RXPCRE2A
   IF (LTR,R15,R15,NZ)
     SEGDO DELETE_RXPCRE2A
     PRGQUIT RC=8
   ENDIF
*
   XUNPK COMPILE_DATA,8,COMPILE_CHAR           unpack compile handle
   MVC   COMPILE_CHAR+16(5),COMPILE_DATA+8     put indicators on end
   SEGDO SET_COMPILE_HANDLE
*
 SEGE COMPILE_PROCESS
*______________________________________________________________________
*
 SEGS MATCH_PROCESS
*
   LA    R1,THE_COMMAND            point to the command
   ST    R1,MY_P_1
   LA    R1,COMPILE_DATA           point to the compile handle
   ST    R1,MY_P_2
   MVC   MY_P_3,THE_STR_ADDR       string address
   MVC   MY_P_4,THE_STR_LENGTH     string length
   MVC   MY_P_5,THE_STR_END        end of string address
   MVC   MY_P_6,STEM_NAME_ADR      stem name address
   MVC   MY_P_7,STEM_NAME_LEN      stem name length
   LA    R1,OPTION_WORD            get options value (if any)
   ST    R1,MY_P_8                 save in parm-list
   LA    R1,PCREDUMP               address of DCB
   ST    R1,MY_P_20                save it
   LA    R1,NOSAY_AREA             address of DCB
   ST    R1,MY_P_21                save it
   L     R15,LOAD_ADDRESS          get program address
   LA    R1,MY_PARM
   BASR  R14,R15                   call RXPCRE2A
   ST    R15,R_C                   save return code 1 or 0
   IF (CHI,R15,GT,1)
     SEGDO DELETE_RXPCRE2A
     PRGQUIT RC=8
   ENDIF
*
*  If RXPCRE2A set an error message in EVALBLOCK then it will
*  have priority over the contents of R_C. If not PRGEDEF will
*  put the return code into EVALBLOCK.
*
 SEGE MATCH_PROCESS
*______________________________________________________________________
*
 SEGS SUBSTITUTE_PROCESS
*
   LA    R1,THE_COMMAND            point to the command
   ST    R1,MY_P_1
   LA    R1,COMPILE_DATA           point to the compile handle
   ST    R1,MY_P_2
   MVC   MY_P_3,THE_STR_ADDR       string address
   MVC   MY_P_4,THE_STR_LENGTH     string length
   MVC   MY_P_5,THE_STR_END        end of string address
   MVC   MY_P_6,STEM_NAME_ADR      stem name address
   MVC   MY_P_7,STEM_NAME_LEN      stem name length
   MVC   MY_P_8,SUBS_STR_ADDR      substitute string address
   MVC   MY_P_9,SUBS_STR_LENGTH    substitute string length
   MVC   MY_P_10,THE_STR_END       substitute end of string address
   LA    R1,OPTION_WORD            get options value (if any)
   ST    R1,MY_P_11                save in parm-list
   LA    R1,PCREDUMP               address of DCB
   ST    R1,MY_P_20                save it
   LA    R1,NOSAY_AREA             address of DCB
   ST    R1,MY_P_21                save it
   L     R15,LOAD_ADDRESS          get program address
   LA    R1,MY_PARM
   BASR  R14,R15                   call RXPCRE2A
   ST    R15,R_C                   save return code 1 or 0
   IF (CHI,R15,GT,1)
     SEGDO DELETE_RXPCRE2A
     PRGQUIT RC=8
   ENDIF
*
*  If RXPCRE2A set an error message in EVALBLOCK then it will
*  have priority over the contents of R_C. If not PRGEDEF will
*  put the return code into EVALBLOCK.
*
 SEGE SUBSTITUTE_PROCESS
*______________________________________________________________________
*
 SEGS RELEASE_PROCESS
*
   LA    R1,THE_COMMAND            point to the command
   ST    R1,MY_P_1
   LA    R1,COMPILE_DATA           point to the compile handle
   ST    R1,MY_P_2
   LA    R1,PCREDUMP               address of DCB
   ST    R1,MY_P_20                save it
   LA    R1,NOSAY_AREA             address of DCB
   ST    R1,MY_P_21                save it
   L     R15,LOAD_ADDRESS          get program address
   LA    R1,MY_PARM
   BASR  R14,R15                   call RXPCRE2A
   IF (LTR,R15,R15,NZ)
     SEGDO DELETE_RXPCRE2A
     PRGQUIT RC=8
   ENDIF
*
 SEGE RELEASE_PROCESS
*______________________________________________________________________
*
 SEGS DISCONNECT_PROCESS
*
   MVC   SAV_COMMAND,THE_COMMAND
   MVC   THE_COMMAND,=CL20'DISCONNECT'
   LA    R1,THE_COMMAND            point to the command
   ST    R1,MY_P_1
   LA    R1,PCREDUMP               address of DCB
   ST    R1,MY_P_20                save it
   LA    R1,NOSAY_AREA             address of DCB
   ST    R1,MY_P_21                save it
   L     R15,LOAD_ADDRESS          get program address
   LA    R1,MY_PARM
   BASR  R14,R15                   call RXPCRE2A
   MVC   THE_COMMAND,SAV_COMMAND
   IF (LTR,R15,R15,NZ)
     SEGDO DELETE_RXPCRE2A
     SETRC RC=8
   ENDIF
*
 SEGE DISCONNECT_PROCESS
*______________________________________________________________________
*
 SEGS NOT_VALID
*
*  an error occured so set error message as output from REXX function
*
   IF (CLI,ERR_NO,EQ,1)
     MVC   MESS_TXT(L'MESS_1),MESS_1
   ELSEIF (CLI,ERR_NO,EQ,2)
     MVC   MESS_TXT(L'MESS_2),MESS_2
   ELSEIF (CLI,ERR_NO,EQ,3)
     MVC   MESS_TXT(L'MESS_3),MESS_3
   ELSEIF (CLI,ERR_NO,EQ,4)
     MVC   MESS_TXT(L'MESS_4),MESS_4
   ELSEIF (CLI,ERR_NO,EQ,5)
     MVC   MESS_TXT(L'MESS_5),MESS_5
   ELSEIF (CLI,ERR_NO,EQ,6)
     MVC   MESS_TXT(L'MESS_6),MESS_6
   ELSEIF (CLI,ERR_NO,EQ,7)
     MVC   MESS_TXT(L'MESS_7),MESS_7
   ELSEIF (CLI,ERR_NO,EQ,8)
     MVC   MESS_TXT(L'MESS_8),MESS_8
   ELSEIF (CLI,ERR_NO,EQ,9)
     MVC   MESS_TXT(L'MESS_9),MESS_9
   ELSEIF (CLI,ERR_NO,EQ,10)
     MVC   MESS_TXT(L'MESS_10),MESS_10
   ELSEIF (CLI,ERR_NO,EQ,11)
     MVC   MESS_TXT(L'MESS_11),MESS_11
   ELSEIF (CLI,ERR_NO,EQ,12)
     MVC   MESS_TXT(L'MESS_12),MESS_12
   ELSEIF (CLI,ERR_NO,EQ,13)
     MVC   MESS_TXT(L'MESS_13),MESS_13
   ELSEIF (CLI,ERR_NO,EQ,14)
     MVC   MESS_TXT(L'MESS_14),MESS_14
   ELSEIF (CLI,ERR_NO,EQ,15)
     MVC   MESS_TXT(L'MESS_15),MESS_15
   ELSEIF (CLI,ERR_NO,EQ,16)
     MVC   MESS_TXT(L'MESS_16),MESS_16
   ELSEIF (CLI,ERR_NO,EQ,17)
     MVC   MESS_TXT(L'MESS_17),MESS_17
   ELSEIF (CLI,ERR_NO,EQ,18)
     MVC   MESS_TXT(L'MESS_18),MESS_18
   ELSEIF (CLI,ERR_NO,EQ,19)
     MVC   MESS_TXT(L'MESS_19),MESS_19
   ELSEIF (CLI,ERR_NO,EQ,20)
     MVC   MESS_TXT(L'MESS_20),MESS_20
   ELSEIF (CLI,ERR_NO,EQ,21)
     MVC   MESS_TXT(L'MESS_21),MESS_21
   ELSEIF (CLI,ERR_NO,EQ,22)
     MVC   MESS_TXT(L'MESS_22),MESS_22
   ELSEIF (CLI,ERR_NO,EQ,51)
     MVC   MESS_TXT(L'MESS_51),MESS_51
   ELSEIF (CLI,ERR_NO,EQ,52)
     MVC   MESS_TXT(L'MESS_52),MESS_52
   ELSEIF (CLI,ERR_NO,EQ,53)
     MVC   MESS_TXT(L'MESS_53),MESS_53
   ELSEIF (CLI,ERR_NO,EQ,54)
     MVC   MESS_TXT(L'MESS_54),MESS_54
   ELSEIF (CLI,ERR_NO,EQ,61)
     MVC   MESS_TXT(L'MESS_61),MESS_61
   ELSEIF (CLI,ERR_NO,EQ,62)
     MVC   MESS_TXT(L'MESS_62),MESS_62
   ELSEIF (CLI,ERR_NO,EQ,63)
     MVC   MESS_TXT(L'MESS_63),MESS_63
   ELSEIF (CLI,ERR_NO,EQ,64)
     MVC   MESS_TXT(L'MESS_64),MESS_64
   ELSEIF (CLI,ERR_NO,EQ,71)
     MVC   MESS_TXT(L'MESS_71),MESS_71
   ELSEIF (CLI,ERR_NO,EQ,72)
     MVC   MESS_TXT(L'MESS_72),MESS_72
   ELSEIF (CLI,ERR_NO,EQ,73)
     MVC   MESS_TXT(L'MESS_73),MESS_73
   ELSEIF (CLI,ERR_NO,EQ,74)
     MVC   MESS_TXT(L'MESS_74),MESS_74
   ELSEIF (CLI,ERR_NO,EQ,75)
     MVC   MESS_TXT(L'MESS_75),MESS_75
   ELSEIF (CLI,ERR_NO,EQ,76)
     MVC   MESS_TXT(L'MESS_76),MESS_76
   ELSEIF (CLI,ERR_NO,EQ,77)
     MVC   MESS_TXT(L'MESS_77),MESS_77
   ELSEIF (CLI,ERR_NO,EQ,78)
     MVC   MESS_TXT(L'MESS_78),MESS_78
   ELSEIF (CLI,ERR_NO,EQ,79)
     MVC   MESS_TXT(L'MESS_79),MESS_79
   ELSEIF (CLI,ERR_NO,EQ,80)
     MVC   MESS_TXT(L'MESS_80),MESS_80
   ELSEIF (CLI,ERR_NO,EQ,81)
     MVC   MESS_TXT(L'MESS_81),MESS_81
*
   ELSE    this should never happen
     MVC   MESS_TXT(L'UN_KNOWN),UN_KNOWN
     XUNPK ERR_NO,1,MESS_TXT+L'UN_KNOWN
   ENDIF
   SEGDO CALL_IRXSAY
   IF (CLI,FREE_C_NEEDED,EQ,C'Y')
     SEGDO DISCONNECT_PROCESS
   ENDIF
   SEGDO DELETE_RXPCRE2A
   PRGQUIT RC=8                        quit with RC=8
*
 SEGE NOT_VALID
*______________________________________________________________________
*
 SEGS CHECK_FOR_PCRELIB
*
   L     R2,ATIOT                      task I/O table address there ?
   IF (LTR,R2,R2,Z)                    no
     EXTRACT ATIOT,'S',FIELDS=(TIOT)   get it
     L     R2,ATIOT
   ENDIF
   USING TIOT1,R2
*
   LA    R4,TIOENTRY
   DROP  R2
   USING TIOENTRYD,R4
   XR    R2,R2                         length register
   DO INF
     ICM   R2,1,TIOELNGH               get entry length
   DOEXIT (LTR,R2,R2,Z)                zero length, DDNAME not found
   DOEXIT (CLC,TIOEDDNM,EQ,=CL8'PCRELIB')
     AR   R4,R2                        next entry
   ENDDO
*
   IF (LTR,R2,R2,Z)                    could not find DDNAME
     MVI   PCRELIB_THERE,C'N'
   ELSE
     MVI   PCRELIB_THERE,C'Y'
   ENDIF
*
 SEGE CHECK_FOR_PCRELIB
*______________________________________________________________________
*
 SEGS SET_ENV_HANDLE
*
   XC    IX_PARM(4*4),IX_PARM
   LA    R1,=CL8'IRXEXCOM'
   ST    R1,IX_PARM
   LA    R6,PARM_AREA
   MVC   SHVNEXT,=F'0'
   MVC   SHVUSER,=F'0'
   MVC   SHVBUFL,=F'0'
   MVI   SHVCODE,SHVSTORE              set command to store variable
*
   LA    R1,ENV_H_NAME                 point to env handle name
   ST    R1,SHVNAMA                    store in function call
   MVC   SHVNAML,ENV_H_LEN             copy length of the name
   LA    R1,LOAD_ADDR_CHAR             point to environment handle
   LA    R2,8                          get it's length
   IF (CLI,NO_SAY,EQ,C'Y')
     MVC   NO_SAY_COPY,NO_SAY
     AHI   R2,1
   ENDIF
   ST    R1,SHVVALA                    store address of value
   ST    R2,SHVVALL                    store length of value
*
   SEGDO CALL_IRXEXCOM
*
   IF (LTR,R15,R15,NZ)
     MVC   MESS_TXT(L'E_S_ENV),E_S_ENV
     XUNPK (R15),4,MESS_TXT+L'E_S_ENV
     SEGDO CALL_IRXSAY
     PRGQUIT RC=8
   ENDIF
*
 SEGE SET_ENV_HANDLE
*______________________________________________________________________
*
 SEGS SET_COMPILE_HANDLE
*
   XC    IX_PARM(4*4),IX_PARM
   LA    R1,=CL8'IRXEXCOM'
   ST    R1,IX_PARM
   LA    R6,PARM_AREA
   MVC   SHVNEXT,=F'0'
   MVC   SHVUSER,=F'0'
   MVC   SHVBUFL,=F'0'
   MVI   SHVCODE,SHVSTORE              set command to store variable
*
   LA    R1,COMP_NAME                  point to compile handle name
   ST    R1,SHVNAMA                    store in function call
   MVC   SHVNAML,COMP_LEN              copy length of the name
   LA    R1,COMPILE_CHAR               point to compile handle
   LA    R2,L'COMPILE_CHAR             get it's length
   ST    R1,SHVVALA                    store address of value
   ST    R2,SHVVALL                    store length of value
*
   SEGDO CALL_IRXEXCOM
*
   IF (LTR,R15,R15,NZ)
     MVC   MESS_TXT(L'E_S_CMP),E_S_CMP
     XUNPK (R15),4,MESS_TXT+L'E_S_CMP
     SEGDO CALL_IRXSAY
     PRGQUIT RC=8
   ENDIF
*
 SEGE SET_COMPILE_HANDLE
*______________________________________________________________________
*
 SEGS CALL_IRXSAY
*
   IF (CLI,NO_SAY,EQ,C'Y')
     SEGDO CALL_IRXSAY_NOSAY
   ELSE
     SEGDO CALL_IRXSAY_REAL
   ENDIF
*
 SEGE CALL_IRXSAY
*______________________________________________________________________
*
 SEGS SET_STEM_ZERO
*
   LA    R1,NO_SAY_NAME
   ST    R1,NOSAY_NAME_ADR
   LA    R1,L'NO_SAY_NAME
   ST    R1,NOSAY_NAME_LEN
   XC    NOSAY_ZERO_COUNT,NOSAY_ZERO_COUNT   trigger init process
   MVI   NOSAY_TRIM_VAR,C'N'         do not trim spaces from var
   MVHHI NOSAY_FUNCTION,1            init STEM.0
   LA    R1,NOSAY_AREA               point to STEM. area
   MVC   CALL_STEM_MESS,=CL26'Error in SET_STEM_ZERO'
   SEGDO CALL_PROCSTEM
*
 SEGE SET_STEM_ZERO
*______________________________________________________________________
*
 SEGS SET_STEM_COUNT
*
   LA    R1,NO_SAY_NAME
   ST    R1,NOSAY_NAME_ADR
   LA    R1,L'NO_SAY_NAME
   ST    R1,NOSAY_NAME_LEN
   MVI   NOSAY_TRIM_VAR,C'N'         do not trim spaces from var
   MVHHI NOSAY_FUNCTION,3            set STEM.0 to total lines
   LA    R1,NOSAY_AREA               point to STEM. area
   MVC   CALL_STEM_MESS,=CL26'Error in SET_STEM_COUNT'
   SEGDO CALL_PROCSTEM
*
 SEGE SET_STEM_COUNT
*______________________________________________________________________
*
 SEGS CALL_IRXSAY_NOSAY
*
   LA    R1,MESS_TXT                   point to message text
   ST    R1,NOSAY_TEXT_ADR             store address
   LA    R14,MESS_TXT+L'MESS_TXT-1
   DO FROM=(R15,L'MESS_TXT-1)
   DOEXIT (CLI,0(R14),GT,C' ')         ignore trailing blanks
     AHI   R14,-1                      previous byte
   ENDDO
   AHI   R15,1
   ST    R15,NOSAY_TEXT_LEN            store address
   MVHHI NOSAY_FUNCTION,2              set STEM value
   LA    R1,NOSAY_AREA                 point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in NOSAY 1'
   SEGDO CALL_PROCSTEM
   MVI   MESS_TXT,C' '
   MVC   MESS_TXT+1(L'MESS_TXT-1),MESS_TXT
*
*  populate stem.pos.0 with current stem count
*
   MVHHI NOSAY_FUNCTION,3              set STEM.0 to total lines
   LA    R1,NOSAY_AREA
   MVC   CALL_STEM_MESS,=CL26'Error in NOSAY 2'
   SEGDO CALL_PROCSTEM
*
 SEGE CALL_IRXSAY_NOSAY
*______________________________________________________________________
*
 SEGS CALL_IRXEXCOM
*
   LA    R1,PARM_AREA
   ST    R1,IX_PARM+12
   OI    IX_PARM+12,X'80'
   L     R0,#SAV_REX+12                rexx environment block addr
   LA    R1,IX_PARM
   USING IRXEXTE,15
   L     15,ENVBLOCK_IRXEXTE
   L     15,IRXEXCOM
   DROP  15
   BASR  14,15
*
 SEGE CALL_IRXEXCOM
*______________________________________________________________________
*
 SEGS CALL_IRXSAY_REAL
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
     MVC   MESS_TXT(L'SAY_ERR),SAY_ERR
     XUNPK R_C,4,MESS_TXT+19
     SEGDO WRITE_LOG                   then this error message
   ENDIF
   MVI   MESS_TXT,C' '
   MVC   MESS_TXT+1(L'MESS_TXT-1),MESS_TXT
*
 SEGE CALL_IRXSAY_REAL
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
     STRING (CALL_STEM_MESS,C,R_C,I)
     MVI   ERR_NO,99
     SEGDO NOT_VALID
   ENDIF
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list after call
*
 SEGE CALL_PROCSTEM
*______________________________________________________________________
*
 SEGS DELETE_RXPCRE2A
*
   DELETE EP=RXPCRE2A
*  IF (CLI,NO_SAY,EQ,C'Y')
*    SEGDO SET_STEM_COUNT
*  ENDIF
*
 SEGE DELETE_RXPCRE2A
*______________________________________________________________________
*
 SEGS WRITE_LOG
*
*  this does WTO from MESS_TXT and then clears it to spaces
*
   WTOX
*
 SEGE WRITE_LOG
*______________________________________________________________________
*
               PRGSTAT
*
SPACES         DC    CL20' '
*
ATIOT          DS    F
PCRELIB_THERE  DS    C
*
PCRELIB  DCB   DDNAME=PCRELIB,DSORG=PO,MACRF=R
PCREDUMP DCB   DSORG=PS,DDNAME=PCREDUMP,LRECL=80,MACRF=(PM),RECFM=FB
*
COPY_COMMAND   MVC   THE_COMMAND(0),0(R14)
COPY_ENV_H     MVC   ENV_H_NAME(0),0(R14)
COPY_COMP      MVC   COMP_NAME(0),0(R14)
NO_SAY_NAME    DC    C'RXPCRE2_ERROR_MESS'
*
         PRGSTEM PREF=NOSAY      for  rxpcre2_error_mess.
*
E_S_ENV  DC C'RXPCRE2-001 - Error in set environment handle RC='
E_S_CMP  DC C'RXPCRE2-002 - Error in set compile handle RC='
*
C_P_MS1  DC C'RXPCRE2-003 - Could not find RXPCRE2A in PRCELIB'
C_P_MS2  DC C'RXPCRE2-004 - Could not open PCRELIB'
C_P_MS3  DC C'RXPCRE2-005 - Could not find RXPCRE2A'
SAY_ERR  DC C'RXPCRE2-006 - Return code on SAY='
*
MESS_1   DC C'RXPCRE2-007 - invalid command specified'
MESS_2   DC C'RXPCRE2-008 - command not specified'
MESS_3   DC C'RXPCRE2-009 - command length was 0'
MESS_4   DC C'RXPCRE2-010 - command greater than 20 bytes'
MESS_5   DC C'RXPCRE2-011 - environment not specified as argument 2'
MESS_6   DC C'RXPCRE2-012 - environment length was not 8 bytes'
MESS_7   DC C'RXPCRE2-013 connect - debug command not ''debug'''
MESS_8   DC C'RXPCRE2-014 compile - regex not specified'
MESS_9   DC C'RXPCRE2-015 compile - regex length was 0'
MESS_10  DC C'RXPCRE2-016 compile - regex not null terminated'
MESS_11  DC C'RXPCRE2-017 compile - options > 20 bytes'
MESS_12  DC C'RXPCRE2-018 compile - options were null terminated'
MESS_13  DC C'RXPCRE2-019 compile - options not recognised'
MESS_14  DC C'RXPCRE2-020 execute - argument compile result not passed'
MESS_15  DC C'RXPCRE2-021 execute - argument compile result invalid'
MESS_16  DC C'RXPCRE2-022 execute - string to test is missing'
MESS_17  DC C'RXPCRE2-023 execute - string to test has zero length'
MESS_18  DC C'RXPCRE2-024 execute - stem name not available'
MESS_19  DC C'RXPCRE2-025 execute - stem name has 0 length'
MESS_20  DC C'RXPCRE2-026 execute - stem name > 20 bytes'
MESS_21  DC C'RXPCRE2-027 release - compile result not passed'
MESS_22  DC C'RXPCRE2-028 release - compile result invalid length'
MESS_51  DC C'RXPCRE2-029 connect - env handle not specified'
MESS_52  DC C'RXPCRE2-030 connect - env handle length was 0'
MESS_53  DC C'RXPCRE2-031 connect - env handle > 20 bytes'
MESS_54  DC C'RXPCRE2-032 connect - env handle was DEBUG or NOSAY'
MESS_61  DC C'RXPCRE2-033 connect - comp handle not specified'
MESS_62  DC C'RXPCRE2-034 connect - comp handle length was 0'
MESS_63  DC C'RXPCRE2-035 connect - comp handle > 20 bytes'
MESS_64  DC C'RXPCRE2-036 connect - comp handle was DEBUG'
MESS_71  DC C'RXPCRE2-037 subst - argument compile result not passed'
MESS_72  DC C'RXPCRE2-038 subst - argument compile result invalid'
MESS_73  DC C'RXPCRE2-039 subst - string to test is missing'
MESS_74  DC C'RXPCRE2-040 subst - string to test has zero length'
MESS_75  DC C'RXPCRE2-041 subst - variable name not available'
MESS_76  DC C'RXPCRE2-042 subst - variable name has 0 length'
MESS_77  DC C'RXPCRE2-043 subst - variable name > 20 bytes'
MESS_78  DC C'RXPCRE2-044 subst - subs string not available'
MESS_79  DC C'RXPCRE2-045 subst - subs string has 0 length'
MESS_80  DC C'RXPCRE2-080 option input is not 8 bytes long'
MESS_81  DC C'RXPCRE2-013 connect - nosay command not ''nosay'''
UN_KNOWN DC C'RXPCRE2-nnn - unknown error, RC='
*
               LTORG
*
*              copy of MVT IEFTIOT amended to make DSECTs explicit
               JRGTIOT
*
               PRGESTAT
               PRGEND
               END
