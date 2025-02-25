 TITLE 'RXPCRE2A - C HANDLER FOR PERL COMPATIBLE REGEX'
 PRINT GEN
RXPCRE2A PRGDEF FSEG=MAIN_BIT,INREX=Y                *,RMODE=24
*______________________________________________________________________
*
* This is a sub-routine used by RXPCRE2 to execute the REGEX C
* functions. It should NEVER be called directly.
*
* Version 0.2
* Contributed by:   John Gateley  November 2017.
* 2019-dec      :   add substitute function
*               :   move PCREDUMP DCB to RXPCRE2 to allow RMODE=31
*                   for this load module
*               :   improve housekeeping in case release calls have
*                   not been done
* 2022-aug      :   add option to send messages to rexx stem variable
*                   RXPCRE2_ERROR_MESS instead of using IRXSAY
*
* Copyright (c) 2017, John Gateley.
* All rights reserved.
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
* An article "Calling C functions from Assembler - revisited"
* by A Rudd printed in issue 208 of Xephon magazine of January 2004
* was very useful when I wrote this program.
* http://www.cbttape.org/xephon/xephonm/mvs0401.pdf
*
* As this was the only example I could find of a rexx function
* written in assembler that called C, I used the same method.
* CEEPIPI may also work, I do not know.
*______________________________________________________________________
*
*
*    STEM variables output
*         stem.0             count of stem variables
*         stem_STRING.?      matched substrings
*         stem_POS.?         position and length of matches e.g. 3,4
*         stem_NAME.?        named substrings ' ' if none
*______________________________________________________________________
*
*  REENTRANT STORAGE
*
*                    Define some maximums
MAX_NUM_COMPILES     EQU   12          max simultaneous compiles
MAX_STEM_LEN         EQU   2048        max length of a stem variable
*
STEM_NOSAY_ADDR      DS    F           address of NOSAY stem area
THE_STR_ADDR         DS    F           details of supplied string
THE_STR_LENGTH       DS    F
THE_STR_END          DS    F
SUBS_STR_ADDR        DS    F           details of substitute string
SUBS_STR_LEN         DS    F
SUBS_STR_END         DS    F
SUBST_OUT_PTR        DS    F           pointer to substitute output
SUBST_OUT_LEN        DS    F           length of substitute output
REG_EX_ADDR          DS    F           details of supplied REGEX
REG_EX_LEN           DS    F
REG_EX_END           DS    F
CONV_REGEX_ADDR      DS    F
CONV_REGEX_LEN       DS    F
CONV_SUBJECT_PTR     DS    F
CONV_SUBJECT_LEN     DS    F
CONV_SUBST_PTR       DS    F
CONV_SUBST_LEN       DS    F
CONV_BACK_PTR        DS    F
CONV_BACK_LEN        DS    F
*
OPTION_WORD          DS    F
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
PCREWS_OPTION_BITS   DS    F
PCRE2_INFO_NAMECOUNT EQU   17
PCRE2_INFO_NAM_ENTSZ EQU   18
PCRE2_INFO_NAMETABLE EQU   19
PCRE2_SUBSTITUTE_GLOBAL    EQU   256
*
HIGHEST_MATCH        DS    F           address of highest match
CURRENT_ADDRESS      DS    F
CURRENT_COUNT        DS    F
NUMBER_OF_PARMS      DS    H
DUMP_CALL            DS    C
PROGRAM_CALL         DS    CL8
*
IX_PARM              DS    4F
PARM_AREA            DS    (SHVBLEN)XL1
*
SAVE_R4_R5           DS    2F          temporary reg save
*
FUNC_NAME            DS    A           C function to be called
*
FIRST_SET_MATCH      DS    PL4         stem count at start of search
*
ERR_NO               DS    CL1         some indicators
REPEAT_SRCH          DS    CL1
IGNORE_CASE          DS    CL1
EXCLUDE_WHITE        DS    CL1
MULTI_LINE           DS    CL1
DOT_ALL              DS    CL1
NO_SAY               DS    CL1
WORK_32              DS    CL32        32 bytes used when debugging
CALL_STEM_MESS       DS    CL26        error message for PROCSTEM call
ERR_BUFFER_LEN       DS    F           length of error message
ERR_BUFFER           DS    CL256       area for error message
*
STEM_POS             DS    F           position in string
STEM_LEN             DS    F           length of substring
STEM_TEXT            DS    CL(MAX_STEM_LEN)   storage for stem value
*
MAP_HANDLES    DSECT
THIS_COMPILE   DS    A
THIS_MATCH     DS    A
*
        PRGSTEM PREF=NOSAY,DSECT=Y     for rxpcre2_error_mess.
*
        PRGEDEF
*______________________________________________________________________
*
 SEGS MAIN_BIT
*
   LR    R8,R1                         copy input parameter address
   L     R1,0(,R8)                     get command address
   MVC   NO_SAY,20(R1)                 copy the no_say indicator
   MVC   STEM_NOSAY_ADDR,80(R8)        copy NOSAY address in RXPCRE2
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')         can be set on first call
     L     R9,76(,R8)                  copy DCB address in RXPCRE2
     OPEN  ((R9),OUTPUT),MODE=31       open output file
     IF (LTR,15,R15,NZ)                error on open
       MVI   ERR_NO,88
       SEGDO NOT_VALID
     ENDIF
     SEGDO WRITE_PCREDUMP              write blank line to PCREDUMP
     MVC   MESS_TXT(30),=CL30'RXPCRE2A - starting again'
     SEGDO WRITE_PCREDUMP              write MESS_TXT to PCREDUMP
   ENDIF
*
   L     R2,0(,R8)                     get command address
   IF (CLI,DEBUG_MODE,EQ,C'Y')         can be set on first call
     MVC   MESS_TXT(11),=C'RXPCRE2A - '
     MVC   MESS_TXT+11(20),0(R2)       output command
     SEGDO WRITE_PCREDUMP              write MESS_TXT to PCREDUMP
   ENDIF
   IF (CLC,=C'CONNECT ',EQ,0(R2))
     SEGDO SET_C_ENVIRONMENT
   ELSEIF (CLC,=C'CONNECTD ',EQ,0(R2))
     MVI   DEBUG_MODE,C'Y'
     L     R9,76(,R8)                  copy DCB address in RXPCRE2
     OPEN  ((R9),OUTPUT),MODE=31       open output file
     IF (LTR,15,R15,NZ)                error on open
       MVI   ERR_NO,88
       SEGDO NOT_VALID
     ENDIF
     MVI   PCREDUMP_OPEN,C'Y'          flag as open
     SEGDO WRITE_PCREDUMP              write blank line
     MVC   MESS_TXT(20),=CL20'RXPCRE2A - starting'
     SEGDO WRITE_PCREDUMP              write MESS_TXT to PCREDUMP
     SEGDO SET_C_ENVIRONMENT
   ELSEIF (CLC,=C'COMPILE ',EQ,0(R2))
     LH    R1,TOT_COMPILES               increment count of compiles
     IF (CHI,R1,NL,MAX_NUM_COMPILES)
       MVI   ERR_NO,56
       SEGDO NOT_VALID
     ENDIF
     SEGDO COMPILE_REGEX
   ELSEIF (CLC,=C'MATCH ',EQ,0(R2))
     SEGDO MATCH_REGEX
   ELSEIF (CLC,=C'SUBSTITUTE ',EQ,0(R2))
     SEGDO SUBSTITUTE_REGEX
   ELSEIF (CLC,=C'RELEASE ',EQ,0(R2))
     SEGDO RELEASE_REGEX
   ELSEIF (CLC,=C'DISCONNECT ',EQ,0(R2))
     SEGDO END_C_ENVIRONMENT
   ELSE
     MVI   ERR_NO,55
     SEGDO NOT_VALID
   ENDIF
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(21),=CL21'RXPCRE2A - ending RC='
     XUNPK R_C+3,1,MESS_TXT+21
     SEGDO WRITE_PCREDUMP
     CLOSE ((R9)),MODE=31              close output file
     IF (CFI,R15,NE,0)
       MVC   MESS_TXT(22),=CL22'RXPCRE2A close failed'
       XUNPK (R15),,MESS_TXT+22
       SEGDO WRITE_LOG
     ENDIF
   ENDIF
*
 SEGE MAIN_BIT
*______________________________________________________________________
*
 SEGS COMPILE_REGEX
*
*  compile the REGEX
*
   L     R1,4(R8)                      point to REGEX details
   MVC   REG_EX_ADDR,0(R1)
   MVC   REG_EX_LEN,4(R1)
   MVC   REG_EX_END,8(R1)
*
   L     R1,8(,R8)                     point to options
   MVC   REPEAT_SRCH,8(R1)             and copy them
   MVC   IGNORE_CASE,9(R1)
   MVC   EXCLUDE_WHITE,10(R1)
   MVC   MULTI_LINE,11(R1)
   MVC   DOT_ALL,12(R1)
*
   L     R1,12(,R8)                    point to option word
   MVC   OPTION_WORD,0(R1)             and copy it
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'before REGEX conv'
     XUNPK CONVERT_HANDLE,4,MESS_TXT+20
     SEGDO WRITE_PCREDUMP
   ENDIF
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSCONVP)        set C function required
   L     R14,CONVERT_HANDLE
   L     R15,REG_EX_ADDR
   L     R0,REG_EX_LEN
   LA    R1,CONV_REGEX_ADDR
   STM   R14,R2,MY_P_3                 save parameters
   LA    R14,CONV_REGEX_LEN
   LA    R15,ERR_BUFFER                point to buffer
   STM   R14,R15,MY_P_7                save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'5'
     MVC   PROGRAM_CALL,=CL8'ZCSCONVP'
   ENDIF
   SEGDO CALL_C_FUNCTION               go and compile it
   ST    R15,R_C
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'REGEX converted'
     XUNPK CONV_REGEX_ADDR,4,MESS_TXT+20
     XUNPK CONV_REGEX_LEN,4,MESS_TXT+30
     XUNPK R_C,4,MESS_TXT+60
     SEGDO WRITE_PCREDUMP
     MVC   NUMBER_OF_PARMS,=H'6'
     MVC   PROGRAM_CALL,=CL8'COMPILE2'
   ENDIF
   IF (CLC,R_C,EQ,=F'1')               no conversion required
     XC    R_C,R_C                     so clear return code
   ELSEIF (CLC,R_C,NE,=F'0')
     IF (CLC,ERR_BUFFER,LE,SPACES)     error message not supplied
       XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
       STRING  ('RXPCRE2A-999 - ZCSCONVP error occurred',C),           /
               OUT=ERR_BUFFER
     ENDIF
     SEGDO ZCSCONVP_ERROR
   ENDIF
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(COMPILE2)        set C function required
   L     R14,CONV_REGEX_ADDR           address of REGEX
   L     R15,CONV_REGEX_LEN            length of REGEX
   LA    R0,0                          0 = no options
   IF (CLI,IGNORE_CASE,EQ,C'Y')
     O     R0,PCRE2_CASELESS           add ignore case value
   ENDIF
   IF (CLI,EXCLUDE_WHITE,EQ,C'Y')
     O     R0,PCRE2_EXTENDED           add exclude white space
   ENDIF
   IF (CLI,MULTI_LINE,EQ,C'Y')
     O     R0,PCRE2_MULTILINE          add multiline
   ENDIF
   IF (CLI,DOT_ALL,EQ,C'Y')
     O     R0,PCRE2_DOTALL             add dotall
   ENDIF
   O     R0,OPTION_WORD                add specified compile options
   LA    R1,PCREWS_ERRORNUMBER         error number field
   LA    R2,PCREWS_ERROROFFSET         error offfset field
   LA    R3,0                          0
   STM   R14,R3,MY_P_3                 save parameters
   SEGDO CALL_C_FUNCTION               go and compile it
   ST    R15,PCREWS_RE_PTR             save returned address
   L     R1,8(R8)
   ST    R15,0(R1)                     and save in caller
*
   IF (CLC,PCREWS_RE_PTR,EQ,=F'0')
     SEGDO COMPILE_ERROR
   ENDIF
   XC    ERR_BUFFER,ERR_BUFFER
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(14),=C'REGEX compiled'
     XUNPK PCREWS_RE_PTR,4,MESS_TXT+20
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   SEGDO CREATE_MATCH_BLOCK
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(19),=C'Match block created'
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   PUSH  USING
   USING MAP_HANDLES,R14
   LA    R14,ALL_THE_COMPILES
   LA    R15,MAX_NUM_COMPILES
   DO FROM=(R15)
   DOEXIT (CLC,=F'0',EQ,THIS_COMPILE)
     LA    R14,8(,R14)
   ENDDO
   L     R1,8(R8)
   MVC   THIS_COMPILE,0(R1)            copy compile handle
   MVC   THIS_MATCH,4(R1)              copy match block handle
   LH    R1,TOT_COMPILES               increment count of compiles
   LA    R1,1(,R1)
   STH   R1,TOT_COMPILES
   POP   USING
*
 SEGE COMPILE_REGEX
*______________________________________________________________________
*
 SEGS MATCH_REGEX
*
*  R8 points to A(pcre_comp) str_adr str_len stem_name stem_len option
*
   L     R1,4(R8)                      address in caller
   MVC   PCREWS_RE_PTR,0(R1)           compile block
   MVC   PCREWS_MATCH_DATA,4(R1)       match block
   MVC   REPEAT_SRCH,8(R1)             copy options
   MVC   IGNORE_CASE,9(R1)
   MVC   EXCLUDE_WHITE,10(R1)
*
   MVC   PCREWS_SUBJECT_PTR,8(R8)      string address
   MVC   PCREWS_SUBJECT_LEN,12(R8)     string length
   MVC   THE_STR_ADDR,8(R8)
   MVC   THE_STR_LENGTH,12(R8)
   MVC   THE_STR_END,16(R8)
   MVC   ROOT_NAME_ADR,20(R8)          stem name
   MVC   ROOT_NAME_LEN,24(R8)          stem length
   MVC   POS_NAME_ADR,20(R8)           stem name
   MVC   POS_NAME_LEN,24(R8)           stem length
   MVC   NAMED_NAME_ADR,20(R8)         Stem name
   MVC   NAMED_NAME_LEN,24(R8)         Stem length
   MVC   STRING_NAME_ADR,20(R8)        stem name
   MVC   STRING_NAME_LEN,24(R8)        stem length
*
   L     R1,28(,R8)                    point to option word
   MVC   OPTION_WORD,0(R1)             and copy it
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSCONVS)        set C function required
   L     R14,CONVERT_HANDLE            code page conversion handle
   L     R15,PCREWS_SUBJECT_PTR        string to convert
   L     R0,PCREWS_SUBJECT_LEN         length of string
   LA    R1,CONV_SUBJECT_PTR           field for converted address
   LA    R2,CONV_SUBJECT_LEN           field for converted length
   STM   R14,R2,MY_P_3                 save parameters
   LA    R14,ERR_BUFFER                point to buffer
   LHI   R15,256                       length is 256 bytes
   STM   R14,R15,MY_P_8                save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'5'
     MVC   PROGRAM_CALL,=CL8'ZCSCONVS'
   ENDIF
   SEGDO CALL_C_FUNCTION               go and compile it
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'STRING converted'
     XUNPK CONV_SUBJECT_PTR,4,MESS_TXT+20
     XUNPK CONV_SUBJECT_LEN,4,MESS_TXT+30
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   XC    ROOT_ZERO_COUNT,ROOT_ZERO_COUNT   trigger init process
   MVI   ROOT_TRIM_VAR,C'N'            do not trim spaces from var
   MVHHI ROOT_FUNCTION,1               init STEM.0
   LA    R1,ROOT_AREA                  point to STEM. area
   SEGDO CALL_PROCSTEM
*
   XC    POS_ZERO_COUNT,POS_ZERO_COUNT
   MVI   POS_TRIM_VAR,C'Y'             do trim spaces from var
   MVC   POS_MID_NAME,=CL20'POS'       specify required .MIDDLE.
   MVHHI POS_FUNCTION,1                init STEM.POS.0
   LA    R1,POS_AREA                   point to STEM.POS. area
   SEGDO CALL_PROCSTEM
*
   XC    NAMED_ZERO_COUNT,NAMED_ZERO_COUNT
   MVI   NAMED_TRIM_VAR,C'N'           do not trim spaces from var
   MVC   NAMED_MID_NAME,=CL20'NAME'
   MVHHI NAMED_FUNCTION,1              init STEM.NAME.0
   LA    R1,NAMED_AREA                 point to STEM.NAME. area
   SEGDO CALL_PROCSTEM
*
   XC    STRING_ZERO_COUNT,STRING_ZERO_COUNT
   MVI   STRING_TRIM_VAR,C'N'          do not trim spaces from var
   MVC   STRING_MID_NAME,=CL20'STRING'
   MVHHI STRING_FUNCTION,1             init STEM.STRING.0
   LA    R1,STRING_AREA                point to STEM.STRING. area
   SEGDO CALL_PROCSTEM
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
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
   ZAP   FIRST_SET_MATCH,=P'0'         initialise
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
       SEGDO TRY_FOR_MORE_MATCH
     ENDDO
   ENDIF
*
   IF (CP,ROOT_ZERO_COUNT,GT,=P'0')
     SETRC RC=1                    RC=1 - some matches
   ENDIF                  else     RC=0 - no matches
*
 SEGE MATCH_REGEX
*______________________________________________________________________
*
 SEGS SUBSTITUTE_REGEX
*
*  R8 points to A(pcre_comp) str_adr str_len var_name var_len option
*               subs_str_adr subs_str_len
*
   IF (CLI,INIT_SUBS_DONE,NE,C'Y')
     SEGDO INIT_SUBS_CONVERSION
   ENDIF
*
   L     R1,4(R8)                      address in caller
   MVC   PCREWS_RE_PTR,0(R1)           compile block
   MVC   PCREWS_MATCH_DATA,4(R1)       match block
   MVC   REPEAT_SRCH,8(R1)             copy options
   MVC   IGNORE_CASE,9(R1)
   MVC   EXCLUDE_WHITE,10(R1)
*
   MVC   PCREWS_SUBJECT_PTR,8(R8)      string address
   MVC   PCREWS_SUBJECT_LEN,12(R8)     string length
   MVC   THE_STR_ADDR,8(R8)
   MVC   THE_STR_LENGTH,12(R8)
   MVC   THE_STR_END,16(R8)
   MVC   VAR_NAME_ADDR,20(R8)          variable name
   MVC   VAR_NAME_LEN,24(R8)           variable name length
   MVC   SUBS_STR_ADDR,28(R8)
   MVC   SUBS_STR_LEN,32(R8)
*
   L     R1,40(,R8)                    point to option word
   MVC   OPTION_WORD,0(R1)             and copy it
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSCONVS)        set C function required
   L     R14,CONVERT_HANDLE            code page conversion handle
   L     R15,PCREWS_SUBJECT_PTR        string to convert
   L     R0,PCREWS_SUBJECT_LEN         length of string
   LA    R1,CONV_SUBJECT_PTR           field for converted address
   LA    R2,CONV_SUBJECT_LEN           field for converted length
   STM   R14,R2,MY_P_3                 save parameters
   LA    R14,ERR_BUFFER                point to buffer
   LHI   R15,256                       length is 256 bytes
   STM   R14,R15,MY_P_8                save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'5'
     MVC   PROGRAM_CALL,=CL8'ZCSCONVS'
   ENDIF
   SEGDO CALL_C_FUNCTION               go and compile it
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'STRING converted'
     XUNPK CONV_SUBJECT_PTR,4,MESS_TXT+20
     XUNPK CONV_SUBJECT_LEN,4,MESS_TXT+30
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSCONVR)        set C function required
   L     R14,CONVERT_HANDLE            code page conversion handle
   L     R15,SUBS_STR_ADDR             string to convert
   L     R0,SUBS_STR_LEN               length of string
   LA    R1,CONV_SUBST_PTR             field for converted address
   LA    R2,CONV_SUBST_LEN             field for converted length
   STM   R14,R2,MY_P_3                 save parameters
   LA    R14,ERR_BUFFER                point to buffer
   LHI   R15,256                       length is 256 bytes
   STM   R14,R15,MY_P_8                save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'5'
     MVC   PROGRAM_CALL,=CL8'ZCSCONVR'
   ENDIF
   SEGDO CALL_C_FUNCTION               go and compile it
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'SUBST converted'
     XUNPK CONV_SUBJECT_PTR,4,MESS_TXT+20
     XUNPK CONV_SUBJECT_LEN,4,MESS_TXT+30
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
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
   SEGDO DO_THE_SUBSTITUTE
*
 SEGE SUBSTITUTE_REGEX
*______________________________________________________________________
*
 SEGS CREATE_MATCH_BLOCK
*
*  create a block to hold results of the REGEX
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(MATCDCFP)        set C function
   L     R14,PCREWS_RE_PTR
   L     R15,PCREWS_NULL_PTR
   STM   R14,R15,MY_P_3                save parameters
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'2'
     MVC   PROGRAM_CALL,=CL8'MATCDCFP'
   ENDIF
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_MATCH_DATA         save returned address
   L     R1,8(R8)
   ST    R15,4(R1)                     and save in caller
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(14),=C'match block is'
     XUNPK PCREWS_MATCH_DATA,4,MESS_TXT+20
     SEGDO WRITE_PCREDUMP
   ENDIF
*
 SEGE CREATE_MATCH_BLOCK
*______________________________________________________________________
*
 SEGS DO_FIRST_MATCH
*
*  see if there is a match for the REGEX in the supplied string
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(MATCH2)          set C function
   L     R14,PCREWS_RE_PTR
   L     R15,CONV_SUBJECT_PTR
   L     R0,CONV_SUBJECT_LEN
   LA    R1,0
   LA    R2,0
   O     R2,OPTION_WORD                add specified match options
   L     R3,PCREWS_MATCH_DATA
   L     R4,PCREWS_NULL_PTR
   STM   R14,R4,MY_P_3                 save parameters
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'7'
     MVC   PROGRAM_CALL,=CL8'MATCH2'
   ENDIF
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
       STRING ('Matching error ',C,PCREWS_RC,I)
       MVI   ERR_NO,99
       SEGDO NOT_VALID
     ENDIF
     SEGQUIT                           and quit this segment
   ENDIF
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(GETOVEPT)        set C function
   L     R1,PCREWS_MATCH_DATA
   ST    R1,MY_P_3                     save parameter
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'1'
     MVC   PROGRAM_CALL,=CL8'GETOVEPT'
   ENDIF
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_OVECTOR
*
*  See if output vector wasn't big enough. This should not happen,
*  because we used pcre2_match_data_create_from_pattern() above.
*
   IF (CLC,PCREWS_RC,EQ,=F'0')
     XC    ERR_BUFFER,ERR_BUFFER
     MVC   ERR_BUFFER(37),=C'RXPCRE2A-011 - Ovector not big enough'
     SEGDO CALL_IRXSAY
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(22),=C'Ovector not big enough'
       SEGDO WRITE_PCREDUMP
     ENDIF
   ENDIF
*
   SEGDO OUTPUT_MATCH_RESULTS
*
   SEGDO TRY_NAMED_SUBSTRING
*
 SEGE DO_FIRST_MATCH
*______________________________________________________________________
*
 SEGS TRY_FOR_MORE_MATCH
*
*  g option specified so try for more matches
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(MATCH2)          set C function
   L     R14,PCREWS_RE_PTR
   L     R15,CONV_SUBJECT_PTR
   L     R0,CONV_SUBJECT_LEN
   L     R1,HIGHEST_MATCH
   LA    R2,0
   O     R2,OPTION_WORD                add specified match options
   L     R6,PCREWS_OVECTOR             point to output vector
   IF (CLC,0(4,R6),EQ,4(R6))           empty string ?
     L     R2,PCRE2_ANCHORED           set options in R2
     O     R2,PCRE2_NOT_EMPTY
   ENDIF
   L     R3,PCREWS_MATCH_DATA
   L     R4,PCREWS_NULL_PTR
   STM   R14,R4,MY_P_3                 save parameters
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'7'
     MVC   PROGRAM_CALL,=CL8'MATCH2'
   ENDIF
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
       STRING ('Matching error ',C,PCREWS_RC,I)
       MVI   ERR_NO,99
       SEGDO NOT_VALID
     ENDIF
     SEGQUIT                           and quit this segment
   ENDIF
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(GETOVEPT)        set C function
   L     R1,PCREWS_MATCH_DATA
   ST    R1,MY_P_3                     save parameter
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'1'
     MVC   PROGRAM_CALL,=CL8'GETOVEPT'
   ENDIF
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_OVECTOR
*
*  See if output vector wasn't big enough. This should not happen,
*  because we used pcre2_match_data_create_from_pattern() above.
*
   IF (CLC,PCREWS_RC,EQ,=F'0')
     XC    ERR_BUFFER,ERR_BUFFER
     MVC   ERR_BUFFER(37),=C'RXPCRE2A-012 - Ovector not big enough'
     SEGDO CALL_IRXSAY
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   MESS_TXT(22),=C'Ovector not big enough'
       SEGDO WRITE_PCREDUMP
     ENDIF
   ENDIF
*
   SEGDO OUTPUT_MATCH_RESULTS
*
   SEGDO TRY_NAMED_SUBSTRING
*
 SEGE TRY_FOR_MORE_MATCH
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
*    IF (CFI,R5,GT,MAX_STEM_LEN)       string to long
*      MVI   ERR_NO,31
*      SEGDO NOT_VALID
*    ENDIF
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
*    now R4 points to the text
*    and R5 has the length
     STM   R4,R5,SAVE_R4_R5
     LA    R14,STEM_TEXT               area for stem value
     LR    R15,R5                      copy length
     IF (CFI,R15,GT,MAX_STEM_LEN)      string to long
       L     R15,MAX_STEM_LEN          truncate in STEM_TEXT but we
     ENDIF                             output the full string
     MVCL  R14,R4                      copy fragment to STEM_TEXT
     LM    R4,R5,SAVE_R4_R5
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
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(PATTERN2)
   L     R14,PCREWS_RE_PTR
   LA    R15,PCRE2_INFO_NAMECOUNT
   LA    R0,PCREWS_NAMECOUNT
   STM   R14,R0,MY_P_3
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'3'
     MVC   PROGRAM_CALL,=CL8'PATTERN2'
   ENDIF
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
     XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
     MVC   FUNC_NAME,=V(PATTERN2)
     L     R14,PCREWS_RE_PTR
     LA    R15,PCRE2_INFO_NAMETABLE
     LA    R0,PCREWS_NAME_TABLE
     STM   R14,R0,MY_P_3
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   NUMBER_OF_PARMS,=H'3'
       MVC   PROGRAM_CALL,=CL8'PATTERN2'
     ENDIF
     SEGDO CALL_C_FUNCTION
     MVC   FUNC_NAME,=V(PATTERN2)
     L     R14,PCREWS_RE_PTR
     LA    R15,PCRE2_INFO_NAM_ENTSZ
     LA    R0,PCREWS_NAME_ENTSZ
     STM   R14,R0,MY_P_3
     IF (CLI,DEBUG_MODE,EQ,C'Y')
       MVC   NUMBER_OF_PARMS,=H'3'
       MVC   PROGRAM_CALL,=CL8'PATTERN2'
     ENDIF
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
 SEGS DO_THE_SUBSTITUTE
*
*  see if there is a match for the REGEX in the supplied string
*
   LA    R15,0
   IF (CLI,REPEAT_SRCH,EQ,C'Y')
     LA    R15,PCRE2_SUBSTITUTE_GLOBAL
   ENDIF
   O     R15,OPTION_WORD
   ST    R15,PCREWS_OPTION_BITS
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(SUBSTIT2)        set C function
   L     R14,PCREWS_RE_PTR
   L     R15,CONV_SUBJECT_PTR
   L     R0,CONV_SUBJECT_LEN
   LA    R1,0
   L     R2,PCREWS_OPTION_BITS
   L     R3,PCREWS_MATCH_DATA
   L     R4,PCREWS_NULL_PTR
   STM   R14,R4,MY_P_3                 save parameters
   L     R14,CONV_SUBST_PTR
   L     R15,CONV_SUBST_LEN
   L     R0,SUBST_OUT_PTR
*  LA    R1,256
*  ST    R1,SUBST_OUT_LEN
   MVC   SUBST_OUT_LEN,SUBST_OUT_MAX_LEN
   LA    R1,SUBST_OUT_LEN
   STM   R14,R1,MY_P_10                save parameters
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'11'
     MVC   PROGRAM_CALL,=CL8'SUBSTIT2'
   ENDIF
   MVI   DUMP_CALL,C'Y'
   SEGDO CALL_C_FUNCTION
   ST    R15,PCREWS_RC                 save returned value
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     STRING ('Substitute return was ',C,PCREWS_RC,X)
     XUNPK SUBST_OUT_PTR,4,MESS_TXT+40
     XUNPK SUBST_OUT_LEN,4,MESS_TXT+50
     SEGDO WRITE_PCREDUMP
   ENDIF
   L     R15,PCREWS_RC                 reload RC (write changes R15)
   IF (CHI,R15,GE,1)
     SETRC RC=1                    RC=1 - some matches
   ENDIF                  else     RC=0 - no matches
   IF (LTR,R15,R15,M)                  < 0
     IF (CHI,R15,EQ,-1)
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         MVC   MESS_TXT(8),=C'No subst'
         SEGDO WRITE_PCREDUMP
       ENDIF
     ELSE
       SEGDO SUBSTITUTE_ERROR
*      control does not return to here
     ENDIF
     SEGQUIT                           and quit this segment
   ENDIF
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSCONVB)        set C function required
   L     R14,CONVERT_HANDLE            code page conversion handle
   L     R15,SUBST_OUT_PTR             string to convert
   L     R0,SUBST_OUT_LEN              length of string
   LA    R1,CONV_BACK_PTR              field for converted address
   LA    R2,CONV_BACK_LEN              field for converted length
   STM   R14,R2,MY_P_3                 save parameters
   LA    R14,ERR_BUFFER                point to buffer
   LHI   R15,256                       length is 256 bytes
   STM   R14,R15,MY_P_8                save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'5'
     MVC   PROGRAM_CALL,=CL8'ZCSCONVB'
   ENDIF
   SEGDO CALL_C_FUNCTION               go and compile it
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(20),=CL20'SUBST converted'
     XUNPK CONV_SUBJECT_PTR,4,MESS_TXT+20
     XUNPK CONV_SUBJECT_LEN,4,MESS_TXT+30
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   SEGDO OUTPUT_A_VARIABLE
*
 SEGE DO_THE_SUBSTITUTE
*______________________________________________________________________
*
 SEGS COMPILE_ERROR
*
*  error occured compiling the REGEX - output message
*
   XC    ERR_BUFFER,ERR_BUFFER
   STRING     ('RXPCRE2A-013 - PCRE2 compilation failed at offset ',C, /
               PCREWS_ERROROFFSET,I),OUT=ERR_BUFFER
   SEGDO CALL_IRXSAY
   XC    ERR_BUFFER,ERR_BUFFER
   STRING     ('               error number was ',C,                   /
               PCREWS_ERRORNUMBER,I),OUT=ERR_BUFFER
   SEGDO CALL_IRXSAY
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(GETERROR)        set C function
   L     R14,PCREWS_ERRORNUMBER        get error number
   LA    R15,ERR_BUFFER                point to buffer
   LHI   R0,256                        length is 256 bytes
   STM   R14,R0,MY_P_3                 save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'3'
     MVC   PROGRAM_CALL,=CL8'GETERROR'
   ENDIF
   SEGDO CALL_C_FUNCTION
   SEGDO CALL_IRXSAY
   IF (CLI,C_ENV_SETUP,EQ,C'Y')
     SEGDO END_C_ENVIRONMENT
   ENDIF
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(23),=CL23'RXPCRE2A - ending RC=08'
     SEGDO WRITE_PCREDUMP
     CLOSE ((R9)),MODE=31              close output file
   ENDIF
   PRGQUIT RC=8                        quit the program
*
 SEGE COMPILE_ERROR
*______________________________________________________________________
*
 SEGS ZCSCONVP_ERROR
*
   SEGDO CALL_IRXSAY
   IF (CLI,C_ENV_SETUP,EQ,C'Y')
     SEGDO END_C_ENVIRONMENT
   ENDIF
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(23),=CL23'RXPCRE2A - ending RC=08'
     SEGDO WRITE_PCREDUMP
     CLOSE ((R9)),MODE=31              close output file
   ENDIF
   PRGQUIT RC=8                        quit the program
*
 SEGE ZCSCONVP_ERROR
*______________________________________________________________________
*
 SEGS SUBSTITUTE_ERROR
*
*  error occured in substitute REGEX - output message
*
   XC    ERR_BUFFER,ERR_BUFFER
   STRING     ('RXPCRE2A-016 - PCRE2 substitute failed ',C),           /
               OUT=ERR_BUFFER
   SEGDO CALL_IRXSAY
   XC    ERR_BUFFER,ERR_BUFFER
   STRING     ('               return code was  ',C,PCREWS_RC,I),      /
               OUT=ERR_BUFFER
   SEGDO CALL_IRXSAY
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(GETERROR)        set C function
   L     R14,PCREWS_RC                 get error number
   LA    R15,ERR_BUFFER                point to buffer
   LHI   R0,256                        length is 256 bytes
   STM   R14,R0,MY_P_3                 save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'3'
     MVC   PROGRAM_CALL,=CL8'GETERROR'
   ENDIF
   SEGDO CALL_C_FUNCTION
*  MVC   MESS_TXT,ERR_BUFFER
   SEGDO CALL_IRXSAY
*  IF (CLC,ERR_BUFFER+80(80),GT,SPACES)
*    MVC   MESS_TXT,ERR_BUFFER+80
*    SEGDO CALL_IRXSAY
*    IF (CLC,ERR_BUFFER+160(30),GT,SPACES)
*      MVC   MESS_TXT(30),ERR_BUFFER+160
*      SEGDO CALL_IRXSAY
*    ENDIF
*  ENDIF
   IF (CLI,C_ENV_SETUP,EQ,C'Y')
     SEGDO END_C_ENVIRONMENT
   ENDIF
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(23),=CL23'RXPCRE2A - ending RC=08'
     SEGDO WRITE_PCREDUMP
     CLOSE ((R9)),MODE=31              close output file
   ENDIF
   PRGQUIT RC=8                        quit the program
*
 SEGE SUBSTITUTE_ERROR
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
   ST    R4,STRING_TEXT_ADR            store address
   ST    R5,STRING_TEXT_LEN            store length
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
     STRING (CALL_STEM_MESS,C,R_C,I)
     MVI   ERR_NO,99
     SEGDO NOT_VALID
   ENDIF
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
*
 SEGE CALL_PROCSTEM
*______________________________________________________________________
*
 SEGS SET_C_ENVIRONMENT
*
*  set up C environment with librarian functionality
*
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(30),=CL30'RXPCRE2A - get C environment'
     SEGDO WRITE_PCREDUMP              write MESS_TXT to PCREDUMP
   ENDIF
*
   L     R1,4(,R8)                     point to code page (if any)
   MVC   CODEPAGE_PTR,0(R1)            copy details
   MVC   CODEPAGE_LEN,4(R1)
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
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
     MVC   MESS_TXT(30),=CL30'RXPCRE2A - C environment got'
     SEGDO WRITE_PCREDUMP
   ENDIF
   MVI   C_ENV_SETUP,C'Y'
   XC    ALL_THE_COMPILES(L_ALL_THE_COMPILES),ALL_THE_COMPILES
   XC    TOT_COMPILES,TOT_COMPILES
*
   SEGDO INIT_CONVERSION_ROUTINE
*
   MVI   INIT_SUBS_DONE,C'N'
*
 SEGE SET_C_ENVIRONMENT
*______________________________________________________________________
*
 SEGS INIT_CONVERSION_ROUTINE
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSINIT)
   LA    R14,CONVERT_HANDLE            return address of code pages
   L     R15,CODEPAGE_PTR              may be 0 if not specified
*                                      0 means current code page
   L     R0,CODEPAGE_LEN               length of code page name
   LHI   R1,0                          max size of pattern - default
   STM   R14,R1,MY_P_3                 save in parameter list
   LHI   R14,0                         max size of string  - default
   LA    R15,ERR_BUFFER                point to error message buffer
   STM   R14,R15,MY_P_7                save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'5'
     MVC   PROGRAM_CALL,=CL8'ZCSINIT'
   ENDIF
   SEGDO CALL_C_FUNCTION
   ST    R15,R_C
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(23),=CL23'RXPCRE2A - init convert'
     XUNPK (R15),4,MESS_TXT+25
     XUNPK CONVERT_HANDLE,4,MESS_TXT+35
     IF (CLC,R_C,NE,=F'0')
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
   IF (CLC,R_C,NE,=F'0')
     IF (CLC,ERR_BUFFER,GT,SPACES)
       SEGDO CALL_IRXSAY
     ENDIF
     SEGDO END_C_ENVIRONMENT
     IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
       MVC   MESS_TXT(23),=CL23'RXPCRE2A - ending RC=08'
       SEGDO WRITE_PCREDUMP
       CLOSE ((R9)),MODE=31            close output file
     ENDIF
     XC    ERR_BUFFER,ERR_BUFFER
     MVC   ERR_BUFFER(32),=C'RXPCRE2A-014 - Code page failure'
     SEGDO CALL_IRXSAY
     PRGQUIT RC=8                        get out of program RC=8
   ENDIF
   MVI   CONV_INIT_OK,C'Y'
*
 SEGE INIT_CONVERSION_ROUTINE
*______________________________________________________________________
*
 SEGS INIT_SUBS_CONVERSION
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   SUBST_OUT_MAX_LEN,=A(16*1024)
   MVC   FUNC_NAME,=V(ZCSINIS)
   L     R14,CONVERT_HANDLE            return address of code pages
   LHI   R15,0                         max replacement size - default
   LHI   R0,0                          max substitute size  - default
   LA    R1,SUBST_OUT_PTR              address of substitute output
   STM   R14,R1,MY_P_3                 save in parameter list
   LA    R14,ERR_BUFFER                point to buffer
   ST    R14,MY_P_7                    save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'4'
     MVC   PROGRAM_CALL,=CL8'ZCSINIS'
   ENDIF
   MVI   DUMP_CALL,C'Y'
   SEGDO CALL_C_FUNCTION
   ST    R15,R_C
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(24),=CL24'RXPCRE2A - inits convert'
     XUNPK (R15),4,MESS_TXT+26
     XUNPK CONVERT_HANDLE,4,MESS_TXT+36
     IF (CLC,R_C,NE,=F'0')
       MVC   MESS_TXT+46(8),=C'- FAILED'
     ENDIF
     SEGDO WRITE_PCREDUMP
     L     R3,CONVERT_HANDLE
     XUNPK 0(R3),30,MESS_TXT+10
     SEGDO WRITE_PCREDUMP
     XUNPK 30(R3),30,MESS_TXT+10
     SEGDO WRITE_PCREDUMP
   ENDIF
   IF (CLC,R_C,NE,=F'0')
     IF (CLC,ERR_BUFFER,GT,SPACES)
       SEGDO CALL_IRXSAY
     ENDIF
     SEGDO END_C_ENVIRONMENT
     IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
       MVC   MESS_TXT(23),=CL23'RXPCRE2A - ending RC=08'
       SEGDO WRITE_PCREDUMP
       CLOSE ((R9)),MODE=31            close output file
     ENDIF
     XC    ERR_BUFFER,ERR_BUFFER
     MVC   ERR_BUFFER(32),=C'RXPCRE2A-015 - init subs failure'
     SEGDO CALL_IRXSAY
     PRGQUIT RC=8                      get out of program RC=8
   ENDIF
*
   MVI   INIT_SUBS_DONE,C'Y'
*
 SEGE INIT_SUBS_CONVERSION
*______________________________________________________________________
*
 SEGS RELEASE_REGEX
*
*  release match block and regex compile
*
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   L     R1,4(R8)                      address in caller
   MVC   PCREWS_RE_PTR,0(R1)           compile block
   MVC   PCREWS_MATCH_DATA,4(R1)       match block
   MVC   FUNC_NAME,=V(MATCHDFR)        free any matches
   L     R1,PCREWS_MATCH_DATA
   ST    R1,MY_P_3
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'1'
     MVC   PROGRAM_CALL,=CL8'MATCHDFR'
   ENDIF
   SEGDO CALL_C_FUNCTION
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(15),=C'Called MATCHDFR'
     XUNPK PCREWS_MATCH_DATA,4,MESS_TXT+20
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   MVC   FUNC_NAME,=V(CODEFREE)        free compiled code
   L     R1,PCREWS_RE_PTR
   ST    R1,MY_P_3
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'1'
     MVC   PROGRAM_CALL,=CL8'CODEFREE'
   ENDIF
   SEGDO CALL_C_FUNCTION
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(15),=C'Called CODEFREE'
     XUNPK PCREWS_RE_PTR,4,MESS_TXT+20
     SEGDO WRITE_PCREDUMP
   ENDIF
*
   PUSH  USING
   USING MAP_HANDLES,R14
   LA    R14,ALL_THE_COMPILES
   LA    R15,MAX_NUM_COMPILES
   DO FROM=(R15)
   DOEXIT (CLC,PCREWS_RE_PTR,EQ,THIS_COMPILE)
     LA    R14,8(,R14)
   ENDDO
   IF (CHI,R15,NE,0)
     XC    THIS_COMPILE,THIS_COMPILE   clear compile handle
     XC    THIS_MATCH,THIS_MATCH       clear match block handle
     LH    R1,TOT_COMPILES             decrement count of compiles
     BCTR  R1,0
     STH   R1,TOT_COMPILES
   ELSE
     MVC   MESS_TXT(20),=CL20'RXPCRE2A miss release'
     SEGDO WRITE_LOG
   ENDIF
   POP   USING
*
 SEGE RELEASE_REGEX
*______________________________________________________________________
*
 SEGS END_C_ENVIRONMENT
*
   IF (CLC,=H'0',NE,TOT_COMPILES)
     SEGDO RELEASE_ALL_REGEX
   ENDIF
*
   IF (CLI,CONV_INIT_OK,EQ,C'Y')
     SEGDO RELEASE_CONVERSION_AREA
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
     MVC   MESS_TXT(33),=C'RXPCRE2A - C environment released'
     SEGDO WRITE_PCREDUMP
   ENDIF
   MVI   C_ENV_SETUP,C'N'
   MVI   CONV_INIT_OK,C'N'
   MVI   INIT_SUBS_DONE,C'N'
   XC    TOT_COMPILES,TOT_COMPILES
*
 SEGE END_C_ENVIRONMENT
*______________________________________________________________________
*
 SEGS RELEASE_ALL_REGEX
*
*  release all remaining matches and regex compiles
*
   STM   R4,R5,SAVE_R4_R5
   PUSH  USING
   USING MAP_HANDLES,R4
   LA    R4,ALL_THE_COMPILES
   LA    R5,MAX_NUM_COMPILES
   DO FROM=(R5)
     IF (CLC,=F'0',NE,THIS_MATCH)
       XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
       MVC   FUNC_NAME,=V(MATCHDFR)    free any matches
       L     R1,THIS_MATCH
       ST    R1,MY_P_3
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         MVC   NUMBER_OF_PARMS,=H'1'
         MVC   PROGRAM_CALL,=CL8'MATCHDFR'
       ENDIF
       SEGDO CALL_C_FUNCTION
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         MVC   MESS_TXT(15),=C'Called MATCHDFR'
         XUNPK THIS_MATCH,4,MESS_TXT+20
         SEGDO WRITE_PCREDUMP
       ENDIF
       XC    THIS_MATCH,THIS_MATCH     clear match block handle
     ENDIF
     IF (CLC,=F'0',NE,THIS_COMPILE)
       XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
       MVC   FUNC_NAME,=V(CODEFREE)    free compiled code
       L     R1,THIS_COMPILE
       ST    R1,MY_P_3
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         MVC   NUMBER_OF_PARMS,=H'1'
         MVC   PROGRAM_CALL,=CL8'CODEFREE'
       ENDIF
       SEGDO CALL_C_FUNCTION
       IF (CLI,DEBUG_MODE,EQ,C'Y')
         MVC   MESS_TXT(15),=C'Called CODEFREE'
         XUNPK THIS_COMPILE,4,MESS_TXT+20
         SEGDO WRITE_PCREDUMP
       ENDIF
       XC    THIS_COMPILE,THIS_COMPILE clear compile handle
     ENDIF
     LA    R4,8(,R4)
   ENDDO
   XC    TOT_COMPILES,TOT_COMPILES     clear count
   POP   USING
   LM    R4,R5,SAVE_R4_R5
*
 SEGE RELEASE_ALL_REGEX
*______________________________________________________________________
*
 SEGS RELEASE_CONVERSION_AREA
*
   XC    ERR_BUFFER,ERR_BUFFER         wipe to low values
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
   MVC   FUNC_NAME,=V(ZCSRLSE)
   L     R14,CONVERT_HANDLE            return address of code pages
   LA    R15,ERR_BUFFER                point to buffer
   LHI   R0,256                        length is 256 bytes
   STM   R14,R0,MY_P_3                 save in parameter list
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   NUMBER_OF_PARMS,=H'1'
     MVC   PROGRAM_CALL,=CL8'ZCSRLSE'
   ENDIF
   SEGDO CALL_C_FUNCTION
   IF (CLI,DEBUG_MODE,EQ,C'Y')
     MVC   MESS_TXT(30),=CL30'RXPCRE2A - rlse convert'
     XUNPK (R15),4,MESS_TXT+32
     XUNPK CONVERT_HANDLE,4,MESS_TXT+32
     SEGDO WRITE_PCREDUMP
   ENDIF
*
 SEGE RELEASE_CONVERSION_AREA
*______________________________________________________________________
*
 SEGS CALL_C_FUNCTION
*
*  parameters from 3 onward are set so just add handle and function
*
   IF (CLI,DUMP_CALL,EQ,C'Y')
     IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
       MVC   MESS_TXT(15),=C'RXPCRE2A - CALL'
       MVC   MESS_TXT+16(8),PROGRAM_CALL
       XUNPK FUNC_NAME,,MESS_TXT+30
       SEGDO WRITE_PCREDUMP            write MESS_TXT to PCREDUMP
       LH    R3,NUMBER_OF_PARMS
       LA    R4,MY_P_3
       IF (CFI,R3,GT,0)
         DO FROM=(R3)
           XUNPK 0(R4),4,MESS_TXT+11
           SEGDO WRITE_PCREDUMP        write MESS_TXT to PCREDUMP
           LA    R4,4(,R4)
         ENDDO
       ENDIF
     ENDIF
   ENDIF
   MVI   DUMP_CALL,C' '
   LA    R1,HANDLE                     address of C environment
   ST    R1,MY_P_1
   LA    R1,FUNC_NAME                  function name
   ST    R1,MY_P_2
   LA    R1,MY_PARM                    point to parameters
   L     R15,=V(EDCXHOTU)              call required C function
   BASR  R14,R15
   XC    MY_PARM(LEN_MY_PARM),MY_PARM  clear parm list
*
 SEGE CALL_C_FUNCTION
*______________________________________________________________________
*
 SEGS NOT_VALID
*
*  an error occured so set error message as output from REXX function
*
   XC    ERR_BUFFER,ERR_BUFFER
   IF (CLI,ERR_NO,EQ,55)
     MVC   ERR_BUFFER(L'INV_COMMAND),INV_COMMAND
   ELSEIF (CLI,ERR_NO,EQ,56)
     MVC   ERR_BUFFER(L'MAX_COM_PIL),MAX_COM_PIL
   ELSEIF (CLI,ERR_NO,EQ,88)
     MVC   ERR_BUFFER(L'DEBUG_ERROR),DEBUG_ERROR
   ELSEIF (CLI,ERR_NO,EQ,99)
*    message is already set
   ELSE    this should never happen
     MVC   ERR_BUFFER(L'UN_KNOWN),UN_KNOWN
     XUNPK ERR_NO,1,ERR_BUFFER+L'UN_KNOWN
   ENDIF
   SEGDO CALL_IRXSAY
   IF (CLI,C_ENV_SETUP,EQ,C'Y')
     SEGDO END_C_ENVIRONMENT
   ENDIF
   IF (CLI,DEBUG_MODE,EQ,C'Y'),AND,(CLI,PCREDUMP_OPEN,EQ,C'Y')
     MVC   MESS_TXT(23),=CL23'RXPCRE2A - ending RC=08'
     SEGDO WRITE_PCREDUMP
     CLOSE ((R9)),MODE=31              close output file
   ENDIF
   PRGQUIT RC=8                        get out of program RC=8
*
 SEGE NOT_VALID
*______________________________________________________________________
*
 SEGS WRITE_PCREDUMP
*
   PUT   (R9),MESS_TXT                 output text to PCREDUMP
   MVI   MESS_TXT,C' '                 clear message line
   MVC   MESS_TXT+1(L'MESS_TXT-1),MESS_TXT
*
 SEGE WRITE_PCREDUMP
*______________________________________________________________________
*
 SEGS OUTPUT_A_VARIABLE
*
   LA    R1,=CL8'IRXEXCOM'
   ST    R1,IX_PARM
   LA    R6,PARM_AREA
   USING SHVBLOCK,R6
   MVC   SHVNEXT,=F'0'
   MVC   SHVUSER,=F'0'
   MVI   SHVCODE,SHVSTORE              store a variable
   MVC   SHVBUFL,=F'0'
   MVC   SHVNAMA,VAR_NAME_ADDR         variable name
   MVC   SHVNAML,VAR_NAME_LEN          variable length
*
   MVC   SHVVALA,CONV_BACK_PTR         value address
   MVC   SHVVALL,CONV_BACK_LEN         value length
   LA    R1,PARM_AREA
   ST    R1,IX_PARM+12
   OI    IX_PARM+12,X'80'

   LR    R0,R10                        copy rexx environment addr
   LA    R1,IX_PARM
   USING IRXEXTE,15
   L     15,ENVBLOCK_IRXEXTE
   L     15,IRXEXCOM                   get addr of variable access
   DROP  15
   BASR  14,15                         go set the variable
*
   IF (LTR,R15,R15,NZ)
     MVC   EVALBLOCK_EVDATA(20),=CL20'Error in SET_VAR RC='
     XUNPK (R15),,EVALBLOCK_EVDATA+20
     LA    R1,30                       length of error message
     ST    R1,EVALBLOCK_EVLEN
     PRGQUIT
   ENDIF
*
 SEGE OUTPUT_A_VARIABLE
*______________________________________________________________________
*
 SEGS CALL_IRXSAY
*
   LA    R14,ERR_BUFFER
   XR    R15,R15
   DO FROM=(R1,256)
   DOEXIT (CLI,0(R14),EQ,0)
     LA    R14,1(,R14)
     LA    R15,1(,R15)
   ENDDO
   IF (CFI,R15,NE,0)
     ST    R15,ERR_BUFFER_LEN
     IF (CLI,NO_SAY,EQ,C'Y')
       SEGDO CALL_IRXSAY_NOSAY
     ELSE
       SEGDO CALL_IRXSAY_REAL
     ENDIF
   ENDIF
*
 SEGE CALL_IRXSAY
*______________________________________________________________________
*
 SEGS CALL_IRXSAY_NOSAY
*
   L     R2,STEM_NOSAY_ADDR            point to stem area in caller
   USING NOSAY_AREA,R2                 map to DSECT
   LA    R1,ERR_BUFFER                 point to message text
   ST    R1,NOSAY_TEXT_ADR             store address
   MVC   NOSAY_TEXT_LEN,ERR_BUFFER_LEN set length
   MVHHI NOSAY_FUNCTION,2              set STEM value
   L     R1,STEM_NOSAY_ADDR            point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in NOSAY 1'
   SEGDO CALL_PROCSTEM
   MVHHI NOSAY_FUNCTION,3              set STEM.0 to total lines
   L     R1,STEM_NOSAY_ADDR            point to PRGSTEM expansion
   MVC   CALL_STEM_MESS,=CL26'Error in NOSAY 2'
   SEGDO CALL_PROCSTEM
   DROP  R2
*
 SEGE CALL_IRXSAY_NOSAY
*______________________________________________________________________
*
 SEGS CALL_IRXSAY_REAL
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
     SEGDO WRITE_LOG
     MVC   MESS_TXT(L'E_ON_SAY),E_ON_SAY
     XUNPK R_C,4,MESS_TXT+19
     SEGDO WRITE_LOG
   ENDIF
   MVI   MESS_TXT,C' '
   MVC   MESS_TXT+1(L'MESS_TXT-1),MESS_TXT
*
 SEGE CALL_IRXSAY_REAL
*______________________________________________________________________
*
               PRGSTAT
*
HANDLE         DS    A           address of the C environment
CONVERT_HANDLE DS    A           address of the conversion data
CODEPAGE_PTR   DS    A           points to codepage name
CODEPAGE_LEN   DS    A           length of codepage name
SUBST_OUT_MAX_LEN    DS    F     pointer to substitute output
VAR_NAME_ADDR  DS    A           address of rexx variable name
VAR_NAME_LEN   DS    F           length of rexx variable name
ALL_THE_COMPILES     DS    (MAX_NUM_COMPILES)D   all current compiles
L_ALL_THE_COMPILES   EQU   *-ALL_THE_COMPILES
TOT_COMPILES   DS    H
PCREDUMP_OPEN  DS    CL1         flag dataset open
INIT_SUBS_DONE DS    CL1
C_ENV_SETUP    DS    CL1
DEBUG_MODE     DS    CL1
CONV_INIT_OK   DS    CL1
E_ON_SAY       DC    C'RXPCRE2A-001 - Return code on SAY='
DEBUG_ERROR    DC    C'RXPCRE2A-002 - debug specified but DD name PCRED/
               UMP not allocated'
INV_COMMAND    DC    C'RXPCRE2A-003 - Invalid command'
MAX_COM_PIL    DC    C'RXPCRE2A-004 - Max compiles exceeded'
UN_KNOWN       DC    C'RXPCRE2A-nnn - Unknown error, RC='
*
STKSIZE        DC    F'4096'     stack size
STKLOC         DC    F'1'        stack location (0 < 16MB, 1 >= 16MB)
*
COPY_NAME_TEXT MVC   STEM_TEXT(1),2(R4)
COPY_CODEPAGE  MVC   MESS_TXT+55(0),0(R1)
*
SPACES         DC    CL80' '
*
*        Areas for populating rexx stem variables
*
                     PRGSTEM PREF=ROOT       for  STEM.0
                     PRGSTEM PREF=POS             STEM.POS.?
                     PRGSTEM PREF=NAMED           STEM.NAME.?
                     PRGSTEM PREF=STRING          STEM.STRING.?
*
                     DS    0F          ensure fullword aligned
PCRE2_CASELESS       DC    X'00000008'
PCRE2_DOTALL         DC    X'00000020'
PCRE2_EXTENDED       DC    X'00000080'
PCRE2_MULTILINE      DC    X'00000400'
PCRE2_NOT_EMPTY      DC    X'00000008'
PCRE2_ANCHORED       DC    X'80000000'
*
               LTORG
*
               PRGESTAT
               PRGEND
               END
