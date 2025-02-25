 TITLE 'MEMLIST - REXX function to get member list into stem'
*
MEMLIST PRGDEF FSEG=MAIN_BIT,REXX=Y,RENT=YES
*______________________________________________________________________
*
* Written by John Gateley - use at your own risk
*
* This program acts as a REXX function to populate a stem with
* the member names matching an input pattern.
*
*______________________________________________________________________
*
* EXAMPLE USAGE
*
* /*REXX*/
*   "ISPEXEC LIBDEF ISPLLIB DATASET ID ('MY.LOAD.LIBRARY')"
*
*   R_C = MEMLIST('SDJRG.JCL,'A*******',STEMNAME.')
*   IF  R_C      /= 0 THEN SAY R_C
*   ELSE
*      DO LO_OP_R = 1 TO STEMNANE.0
*         SAY  ':' || STEMNAME.LO_OP_R || ':'
*      END
*
*   DROP STEMNAME.
*
*   "ISPEXEC LIBDEF ISPLLIB "
*   EXIT
*______________________________________________________________________
*
*  REENTRANT STORAGE
*
M_NAME               DS    CL8
M_NAME2              DS    CL8
TEST_MEMBER          DS    CL8
LEN_STATS            DS    H
ERR_NO               DS    C
*
                     PRGSTEM PREF=THE_STEM
*
DODYNPRC_PARM        DS    0D          parameter area for DODYNPRC
DODYNPRC_ACTION      DS    CL1
DODYNPRC_DDNAME      DS    CL8
DODYNPRC_DATASET     DS    CL44
DODYNPRC_MEMBER      DS    CL8
*
        PRGEDEF
*______________________________________________________________________
*
 SEGS MAIN_BIT
*
   SEGDO GET_ARGUMENTS
*
   TR    M_NAME,TRTAB                  translate member to skeletal
   MVC   M_NAME2,M_NAME                forms 1 and 2 in M_NAME
   TR    M_NAME2,TRTAB1                and M_NAME2
*
   MVI   THE_STEM_TRIM_VAR,C'Y'        say trim all spaces from var
   MVHHI THE_STEM_FUNCTION,1           init STEM.0
   SEGDO CALL_PROCSTEM
*
   SEGDO DYNALLOC_A
*
   SEGDO GET_MEMBER_LIST
*
   SEGDO DYNALLOC_D
*
   MVHHI THE_STEM_FUNCTION,3           set STEM.0 to total lines
   SEGDO CALL_PROCSTEM
*
   MVI   EVALBLOCK_EVDATA,C' '
   MVC   EVALBLOCK_EVDATA+1(80),EVALBLOCK_EVDATA
   MVI   EVALBLOCK_EVDATA,C'0'         set result value
   LA    R1,1
   ST    R1,EVALBLOCK_EVLEN            set result length
*
 SEGE MAIN_BIT
*______________________________________________________________________
*
 SEGS DYNALLOC_A
*
   MVI   DODYNPRC_ACTION,C'A'        dynamically allocate
   MVC   DODYNPRC_DDNAME,=CL8'ABCDEFGH'
   XC    DODYNPRC_MEMBER,DODYNPRC_MEMBER
   LA    R1,DODYNPRC_PARM
   ST    R1,MY_P_1
   LA    R1,MY_PARM
   L     R15,=V(DODYNPRC)
   BASR  R14,R15
   IF (CFI,R15,NE,0)
     ST    R15,R_C
     PRGQUIT
   ENDIF
*
 SEGE DYNALLOC_A
*______________________________________________________________________
*
 SEGS DYNALLOC_D
*
   MVI   DODYNPRC_ACTION,C'D'        dynamically de-allocate
   LA    R1,DODYNPRC_PARM
   ST    R1,MY_P_1
   LA    R1,MY_PARM
   L     R15,=V(DODYNPRC)
   BASR  R14,R15
   IF (LTR,R15,R15,NZ)
     MVI   ERR_NO,11
     SEGDO NOT_VALID
   ENDIF
*
 SEGE DYNALLOC_D
*______________________________________________________________________
*
 SEGS CALL_PROCSTEM
*
   LA    R1,THE_STEM_AREA              point to PRGSTEM area
   ST    R1,MY_P_1                     save as first parm
   LA    R1,MY_PARM                    point to parameter list
   L     R15,=V(PROCSTEM)              get program address
   BASR  R14,R15                       go and set stem value
   IF (LTR,R15,R15,NZ)                 error ?
     ST    R15,R_C                     set return code
     PRGQUIT                           QUIT
   ENDIF
*
 SEGE CALL_PROCSTEM
*______________________________________________________________________
*
 SEGS GET_MEMBER_LIST
*
   OPEN  (LISTMEM,INPUT)
   IF (LTR,R15,R15,NZ)
     MVI   ERR_NO,10
     SEGDO NOT_VALID
   ENDIF
   DO INF
     GET   LISTMEM
     LH    R3,0(R1)                    number of active bytes in block
     LA    R3,0(R3,R1)                 point to end of block
     LA    R9,2(0,R1)                  jump past length
     DO INF
     DOEXIT (CR,R9,GE,R3)              at end of the block
     DOEXIT (CLC,0(8,R9),EQ,HIGHVALS)  end of directory
     DOEXIT (CLC,0(8,R9),EQ,LOWVALS)   not wanted
       L     R1,8(R9)
       SLL   R1,27                     last byte contains length of
       SRL   R1,26                     ..user data in halfwords so dump
       STH   R1,LEN_STATS              ..the rest and make it bytes
       MVC   TEST_MEMBER,0(R9)            copy member name
       OC    TEST_MEMBER,M_NAME2          transform to test
       IF (CLC,TEST_MEMBER(8),EQ,M_NAME)  do we want this ?
         LA    R1,0(R9)
         ST    R1,THE_STEM_TEXT_ADR
         LA    R1,8
         ST    R1,THE_STEM_TEXT_LEN
         MVHHI THE_STEM_FUNCTION,2     set STEM value
         SEGDO CALL_PROCSTEM
       ENDIF
       LA    R9,12(,R9)                add standard length
       AH    R9,LEN_STATS              plus length of user stats
     ENDDO
   DOEXIT (CLC,0(8,R9),EQ,HIGHVALS)    thats all
   ENDDO
*
ERR      DS    0H
DIREND   DS    0H
*
   CLOSE LISTMEM
*
 SEGE GET_MEMBER_LIST
*______________________________________________________________________
*
 SEGS GET_ARGUMENTS
*
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,1
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,2
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'44')
     MVI   ERR_NO,3
     SEGDO NOT_VALID
   ENDIF
   MVI   DODYNPRC_DATASET,C' '
   MVC   DODYNPRC_DATASET+1(L'DODYNPRC_DATASET),DODYNPRC_DATASET
   L     R1,ARGTABLE_ARGSTRING_PTR
   L     R2,ARGTABLE_ARGSTRING_LENGTH
   BCTR  R2,0
   EX    R2,COPY_DSNAME
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for second argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,4
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,5
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,NE,=F'8')
     MVI   ERR_NO,6
     SEGDO NOT_VALID
   ENDIF
   MVI   M_NAME,C' '
   MVC   M_NAME+1(L'M_NAME),M_NAME
   L     R1,ARGTABLE_ARGSTRING_PTR
   L     R2,ARGTABLE_ARGSTRING_LENGTH
   BCTR  R2,0
   EX    R2,COPY_MEMNAME
*
   LA    ARG_POINT,ARGTABLE_NEXT       check for third argument
   IF (CLC,ARGTABLE_ARGSTRING_PTR,EQ,=8X'FF')
     MVI   ERR_NO,7
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,EQ,=F'0')
     MVI   ERR_NO,8
     SEGDO NOT_VALID
   ELSEIF (CLC,ARGTABLE_ARGSTRING_LENGTH,GT,=F'20')
     MVI   ERR_NO,9
     SEGDO NOT_VALID
   ENDIF
   MVC   THE_STEM_NAME_ADR,ARGTABLE_ARGSTRING_PTR
   MVC   THE_STEM_NAME_LEN,ARGTABLE_ARGSTRING_LENGTH
   L     R1,ARGTABLE_ARGSTRING_PTR
   L     R2,ARGTABLE_ARGSTRING_LENGTH
   BCTR  R2,0
   LA    R1,0(R2,R1)
   IF (CLI,0(R1),EQ,C'.')          IF NAME ENDS IN '.'
     ST    R2,ARGTABLE_ARGSTRING_LENGTH    IGNORE IT
   ENDIF
*
 SEGE GET_ARGUMENTS
*______________________________________________________________________
*
 SEGS NOT_VALID
*
   MVC   EVALBLOCK_EVLEN,=F'30'
   IF (CLI,ERR_NO,EQ,1)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 1 not entered'
   ELSEIF (CLI,ERR_NO,EQ,2)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 1 was zero length'
   ELSEIF (CLI,ERR_NO,EQ,3)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 1 > 44 bytes'
   ELSEIF (CLI,ERR_NO,EQ,4)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 2 not entered'
   ELSEIF (CLI,ERR_NO,EQ,5)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 2 was zero length'
   ELSEIF (CLI,ERR_NO,EQ,6)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 2 not = 8 bytes'
   ELSEIF (CLI,ERR_NO,EQ,7)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 3 not entered'
   ELSEIF (CLI,ERR_NO,EQ,8)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 3 was zero length'
   ELSEIF (CLI,ERR_NO,EQ,9)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Argument 3 > 20 bytes'
   ELSEIF (CLI,ERR_NO,EQ,10)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Could not open dataset'
   ELSEIF (CLI,ERR_NO,EQ,11)
     MVC   EVALBLOCK_EVDATA(30),=CL30'Could not DYNALOC dataset'
   ELSE
     MVC   EVALBLOCK_EVDATA(30),=CL30'Unknown error, rc='
     XR    R1,R1
     IC    R1,ERR_NO                   take error number
     CVD   R1,DOUB_WORD
     OI    DOUB_WORD+7,X'0F'
     UNPK  EVALBLOCK_EVDATA+18(4),DOUB_WORD    put in output message
     XC    R_C,R_C
   ENDIF
*
   PRGQUIT
*
 SEGE NOT_VALID
*______________________________________________________________________
*
 SEGS WRITE_LOG
*
   WTOX
*
 SEGE WRITE_LOG
*______________________________________________________________________
*
         PRGSTAT
*
HIGHVALS DC   XL8'FFFFFFFFFFFFFFFF'
LOWVALS  DC   XL8'0000000000000000'
TRTAB    DC   256AL1(*-TRTAB)
         ORG  TRTAB+C'*'
         DC   X'FF'
         ORG
TRTAB1   DC   256X'00'
         ORG  TRTAB1+X'FF'
         DC   X'FF'
         ORG
*
LISTMEM  DCB   DDNAME=ABCDEFGH,MACRF=(GL),DSORG=PS,EODAD=DIREND,       /
               LRECL=256,BLKSIZE=256,RECFM=FB
*
COPY_DSNAME    MVC   DODYNPRC_DATASET(0),0(R1)
COPY_MEMNAME   MVC   M_NAME(0),0(R1)
*
         LTORG
*______________________________________________________________________
*
         PRGESTAT
         PRGEND
         END
