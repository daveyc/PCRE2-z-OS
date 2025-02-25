



#ifndef HAVE_CONFIG_H
#   define HAVE_CONFIG_H 1
#endif
#ifdef HAVE_CONFIG_H
#   include "config.h"
#endif

#ifdef NATIVE_ZOS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "PCRZCONH.h"


static unsigned char CHARSET_IBM1047[] = "IBM-1047";

/*
THE z/OS specific functionality
-------------------------------

Contributed by:   Ze'ev Atlas.

Copyright (c) 2012, Ze'ev Atlas.
All rights reserved.


THE "BSD" LICENCE
-----------------

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of the University of Cambridge nor the name of Google
      Inc. nor the names of their contributors may be used to endorse or
      promote products derived from this software without specific prior
      written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.                                          */

/* The functions herein do not use Reg Exp because Reg Exp is not yet
available.  They help to prepare COBOL fixed length character strings for Reg
Exp manipulation.  COBOL character strings are fundamentally different then C
(and thus also C++, Java, C# and virtually any other language) character
strings in that they are fixed length or length terminated (i.e. the length of
the string is known either in compile time or in run time via some variable.)
That length is retrievable by means of querying the prefix variable or using
the LENGTH OF special register.  There is no terminator character (null or
otherwise.)  COBOL notion of fixed length or length terminated character
strings relate directly to the notion of fixed length records - record length
is pre-defined and known in similar manner and there is no record terminator
character.
Most other languages use the null terminated notation (i.e. the end-of-string
is signaled by the null character.  Regular Expression as defined and implemented
by Perl, Posix and PCRE depend on the string being null terminated.
To add to the complication, C on z/OS adds record terminator to the end of the
record even on fixed length records, but on text records, at least from the
standard input, the last spaces are chopped.  I coined that type as space
terminated records and strings.
The functions herein provide the means to query a character string and convert
it to the desired format.
Currently, these functions compile only under NATIVE_ZOS as I suspect that nobody
else will want or need them, however, it is pretty simple to remove this
limitation                                                                     */

/* As of PCRE2 10.41, all actual error messages printouts in PCRZ functions were
eliminated in favor of providing those error messages as an optional string
returned                                                                       */

extern int pcrz_what_term_str (char * str, int maxlen);
extern int pcrz_is_space_term_str (char * str, int maxlen);
extern int pcrz_is_null_term_str (char * str, int maxlen);
extern int pcrz_space_to_null_term_str (char * str, int maxlen);
extern int pcrz_null_to_space_term_str (char * str, int maxlen);

extern int pcrz_codeset_init (ZCSSTRCT ** handle_ptr,
                      char * codeset_name,
                      size_t codeset_name_size,
                      size_t pattern_size,
                      size_t subject_size,
                      char * errstr);
extern int pcrz_codeset_init_substitute (ZCSSTRCT * handle_ptr,
                      size_t replacement_size,
                      size_t substitute_size,
                      char **substitute_ptr,
                      char * errstr);
extern int pcrz_codeset_convert_pattern (ZCSSTRCT *zcsstrct_ptr,
         char * input_pattern, size_t pattern_length,
         char ** output_pattern, size_t ** out_pattern_length,
         char * errstr);
extern int pcrz_codeset_convert_subject (ZCSSTRCT *zcsstrct_ptr,
         char * input_subject, size_t subject_length,
         char ** output_subject, size_t ** out_subject_length,
         char * errstr);
extern int pcrz_codeset_convert_replacement (ZCSSTRCT *zcsstrct_ptr,
         char * input_replacement, size_t replacement_length,
         char ** output_replacement, size_t ** out_replacement_length,
         char * errstr);
extern int pcrz_codeset_convert_substitute_back (ZCSSTRCT *zcsstrct_ptr,
         char * input_substitute, size_t substitute_length,
         char ** output_substitute, size_t ** out_substitute_length,
         char * errstr);
extern void pcrz_codeset_release (ZCSSTRCT *zcsstrct_ptr);
int pcrz_codeset_convert (ZCSSTRCT *zcsstrct_ptr, char func,
         char * input_subject, size_t subject_length,
         char ** output_subject, size_t ** out_subject_length,
         char * errstr);
/* to make a length terminated into null terminated use COBOL (or PL/I
equivalent) technique of concatenating the sting with null:
           STRING YOUR-LENGTH-TERMINATED_STRING, LOW-VALUE
                  DELIMITED BY SIZE INTO YOUR-TARGET-STRING
*/

/* This function examines the presumably COBOL character string as is.  If it
is not null terminated, then if there is at least one space in the end it will
return -2 for space terminated and if the last character is not space it will
return -3 for length terminated.  The user may disregard the difference between
length and space terminated, depending on the application.  If the string is
null  terminated, then the function will return -1.
Note: maxlen is one more then the null terminated string length to allow for
the null terminator.  For the others it is the fixed length of the string.  */
int pcrz_what_term_str (char * str, int maxlen)
{
   int i;
   /* is it null terminated? */
   for (i=0; i<maxlen; i++)
   {
      if (str [i] == CHAR_NULL)
      {
         return PCRZ_NULL_TERMINATED; /* null terminated */
      }
   }
   /* no! then choose among length terminated variants*/
   if (str [maxlen-1] ==  CHAR_SPACE)
   {
      return PCRZ_SPACE_TERMINATED; /*space terminated*/
   }
   else
   {
      return PCRZ_LENGTH_TERMINATED; /*length terminated */
   }
}

/* This function examines the presumably COBOL character string as is.  If it
is not null terminated, then it looks for the last non-space character and
returns the actual length of the string to that character.  If the last
character is not space it will return -3 for length terminated.  If the string
is null terminated, then the function will return -1                        */
int pcrz_is_space_term_str (char * str, int maxlen)
{
   int i;
   for (i=0; i<maxlen; i++)
   {
      if (str [i] == CHAR_NULL)
      {
         return PCRZ_NULL_TERMINATED; /* no, it is null terminated */
      }
   }
   for (i=maxlen-1; i>=0; i--)
   {
      if (str [i] !=  CHAR_SPACE)
      {
         return i+1; /* length of spaec terminated */
      }
   }
   return PCRZ_LENGTH_TERMINATED; /*length terminated */
}

/* This function examines the presumably COBOL character string as is.  If it
is null terminated then its length is returned.  Otherewise, if there is at
least one space in the end it will return -2 for space terminated and if the
last character is not space it will return -3 for length terminated.  The user
may disregard the difference between length and space terminated, depending on
the application.
Note: maxlen is one more then the null terminated string length to allow for
the null terminator.  For the others it is the fixed length of the string.  */
int pcrz_is_null_term_str (char * str, int maxlen)
{
   int i;
   for (i=0; i<maxlen; i++)
   {
      if (str [i] == CHAR_NULL)
      {
         return i; /* length of null terminated */
      }
   }
   /* no! tehn choose among length terminated variants*/
   if (str [maxlen-1] ==  CHAR_SPACE)
   {
      return PCRZ_SPACE_TERMINATED; /*space terminated*/
   }
   else
   {
      return PCRZ_LENGTH_TERMINATED; /*length terminated */
   }
}

/* This function examines the presumably COBOL character string.  If it is
already null terminated, only the null terminated string part is examined
farther.  The function then looks for the last non-space character, marks the
next character as null and returns the actual length of the null terminated
string.  If the last non-space character is the last character (no room for
null character, then the function will return -3 for length terminated.
Note: maxlen is one more then the null terminated string length to allow for
the null terminator.  For the others it is the fixed length of the string.  */
int pcrz_space_to_null_term_str (char * str, int maxlen)
{
   int i;
   int j;
   j = maxlen;
   for (i=0; i<maxlen; i++)
   {
      if (str [i] ==  HAR_NULL)
      {
         j= i;
         break;
      }
   }
   for (i=j-1; i>=0; i--)
   {
      if (i < 0)
      {
         str [i+1] = CHAR_NULL;
         return 0;
      }
      if (str [i] !=  CHAR_SPACE)
      {
         if (i+1 == maxlen)
         {
            return PCRZ_LENGTH_TERMINATED; /*length terminated */
         }
         else
         {
            str [i+1] = CHAR_NULL;
            return i+1; /* length of spaec terminated */
         }
      }
   }
}

/* This function examines the presumably COBOL character string.  If it is null
terminated, the null character and all characters after it to maxlen are
replaced by space.  The function then looks for the last non-space character
and returns the actual length of the space terminated string.  If the last
non-space character is the last character, then the function will return -3 for
length terminated.
Note: maxlen is one more then the null terminated string length to allow for
the null terminator.  For the others it is the fixed length of the string.  */
int pcrz_null_to_space_term_str (char * str, int maxlen)
{
   int i;
   int j;
   for (i=0; i<maxlen; i++)
   {
      if (str [i] == CHAR_NULL)
      {
         for (j=i; j < maxlen; j++)
         {
            str [j] =  CHAR_SPACE;
         }
         break;
      }
   }
   if (str [maxlen - 1] !=  CHAR_SPACE)
   {
      return PCRZ_LENGTH_TERMINATED; /*length terminated */
   }
   for (i=maxlen-1; i>=0; i--)
   {
      if (str [i] !=  CHAR_SPACE)
      {
          return i; /* length of space terminated */
      }
   }
}


/****************************************************************************
* This section  provides means to handle the EBCDIC horror within the       *
* context of PCRE2 and its REXX interface.  Read more about the problem and *
* solution in the PCRE2 Native z/OS Port documentation.                     *
****************************************************************************/
/* provided functionalities:
*  1. Determine the native (or supplied) input CODESET.
*  2. Initializie conversion. (to, from)
*  3. Allocate two blocks of memory for the IBM1047 intermediate
*     strings, one for the pattern and one for the subject string.
*  4. Front end conversion, converting pattern and subject strings
*     from the local codeset to IBM1047
*  5. Releasing the formerly initialized environment and deallocating
*     allocated memory.
*  6. Appropriate structure to point to all the above, via the
*     PCRZCONH header file, that defines the ZCSSTRCT structure.
*  7. Added dealing with replacement and substitute result string.
*  Obviously the package should be smart enough to notice whether the
*  local codeset is IBM1047 and do nothing.
*  The LE language (COBOL) and REXX user does not need to know the
*  intertnal structures or even the the structure of ZCSSTRCT.
*  The current documentation would change to reflect what was learned
*  about the EBCDIC horror, accurate explanation on the compiled
*  version, and so on.  Appropriate changes would be introduced to
*  the functions that try to determine the compiler and locale
*  settings.
*/

/*
*  pcrz_codeset_init <=> ZCSINIT
*  1. Determines the native (or supplied) input CODESET.
*  2. Initializie conversion. (to, from)
*  3. Allocate two blocks of memory for the IBM1047 intermediate
*     strings, one for the pattern and one for the subject string.
*  codeset_name is optional and provides the input CODESET,
*  defaults to local codeset. Note however, that if your emulator
*  defaults to something different then the installation locale,
*  you have to override the locale by supplying your codeset.
*  Note that the internal CODESET is always IBM1047 which is the IBM
*  C compiler default and the way the binaries are distributed.
*  pattern_size and subject_size are the estimated max sizes for
*  the largest pattern and largest subject string, default to 2048 and
*  8192 corr.
*  The returned handle should be preserved.
*/

int pcrz_codeset_init (ZCSSTRCT ** handle_ptr, char * codeset_name,
                      size_t codeset_name_size,
                      size_t pattern_size,
                      size_t subject_size,
                      char * errstr)
{

    char *codeset;
    int rc;
    ZCSSTRCT * zcsstrct_ptr;

    /* Allocate the return block */
    zcsstrct_ptr = (ZCSSTRCT *) malloc (sizeof (ZCSSTRCT));
    if (zcsstrct_ptr == NULL)
    {
        if (errstr != NULL)
        {
            sprintf(errstr,
                "failed to allocate ZCSSTRCT block: %",
                strerror(errno));
        }
        return(PCRZERR_ZCSSTRCT_ALLOC);
    }

    /* Initialize the structure to empty */
    *handle_ptr = zcsstrct_ptr;
    memcpy(zcsstrct_ptr->charset_locale, " ", 2);
    zcsstrct_ptr->cd           = NULL;
    zcsstrct_ptr->cd_reverse   = NULL;
    zcsstrct_ptr->pattern_size = 0;
    zcsstrct_ptr->subject_size = 0;
    zcsstrct_ptr->replacement_size = 0;
    zcsstrct_ptr->pattern_1047 = NULL;
    zcsstrct_ptr->subject_1047 = NULL;
    zcsstrct_ptr->replacement_1047 = NULL;

    /* Determine the codeset name */
    if (codeset_name != NULL)
    {
        codeset = codeset_name;
        if (codeset_name_size == PCRE2_ZERO_TERMINATED)
        {
           if (strlen(codeset_name) > PCRZ_MAX_CODESET_SIZE - 1)
           {
              if (errstr != NULL)
              {
                  sprintf(errstr,
                      "null teminated CODESET name length %d too large",
                      strlen(codeset));
              }
              free (zcsstrct_ptr);
              zcsstrct_ptr = NULL;
              return(PCRZERR_CODESET_NAME_LEN2);
           }
           if (errstr != NULL)
           {
              sprintf(errstr, "null terminated CODSET is %s", codeset)  ;
           }
        }
        else
        {
           if (codeset_name_size == PCRZ_SPACE_TERMINATED)
           {
              rc = pcrz_space_to_null_term_str ((char *) codeset_name,
                             PCRZ_MAX_CODESET_SIZE);
              if (rc == PCRZ_LENGTH_TERMINATED)
              {
                 if (errstr != NULL)
                 {
                     sprintf(errstr,
                         "space terminated CODESET name too long");
                 }
                 free (zcsstrct_ptr);
                 zcsstrct_ptr = NULL;
                 return(PCRZERR_CODESET_NAME_LEN3);
              }
              if (errstr != NULL)
              {
                 sprintf(errstr, "space terminated CODSET is %.16s",
                     codeset);
              }
           }
           else
           {
              if (codeset_name_size > PCRZ_MAX_CODESET_SIZE - 1)
              {
                 if (errstr != NULL)
                 {
                     sprintf(errstr,
                         "supplied length CODESET name size %d too large",
                         codeset_name_size);
                 }
                 free (zcsstrct_ptr);
                 zcsstrct_ptr = NULL;
                 return(PCRZERR_CODESET_NAME_LEN4);
              }
              else
              {
                 codeset[codeset_name_size] = CHAR_NULL;
                 if (errstr != NULL)
                 {
                     sprintf(errstr, "supplied length CODSET is %s",
                         codeset);
                 }
              }
           }
        }
    }
    else
    {
        setlocale(LC_ALL, "");
        codeset = nl_langinfo(CODESET);
        if (errstr != NULL)
        {
            sprintf(errstr, "CODSET is %s", codeset);
        }
    }

    strcpy(zcsstrct_ptr->charset_locale, codeset);

    /* Do we need conversion? */
    if (strcmp(codeset, CHARSET_IBM1047) != 0)
    /* Yes, open and allocate descriptor */
    {
        if ((zcsstrct_ptr->cd = iconv_open(CHARSET_IBM1047, codeset))
           == (iconv_t)(-1))
        {
            if (errstr != NULL)
            {
                sprintf(errstr,
                    "Cannot open converter from %s to %s, %s",
                    codeset, CHARSET_IBM1047, strerror(errno));
            }
            free (zcsstrct_ptr);
            zcsstrct_ptr = NULL;
            return(PCRZERR_OPEN_CONVETER5);
        }
        if ((zcsstrct_ptr->cd_reverse = iconv_open(codeset, CHARSET_IBM1047))
           == (iconv_t)(-1))
        {
            if (errstr != NULL)
            {
                sprintf(errstr,
                    "Cannot open converter from %s to %s, %s",
                    CHARSET_IBM1047, codeset, strerror(errno));
            }
            free (zcsstrct_ptr->cd);
            free (zcsstrct_ptr);
            zcsstrct_ptr = NULL;
            return(PCRZERR_OPEN_CONVETER8);
        }

    }

    if (pattern_size == 0)
    {
       zcsstrct_ptr->pattern_size = PCRZ_MAX_PATTERN_SIZE;
    }
    else
    {
       zcsstrct_ptr->pattern_size = pattern_size;
    }

    if (subject_size == 0)
    {
       zcsstrct_ptr->subject_size = PCRZ_MAX_SUBJECT_SIZE;
    }
    else
    {
       zcsstrct_ptr->subject_size = subject_size;
    }

    /* Allocate space for converted pattern */
    zcsstrct_ptr->pattern_1047 = (char *)
                  malloc (zcsstrct_ptr->pattern_size);
    if (zcsstrct_ptr->pattern_1047 == NULL)
    {
        if (errstr != NULL)
        {
            sprintf(errstr,
                "failed to allocate space for converted pattern: %s",
                strerror(errno));
        }
        iconv_close(zcsstrct_ptr->cd);
        iconv_close(zcsstrct_ptr->cd_reverse);
        free (zcsstrct_ptr);
        zcsstrct_ptr = NULL;
        return(PCRZERR_PATTERN_ALLOC);
    }

    /* Allocate space for converted subject */
    zcsstrct_ptr->subject_1047 = (char *)
                  malloc ( zcsstrct_ptr->subject_size);
    if (zcsstrct_ptr->subject_1047 == NULL)
    {
        if (errstr != NULL)
        {
            sprintf(errstr,
                "failed to allocate space for converted subject: %s",
                strerror(errno));
        }
        iconv_close(zcsstrct_ptr->cd);
        iconv_close(zcsstrct_ptr->cd_reverse);
        free (zcsstrct_ptr->pattern_1047);
        free (zcsstrct_ptr);
        zcsstrct_ptr = NULL;
        return(PCRZERR_SUBJECT_ALLOC);
    }

    return (0);
}

/*-------------------------------------------------------------------*/

/*
*  pcrz_codeset_init_substitute <=> ZCSINIS
*  1. calls pcrz_codeset_init.
*  2. deals with the replacement string and allocates memory for it
*  3. Allocate two blocks of memory for the IBM1047 intermediate
*     strings and for the ultimate output buffer of the substitute
*     result.
*  The returned handle originates from pcrz_codeset_init and should be
*  preserved.  No need to call pcrz_codeset_init independently.
*  However, if pcrz_codeset_init_substitute fails, it frees only
*  it's own memory.  You still need to do release.
*/

int pcrz_codeset_init_substitute (ZCSSTRCT * handle_ptr,
                      size_t replacement_size,
                      size_t substitute_size,
                      char **substitute_ptr,
                      char * errstr)
{
    int rc;

    if (replacement_size == 0)
    {
       handle_ptr->replacement_size = PCRZ_MAX_REPLACEMENT_SIZE;
    }
    else
    {
       handle_ptr->replacement_size = replacement_size;
    }

    if (substitute_size == 0)
    {
       handle_ptr->substitute_size = PCRZ_MAX_SUBSTITUTE_SIZE;
    }
    else
    {
       handle_ptr->substitute_size = substitute_size;
    }

    /* Allocate space for converted subject */
    handle_ptr->replacement_1047 = (char *)
                  malloc ( handle_ptr->replacement_size);
    if (handle_ptr->replacement_1047 == NULL)
    {
        if (errstr != NULL)
        {
            sprintf(errstr,
                "failed to allocate space for converted replacement: %s",
                strerror(errno));
        }
      /*iconv_close(handle_ptr->cd);*/
        iconv_close(handle_ptr->cd_reverse);
      /*free (handle_ptr->pattern_1047);
        free (handle_ptr->subject_1047);
        free (handle_ptr);
        handle_ptr = NULL;*/
        return(PCRZERR_REPLACEMENT_ALLOC);
    }

    /* Allocate space for converted buffer */
    handle_ptr->substitute_1047 = (char *)
                  malloc ( handle_ptr->substitute_size);
    if (handle_ptr->substitute_1047 == NULL)
    {
        if (errstr != NULL)
        {
            sprintf(errstr,
                "failed to allocate space for converted substitute: %s",
                strerror(errno));
        }
      /*iconv_close(handle_ptr->cd);*/
        iconv_close(handle_ptr->cd_reverse);
      /*free (handle_ptr->pattern_1047);
        free (handle_ptr->subject_1047);*/
        free (handle_ptr->replacement_1047);
      /*free (handle_ptr);
        handle_ptr = NULL;*/
        return(PCRZERR_SUBSTITUTE_ALLOC);
    }

    /* Allocate space for result buffer */
    handle_ptr->substitute = (char *)
                  malloc ( handle_ptr->substitute_size);
    if (handle_ptr->substitute == NULL)
    {
        if (errstr != NULL)
        {
            sprintf(errstr,
                "failed to allocate space for converted substitute back: %s",
                strerror(errno));
        }
      /*iconv_close(handle_ptr->cd);*/
        iconv_close(handle_ptr->cd_reverse);
      /*free (handle_ptr->pattern_1047);
        free (handle_ptr->subject_1047);*/
        free (handle_ptr->replacement_1047);
        free (handle_ptr->substitute_1047);
      /*free (handle_ptr);
        handle_ptr = NULL;*/
        return(PCRZERR_SUBSTITUTE_BACK_ALLOC);
    }

    * substitute_ptr = handle_ptr->substitute_1047;

    return (0);
}

/*-------------------------------------------------------------------*/

/*
*  pcrz_codeset_convert_pattern <=> ZCSCONVP
*  pcrz_codeset_convert_subject <=> ZCSCONVS
*  pcrz_codeset_convert_replacement <=> ZCSCONVR
*  pcrz_codeset_convert_substitute_back <=> ZCSCONVB
*  4. Front end conversion, converting pattern and subject strings
*     from the local codeset to IBM1047
*  This is done via two functions (to avoid user direct interuction
*  with the gory details:
*  int pcrz_codeset_convert_pattern (ZCSSTRCT zcsstrct_ptr,
*        char * input_pattern, size_t pattern_length);
*  int pcrz_codeset_convert_subject (ZCSSTRCT zcsstrct_ptr,
*        char * input_subject, size_t subject_length);
*  int pcrz_codeset_convert_replacement (ZCSSTRCR zcsstrct_ptr,
*        char * input_replacement, size_t replacement_length);
*  int pcrz_codeset_convert_substitute_back (ZCSSTRCB zcsstrct_ptr,
*        char * input_substitute, size_t substitute_length);
*  where zcsstrct_ptr is the structure returnde by ZCSINIT and
*  input_pattern is the input pattern, or input_subject which is
*  the input subject string.  The functions return pointer to
*  converted string and its length.
*  Note that pcrz_codeset_convert_substitute_back is different in
*  that it convert back from IBM1047 to the original codeset
*/

int pcrz_codeset_convert_pattern (ZCSSTRCT *zcsstrct_ptr,
         char * input_pattern, size_t pattern_length,
         char ** output_pattern, size_t ** out_pattern_length,
         char * errstr)
{
   return
    pcrz_codeset_convert (zcsstrct_ptr, 'p',
         input_pattern, pattern_length,
         output_pattern, out_pattern_length, errstr);
}

/*-------------------------------------------------------------------*/

int pcrz_codeset_convert_subject (ZCSSTRCT *zcsstrct_ptr,
         char * input_subject, size_t subject_length,
         char ** output_subject, size_t ** out_subject_length,
         char * errstr)
{
   return
    pcrz_codeset_convert (zcsstrct_ptr, 's',
         input_subject, subject_length,
         output_subject, out_subject_length, errstr);
}

/*-------------------------------------------------------------------*/

int pcrz_codeset_convert_replacement (ZCSSTRCT *zcsstrct_ptr,
         char * input_replacement, size_t replacement_length,
         char ** output_replacement, size_t ** out_replacement_length,
         char * errstr)
{
   return
    pcrz_codeset_convert (zcsstrct_ptr, 'r',
         input_replacement, replacement_length,
         output_replacement, out_replacement_length, errstr);
}

/*-------------------------------------------------------------------*/

/*
Use this subroutine to convert the substitute_1047 back to the original
codeset.  The input is the substitute_1047, the output is either
substiute which is converted back or substitute_1047 if no conversion.
Supplied parameters are substitute_1047, its length, substitute, same
length.
*/

int pcrz_codeset_convert_substitute_back (ZCSSTRCT *zcsstrct_ptr,
         char * input_substitute, size_t substitute_length,
         char ** output_substitute, size_t ** out_substitute_length,
         char * errstr)
{
   return
    pcrz_codeset_convert (zcsstrct_ptr, 'b',
         input_substitute, substitute_length,
         output_substitute, out_substitute_length, errstr);
}

/*-------------------------------------------------------------------*/

int pcrz_codeset_convert (ZCSSTRCT *zcsstrct_ptr, char func,
         char * input_subject, size_t subject_length,
         char ** output_subject, size_t ** out_subject_length,
         char * errstr)
{
   size_t subj_len;
   size_t subj_len_save;
   size_t target_len;
   char * subj_ptr;
   int rc;

   switch(func)
   {
       case 's':
          target_len = zcsstrct_ptr->subject_size;
          subj_ptr = zcsstrct_ptr->subject_1047;
          break;
       case 'p':
          target_len = zcsstrct_ptr->pattern_size;
          subj_ptr = zcsstrct_ptr->pattern_1047;
          break;
       case 'r':
          target_len = zcsstrct_ptr->replacement_size;
          subj_ptr = zcsstrct_ptr->replacement_1047;
          break;
       case 'b':
          target_len = zcsstrct_ptr->substitute_size;
          subj_ptr = zcsstrct_ptr->substitute;
          break;


       default:
          if (errstr != NULL)
          {
              sprintf(errstr, "Unknown supplied function %s", func);
          }
          return (PCRZERR_UNKNOWN_FUNCTION);
   }

   if (subject_length == 0)
   {
      subj_len = strlen(input_subject);
   }
   else
   {
      subj_len = subject_length;
   }
   if (subj_len > target_len -1)
   {
      if (errstr != NULL)
      {
          sprintf(errstr, "target length %d gt allocated block %d", subj_len, target_len);
      }
      return (PCRZERR_TARGET_LEN_ALLOC);
   }
   subj_len_save = subj_len;

   if (zcsstrct_ptr->cd == NULL)
   {
      * output_subject = input_subject;
      * out_subject_length = subj_len_save;
      return (1); /* no conversion is necessary. use input */
   }

   if (func != 'b')
   {
       rc = iconv(zcsstrct_ptr->cd, &input_subject, &subj_len,
                  &subj_ptr, &target_len);
   }
   else
   {
       rc = iconv(zcsstrct_ptr->cd_reverse, &input_subject, &subj_len,
                  &subj_ptr, &target_len);
   }

   if (rc == -1)
   {
      if (errstr != NULL)
      {
          sprintf(errstr, "Error in converting subject string, %s",
          strerror(errno));
      }
      return (PCRZERR_ICONV_ERROR);
   }

   switch(func)
   {
       case 's':
          * output_subject = zcsstrct_ptr->subject_1047;
          break;
       case 'p':
          * output_subject = zcsstrct_ptr->pattern_1047;
          break;
       case 'r':
          * output_subject = zcsstrct_ptr->replacement_1047;
          break;
       case 'b':
          * output_subject = zcsstrct_ptr->substitute;
          break;
   }

   * out_subject_length = subj_len_save;
   return (0); /* conversion successful, use zcsstrct_ptr->subject_1047 */
}

/*-------------------------------------------------------------------*/

/*
*  pcrz_codeset_release <=> ZCSRLSE
*  5. Releasing the formerly initialized environment and deallocating
*     allocated memory.
*  This is done via the function:
*  void pcrz_codeset_release (ZCSSTRCT zcsstrct_ptr);
*  where struct is the structure returnde by ZCSINIT and input_pattern
*  is the input pattern.
*/

void pcrz_codeset_release (ZCSSTRCT *zcsstrct_ptr)
{
   if (zcsstrct_ptr->subject_1047 != NULL)
   {
        free (zcsstrct_ptr->subject_1047);
        free (zcsstrct_ptr->pattern_1047);
   }
   if (zcsstrct_ptr->replacement_1047 != NULL)
   {
        free (zcsstrct_ptr->replacement_1047);
        free (zcsstrct_ptr->substitute_1047);
        free (zcsstrct_ptr->substitute);
   }

   if (zcsstrct_ptr->cd != NULL)
   {
        iconv_close(zcsstrct_ptr->cd);
   }
   if (zcsstrct_ptr->cd_reverse != NULL)
   {
        iconv_close(zcsstrct_ptr->cd_reverse);
   }

   free (zcsstrct_ptr);
}
#endif
/*autoconv-0001 C:\projects\pcre2port\pcrzfunc.c converted to PCRZFUNC*/
/*autoconv-0001 C:\projects\pcre2port\pcrzfunc.c converted to PCRZFUNC*/
/*autoconv-0001 C:\projects\pcre2port\pcrzfunc.c converted to PCRZFUNC*/
