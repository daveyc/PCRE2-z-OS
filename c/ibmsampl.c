
 
/*
 An adaptation of some code samples provided by IBM in their published
 documentation of the POSIX functions.  This adaptation  was done for
 the sole purpose of demonstrating the difference between PCRE2 and
 POSIX.  This code is provided strictly for demonstration  purpose and
 should not be used for any other purpose.  For any other use, please
 consult with IBM copyright notes.
-----------------------------------------------------------------------
THIS SOFTWARE IS PROVIDED "AS IS" AND ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
NO EVENT SHALL THE PROVIDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
 */
/* This header invokes PCRE2 headers to make the compiler
 use the PCRE2 equivalents of the traditional POSIX functions*/
#include "PC2PSXAP.h"
#ifdef COMPILE_WITH_PCRE2
static unsigned char SHOULD_PRODUCE[] = "oneself";
#else
static unsigned char SHOULD_PRODUCE[] = "oneselfsufficient";
#endif
 
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
 
main() {
    regex_t    preg;
    char       *string = "oneselfsufficient ";
    char       *pattern = "one(self|selfsufficient)";
    int        rc;
    char       buffer[100];
    size_t     nmatch = 2;
    regmatch_t pmatch[2];
 
 printf("+++ %s %s\n", COMPILE_OPTION, SHOULD_PRODUCE);
 
    if ((rc = regcomp(&preg, pattern, REG_EXTENDED)) != 0) {
       regerror(rc, &preg, buffer, 100);
       printf("regcomp() failed with '%s'\n", buffer);
       exit(1);
    }
 
    if ((rc = regexec(&preg, string, nmatch, pmatch, 0)) != 0) {
       printf("failed to ERE match '%s' with '%s',returning %d.\n",
       string, pattern, rc);
       exit (2);
    }
      printf("With the whole expression, "
             "a matched substring \"%.*s\" is found at position %d to %d.\n",
             pmatch[0].rm_eo - pmatch[0].rm_so, &string[pmatch[0].rm_so],
             pmatch[0].rm_so, pmatch[0].rm_eo - 1);
      printf("With the sub-expression, "
             "a matched substring \"%.*s\" is found at position %d to %d.\n",
             pmatch[1].rm_eo - pmatch[1].rm_so, &string[pmatch[1].rm_so],
             pmatch[1].rm_so, pmatch[1].rm_eo - 1);
    regfree(&preg);
}
/*autoconv-0001 C:\projects\pcre2port\IBMSAMPL.c converted to IBMSAMPL*/
