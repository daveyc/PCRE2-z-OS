 
 
#ifdef HAVE_CONFIG_H
#include "CONFIG.h"
#endif
 
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <locale.h>
 
#define PCRE2_DFTABLES            /* for pcre2_internal.h, pcre2_maketables.c */
 
#define PCRE2_CODE_UNIT_WIDTH 0   /* Must be set, but not relevant here */
#include "INTERNA2.h"
/* ARGSUSED */
int
main(int argc, char *argv[])
{
    int nflag;
 
    /*if (pledge("stdio", NULL) == -1)
        err(1, "pledge");*/
 
    /* This utility may NOT do getopt(3) option parsing. */
    if (*++argv && !strcmp(*argv, "-n")) {
        ++argv;
        nflag = 1;
    }
    else
        nflag = 0;
 
    while (*argv) {
        (void)fputs(*argv, stdout);
        if (*++argv)
            putchar(' ');
    }
    if (!nflag)
        putchar('\n');
 
    return 0;
}
 
