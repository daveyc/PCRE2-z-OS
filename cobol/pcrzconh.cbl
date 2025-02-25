      * this is a description of the working-storage section, common
      * needed variables.
      *
      * Copyright (c) 2019 Ze'ev Atlas
      * Please refer to the LICENSE document to see all other
      * applicable copyrights.
      *
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
      * PCRZCONH
      * Support some z/OS specific routines in PCRZFUNC
      *---------------------------------------------------------------
      * For COBOL internal reasons I put all constants from PCRZCONH.h
      * in PCRE2.cpy
      *****************************************************************

       01   :PREFIX:-PCRZ-ZCSSTRCT.

            05  :PREFIX:-charset-locale pic x(16).
            05  :PREFIX:-cd               usage pointer.
            05  :PREFIX:-cd-reverse       usage pointer.
            05  :PREFIX:-pattern-size     pic s9(9) comp.
            05  :PREFIX:-subject-size     pic s9(9) comp.
            05  :PREFIX:-replacement-size pic s9(9) comp.
            05  :PREFIX:-substitute-size  pic s9(9) comp.
            05  :PREFIX:-pattern-1047     usage pointer.
            05  :PREFIX:-subject-1047     usage pointer.
            05  :PREFIX:-replacement-1047 usage pointer.
            05  :PREFIX:-substitute-1047  usage pointer.
            05  :PREFIX:-substitute       usage pointer.
