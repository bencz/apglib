       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9038.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  23.02.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEST-PGM                                     *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9038".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  NUMVAL-WRK                    PIC S9(5).
       01  NUMSTR-WRK                    PIC X(5).

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9038"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* TEST -4
           MOVE     "   -4" TO NUMSTR-WRK.
           COMPUTE  NUMVAL-WRK = FUNCTION NUMVAL(NUMSTR-WRK).

     ‚* TEST -23
           MOVE     "  -23" TO NUMSTR-WRK.
           COMPUTE  NUMVAL-WRK = FUNCTION NUMVAL(NUMSTR-WRK).

     ‚* TEST -129
           MOVE     " -129" TO NUMSTR-WRK.
           COMPUTE  NUMVAL-WRK = FUNCTION NUMVAL(NUMSTR-WRK).

     ‚* TEST -2299
           MOVE     "-2299" TO NUMSTR-WRK.
           COMPUTE  NUMVAL-WRK = FUNCTION NUMVAL(NUMSTR-WRK).

     ‚* TEST +299
           MOVE     " +299" TO NUMSTR-WRK.
           COMPUTE  NUMVAL-WRK = FUNCTION NUMVAL(NUMSTR-WRK).

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
