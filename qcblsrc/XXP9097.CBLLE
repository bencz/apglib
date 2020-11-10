       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9097.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.03.2018                                   *
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
         SOURCE-COMPUTER.   IBM-AS400.
         OBJECT-COMPUTER.   IBM-AS400,
                            PROGRAM COLLATING SEQUENCE IS IBM-ASCII.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                          ALPHABET IBM-ASCII IS NLSSORT.

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

       01  PGM-WRK                        PIC X(10) VALUE "XXP9095".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

       01  MY-STRUCT.
        05 MY-VAR                         PIC N(10) NATIONAL.
        05 MY-VAR2                        PIC N(10) NATIONAL.

       01  MY-STRUCT2.
        05 MY-VAR                         PIC N(10) NATIONAL.
        05 MY-VAR2                        PIC X(10).

       01  MY-TARGET                      PIC N(20) NATIONAL.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9097"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     "ABCDEF" TO MY-VAR  OF MY-STRUCT.
           MOVE     "ABCDEF" TO MY-VAR2 OF MY-STRUCT.
           MOVE     "ABCDEF" TO MY-VAR  OF MY-STRUCT2.
           MOVE     "ABCDEF" TO MY-VAR2 OF MY-STRUCT2.

           MOVE     MY-STRUCT  TO MY-TARGET.
           MOVE     MY-STRUCT2 TO MY-TARGET.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
