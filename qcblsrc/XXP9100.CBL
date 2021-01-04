       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9100.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.07.2020                                   *
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

       01  PGM-WRK                        PIC X(10) VALUE "XXP9099".
       01  PGM-TYP                        PIC X(8)  VALUE "TEST".

       01  DATE-VALUE                     PIC 9(7).
       01  DATE-RETURN                    PIC 9(9).

       01  DATE-NOW                       PIC 9(8) VALUE 20200317.
       01  DATE-NEW                       PIC 9(8).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9100"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     1601001  TO DATE-VALUE.
           COMPUTE  DATE-RETURN = FUNCTION INTEGER-OF-DAY(DATE-VALUE).

           COMPUTE  DATE-RETURN = FUNCTION INTEGER-OF-DATE(DATE-NOW).
           COMPUTE  DATE-RETURN = DATE-RETURN + 30.
           COMPUTE  DATE-NOW = FUNCTION DATE-OF-INTEGER(DATE-RETURN).

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
