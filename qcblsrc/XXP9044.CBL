       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9044.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.07.2012                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9044".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  TEST-STRUCT.
        05 TEST1                         PIC X(10) VALUE "TEST".
        05 FILLER                        PIC X(5)  VALUE "-----".
        05 TEST2                         PIC X(10) VALUE "TEST".
        05 FILLER                        PIC X(5)  VALUE "-----".
        05 TEST3                         PIC X(10) VALUE "TEST".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9044"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* BEIM INITIALIZE WIRD DER FILLTER IGNORIERT
           INITIALIZE TEST-STRUCT.
           MOVE     SPACES TO TEST-STRUCT.

     ‚* BEIM INITIALIZE WIRD DER FILLTER IGNORIERT
           MOVE     ALL "X" TO TEST-STRUCT.
           INITIALIZE TEST-STRUCT.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
