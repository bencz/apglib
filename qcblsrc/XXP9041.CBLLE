       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9041.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.04.2012                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP9041".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  FILENAME-SAV                  PIC X(640).
       01  USER-NAME                     PIC X(10).
       01  MEMBER-NAME                   PIC X(10).
       01  CREATE-DATE                   PIC X(8).

       01  MAXLEN-WRK                    PIC 9(4).
       01  POS                           PIC 9(4).
       01  LEN                           PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9041"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE POS, LEN.
           INITIALIZE MEMBER-NAME.
           INITIALIZE CREATE-DATE.
     ‚* DER DATEINAME IST IMMER WIE FOLGT AUFGEBAUT:
     ‚*  NAME_USER_CRTDATE-CRTTIME
           MOVE     "LGSAMAPG_APG_20120419-132012" TO FILENAME-SAV.
           INSPECT  FILENAME-SAV TALLYING LEN FOR
                    CHARACTERS BEFORE INITIAL "_".

     ‚* DATEINAMEN SICHERN
           MOVE     1                       TO POS.
           MOVE     FILENAME-SAV(POS:LEN)   TO MEMBER-NAME.
           ADD      LEN                     TO POS.

     ‚* JETZT WEITERSUCHEN, AUF DEN 1. BUCHSTABEN VON USER SETZEN
           INITIALIZE LEN.
           ADD      1 TO POS.
           INSPECT  FILENAME-SAV(POS:) TALLYING LEN FOR
                    CHARACTERS BEFORE INITIAL "_".
           MOVE     FILENAME-SAV(POS:LEN) TO USER-NAME.
           ADD      LEN                     TO POS.

     ‚* DA UNS DER USER NICHT INTERESSIERT, HIER DIREKT WEITERSUCHEN
           INITIALIZE LEN.
           ADD      1 TO POS.
           INSPECT  FILENAME-SAV(POS:) TALLYING LEN FOR
                    CHARACTERS BEFORE INITIAL "_".
           MOVE     FILENAME-SAV(POS:LEN) TO CREATE-DATE.
           ADD      LEN                     TO POS.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
