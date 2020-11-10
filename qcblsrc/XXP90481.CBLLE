       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP90481.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TESTPROGRAMM                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.03.2014                                   *
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

       01  PGM-WRK                       PIC X(10) VALUE "XXP90481".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  TEST-1                        PIC X(10).
       01  NULL-VAR                      PIC 9(4).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP90481"   TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           INITIALIZE TEST-1, NULL-VAR.

     ‚* DIESES IF STÜRZT NICHT AB, WEIL SCHON DIE ERSTE BEDINGUNG
     ‚* ZUTRIFFT UND WIR DAHER GAR NICHT AUF DEN FEHLERHAFTEN
     ‚* SUBSTRING PRÜFEN
           IF       TEST-1            = SPACES
            OR      TEST-1(NULL-VAR:) = SPACES
                    CONTINUE
           END-IF.

     ‚* HIER WIRD ZUERST DER FEHLERHAFTE SUBSTRING GEPRÜFT UND DAHER
     ‚* STÜRZEN WIR AUCH HIER MIT EINEM MCH0603 AB
           IF       TEST-1(NULL-VAR:) = SPACES
            OR      TEST-1            = SPACES
                    CONTINUE
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
