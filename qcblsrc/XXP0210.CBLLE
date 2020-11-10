       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0210.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ABSCHLIEßEN EINER BESTELLUNG                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.03.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ABSCHLIEßEN EINER BESTELLUNG                 *
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

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI BESTELLUNGEN
     ‚*-------------------------------------------------------------
           SELECT PKDBST1-DP
                  ASSIGN       TO  DATABASE-PKDBST1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PKDBST1: KBLFDN
       FD  PKDBST1-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDBST1-P.
           COPY DDS-ALL-FORMATS OF PKDBST1.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0210".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-KBLFDN                      LIKE KBLFDN OF PKDBST1-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-KBLFDN.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0210" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PKDBST1-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-KBLFDN = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* ERST LÖSCHEN DES PROGRAMM, DANN DIE TAGS
           PERFORM  UPDATE-RECORD.

       ENDE.
           CLOSE                 PKDBST1-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DES DATENSATZES
     ‚*--------------------------------------------------------------
       UPDATE-RECORD SECTION.
       UPD-REC-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* DATENSATZ LÖSCHEN
           INITIALIZE PKDBST1F OF PKDBST1-P.
           MOVE     X-KBLFDN            TO KBLFDN OF PKDBST1-P.
           READ     PKDBST1-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             MODIFIKATIONSDATUM
                    IF   KBVSTS OF PKDBST1-P NOT = "4"
                     AND KBVSTS OF PKDBST1-P NOT = "9"
                         MOVE    T-USER      TO KBMSER OF PKDBST1-P
                         MOVE    T-TERM      TO KBMBS  OF PKDBST1-P
                         MOVE    PGM-WRK     TO KBMPGM OF PKDBST1-P
                         MOVE    DATE-8      TO KBMDTA OF PKDBST1-P
                         MOVE    TIME-6      TO KBMTIA OF PKDBST1-P
     ‚*                  ERLEDIGTDATUM
                         MOVE    T-USER      TO KBEUSR OF PKDBST1-P
                         MOVE    T-TERM      TO KBEBS  OF PKDBST1-P
                         MOVE    PGM-WRK     TO KBEPGM OF PKDBST1-P
                         MOVE    DATE-8      TO KBDTE  OF PKDBST1-P
                         MOVE    TIME-6      TO KBTIE  OF PKDBST1-P
                         MOVE    "9"         TO KBVSTS OF PKDBST1-P
                    END-IF
                    REWRITE PKDBST1-P
           END-IF.

       UPD-REC-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PERMISSION: BERECHTIGUNGSPRÜFUNG
     ‚*---------------------------------------------------------------

           COPY     PERMISSION OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
