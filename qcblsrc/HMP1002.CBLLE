       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP1002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LÖSCHEN EINES TAGS                                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LÖSCHEN EINES TAGS                           *
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
     ‚* PHYSISCHE DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DP
                  ASSIGN       TO  DATABASE-PTAGLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TAGS
       FD  PTAGLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-P.
           COPY DDS-ALL-FORMATS OF PTAGLIB.
      /
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP1002".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PTLFDN                      LIKE PTLFDN OF PTAGLIB-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PTLFDN.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP1002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PTAGLIB-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-PTLFDN = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* ERST LÖSCHEN DES PROGRAMM, DANN DIE TAGS
           PERFORM  DELETE-PTAGLIB.

       ENDE.
           CLOSE                 PTAGLIB-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TAGS
     ‚*--------------------------------------------------------------
       DELETE-PTAGLIB SECTION.
       DEL-TAG-00.

     ‚* ALLE TAGS LÖSCHEN
           INITIALIZE PTAGLIBF OF PTAGLIB-P.
           MOVE     X-PTLFDN            TO PTLFDN OF PTAGLIB-P.
           READ     PTAGLIB-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE   PTAGLIB-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE     CONST-LIB   TO LIB-WRK
                    MOVE     "PTAGLIB"   TO FILE-WRK
                    MOVE     PTAGLIB-P   TO RECORD-WRK
                    CALL     "CFP8100" USING LIB-WRK
                                             FILE-WRK
                                             PGM-WRK
                                             RECORD-WRK
                    END-CALL
           END-IF.

       DEL-TAG-90.
           EXIT.
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
