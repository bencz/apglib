       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HMP0008.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LÖSCHEN EINES PROGRAMMS                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.05.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LÖSCHEN EINES PROGRAMMS (PPGMLIB/PTAGLIB)    *
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
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI PROGRAMME-TEXTE
     ‚*-------------------------------------------------------------
           SELECT PPGMLST-DP
                  ASSIGN       TO  DATABASE-PPGMLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PROGRAMME
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
      /
     ‚*--- PROGRAMMTEXTE
       FD  PPGMLST-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLST-P.
           COPY DDS-ALL-FORMATS OF PPGMLST.
      /
     ‚*--- TAGS
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HMP0008".
       01  PGM-TYP                       PIC X(8)  VALUE "HERMES".

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-PLLIB                       LIKE PLLIB  OF PPGMLIB-P.
       01  X-PLFILE                      LIKE PLFILE OF PPGMLIB-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-PLLIB
                                X-PLFILE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HMP0008" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PPGMLIB-DP
                          PTAGLIB-DL
                          PPGMLST-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-PLLIB  = SPACES
            OR      X-PLFILE = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERST LÖSCHEN DES PROGRAMM, DANN DIE TAGS
           PERFORM  DELETE-PPGMLIB.
           IF       RET-CODE = SPACES
                    PERFORM  DELETE-PTAGLIB
           END-IF.
           IF       RET-CODE = SPACES
                    PERFORM  DELETE-PPGMLST
           END-IF.

       ENDE.
           CLOSE                 PPGMLIB-DP
                                 PPGMLST-DP
                                 PTAGLIB-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER PPGMLIB
     ‚*--------------------------------------------------------------
       DELETE-PPGMLIB SECTION.
       DEL-PGM-00.

           INITIALIZE RET-CODE.
     ‚* LÖSCHEN DER PPGMLIB
           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     X-PLLIB         TO PLLIB  OF PPGMLIB-P.
           MOVE     X-PLFILE        TO PLFILE OF PPGMLIB-P.
           READ     PPGMLIB-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PPGMLIB-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PPGMLIB"   TO FILE-WRK
                    MOVE PPGMLIB-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           ELSE
                    MOVE "1" TO RET-CODE
           END-IF.

       DEL-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TAGS
     ‚*--------------------------------------------------------------
       DELETE-PTAGLIB SECTION.
       DEL-TAG-00.

           INITIALIZE RET-CODE.
     ‚* ALLE TAGS LÖSCHEN
           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     X-PLLIB             TO PTLIB  OF PTAGLIB-L.
           MOVE     X-PLFILE            TO PTFILE OF PTAGLIB-L.
           MOVE     SPACES              TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-TAG-90
           END-IF.

       DEL-TAG-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-TAG-90
           END-IF.

           IF       PTLIB  OF PTAGLIB-L NOT = X-PLLIB
            OR      PTFILE OF PTAGLIB-L NOT = X-PLFILE
                    REWRITE PTAGLIB-L
                    GO TO DEL-TAG-90
           END-IF.

           DELETE   PTAGLIB-DL.
     ‚* LÖSCHEN PROTOKOLLIEREN
           MOVE     "APG"       TO LIB-WRK.
           MOVE     "PTAGLIB"   TO FILE-WRK.
           MOVE     PTAGLIB-L   TO RECORD-WRK.
           CALL     "CFP8100" USING LIB-WRK
                                    FILE-WRK
                                    PGM-WRK
                                    RECORD-WRK
           END-CALL.

           GO TO    DEL-TAG-20.

       DEL-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TEXTE
     ‚*--------------------------------------------------------------
       DELETE-PPGMLST SECTION.
       DEL-TXT-00.

           INITIALIZE RET-CODE.
     ‚* ALLE TAGS LÖSCHEN
           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     X-PLLIB             TO PSLIB  OF PPGMLST-P.
           MOVE     X-PLFILE            TO PSFILE OF PPGMLST-P.
           MOVE     ZEROES              TO PSLFPO OF PPGMLST-P.
           START    PPGMLST-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-TXT-90
           END-IF.

       DEL-TXT-20.
           READ     PPGMLST-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-TXT-90
           END-IF.

           IF       PSLIB  OF PPGMLST-P NOT = X-PLLIB
            OR      PSFILE OF PPGMLST-P NOT = X-PLFILE
                    REWRITE PPGMLST-P
                    GO TO DEL-TXT-90
           END-IF.

           DELETE   PPGMLST-DP.
     ‚* LÖSCHEN PROTOKOLLIEREN
           MOVE     "APG"       TO LIB-WRK.
           MOVE     "PPGMLST"   TO FILE-WRK.
           MOVE     PPGMLST-P   TO RECORD-WRK.
           CALL     "CFP8100" USING LIB-WRK
                                    FILE-WRK
                                    PGM-WRK
                                    RECORD-WRK
           END-CALL.

           GO TO    DEL-TXT-20.

       DEL-TXT-90.
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
