     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0016.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE KW-DATEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.02.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LÖSCHEN EINES TODO-PUNKTES                   *
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

       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* POPLLST: OPLFDN
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DP
                  ASSIGN       TO  DATABASE-POPLLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PTXTFIT: FTFILE, FTKEY, FTLFPO
     ‚*-------------------------------------------------------------
           SELECT PTXTFIT-DP
                  ASSIGN       TO  DATABASE-PTXTFIT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- POPLLST: OPLFDN
       FD  POPLLST-DP
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-P.
           COPY DDS-ALL-FORMATS OF POPLLST.
      /
     ‚*--- PTXTFIT: FTFILE, FTKEY, FTID
       FD  PTXTFIT-DP
           LABEL RECORDS ARE STANDARD.
       01  PTXTFIT-P.
           COPY DDS-ALL-FORMATS OF PTXTFIT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0016".
       01  PGM-TYP                       PIC X(8)  VALUE "OPL   ".

       01  FTKEY-WRK                     LIKE FTKEY  OF PTXTFIT-P.

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-OPLFDN                      LIKE OPLFDN OF POPLLST-P.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-OPLFDN.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0016" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     I-O   POPLLST-DP
                          PTXTFIT-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* OPL-SATZ LÖSCHEN
           PERFORM  DELETE-POPLLST.

     ‚* TEXTE LÖSCHEN
           MOVE     OPLFDN OF POPLLST-P TO FTKEY-WRK.
           PERFORM  DELETE-PTXTFIT.

       ENDE.
           CLOSE    POPLLST-DP
                    PTXTFIT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* LÖSCHEN DES POPLLST-SATZES
     ‚*---------------------------------------------------------------
       DELETE-POPLLST SECTION.
       DLT-OPL-00.

     ‚* POPLLST LÖSCHEN
           INITIALIZE POPLLSTF OF POPLLST-P.
           MOVE     X-OPLFDN            TO OPLFDN OF POPLLST-P.
           READ     POPLLST-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE POPLLST-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "POPLLST"   TO FILE-WRK
                    MOVE POPLLST-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       DLT-OPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE TEXTE LÖSCHEN
     ‚*--------------------------------------------------------------
       DELETE-PTXTFIT SECTION.
       DLT-TXT-00.

           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     "POPLLST"           TO FTFILE OF PTXTFIT-P.
           MOVE     FTKEY-WRK           TO FTKEY  OF PTXTFIT-P.
           MOVE     ZEROES              TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DLT-TXT-90
           END-IF.

       DLT-TXT-20.
           READ     PTXTFIT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DLT-TXT-90
           END-IF.

           IF       FTFILE OF PTXTFIT-P NOT = "POPLLST"
            OR      FTKEY  OF PTXTFIT-P NOT = FTKEY-WRK
                    REWRITE PTXTFIT-P
                    GO TO DLT-TXT-90
           END-IF.

     ‚* SATZ LÖSCHEN
           DELETE   PTXTFIT-DP.
           IF       FILE-STATUS = ZEROES
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PTXTFIT"   TO FILE-WRK
                    MOVE PTXTFIT-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

           GO TO    DLT-TXT-20.

       DLT-TXT-90.
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
