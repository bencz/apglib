     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0017.
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
     ‚* POPLLSTL2: OPSHRN, OPUSER
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DL
                  ASSIGN       TO  DATABASE-POPLLSTL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
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
     ‚*--- POPLLSTL2: OPSHRN, OPUSER
       FD  POPLLST-DL
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-L.
           COPY DDS-ALL-FORMATS OF POPLLSTL2.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0017".
       01  PGM-TYP                       PIC X(8)  VALUE "OPL   ".

       01  DELETE-KZ                     PIC 9(1).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-OPLFDN                      LIKE OPLFDN OF POPLLST-P.
       01  X-OPSHRN                      LIKE OPSHRN OF POPLLST-P.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-OPLFDN
                                X-OPSHRN.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0017" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     INPUT POPLLST-DP.
           OPEN     I-O   POPLLST-DL.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DES DATENSATZES
           PERFORM  READ-POPLLST.
           IF       RET-CODE = SPACES
                    MOVE ZEROES TO DELETE-KZ
           ELSE
                    MOVE 1      TO DELETE-KZ
           END-IF.

     ‚* AKTUALISIEREN VON STATUS UND TEXT
           PERFORM  UPDATE-SHARE-NUMBER.

       ENDE.
           CLOSE    POPLLST-DP
                    POPLLST-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* LESEN DES POPLLST-SATZES
     ‚*---------------------------------------------------------------
       READ-POPLLST SECTION.
       REA-OPL-00.

           INITIALIZE RET-CODE.
     ‚* POPLLST LESEN
           INITIALIZE POPLLSTF OF POPLLST-P.
           MOVE     X-OPLFDN            TO OPLFDN OF POPLLST-P.
           READ     POPLLST-DP.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       REA-OPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AKTUALISIEREN ALLER DAZUGEHÖRENDEN EINTRÄGE
     ‚*--------------------------------------------------------------
       UPDATE-SHARE-NUMBER SECTION.
       UPD-SHN-00.

           INITIALIZE POPLLSTF OF POPLLST-L.
           MOVE     X-OPSHRN            TO OPSHRN OF POPLLST-L.
           MOVE     SPACES              TO OPUSER OF POPLLST-L.
           START    POPLLST-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-SHN-90
           END-IF.

       UPD-SHN-20.
           READ     POPLLST-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-SHN-90
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       OPSHRN OF POPLLST-L NOT = X-OPSHRN
                    REWRITE POPLLST-L
                    GO TO UPD-SHN-90
           END-IF.

           IF       DELETE-KZ = ZEROES
     ‚*             DATENSATZ AKTUALISIEREN
                    IF   OPTYPE OF POPLLST-L NOT = OPTYPE OF POPLLST-P
                     OR  OPTEXT OF POPLLST-L NOT = OPTEXT OF POPLLST-P
                     OR  OPPRZT OF POPLLST-L NOT = OPPRZT OF POPLLST-P
                     OR  OPSTAT OF POPLLST-L NOT = OPSTAT OF POPLLST-P
     ‚*                  KATEGORIE ÜBERNEHMEN
                         MOVE OPTYPE OF POPLLST-P TO
                              OPTYPE OF POPLLST-L
     ‚*                  TEXT ÜBERNEHMEN
                         MOVE OPTEXT OF POPLLST-P TO
                              OPTEXT OF POPLLST-L
     ‚*                  PROZENTZAHL ÜBERNEHMEN
                         MOVE OPPRZT OF POPLLST-P TO
                              OPPRZT OF POPLLST-L
     ‚*                  STATUS ÜBERNEHMEN
                         MOVE OPSTAT OF POPLLST-P TO
                              OPSTAT OF POPLLST-L
     ‚*                  USER FÜLLEN
                         PERFORM  COPY-GET-TIME
                         MOVE T-USER             TO OPMSER OF POPLLST-L
                         MOVE T-TERM             TO OPMBS  OF POPLLST-L
                         MOVE PGM-WRK            TO OPMPGM OF POPLLST-L
                         MOVE DATE-8             TO OPMDTA OF POPLLST-L
                         MOVE TIME-6             TO OPMTIA OF POPLLST-L
                    END-IF
                    REWRITE  POPLLST-L
           ELSE
     ‚*             DATENSATZ LÖSCHEN
                    REWRITE  POPLLST-L
                    CALL "XXP0016" USING OPLFDN OF POPLLST-L
                    END-CALL
           END-IF.

           GO TO    UPD-SHN-20.

       UPD-SHN-90.
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
