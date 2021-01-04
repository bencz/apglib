     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0018.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE KW-DATEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.03.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  KOPIEREN EINES TODO-PUNKTES                  *
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

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0018".
       01  PGM-TYP                       PIC X(8)  VALUE "OPL   ".

       01  DELETE-KZ                     PIC 9(1).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-OPLFDN                      LIKE OPLFDN OF POPLLST-P.
       01  X-OPUSER                      LIKE OPUSER OF POPLLST-P.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-OPLFDN
                                X-OPUSER.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0018" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     I-O   POPLLST-DP.
           OPEN     INPUT POPLLST-DL.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DES DATENSATZES
           PERFORM  READ-POPLLST.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB DER USER DIE SHARE-NUMBER SCHON HAT
           PERFORM  CHECK-SHARE-NUMBER.
           IF       RET-CODE = SPACES
                    PERFORM WRITE-POPLLST
           END-IF.

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
           READ     POPLLST-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       REA-OPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN EINES NEUEN DATENSATZ ALS KOPIE VOM GELESENEN
     ‚*--------------------------------------------------------------
       WRITE-POPLLST SECTION.
       WRI-OPL-00.

     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "POPLLST" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK                  TO OPLFDN OF POPLLST-P.
           MOVE     X-OPUSER                  TO OPUSER OF POPLLST-P.

     ‚* GIBT ES DIE KATEGORIE AUCH BEIM ZIEL-USER? WENN NICHT, DANN
     ‚* UNTER DEFAULT EINTRAGEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H102"                    TO CFID   OF CFG-CPY.
           MOVE     OPTYPE OF POPLLST-P       TO CFKEY  OF CFG-CPY.
           MOVE     X-OPUSER                  TO CFKEY2 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    MOVE "*DEFAULT"           TO OPTYPE OF POPLLST-P
           END-IF.

     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                    TO OPAUSR OF POPLLST-P.
           MOVE     T-TERM                    TO OPABS  OF POPLLST-P.
           MOVE     PGM-WRK                   TO OPAPGM OF POPLLST-P.
           MOVE     DATE-8                    TO OPADTA OF POPLLST-P.
           MOVE     TIME-6                    TO OPATIA OF POPLLST-P.

     ‚* MOD LÖSCHEN
           MOVE     SPACES                    TO OPMSER OF POPLLST-P.
           MOVE     SPACES                    TO OPMBS  OF POPLLST-P.
           MOVE     SPACES                    TO OPMPGM OF POPLLST-P.
           MOVE     ZEROES                    TO OPMDTA OF POPLLST-P.
           MOVE     ZEROES                    TO OPMTIA OF POPLLST-P.

           WRITE    POPLLST-P.

       WRI-OPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB USER SCHON SHARE-NUMBER HAT
     ‚*--------------------------------------------------------------
       CHECK-SHARE-NUMBER SECTION.
       CHK-SHN-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB SATZ SCHON VORHANDEN
           INITIALIZE POPLLSTF OF POPLLST-L.
           MOVE     OPSHRN OF POPLLST-P TO OPSHRN OF POPLLST-L.
           MOVE     X-OPUSER            TO OPUSER OF POPLLST-L.
           START    POPLLST-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-SHN-90
           END-IF.

       CHK-SHN-20.
           READ     POPLLST-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-SHN-90
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       OPSHRN OF POPLLST-L NOT = OPSHRN OF POPLLST-P
            OR      OPUSER OF POPLLST-L NOT = X-OPUSER
                    GO TO CHK-SHN-90
           END-IF.

     ‚* DATENSATZ SCHON VORHANDEN
           MOVE     "1" TO RET-CODE.

       CHK-SHN-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
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
