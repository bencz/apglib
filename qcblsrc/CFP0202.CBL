       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0204.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LÖSCHEN EINES PROGRAMMTEXTES                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  17.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  LÖSCHEN PCFGTXT-SATZ                         *
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
     ‚* PHYSISCHE DATEI PROGRAMMTEXTE
     ‚*-------------------------------------------------------------
           SELECT PCFGTXT-DP
                  ASSIGN       TO  DATABASE-PCFGTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEXTE
       FD  PCFGTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGTXT-P.
           COPY DDS-ALL-FORMATS OF PCFGTXT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0202".

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TXSPR                       LIKE TXSPR  OF PCFGTXT-P.
       01  X-TXID                        LIKE TXID   OF PCFGTXT-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TXSPR
                                X-TXID.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0202" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCFGTXT-DP.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-TXSPR  = SPACES
            OR      X-TXID   = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE PCFGTXTF OF PCFGTXT-P.
           MOVE     X-TXSPR         TO TXSPR  OF PCFGTXT-P.
           MOVE     X-TXID          TO TXID   OF PCFGTXT-P.
           READ     PCFGTXT-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCFGTXT-DP
     ‚*             LÖSCHEN PROTOKOLLIEREN
                    MOVE CONST-LIB   TO LIB-WRK
                    MOVE "PCFGTXT"   TO FILE-WRK
                    MOVE PCFGTXT-P   TO RECORD-WRK
                    CALL "CFP8100" USING LIB-WRK
                                         FILE-WRK
                                         PGM-WRK
                                         RECORD-WRK
                    END-CALL
           END-IF.

       ENDE.
           CLOSE                 PCFGTXT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
