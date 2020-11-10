     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CFP0004.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB INFOTEXTE VORHANDEN SIND                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  26.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN PTXTFIT                               *
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
     ‚*--- PTXTFIT: FTFILE, FTKEY, FTLFPO
       FD  PTXTFIT-DP
           LABEL RECORDS ARE STANDARD.
       01  PTXTFIT-P.
           COPY DDS-ALL-FORMATS OF PTXTFIT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0004".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-FTFILE                      LIKE FTFILE OF PTXTFIT-P.
       01  X-FTKEY                       LIKE FTKEY  OF PTXTFIT-P.
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-FTFILE
                                         X-FTKEY
                                         X-RETCODE.
     ‚*-------------------------------------------------------
       ANFANG.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PTXTFIT-DP
           END-IF.

           INITIALIZE X-RETCODE.
     ‚* JETZT PRÜFEN OB MIND. 1 DATENSATZ VORHANDEN IST
           IF       X-FTFILE NOT = SPACES
            AND     X-FTKEY  NOT = SPACES
                    PERFORM  CHECK-TEXTE
                    IF   DATEN = ZEROES
                         MOVE "1" TO X-RETCODE
                    END-IF
           ELSE
                    MOVE "1" TO X-RETCODE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       CHECK-TEXTE SECTION.
       CHK-TXT-00.

           INITIALIZE DATEN.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PTXTFITF OF PTXTFIT-P.
           MOVE     X-FTFILE            TO FTFILE OF PTXTFIT-P.
           MOVE     X-FTKEY             TO FTKEY  OF PTXTFIT-P.
           MOVE     ZEROES              TO FTLFPO OF PTXTFIT-P.
           START    PTXTFIT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TXT-90
           END-IF.

       CHK-TXT-20.
           READ     PTXTFIT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-TXT-90
           END-IF.

     ‚* KEY PRÜFEN
           IF       FTFILE OF PTXTFIT-P NOT = X-FTFILE
            OR      FTKEY  OF PTXTFIT-P NOT = X-FTKEY
                    GO TO CHK-TXT-90
           END-IF.

           MOVE     1 TO DATEN.

       CHK-TXT-90.
           EXIT.
      /
