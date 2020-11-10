       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0016.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TEXTE ALS GELESEN MARKIEREN                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  18.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEXTE ALS GELESEN MARKIEREN                  *
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
     ‚* PHINTXT: HWPGM. HWUSER, HWLFPO
     ‚*-------------------------------------------------------------
           SELECT PHINTXT-DP
                  ASSIGN       TO  DATABASE-PHINTXT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PHINTXT: HWPGM. HWUSER, HWLFPO
       FD  PHINTXT-DP
           LABEL RECORDS ARE STANDARD.
       01  PHINTXT-P.
           COPY DDS-ALL-FORMATS OF PHINTXT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0016".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-HWPGM                       LIKE HWPGM  OF PHINTXT-P.
       01  X-HWLFPO                      LIKE HWLFPO OF PHINTXT-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-HWPGM
                                X-HWLFPO.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0016" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PHINTXT-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-HWPGM  = SPACES
            OR      X-HWLFPO = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* ALLE ALS GELESEN MARKIEREN
           PERFORM  UPDATE-RECORD.

       ENDE.
           CLOSE                 PHINTXT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* MARKIEREN ALLER DATENSÄTZE
     ‚*--------------------------------------------------------------
       UPDATE-RECORD SECTION.
       UPD-REC-00.

     ‚* ERMITTELN DES ERSTEN TAGES
           INITIALIZE PHINTXTF OF PHINTXT-P.
           MOVE     X-HWPGM             TO HWPGM  OF PHINTXT-P.
           MOVE     SPACES              TO HWUSER OF PHINTXT-P.
           MOVE     ZEROES              TO HWLFPO OF PHINTXT-P.
           START    PHINTXT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-REC-90
           END-IF.

       UPD-REC-20.
           READ     PHINTXT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-REC-90
           END-IF.

           IF       HWPGM  OF PHINTXT-P NOT = X-HWPGM
                    REWRITE PHINTXT-P
                    GO TO UPD-REC-90
           END-IF.

     ‚* NUR BESTIMMTE TEXTE
           IF       HWLFPO OF PHINTXT-P NOT = X-HWLFPO
                    REWRITE PHINTXT-P
                    GO TO UPD-REC-20
           END-IF.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* TEXT GELESEN
           MOVE     1                 TO HWFK01 OF PHINTXT-P.

     ‚* USER/DATUM FÜLLEN
           MOVE     T-USER            TO HWMSER OF PHINTXT-P.
           MOVE     T-TERM            TO HWMBS  OF PHINTXT-P.
           MOVE     PGM-WRK           TO HWMPGM OF PHINTXT-P.
           MOVE     DATE-8            TO HWMDTA OF PHINTXT-P.
           MOVE     TIME-6            TO HWMTIA OF PHINTXT-P.
           REWRITE  PHINTXT-P.

           GO TO    UPD-REC-20.

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
