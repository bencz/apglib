       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP9033.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SEQUENZ FÜLLEN                                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  NACHTRÄGLICHES FÜLLEN DER KW                 *
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
     ‚* PHYSISCHE DATEI DATUM
     ‚*-------------------------------------------------------------
           SELECT PCFGDAT-DP
                  ASSIGN       TO  DATABASE-PCFGDAT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- DATUMS-TABELLE
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9033".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

       01  DATE-WRK                      LIKE DATE-8.
       01  KW-WRK                        PIC 9(2).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           OPEN     I-O   PCFGDAT-DP.

           PERFORM  CHANGE-PCFGDAT.

       ENDE.

           CLOSE                 PCFGDAT-DP.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ÄNDERN DER PCFGDAT
     ‚*--------------------------------------------------------------
       CHANGE-PCFGDAT SECTION.
       CHG-DAT-00.

     ‚* PKDPLAN LESEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     ZEROES              TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-DAT-90
           END-IF.

       CHG-DAT-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-DAT-90
           END-IF.

     ‚* NÄCHSTE LAUFENDE NUMMER ERMITTELN
           MOVE     "PSEQNBR" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK TO DTSEQ  OF PCFGDAT-P.

           REWRITE  PCFGDAT-P.

           GO TO    CHG-DAT-20.

       CHG-DAT-90.
           EXIT.
      /
