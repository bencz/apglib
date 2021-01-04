       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8009.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DATUMSUNTERSCHIED BERECHNEN                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  16.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BERECHNEN DES TAGESUNERSCHIED                *
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
     ‚* PCFGDAT: DTLFDN
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
     ‚*- PCFGDAT: DTLFDN
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0022".

       01  DATE-WRK                      LIKE DATE-8.

       01  STR-SEQ                       LIKE DTSEQ  OF PCFGDAT-P.
       01  END-SEQ                       LIKE DTSEQ  OF PCFGDAT-P.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-STR-DATE                    LIKE DATE-8.
       01  X-END-DATE                    LIKE DATE-8.
       01  X-RET-DAYS                    LIKE DIFF-DAYS.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-STR-DATE
                                X-END-DATE
                                X-RET-DAYS.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN INPUT PCFGDAT-DP
           END-IF.

           INITIALIZE X-RET-DAYS.
     ‚* DIFFERENZ BERECHNEN
           IF       X-STR-DATE NOT = ZEROES
            AND     X-END-DATE NOT = ZEROES
                    PERFORM GET-DIFFERENCE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZAHL TAGE ERMITTELN
     ‚*--------------------------------------------------------------
       GET-DIFFERENCE SECTION.
       GET-DIF-00.

     ‚* STARTDATUM LESEN
           MOVE     X-STR-DATE   TO DATE-WRK.
           PERFORM  READ-PCFGDAT.
           MOVE     DTSEQ  OF PCFGDAT-P TO STR-SEQ.

     ‚* ENDEDATUM LESEN
           MOVE     X-END-DATE   TO DATE-WRK.
           PERFORM  READ-PCFGDAT.
           MOVE     DTSEQ  OF PCFGDAT-P TO END-SEQ.

     ‚* DIFFERNEZ BERECHNEN
           COMPUTE  X-RET-DAYS =   END-SEQ
                                 - STR-SEQ
           END-COMPUTE.

       GET-DIF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATENSATZ LESEN
     ‚*--------------------------------------------------------------
       READ-PCFGDAT SECTION.
       REA-DAT-00.

           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-WRK        TO DTLFDN OF PCFGDAT-P.
           READ     PCFGDAT-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGDATF OF PCFGDAT-P
           END-IF.

       REA-DAT-90.
           EXIT.
      /
