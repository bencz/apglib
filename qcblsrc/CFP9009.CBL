       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9009.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DATUM PRÜFEN                                                  *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DATUM PRÜFEN                                 *
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
     ‚*--- DATUM
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9009".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-DATE-NUM-8                  LIKE DATE-DB.
       01  X-RETCODE                     LIKE DATUM-RETCODE.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-DATE-NUM-8
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9009" TO PGM-WRK.

           OPEN     INPUT PCFGDAT-DP.

           INITIALIZE X-RETCODE.
     ‚* DATUM NICHT GEFÜLLT?
           IF       X-DATE-NUM-8 = ZEROES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* DATENBANK LESEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     X-DATE-NUM-8   TO DTLFDN OF PCFGDAT-P.
           READ     PCFGDAT-DP.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "2" TO X-RETCODE
                    GO TO ENDE
           END-IF.

       ENDE.
           CLOSE    PCFGDAT-DP.

           GOBACK.
      /
