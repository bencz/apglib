       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0202.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN DES ERSTEN WOCHENTAGES                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  04.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN DES ERSTEN WOCHENTAGES             *
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
     ‚* PHYSISCHE DATEI KÜCHENDIENST
     ‚*-------------------------------------------------------------
           SELECT PKDPLAN-DP
                  ASSIGN       TO  DATABASE-PKDPLAN
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- KÜCHENDIENST
       FD  PKDPLAN-DP
           LABEL RECORDS ARE STANDARD.
       01  PKDPLAN-P.
           COPY DDS-ALL-FORMATS OF PKDPLAN.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0204".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

     ‚* PARAMETER FÜR DAS CFP8100
       01  PARCHIV-WRK.
            COPY DDS-ALL-FORMATS OF PARCHIV.
       01  LIB-WRK                       LIKE HSLIB  OF PARCHIV-WRK.
       01  FILE-WRK                      LIKE HSFILE OF PARCHIV-WRK.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-KDDATE                      LIKE KDDATE OF PKDPLAN-P.
       01  X-VON-DATE                    LIKE KDDATE OF PKDPLAN-P.
       01  X-BIS-DATE                    LIKE KDDATE OF PKDPLAN-P.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-KDDATE
                                X-VON-DATE
                                X-BIS-DATE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0204" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     INPUT PKDPLAN-DP
                          PCFGDAT-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-KDDATE = ZEROES
            OR      X-KDTYP  = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FIRST-DAY, WOTA-WRK.
     ‚* ERMITTELN DES AKTUELLEN TAGES
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-DB   TO DTLFDN OF PCFGDAT-P.
           READ     PCFGDAT-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

     ‚* ERSTEN TAG BERECHNEN
           MOVE     DATE-DB             TO FIRST-DAY.
           MOVE     DTWOTA OF PCFGDAT-P TO WOTA-WRK.
           PERFORM  WOCHENTAG-RUNTERRECHNEN.

           INITIALIZE COUNTER.
     ‚* JETZT VOM ERSTEN TAG AN, DIE GANZE WOCHE LESEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     FIRST-DAY TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

       GET-FWD-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-FWD-90
           END-IF.

     ‚* MONTAG - FREITAG FÜLLEN, NICHT MEHR
           ADD      1 TO COUNTER.
           IF       COUNTER <= MAX-TAGE
                    MOVE     DTLFDN OF PCFGDAT-P TO MAX-DATE
                    GO TO GET-FWD-20
           END-IF.

           IF       COUNTER <= MAX-TAGE-REAL
                    MOVE     DTLFDN OF PCFGDAT-P TO MAX-DATE-REAL
                    GO TO GET-FWD-20
           END-IF.

       ENDE.
           CLOSE                 PKDPLAN-DP
                                 PCFGDAT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* WOCHENTAGE RUNTERRECHNEN
     ‚*--------------------------------------------------------------
       WOCHENTAG-RUNTERRECHNEN SECTION.
       WOT-REC-00.

           INITIALIZE COUNTER.
     ‚* READ PRIOR
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATE-DB     TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WOT-REC-90
           END-IF.

       WOT-REC-20.
           READ     PCFGDAT-DP PRIOR RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WOT-REC-90
           END-IF.

     ‚* MONTAG - FREITAG FÜLLEN, NICHT MEHR
           ADD      1 TO COUNTER.
           IF       COUNTER >= WOTA-WRK
                    GO TO WOT-REC-90
           END-IF.

           MOVE     DTLFDN OF PCFGDAT-P TO FIRST-DAY.
           GO TO    WOT-REC-20.

       WOT-REC-90.
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
