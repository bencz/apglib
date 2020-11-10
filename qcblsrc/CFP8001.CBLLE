       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERSTELLEN PCFGDAT-SÄTZE                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.08.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERSTELLEN PCFGDAT-SÄTZE                      *
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

       01  PGM-WRK                       PIC X(10) VALUE "CFP8001".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  DATUM-WRK                     LIKE DATE-8.
       01  DATUM-RED REDEFINES DATUM-WRK.
        05 JAHR                          PIC 9(4).
        05 MONAT                         PIC 9(2).
        05 TAG                           PIC 9(2).

       01  MONAT-SAV                     PIC 9(2).
       01  TAGMAX                        PIC 9(2).
       01  WOTA-WRK                      LIKE DTWOTA OF PCFGDAT-P.
       01  KW-WRK                        LIKE DTKW   OF PCFGDAT-P.

     ‚* SCHALTER FÜR SCHALTJAHRESPRÜFUNG
       01  SJA-VALUE                     PIC 9(9).
       01  SJA-REST                      PIC 9(9).
       01  SJA-4                         PIC 9(1).
       01  SJA-100                       PIC 9(1).
       01  SJA-400                       PIC 9(1).
       01  SJA-JAHR                      PIC 9(4).
       01  IS-SCHALTJAHR                 PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-DATUM-VON                   LIKE DATE-8.
       01  X-DATUM-BIS                   LIKE DATE-8.
       01  X-START-TAG                   PIC 9(1).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-DATUM-VON
                                X-DATUM-BIS
                                X-START-TAG.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP8001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCFGDAT-DP.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-DATUM-VON = ZEROES
            OR      X-DATUM-BIS = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* AUFADDIEREN DES DATUMS UND SCHREIBEN DER PCFGDAT-SÄTZE
           PERFORM  BERECHNEN-DATUM.

       ENDE.
           CLOSE                 PCFGDAT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUFADDIEREN DES DATUMS UND SCHREIBEN DER SÄTZE
     ‚*--------------------------------------------------------------
       BERECHNEN-DATUM SECTION.
       BER-DAT-00.

           INITIALIZE MONAT-SAV, TAGMAX.
           MOVE     X-DATUM-VON TO DATUM-WRK.
           MOVE     X-START-TAG TO WOTA-WRK.

       BER-DAT-20.
           IF       DATUM-WRK > X-DATUM-BIS
                    GO TO BER-DAT-90
           END-IF.

     ‚* ERMITTELN DES WOCHENTAGS VOM STARTDATUM 19700101 AUS
     ***   PERFORM  CHECK-WOCHENTAG.
     ***   PERFORM  CHECK-KALENDERWOCHE.

     ‚* SCHREIBEN DES DATENSATZES
           PERFORM  WRITE-PCFGDAT.
           ADD      1 TO TAG OF DATUM-RED.

     ‚* PRÜFEN WIEVIELE TAGE DIESER MONAT HAT
           PERFORM  CHECK-MONAT.
           IF       TAG OF DATUM-RED > TAGMAX
                    MOVE 1 TO TAG   OF DATUM-RED
                    ADD  1 TO MONAT OF DATUM-RED
                    IF   MONAT OF DATUM-RED >= 13
                         MOVE 1 TO MONAT OF DATUM-RED
                         ADD  1 TO JAHR  OF DATUM-RED
                    END-IF
           END-IF.

     ‚* WOCHENTAGE KÖNNEN DANACH EINFACH ADDIERT WERDEN
           ADD      1                 TO WOTA-WRK.
           IF       WOTA-WRK >= 8
                    MOVE 1 TO WOTA-WRK
           END-IF.

           GO TO    BER-DAT-20.

       BER-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DES PCFGDAT-SATZES
     ‚*--------------------------------------------------------------
       WRITE-PCFGDAT SECTION.
       WRI-TAB-00.

           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     DATUM-WRK         TO DTLFDN OF PCFGDAT-P.
           MOVE     KW-WRK            TO DTKW   OF PCFGDAT-P.
           MOVE     WOTA-WRK          TO DTWOTA OF PCFGDAT-P.

     ‚* USER/TERMINAL/DATUM FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER            TO DTAUSR OF PCFGDAT-P.
           MOVE     T-TERM            TO DTABS  OF PCFGDAT-P.
           MOVE     PGM-WRK           TO DTAPGM OF PCFGDAT-P.
           MOVE     DATE-8            TO DTADTA OF PCFGDAT-P.
           MOVE     TIME-6            TO DTATIA OF PCFGDAT-P.
           WRITE    PCFGDAT-P.

       WRI-TAB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES WOCHENTAGS
     ‚*--------------------------------------------------------------
       CHECK-WOCHENTAG SECTION.
       CHK-WTA-00.

           INITIALIZE RET-CODE.
     ‚* DER 01.01.1970 WAR EIN DONNERSTAG = 4

       CHK-WTA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN WIEVIELE TAGE DER MONAT HAT
     ‚*--------------------------------------------------------------
       CHECK-MONAT SECTION.
       CHK-MNT-00.

     ‚* GLEICHER MONAT? DANN NICHT NEU ERMITTELN
           IF       MONAT OF DATUM-RED = MONAT-SAV
                    GO TO CHK-MNT-90
           END-IF.

     ‚* WIR MACHEN ES UNS HIER DAS MAL GANZ EINFACH ...
           EVALUATE MONAT OF DATUM-RED
                    WHEN 1   MOVE 31 TO TAGMAX
                    WHEN 2   MOVE 28 TO TAGMAX
                    WHEN 3   MOVE 31 TO TAGMAX
                    WHEN 4   MOVE 30 TO TAGMAX
                    WHEN 5   MOVE 31 TO TAGMAX
                    WHEN 6   MOVE 30 TO TAGMAX
                    WHEN 7   MOVE 31 TO TAGMAX
                    WHEN 8   MOVE 31 TO TAGMAX
                    WHEN 9   MOVE 30 TO TAGMAX
                    WHEN 10  MOVE 31 TO TAGMAX
                    WHEN 11  MOVE 30 TO TAGMAX
                    WHEN 12  MOVE 31 TO TAGMAX
           END-EVALUATE.

     ‚* HABEN WIR EIN SCHALTJAHR?
           IF       MONAT OF DATUM-RED = 2
                    MOVE     JAHR OF DATUM-RED TO SJA-JAHR
                    PERFORM  CHECK-SCHALTJAHR
                    IF   IS-SCHALTJAHR NOT = ZEROES
                         MOVE 29 TO TAGMAX
                    END-IF
           END-IF.

     ‚* MONAT SICHERN
           MOVE     MONAT OF DATUM-RED TO MONAT-SAV.

       CHK-MNT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB EIN JAHR EIN SCHALTJAHR IST
     ‚*--------------------------------------------------------------
       CHECK-SCHALTJAHR SECTION.
       CHK-SJA-00.

           INITIALIZE IS-SCHALTJAHR.
           MOVE     ZEROES TO SJA-4, SJA-100, SJA-400.
     ‚* WENN DAS JAHR DURCH 100 OHNE REST GETEILT WERDEN KANN,
     ‚* DANN IST DAS JAHR KEIN SCHALTJAHR, AUßER ES IST DURCH 400
     ‚* TEILBAR
           MOVE     ZEROES TO SJA-VALUE, SJA-REST.
           DIVIDE   SJA-JAHR BY 4   GIVING SJA-VALUE
                    REMAINDER SJA-REST.
           IF       SJA-REST = ZEROES
                    MOVE 1 TO SJA-4
           ELSE
                    GO TO CHK-SJA-90
           END-IF.

     ‚* WENN DAS JAHR DURCH 100 OHNE REST GETEILT WERDEN KANN,
     ‚* DANN IST DAS JAHR KEIN SCHALTJAHR, AUßER ES IST DURCH 400
     ‚* TEILBAR
           MOVE     ZEROES TO SJA-VALUE, SJA-REST.
           DIVIDE   SJA-JAHR BY 100 GIVING SJA-VALUE
                    REMAINDER SJA-REST.
           IF       SJA-REST = ZEROES
                    MOVE 1 TO SJA-100
           END-IF.

     ‚* SCHAUEN WIR MAL OB ES DURCH 400 TEILBAR IST...
           MOVE     ZEROES TO SJA-VALUE, SJA-REST.
           DIVIDE   SJA-JAHR BY 400 GIVING SJA-VALUE
                    REMAINDER SJA-REST.
           IF       SJA-REST = ZEROES
                    MOVE 1 TO SJA-400
           END-IF.

     ‚* WENN DAS JAHR DURCH 100, ABER NICHT DURCH 400 TEILBAR IST,
     ‚* IST ES EIN SCHALTJAHR
           IF       SJA-4   NOT = ZEROES
                    MOVE 1 TO IS-SCHALTJAHR
                    IF   SJA-100 NOT = ZEROES
                     AND SJA-400     = ZEROES
                         MOVE ZEROES TO IS-SCHALTJAHR
                    END-IF
           END-IF.

       CHK-SJA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
