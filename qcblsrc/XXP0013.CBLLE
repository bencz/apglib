     ˆ* BAUSTELLE
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              PGMDTAQ.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUTOR         :  M. SCHATTE                                   *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  06. APRIL 2010                               *
     ‚*                                                               *
     ‚* AUFGABE       :  Anzeige Einträge in einer DTAQ.              *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
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
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-DSPDTAQ
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----BILDSCHIRM
       FD  WS-DISPLAY.
       01  WS-REC.
           COPY DDS-ALL-FORMATS OF DSPDTAQ.
      /
       WORKING-STORAGE SECTION.
     ‚* STANDARDFELDER ALLE PROGRAMME
           COPY DCLSTDVAR OF QCPYSRC.

     ‚* STANDARD DISPLAYFELDER
           COPY DCLDSPVAR OF QCPYSRC.

     ‚* QMHQRDQD API STRUKTUR AUS QSYSINC KOPIEREN
           COPY QMHQRDQD OF QSYSINC-QCBLLESRC.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF DSPDTAQ
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-INDIC.

       01  ERROR-REC.
           05 BYTESPROV                     PIC 9(8) BINARY.
           05 BYTESAVAIL                    PIC 9(8) BINARY.
           05 EXCEPTID                      PIC X(7).
           05 FILLER1                       PIC X(1).
           05 FILLER2                       PIC X(16).

       01  DTAQ.
        05 DTAQ-NAME                      PIC X(10).
        05 DTAQ-LIB                       PIC X(10) VALUE "*LIBL".

       01  ANZ-API                        PIC 9(5) COMP-3.
       01  ANZ-CUR-ALLE                   PIC 9(5) COMP-3.
       01  ANZ-CUR-IA                     PIC 9(5) COMP-3.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           OPEN     I-O WS-DISPLAY.

     ‚* FORMAT AUSGEBEN
           PERFORM  FORMAT1-ANZEIGEN.

       ENDE.
           CLOSE    WS-DISPLAY.
           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* EINTRAG ÄNDERN
     ‚*--------------------------------------------------------------
       FORMAT1-ANZEIGEN SECTION.
       FM1-ANZ-00.

           INITIALIZE FORMAT1.

           MOVE     "LGBATFREE"  TO DTAQ1 OF FORMAT1.
           MOVE     "LGBATALL"   TO DTAQ2 OF FORMAT1.
           MOVE     "LGBATALL2"  TO DTAQ3 OF FORMAT1.
           MOVE     "LGBAT01"    TO DTAQ4 OF FORMAT1.
           MOVE     "LGBAT02"    TO DTAQ5 OF FORMAT1.
           MOVE     "LGBAT03"    TO DTAQ6 OF FORMAT1.

       FM1-ANZ-10.
           MOVE     ZEROES TO ANZ1API  OF FORMAT1,
                              ANZ2API  OF FORMAT1
                              ANZ3API  OF FORMAT1
                              ANZ4API  OF FORMAT1
                              ANZ5API  OF FORMAT1
                              ANZ6API  OF FORMAT1
                              ANZ7API  OF FORMAT1
                              ANZ8API  OF FORMAT1
                              ANZ9API  OF FORMAT1
                              ANZ10API OF FORMAT1
                              ANZ11API OF FORMAT1
                              ANZ12API OF FORMAT1
                              ANZ13API OF FORMAT1
                              ANZ14API OF FORMAT1
                              ANZ15API OF FORMAT1
                              ANZ1GES  OF FORMAT1
                              ANZ2GES  OF FORMAT1
                              ANZ3GES  OF FORMAT1
                              ANZ4GES  OF FORMAT1
                              ANZ5GES  OF FORMAT1
                              ANZ6GES  OF FORMAT1
                              ANZ7GES  OF FORMAT1
                              ANZ8GES  OF FORMAT1
                              ANZ9GES  OF FORMAT1
                              ANZ10GES OF FORMAT1
                              ANZ11GES OF FORMAT1
                              ANZ12GES OF FORMAT1
                              ANZ13GES OF FORMAT1
                              ANZ14GES OF FORMAT1
                              ANZ15GES OF FORMAT1
                              ANZ1IA   OF FORMAT1
                              ANZ2IA   OF FORMAT1
                              ANZ3IA   OF FORMAT1
                              ANZ4IA   OF FORMAT1
                              ANZ5IA   OF FORMAT1
                              ANZ6IA   OF FORMAT1
                              ANZ7IA   OF FORMAT1
                              ANZ8IA   OF FORMAT1
                              ANZ9IA   OF FORMAT1
                              ANZ10IA  OF FORMAT1
                              ANZ11IA  OF FORMAT1
                              ANZ12IA  OF FORMAT1
                              ANZ13IA  OF FORMAT1
                              ANZ14IA  OF FORMAT1
                              ANZ15IA  OF FORMAT1.

     ‚* DATEN DER EINZELNEN DTAQS ERMITTELN
           IF       DTAQ1 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ1  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ1API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ1GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ1IA   OF FORMAT1
           END-IF.

           IF       DTAQ2 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ2  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ2API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ2GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ2IA   OF FORMAT1
           END-IF.

           IF       DTAQ3 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ3  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ3API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ3GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ3IA   OF FORMAT1
           END-IF.

           IF       DTAQ4 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ4  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ4API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ4GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ4IA   OF FORMAT1
           END-IF.

           IF       DTAQ5 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ5  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ5API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ5GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ5IA   OF FORMAT1
           END-IF.

           IF       DTAQ6 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ6  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ6API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ6GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ6IA   OF FORMAT1
           END-IF.

           IF       DTAQ7 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ7  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ7API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ7GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ7IA   OF FORMAT1
           END-IF.

           IF       DTAQ8 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ8  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ8API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ8GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ8IA   OF FORMAT1
           END-IF.

           IF       DTAQ9 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ9  OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ9API  OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ9GES  OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ9IA   OF FORMAT1
           END-IF.

           IF       DTAQ10 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ10 OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ10API OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ10GES OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ10IA  OF FORMAT1
           END-IF.

           IF       DTAQ11 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ11 OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ11API OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ11GES OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ11IA  OF FORMAT1
           END-IF.

           IF       DTAQ12 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ12 OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ12API OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ12GES OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ12IA  OF FORMAT1
           END-IF.

           IF       DTAQ13 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ13 OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ13API OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ13GES OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ13IA  OF FORMAT1
           END-IF.

           IF       DTAQ14 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ14 OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ14API OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ14GES OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ14IA  OF FORMAT1
           END-IF.

           IF       DTAQ15 OF FORMAT1 NOT = SPACES
                    MOVE DTAQ15 OF FORMAT1 TO DTAQ-NAME
                    PERFORM ERMITTELN-DTAQ-DATEN
                    MOVE ANZ-API           TO ANZ15API OF FORMAT1
                    MOVE ANZ-CUR-ALLE      TO ANZ15GES OF FORMAT1
                    MOVE ANZ-CUR-IA        TO ANZ15IA  OF FORMAT1
           END-IF.

     ‚*€   MOVE     CORR INXX TO FMT01-O-INDIC.
           MOVE     CORR FORMAT1 TO FMT01-O.
           WRITE    WS-REC FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I TO FORMAT1.
     ‚*€   MOVE     AUS  TO INXX.

     ‚* F3 = ENDE PGM
           IF       IN03 OF FMT01-I = AN
                    MOVE "1" TO F3
                    GO TO FM1-ANZ-90
           END-IF.

     ‚* F12 = ENDE PGM
           IF       IN12 OF FMT01-I = AN
                    GO TO FM1-ANZ-90
           END-IF.

           GO TO    FM1-ANZ-10.

       FM1-ANZ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DTAQ DATEN AUS API UND PCURDQE ERMITTELN
     ‚*--------------------------------------------------------------
       ERMITTELN-DTAQ-DATEN SECTION.
       ERM-DTQ-00.

           INITIALIZE QMH-QRDQD-RDQD0100,
                      ANZ-API,
                      ANZ-CUR-ALLE,
                      ANZ-CUR-IA.

           CALL     "QMHQRDQD" USING   QMH-QRDQD-RDQD0100
                               CONTENT LENGTH OF QMH-QRDQD-RDQD0100
                                       "RDQD0100  "
                                       DTAQ
           END-CALL.

           MOVE     NUMBER-MESSAGES OF QMH-QRDQD-RDQD0100 TO ANZ-API.

     ‚*€   CALL     "SQLANZDTQ" USING DTAQ-NAME
     ‚*€                              ANZ-CUR-ALLE
     ‚*€                              ANZ-CUR-IA
     ‚*€   END-CALL.

       ERM-DTQ-90.
           EXIT.
      /
