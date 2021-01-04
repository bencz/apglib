       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0019.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* xxx                                                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  XXXXXXXXXX                                   *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  XXXXX JJJJ                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
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

     ‚*----------------------------------------------------------------
     ‚*    BILDSCHIRMDATEI
     ‚*----------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-HLD0019DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
       DATA DIVISION.
       FILE SECTION.

       FD   WS-DISPLAY.
       01   WS-REC.
            COPY   DDS-ALL-FORMATS   OF HLD0019DE.
      /
       WORKING-STORAGE SECTION.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0019".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* WORKBEREICH HANDLING DISPLAY
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH STANDARDFELDER ALLE PROGRAMME
           COPY WRKSTD     OF APG-QCPYSRC.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  HLD0019DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF  HLD0019DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0019" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "HLD0019DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

           INITIALIZE FORMAT1.

       ANF040.
     ‚*€   MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
     ‚*€   MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F1 ENDE
           IF       IN01 IN FORMAT1-I-INDIC  = AN
                    MOVE "F1" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN02 IN FORMAT1-I-INDIC  = AN
                    MOVE "F2" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN04 IN FORMAT1-I-INDIC  = AN
                    MOVE "F4" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN05 IN FORMAT1-I-INDIC  = AN
                    MOVE "F5" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN06 IN FORMAT1-I-INDIC  = AN
                    MOVE "F6" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN07 IN FORMAT1-I-INDIC  = AN
                    MOVE "F7" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN08 IN FORMAT1-I-INDIC  = AN
                    MOVE "F8" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN09 IN FORMAT1-I-INDIC  = AN
                    MOVE "F9" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN10 IN FORMAT1-I-INDIC  = AN
                    MOVE "F10" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN11 IN FORMAT1-I-INDIC  = AN
                    MOVE "F11" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN12 IN FORMAT1-I-INDIC  = AN
                    MOVE "F12" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN13 IN FORMAT1-I-INDIC  = AN
                    MOVE "F13" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN14 IN FORMAT1-I-INDIC  = AN
                    MOVE "F14" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN15 IN FORMAT1-I-INDIC  = AN
                    MOVE "F15" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN16 IN FORMAT1-I-INDIC  = AN
                    MOVE "F16" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN17 IN FORMAT1-I-INDIC  = AN
                    MOVE "F17" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN18 IN FORMAT1-I-INDIC  = AN
                    MOVE "F18" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN19 IN FORMAT1-I-INDIC  = AN
                    MOVE "F19" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN20 IN FORMAT1-I-INDIC  = AN
                    MOVE "F20" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN21 IN FORMAT1-I-INDIC  = AN
                    MOVE "F21" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN22 IN FORMAT1-I-INDIC  = AN
                    MOVE "F22" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN23 IN FORMAT1-I-INDIC  = AN
                    MOVE "F23" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.
           IF       IN24 IN FORMAT1-I-INDIC  = AN
                    MOVE "F24" TO FKEY OF FORMAT1
                    GO TO ANF040
           END-IF.

     ‚* F3 ENDE
           IF       IN03 IN FORMAT1-I-INDIC  = AN
                    GO TO ENDE
           END-IF.

           GO TO    ANF040.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
