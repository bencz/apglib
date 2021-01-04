       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0049.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE VERSCHIEDER EDITIERCODES                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.11.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE EDITIERCODES                         *
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
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA,
                         ATTRIBUTE-DATA IS BS-ATRIBUTE,
                         I-O-FEEDBACK   IS FILE-IO-FEEDBACK.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

       01  INXX.
        05 IN01                          PIC 1.
        05 IN02                          PIC 1.
        05 IN03                          PIC 1.
        05 IN40                          PIC 1.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF XXD0049DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0049DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0049".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST   ".

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0049" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0049DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* ALLE DATEIEN ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
           END-IF.

           MOVE     AUS TO INXX.
           INITIALIZE F3, F12.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ANF020.
           PERFORM  NUMERIC-INPUT.
           IF       F3  = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

     ‚* WIEDERHOLEN BIS USER BEENDET ...
           GO TO    ANF020.

       ENDE.
     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* EINGABE DER ZAHL
     ‚*--------------------------------------------------------------
       NUMERIC-INPUT SECTION.
       NUM-INP-00.

           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           INITIALIZE FORMAT1.
           MOVE     "XXP0049"              TO MAINUEB OF FORMAT1.
           MOVE     "XXP0049*1"            TO SUBUEB  OF FORMAT1.
           MOVE     -123456789012345,67890 TO EINGABE OF FORMAT1.
           PERFORM  CALCULATE-VALUES.

       NUM-INP-10.
           MOVE     AUS TO INXX.
     ‚* SIND INFORMATIONEN VORHANDEN?
           CALL     "CFP0010" USING PGM-WRK
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    MOVE AN  TO IN40 OF INXX
           ELSE
                    MOVE AUS TO IN40 OF INXX
           END-IF.

           MOVE     CORR INXX       TO  FORMAT1-O-INDIC.
           MOVE     "FMT01"         TO  FMT-WRK.
           MOVE     FORMAT1         TO  WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK       TO FORMAT1-I.
           MOVE     CORR FORMAT1-I  TO FORMAT1.
           MOVE     AUS TO INXX.
           INITIALIZE DSP-ATTR.

     ‚* F3 GEDRÜCKT ?
           IF       IN03 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F3
                    GO TO NUM-INP-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO NUM-INP-90
           END-IF.

     ‚* F20 PROGRAMMINFORMATIONEN
           IF       IN20 IN FORMAT1-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO NUM-INP-10
           END-IF.

     ‚* JETZT ALLE DATEN BERECHNEN
           PERFORM  CALCULATE-VALUES.

           GO TO    NUM-INP-10.

       NUM-INP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE ANZEIGEWERTE BERECHNEN
     ‚*--------------------------------------------------------------
       CALCULATE-VALUES SECTION.
       CAL-VAL-00.

     ‚* EINGABE ÜBERNEHMEN
           MOVE     EINGABE OF FORMAT1 TO EDTCDE1 OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE2 OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE3 OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE4 OF FORMAT1.

           MOVE     EINGABE OF FORMAT1 TO EDTCDEA OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEB OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEC OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDED OF FORMAT1.

           MOVE     EINGABE OF FORMAT1 TO EDTCDEJ OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEK OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEL OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEM OF FORMAT1.

           MOVE     EINGABE OF FORMAT1 TO EDTCDEN OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEO OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEP OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDEQ OF FORMAT1.

     ‚* BENUTZERDEFINIERTE EDITIERCODES
           MOVE     EINGABE OF FORMAT1 TO EDTCDE5 OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE6 OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE7 OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE8 OF FORMAT1.
     ‚*€   MOVE     EINGABE OF FORMAT1 TO EDTCDE9 OF FORMAT1.

     ‚* SONSTIGE EDITIERCODES
           MOVE     EINGABE OF FORMAT1 TO EDTCDEZ OF FORMAT1.
           MOVE     EINGABE OF FORMAT1 TO EDTCDE  OF FORMAT1.

       VAL-CAL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN VON INFORMATIONSTEXTEN
     ‚*--------------------------------------------------------------
       ANZEIGE-PGM-INFOS SECTION.
       ANZ-PGM-00.

           INITIALIZE F12.
     ‚* INFORMATIONEN ANZEIGEN
           CALL     "CFP0009" USING PGM-WRK
                                    F12
           END-CALL.
           INITIALIZE F12.

       ANZ-PGM-90.
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
     ‚*---------------------------------------------------------------
     ‚* COPY-DSP-*: ALLE DISPLAYROUTINEN
     ‚*---------------------------------------------------------------

           COPY     DSPWORKING OF APG-QCPYSRC.
      /
