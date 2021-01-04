       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP0047.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE VERSCHIEDER STRING-OPERATIONEN                        *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.09.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE STRINGOPERATIONEN                    *
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
           COPY  DDS-FMT01-O OF XXD0047DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF XXD0047DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0047".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST   ".

       01  STRING-WRK                    LIKE INPSTR OF FORMAT1.
       01  LEN                           PIC 9(2).

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0047" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0047DE" TO DISPLAY-WRK.
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
           PERFORM  STRING-INPUT.
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
     ‚* EINGABE DES STRINGS
     ‚*--------------------------------------------------------------
       STRING-INPUT SECTION.
       STR-INP-00.

           INITIALIZE F3, F5, F12.
     ‚* LESEN ÜBERSCHRIFT
           INITIALIZE FORMAT1.
           MOVE     "XXP0047"         TO MAINUEB OF FORMAT1.
           MOVE     "XXP0047*1"       TO SUBUEB  OF FORMAT1.
     ‚* SUBSTRING MIT 1 VORBELEGEN
           MOVE     1                 TO SUBPOS  OF FORMAT1.

       STR-INP-10.
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
                    GO TO STR-INP-90
           END-IF.

     ‚* F12 ABBRECHEN ?
           IF       IN12 OF FORMAT1-I-INDIC  = AN
                    MOVE "1" TO F12
                    GO TO STR-INP-90
           END-IF.

     ‚* F20 PROGRAMMINFORMATIONEN
           IF       IN20 IN FORMAT1-I-INDIC = AN
                    PERFORM ANZEIGE-PGM-INFOS
                    GO TO STR-INP-10
           END-IF.

     ‚* PLAUSIPRÜFUNG AUF MINUS-WERTE UM DAS PGM DSM-SICHER ZU MACHEN
           IF       SUBPOS OF FORMAT1 < ZEROES
            OR      SUBLEN OF FORMAT1 < ZEROES
                    GO TO STR-INP-10
           END-IF.

     ‚* JETZT ALLE DATEN BERECHNEN
           PERFORM  CALCULATE-VALUES.

           GO TO    STR-INP-10.

       STR-INP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE ANZEIGEWERTE BERECHNEN
     ‚*--------------------------------------------------------------
       CALCULATE-VALUES SECTION.
       CAL-VAL-00.

     ‚* STRING UMDREHEN
           MOVE     FUNCTION REVERSE(INPSTR OF FORMAT1) TO
                    REVSTR OF FORMAT1.
     ‚* TRIM-FUNKTIONEN AUSFÜHREN
           MOVE     FUNCTION TRIM(INPSTR OF FORMAT1)    TO
                    STRIM  OF FORMAT1.
           MOVE     FUNCTION TRIML(INPSTR OF FORMAT1)   TO
                    STRIML OF FORMAT1.
           MOVE     FUNCTION TRIMR(INPSTR OF FORMAT1)   TO
                    STRIMR OF FORMAT1.
     ‚* UPPER/LOWER CASE-FUNKTIONEN AUSFÜHREN
           MOVE     FUNCTION UPPER-CASE(INPSTR OF FORMAT1) TO
                    STRUC  OF FORMAT1.
           MOVE     FUNCTION LOWER-CASE(INPSTR OF FORMAT1) TO
                    STRLC  OF FORMAT1.

     ‚* LEERZEICHEN VORNE ZÄHLEN
           MOVE     INPSTR OF FORMAT1 TO STRING-WRK.
           PERFORM  GET-LEADING-SPACES.
           MOVE     LEN               TO LSINP  OF FORMAT1.
           MOVE     REVSTR OF FORMAT1 TO STRING-WRK.
           PERFORM  GET-LEADING-SPACES.
           MOVE     LEN               TO LSREV  OF FORMAT1.

     ‚* LEERZEICHEN HINTEN ZÄHLEN
           MOVE     INPSTR OF FORMAT1 TO STRING-WRK.
           PERFORM  GET-TAILING-SPACES.
           MOVE     LEN               TO TSINP  OF FORMAT1.
           MOVE     REVSTR OF FORMAT1 TO STRING-WRK.
           PERFORM  GET-TAILING-SPACES.
           MOVE     LEN               TO TSREV  OF FORMAT1.

     ‚* GESAMTLÄNGE BERECHNEN
           IF       INPSTR OF FORMAT1 NOT = SPACES
                    MOVE LENGTH OF INPSTR OF FORMAT1 TO
                                   CALC11 OF FORMAT1
                    MOVE LSINP  OF FORMAT1         TO CALC12 OF FORMAT1
                    MOVE TSINP  OF FORMAT1         TO CALC13 OF FORMAT1
                    COMPUTE  LENINP OF FORMAT1 =   CALC11 OF FORMAT1
                                                 - CALC12 OF FORMAT1
                                                 - CALC13 OF FORMAT1
                    END-COMPUTE
           END-IF.

     ‚* GESAMTLÄNGE BERECHNEN
           IF       REVSTR OF FORMAT1 NOT = SPACES
                    MOVE LENGTH OF REVSTR OF FORMAT1 TO
                                   CALC21 OF FORMAT1
                    MOVE LSREV  OF FORMAT1         TO CALC22 OF FORMAT1
                    MOVE TSREV  OF FORMAT1         TO CALC23 OF FORMAT1
                    COMPUTE  LENREV OF FORMAT1 =   CALC21 OF FORMAT1
                                                 - CALC22 OF FORMAT1
                                                 - CALC23 OF FORMAT1
                    END-COMPUTE
           END-IF.

     ‚* IST DIE ANGABE OK?
           IF       SUBPOS OF FORMAT1 + SUBLEN OF FORMAT1 <=
                    LENGTH OF INPSTR OF FORMAT1 + 1
            AND     SUBPOS OF FORMAT1 NOT = ZEROES
                    MOVE SUBPOS OF FORMAT1 TO SUB1   OF FORMAT1
                    MOVE SUBLEN OF FORMAT1 TO SUB2   OF FORMAT1
                    IF   SUBLEN OF FORMAT1 = ZEROES
                         MOVE INPSTR OF FORMAT1(SUBPOS OF FORMAT1:) TO
                              SUBSTR OF FORMAT1
                    ELSE
                         MOVE INPSTR OF FORMAT1(SUBPOS OF FORMAT1:
                                                SUBLEN OF FORMAT1) TO
                              SUBSTR OF FORMAT1
                    END-IF
           ELSE
                    MOVE SPACES TO SUBSTR OF FORMAT1
                    MOVE ZEROES TO SUB1   OF FORMAT1
                    MOVE ZEROES TO SUB2   OF FORMAT1
           END-IF.

     ‚* FÜHRENDE LEERZEICHEN
           MOVE     ZEROES TO INSLSP OF FORMAT1.
           INSPECT  INPSTR OF FORMAT1 TALLYING INSLSP OF FORMAT1
                    FOR LEADING SPACES.

     ‚* ZEICHEN FOR DEM *
           MOVE     ZEROES TO INSCBI OF FORMAT1.
           INSPECT  INPSTR OF FORMAT1 TALLYING INSCBI OF FORMAT1
                    FOR CHARACTERS BEFORE INITIAL "*".

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
     ‚*--------------------------------------------------------------
     ‚* FÜHRENDE LEERZEICHEN ERMITTELN
     ‚*--------------------------------------------------------------
       GET-LEADING-SPACES SECTION.
       GET-LES-00.

           INITIALIZE LEN.
           INSPECT  STRING-WRK TALLYING LEN FOR LEADING SPACES.

       GET-LES-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* HINTERE LEERZEICHEN ERMITTELN
     ‚*--------------------------------------------------------------
       GET-TAILING-SPACES SECTION.
       GET-LES-00.

           MOVE     LENGTH OF STRING-WRK TO LEN.
           PERFORM  UNTIL LEN = ZEROES
                     OR STRING-WRK(LEN:1) NOT = SPACES
                    SUBTRACT 1 FROM LEN
           END-PERFORM.

     ‚* GESAMTLÄNGE MINUS GEFÜLLTER LÄNGE
           COMPUTE  LEN =   LENGTH OF STRING-WRK
                          - LEN
           END-COMPUTE.

       GET-LES-90.
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
