       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0021.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TEXT TEILEN                                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.12.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEXT TEILEN                                  *
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
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0021".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  LEN-TXT                       PIC 9(3).
       01  LEN-FLD                       PIC 9(3).
       01  LEN-ADD                       PIC 9(3).
       01  POS                           PIC 9(3).
       01  POS-TMP                       PIC 9(3).
       01  TEXT-WRK                      PIC X(500).
       01  ENDE-TEILEN                   PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-TEXT-INPUT                  LIKE TEXT-WRK.
       01  X-MAXLEN-RECORD               PIC 9(4).
       01  X-OUTPUT-TAB.
        05 TAB-MAX                       PIC 9(4).
        05 TAB-REC                       OCCURS 100 TIMES.
         06 ZEILE                        PIC X(100).
       01  X-RETCODE                     LIKE RET-CODE.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-TEXT-INPUT
                                X-MAXLEN-RECORD
                                X-OUTPUT-TAB
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETCODE.
           INITIALIZE X-OUTPUT-TAB.
           INITIALIZE TAB-MAX OF X-OUTPUT-TAB.
     ‚* TEXT ÜBERHAUPT VORHANDEN?
           IF       X-TEXT-INPUT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE LEN-FLD.
     ‚* DATEN ÜBERNEHMEN
           MOVE     X-TEXT-INPUT    TO TEXT-WRK.
           MOVE     X-MAXLEN-RECORD TO LEN-FLD.

     ‚* FÜLLEN DER LÄNGE DES TEXTFELDES IM SUBFILE UND DER LÄNGE
     ‚* DES FEHLERTEXTES IN HILFSVARIABLEN
           INITIALIZE ENDE-TEILEN.
           MOVE     1 TO POS.
           PERFORM  WITH TEST BEFORE VARYING LEN-TXT FROM LENGTH OF
                    TEXT-WRK BY -1 UNTIL LEN-TXT = ZEROES OR
                    TEXT-WRK(LEN-TXT:1) NOT = SPACES
                    CONTINUE
           END-PERFORM.

       ANF010.
     ‚* SPACES AM ANFANG EINER NEUEN ZEILE WERDEN ÜBERSPRUNGEN
           IF       TEXT-WRK(POS:1) = SPACE
                    ADD 1 TO POS
                    IF   POS = LEN-TXT
                         GO TO ENDE
                    ELSE
                         GO TO ANF010
                    END-IF
           END-IF.

     ‚* KOMMT NACH DER AKTUELLEN ZEILE EIN SPACE ODER ENDET SIE MIT
     ‚* EINEM SATZZEICHEN, SO KANN DIREKT INS SFL GESCHRIEBEN WERDEN
           IF       ENDE-TEILEN = 1
            OR      TEXT-WRK(POS + LEN-FLD:1)     = SPACE
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = SPACE
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = "."
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = ","
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = ":"
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = ";"
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = "-"
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = "!"
            OR      TEXT-WRK(POS + LEN-FLD - 1:1) = "?"
                    MOVE LEN-FLD TO LEN-ADD
           ELSE
     ‚*             VON AKTUELLER POSITION AUS NACH VORNE SUCHEN UM
     ‚*             NÄCHSTES LÜCKE ZUM TRENNEN ZU FINDEN
                    COMPUTE POS-TMP = POS + LEN-FLD - 1
                    PERFORM WITH TEST BEFORE UNTIL POS-TMP = POS
                            OR TEXT-WRK(POS-TMP:1) = SPACE
                            OR TEXT-WRK(POS-TMP:1) = "."
                            OR TEXT-WRK(POS-TMP:1) = ","
                            OR TEXT-WRK(POS-TMP:1) = ":"
                            OR TEXT-WRK(POS-TMP:1) = ";"
                            OR TEXT-WRK(POS-TMP:1) = "-"
                            OR TEXT-WRK(POS-TMP:1) = "!"
                            OR TEXT-WRK(POS-TMP:1) = "?"
                            SUBTRACT 1 FROM POS-TMP
                    END-PERFORM
     ‚*             TRENNZEICHEN GEFUNDEN?
                    IF   POS-TMP NOT = POS
     ‚*                  LÄNGE BIS ZUM TRENNZEICHEN ALS
     ‚*                  HINZUZUFÜGENDE LÄNGE VERWENDEN
                         COMPUTE LEN-ADD = POS-TMP - POS
                    ELSE
     ‚*                  KEIN TRENNZEICHEN GEFUNDEN. DANN MUSS DER
     ‚*                  STRING UNTERBROCHEN WERDEN
                         MOVE LEN-FLD TO LEN-ADD
                    END-IF
           END-IF.

     ‚* AUFBEREITEN UND SCHREIBEN SUBFILE-ZEILE
           ADD      1 TO TAB-MAX OF X-OUTPUT-TAB.
           IF       TAB-MAX OF X-OUTPUT-TAB > 100
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

           INITIALIZE ZEILE OF TAB-REC(TAB-MAX OF X-OUTPUT-TAB).
           MOVE     TEXT-WRK(POS:LEN-ADD) TO
                    ZEILE OF TAB-REC(TAB-MAX OF X-OUTPUT-TAB).

           ADD      LEN-ADD TO POS.
           IF       POS < LEN-TXT
            AND     ENDE-TEILEN = ZEROES
     ‚*             BERECHNEN WIE VIELE STELLEN NOCH ÜBRIG SIND
                    IF   POS + LEN-FLD >= LENGTH OF TEXT-WRK
                         MOVE 1 TO ENDE-TEILEN
                         COMPUTE LEN-FLD = LEN-TXT - POS
                    END-IF
                    GO TO ANF010
           END-IF.

       ENDE.
           GOBACK.
      /
