       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0018.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ZAHL IN TEXT KONVERTIEREN (LGPGM350)                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.07.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ZAHL IN TEXT KONVERTIEREN                    *
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
     ‚*
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0018".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01   NUM-WRK                       PIC X(31).
       01   NUM-RED REDEFINES NUM-WRK     PIC S9(21)V9(10).
     ‚* ANZAHL VORKOMMASTELLEN
       01   LEN-BEFORE                    PIC 9(2).

     ‚* DIVERSE WORKFELDER FÜR VERGLEICHE UND ZEICHENMANIPLUATION
       01   CHAR-WRK                      PIC X(1).
       01   CHAR-RED REDEFINES CHAR-WRK   PIC 9(1).
       01   CHAR-NUM                      PIC 9(3).
       01   LEN1-WRK                      PIC 9(2).
       01   LEN2-WRK                      PIC 9(2).
       01   LEN3-WRK                      PIC 9(2).
       01   POS-WRK                       PIC 9(2).
       01   DECPOS-WRK                    PIC X(1).

     ‚* FELDER FÜR DIE AUFBEREITUNG DES AUSGABEFELDS
       01   OUTPUT-WRK                    PIC X(40).
       01   OUTPUT-TMP                    PIC X(40).
       01   LEN-BEFORE-WRK                PIC 9(2).
       01   LEN-DECIMAL-WRK               PIC 9(2).
       01   LEN-OUTPUT                    PIC 9(2).

     ‚* VORZEICHEN VORHANDEN?
       01   NUM-NEGATIVE                  PIC 9(1).

       01  NEGATIV-KZ1                   PIC 9(3).
       01  NEGATIV-KZ2                   PIC 9(3).

     ‚* GEWÜNSCHTE OPTIONEN FÜR FORMATIERUNG LAUT EDITCODE
       01   OPTION-REC.
        05  NULL-VALUES                   PIC 9(1).
        05  DECIMAL-POS                   PIC 9(1).
        05  SIGN-POS                      PIC 9(1).
        05  DECIMAL-SEP                   PIC 9(1).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚* ZU KONVERTIERENDE NUMMER
       01   X-NUM-INPUT                   PIC S9(21)V9(10).
     ‚* AUSGABESTRING
       01   X-NUM-OUTPUT                  PIC X(40).
     ‚* LÄNGE DES ZIELFELDES FÜR DIE KONVERTIERTE ZAHL
       01   X-FMT-OUTPUT-LEN              PIC 9(2).
     ‚* GEWÜNSCHTE AUSGABEZAHL - ANZAHL STELLEN
       01   X-FMT-PRECISION               PIC 9(2).
     ‚* GEWÜNSCHTE AUSGABEZAHL - DAVON NACHKOMMA
       01   X-FMT-DECIMAL                 PIC 9(2).
     ‚* EDITIERCODE
       01   X-FMT-EDTCDE                  PIC X(1).
     ‚* SOLL BEI KÜRZUNG DER NACHKOMMASTELLEN GERUNDET WERDEN
       01   X-FMT-ROUNDED                 PIC 9(1).
     ‚* DEZIMALFORMAT (WIE IN CHGJOB: BLANK, J, I)
       01   X-FMT-DECFMT                  PIC X(1).
     ‚* AUSGABE LINKSBÜNDIG AUSRICHTEN 0 = NEIN, 1 = JA
       01   X-FMT-JUSTIFIED               PIC 9(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING    X-NUM-INPUT
                                   X-NUM-OUTPUT
                                   X-FMT-OUTPUT-LEN
                                   X-FMT-PRECISION
                                   X-FMT-DECIMAL
                                   X-FMT-EDTCDE
                                   X-FMT-ROUNDED
                                   X-FMT-DECFMT
                                   X-FMT-JUSTIFIED.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0018" TO PGM-WRK.

     ‚* DIE CHARACTER-WERTE DES MINUS-ZEICHENS
     ‚* JE NACH BETRIEBSSYSTEM SETZEN
           IF       BETRIEBSSYSTEM > ZEROES
                    MOVE 86  TO NEGATIV-KZ1
                    MOVE 41  TO NEGATIV-KZ2
           ELSE
                    MOVE 240 TO NEGATIV-KZ1
                    MOVE 32  TO NEGATIV-KZ2
           END-IF.

           INITIALIZE X-NUM-OUTPUT.
           INITIALIZE OUTPUT-WRK.
           INITIALIZE LEN-OUTPUT.
           INITIALIZE NUM-NEGATIVE.
           INITIALIZE OPTION-REC.
           INITIALIZE NUM-WRK.

     ‚* PRÜFEN DER PARAMETER
           PERFORM  CHECK-PARMS.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

       ANF010.
           COMPUTE  LEN-BEFORE = X-FMT-PRECISION - X-FMT-DECIMAL
           END-COMPUTE.
           MOVE     LEN-BEFORE    TO LEN-BEFORE-WRK.
           MOVE     X-FMT-DECIMAL TO LEN-DECIMAL-WRK.

     ‚* PRÜFEN OB DIE ZAHL NEGATIV IST
     ‚* DIE ZAHL IST NEGATIV WENN DER WERT HIER KLEINER 240 IST
           MOVE     X-NUM-INPUT(31:1) TO NEGATIVE-WRK.
           PERFORM  COPY-CHECK-NEGATIVITY.
           MOVE     NEGATIVE-FLAG TO NUM-NEGATIVE.

     ‚* KONVERTIERUNG DER NUMMER IN DAS GEWÜNSCHTE FORMAT
           IF       X-FMT-PRECISION = 31
            AND     X-FMT-DECIMAL   = 10
                    MOVE X-NUM-INPUT TO NUM-RED
           ELSE
     ‚*             VORKOMMASTELLEN ÜBERNEHMEN
                    MOVE X-NUM-INPUT(22 - LEN-BEFORE:LEN-BEFORE) TO
                         NUM-RED(1:LEN-BEFORE)
     ‚*             NACHKOMMASTELLEN ÜBERNEHMEN
                    IF   X-FMT-DECIMAL > ZEROES
                         MOVE X-NUM-INPUT(22:X-FMT-DECIMAL) TO
                              NUM-RED(LEN-BEFORE + 1:X-FMT-DECIMAL)
                    END-IF

     ‚*             MUSS GERUNDET WERDEN ?
                    IF   X-FMT-ROUNDED = 1
                     AND X-FMT-DECIMAL NOT = 10
                         IF X-NUM-INPUT(22 + X-FMT-DECIMAL:1) >= "5"
                            MOVE NUM-WRK(X-FMT-PRECISION:1) TO CHAR-WRK
                            ADD 1 TO CHAR-RED
                            MOVE CHAR-WRK TO NUM-WRK(X-FMT-PRECISION:1)
                         END-IF
                    END-IF
           END-IF.
     ‚* DIESER WERT WIRD SPÄTER UNTEN BENÖTIGT
           MOVE     LEN-BEFORE TO LEN1-WRK.
           SUBTRACT 1 FROM LEN1-WRK.

     ‚* DIE ZAHL IST NEGATIV WENN DER WERT HI1R KLEINER 240 IST
           MOVE     NUM-WRK(X-FMT-PRECISION:1) TO NEGATIVE-WRK.
           PERFORM  COPY-CHECK-NEGATIVITY.
           IF       NUM-NEGATIVE = 1
            AND     NEGATIVE-FLAG = 1
     ‚*             WENN DIE ZAHL NEGATIV IST, AUF JEDEN FALL POSITIV SETZEN
                    MOVE NEGATIVE-WRK TO NUM-WRK(X-FMT-PRECISION:1)
           END-IF.

     ‚* NUN JE NACH EDITIERCODE DIE KONVERTIERUNG VORNEHMEN
     ‚* UND BENÖTIGTE LÄNGE BERECHNEN
           EVALUATE X-FMT-EDTCDE
                WHEN "1"
     ‚*               TRENNZEICHEN, NULLINHALT, KEIN VORZEICHEN
                      DIVIDE LEN1-WRK BY 3 GIVING LEN-OUTPUT
                             REMAINDER LEN2-WRK
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 1 TO NULL-VALUES
                      MOVE 1 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 0 TO SIGN-POS
                WHEN "2"
     ‚*               TRENNZEICHEN, KEIN VORZEICHEN
                      DIVIDE LEN1-WRK BY 3 GIVING LEN-OUTPUT REMAINDER
                             LEN2-WRK
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 0 TO NULL-VALUES
                      MOVE 1 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 0 TO SIGN-POS
                WHEN "3"
     ‚*               NULLINHALT, KEIN VORZEICHEN
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 1 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 0 TO SIGN-POS
                WHEN "4"
     ‚*               KEIN NULLINHALT,KEIN TRENNZEICHEN,KEIN VORZEICHEN
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 0 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 0 TO SIGN-POS
                WHEN "J"
     ‚*               NULLINHALT, TRENNZEICHEN, VORZEICHEN RECHTS
                      DIVIDE LEN1-WRK BY 3 GIVING LEN-OUTPUT REMAINDER
                             LEN2-WRK
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 1 TO NULL-VALUES
                      MOVE 1 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 2 TO SIGN-POS
                WHEN "K"
     ‚*               TRENNZEICHEN, VORZEICHEN RECHTS
                      DIVIDE LEN1-WRK BY 3 GIVING LEN-OUTPUT REMAINDER
                             LEN2-WRK
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 0 TO NULL-VALUES
                      MOVE 1 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 2 TO SIGN-POS
                WHEN "L"
     ‚*               NULLINHALT, VORZEICHEN RECHTS
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 1 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 2 TO SIGN-POS
                WHEN "M"
     ‚*               KEIN NULLINHALT,KEIN TRENNZEICHEN,VORZEICHEN RECHTS
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 0 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 2 TO SIGN-POS
                WHEN "N"
     ‚*               NULLINHALT, TRENNZEICHEN, VORZEICHEN LINKS
                      DIVIDE LEN1-WRK BY 3 GIVING LEN-OUTPUT REMAINDER
                             LEN2-WRK
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 1 TO NULL-VALUES
                      MOVE 1 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 1 TO SIGN-POS
                WHEN "O"
     ‚*               TRENNZEICHEN, VORZEICHEN LINKS
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 0 TO NULL-VALUES
                      MOVE 1 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 1 TO SIGN-POS
                WHEN "P"
     ‚*               NULLINHALT, VORZEICHEN LINKS
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 1 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 1 TO SIGN-POS
                WHEN "Q"
     ‚*               KEIN NULLINHALT,KEIN TRENNZEICHEN,VORZEICHEN LINKS
                      IF X-FMT-DECIMAL > ZEROES
                         ADD 1 TO LEN-OUTPUT
                      END-IF
                      ADD 1 TO LEN-OUTPUT
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE 0 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 1 TO DECIMAL-SEP
                      MOVE 1 TO SIGN-POS
                WHEN "Z"
     ‚*               FÜHRENDE NULLEN UNTERDRÜCKEN
                      ADD X-FMT-PRECISION TO LEN-OUTPUT
                      MOVE NUM-WRK TO OUTPUT-WRK
                      MOVE X-FMT-PRECISION TO LEN-BEFORE-WRK
                      MOVE X-FMT-PRECISION TO LEN-BEFORE
                      MOVE ZEROES TO LEN-DECIMAL-WRK
                      MOVE 0 TO NULL-VALUES
                      MOVE 0 TO DECIMAL-POS
                      MOVE 0 TO DECIMAL-SEP
                      MOVE 0 TO SIGN-POS
                WHEN SPACES
                      MOVE 1 TO DECIMAL-SEP
           END-EVALUATE.

     ‚* NUN DIE EIGENTLICHEN FORMATIERUNGEN DURCHFÜHREN
           PERFORM  SUPPRESS-ZEROES.
           IF       OUTPUT-WRK NOT = SPACES
                    IF     SIGN-POS OF OPTION-REC > ZEROES
                           PERFORM  PLACE-SIGN
                    END-IF
                    IF     DECIMAL-SEP OF OPTION-REC > ZEROES
                     AND   X-FMT-DECIMAL > ZEROES
                           PERFORM  PLACE-DECIMAL-SEP
                    END-IF
                    IF     DECIMAL-POS OF OPTION-REC > ZEROES
                           PERFORM  PLACE-DECIMAL-POS
                    END-IF
           END-IF.

           IF       X-FMT-OUTPUT-LEN = ZEROES
                    MOVE LEN-OUTPUT TO X-FMT-OUTPUT-LEN
           END-IF.

     ‚* PRÜFEN OB DIE RÜCKGABELÄNGE AUSREICHT
           IF       LEN-OUTPUT > LENGTH OF X-NUM-OUTPUT
            OR      LEN-OUTPUT > X-FMT-OUTPUT-LEN
                    MOVE "*OUTLEN" TO X-NUM-OUTPUT
                    GO TO ENDE
           END-IF.

     ‚* WERT JE NACH POSITION RECHTSBÜNDIG ZURÜCKGEBEN
           COMPUTE  CHAR-NUM = (X-FMT-OUTPUT-LEN - LEN-BEFORE-WRK -
                                LEN-DECIMAL-WRK) + 1
           END-COMPUTE.
           COMPUTE  POS-WRK  = LEN-BEFORE-WRK + LEN-DECIMAL-WRK
           END-COMPUTE.
           MOVE     OUTPUT-WRK TO X-NUM-OUTPUT(CHAR-NUM:POS-WRK).
     ‚* LINKSBÜNDIG AUSRICHTEN
           IF       X-FMT-JUSTIFIED = 1
                    INITIALIZE POS-WRK
                    INSPECT X-NUM-OUTPUT TALLYING POS-WRK FOR
                            LEADING SPACES
                    IF      POS-WRK > ZEROES
                     AND    POS-WRK NOT = LENGTH OF X-NUM-OUTPUT
                            INITIALIZE OUTPUT-WRK
                            MOVE X-NUM-OUTPUT(POS-WRK + 1:) TO
                                 OUTPUT-WRK
                            MOVE OUTPUT-WRK TO X-NUM-OUTPUT
                    END-IF
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* PRÜFEN DER ÜBERGEBENEN PARAMETER
     ‚*---------------------------------------------------------------
       CHECK-PARMS  SECTION.
       CHK-PAR-00.

           INITIALIZE RET-CODE.

     ‚* PRÜFUNG DER ÜBERGEBENEN PARAMETER
      *    IF       X-NUM-INPUT     NOT NUMERIC
      *     OR      X-FMT-PRECISION NOT NUMERIC
      *     OR      X-FMT-DECIMAL   NOT NUMERIC
      *     OR      X-FMT-EDTCDE    = SPACES
      *             MOVE "*PARMERR" TO X-NUM-OUTPUT
      *             MOVE "1" TO RET-CODE
      *             GO TO CHK-PAR-90
      *    END-IF.

     ‚* ABFRAGEN OB DIE ZU ERSTELLENDE LÄNGE GÜLTIG IST
           IF       X-FMT-PRECISION > 31
            OR      X-FMT-PRECISION = ZEROES
                    MOVE "*PRECLEN" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

           IF       X-FMT-DECIMAL > 10
            OR      X-FMT-DECIMAL >= X-FMT-PRECISION
                    MOVE "*DECLEN" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

     ‚* LÄNGE DES AUSGABEFELDES GÜLTIG
           IF       X-FMT-OUTPUT-LEN > LENGTH OF X-NUM-OUTPUT
                    MOVE "*OUTLEN" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

     ‚* ABFRAGEN DAS RUNDUNGSKENNZEICHEN GÜLTIG IST
           IF       X-FMT-ROUNDED > 1
                    MOVE "*ROUNDERR" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

     ‚* ABFRAGEN OB DER EDITIERCODE GÜLTIG IST
           IF       X-FMT-EDTCDE NOT = "1"
            AND     X-FMT-EDTCDE NOT = "2"
            AND     X-FMT-EDTCDE NOT = "3"
            AND     X-FMT-EDTCDE NOT = "4"
            AND     X-FMT-EDTCDE NOT = "J"
            AND     X-FMT-EDTCDE NOT = "K"
            AND     X-FMT-EDTCDE NOT = "L"
            AND     X-FMT-EDTCDE NOT = "M"
            AND     X-FMT-EDTCDE NOT = "N"
            AND     X-FMT-EDTCDE NOT = "O"
            AND     X-FMT-EDTCDE NOT = "P"
            AND     X-FMT-EDTCDE NOT = "Q"
            AND     X-FMT-EDTCDE NOT = "Z"
            AND     X-FMT-EDTCDE NOT = SPACES
                    MOVE "*EDTERR" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

     ‚* ABFRAGEN OB DAS DEZIMALFORMAT GÜLTIG IST
           IF       X-FMT-DECFMT NOT = "I"
            AND     X-FMT-DECFMT NOT = "J"
            AND     X-FMT-DECFMT NOT = " "
                    MOVE "*DECFMT" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

     ‚* ABFRAGEN OB DIE AUSRICHTUNG GÜLTIG IST
           IF       X-FMT-JUSTIFIED > 1
                    MOVE "*JUSTIFIED" TO X-NUM-OUTPUT
                    MOVE "1" TO RET-CODE
                    GO TO CHK-PAR-90
           END-IF.

       CHK-PAR-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SETZEN DES VORZEICHENS
     ‚*---------------------------------------------------------------
       PLACE-SIGN  SECTION.
       PLC-SGN-00.

     ‚* VORZEICHEN LINKS?
           IF       SIGN-POS = 1
     ‚*             VORNE EIN ZEICHEN EINRÜCKEN
                    INITIALIZE OUTPUT-TMP
                    MOVE OUTPUT-WRK TO OUTPUT-TMP(2:)
                    MOVE OUTPUT-TMP TO OUTPUT-WRK
                    IF NUM-NEGATIVE = 1
                       MOVE "-" TO OUTPUT-WRK(1:1)
                    END-IF
                    ADD  1 TO LEN-BEFORE-WRK
           END-IF.

     ‚* VORZEICHEN RECHTS?
           IF       SIGN-POS = 2
                    IF NUM-NEGATIVE = 1
                       COMPUTE POS-WRK = LEN-BEFORE-WRK +
                                         LEN-DECIMAL-WRK + 1
                       END-COMPUTE
                       MOVE "-" TO OUTPUT-WRK(POS-WRK:1)
                    END-IF
                    ADD  1 TO LEN-DECIMAL-WRK
           END-IF.

       PLC-SGN-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SETZEN DES DEZIMALSEPERATORS(KOMMA)
     ‚*---------------------------------------------------------------
       PLACE-DECIMAL-SEP  SECTION.
       DEC-SEP-00.

           INITIALIZE OUTPUT-TMP.

     ‚* TRENNZEICHEN EINFÜGEN
           IF       LEN-BEFORE-WRK > ZEROES
                    MOVE OUTPUT-WRK(1:LEN-BEFORE-WRK) TO OUTPUT-TMP
           END-IF.

     ‚* JE NACH DEZIMALFORMAT ANDEREN SEPERATOR EINFÜGEN
           IF       X-FMT-DECFMT = " "
                    MOVE     "." TO OUTPUT-TMP(LEN-BEFORE-WRK + 1:1)
           ELSE
                    MOVE     "," TO OUTPUT-TMP(LEN-BEFORE-WRK + 1:1)
           END-IF.

           MOVE     OUTPUT-WRK(LEN-BEFORE-WRK + 1:LEN-DECIMAL-WRK) TO
                    OUTPUT-TMP(LEN-BEFORE-WRK + 2:LEN-DECIMAL-WRK).
           MOVE     OUTPUT-TMP TO OUTPUT-WRK.
           ADD      1 TO LEN-DECIMAL-WRK.

       DEC-SEP-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SETZEN DES DEZIMALPOSITIONSZEICHEN (999.999.999)
     ‚*---------------------------------------------------------------
       PLACE-DECIMAL-POS  SECTION.
       DEC-POS-00.

           INITIALIZE OUTPUT-TMP.
           INITIALIZE LEN1-WRK.
           INITIALIZE LEN2-WRK.
           INITIALIZE LEN3-WRK.
           INITIALIZE POS-WRK.

           IF       LEN-BEFORE-WRK = ZEROES
                    GO TO DEC-POS-90
           END-IF.

           IF       X-FMT-DECFMT = " "
                    MOVE "," TO DECPOS-WRK
           ELSE
                    MOVE "." TO DECPOS-WRK
           END-IF.

     ‚* WENN DIE VORKOMMASTELLEN < 4 STELLEN SIND. DANN NICHTS
     ‚* UMSETZEN
           IF       LEN-BEFORE-WRK < 4
                    GO TO DEC-POS-90
           END-IF.

     ‚* DIE SCHLEIFE LÄUFT VON HINTEN NACH VORNE
     ‚* LEN1-WRK = POSITION IN QUELLVARIABLE
     ‚* LEN2-WRK = ZÄHLER FÜR TRENNZEICHENSETZUNG
     ‚* LEN3-WRK = VERARBEITETE VORKOMMALÄNGE
     ‚* POS-WRK = POSITION IN ZIELVARIABLE
           MOVE     LEN-BEFORE-WRK TO LEN3-WRK.
     ‚* BEI EINEM VORZEICHEN MUSS EINE STELLE MEHR VERARBEITET WERDEN
           IF       SIGN-POS = 1
                    ADD 1 TO LEN-BEFORE-WRK
           END-IF.
           PERFORM  WITH TEST AFTER VARYING LEN1-WRK FROM
                    LEN3-WRK BY -1 UNTIL LEN1-WRK = 1
     ‚*             NUR NUMERISCHE ZEICHEN MITZÄHLEN
                    IF  OUTPUT-WRK(LEN1-WRK:1) IS NUMERIC
                        ADD 1 TO LEN2-WRK
                    END-IF
                    ADD 1 TO POS-WRK
     ‚*             NACH JEWEILS 3 ZEICHEN EIN TRENNZEICHEN
                    IF  LEN2-WRK = 4
                        MOVE DECPOS-WRK TO OUTPUT-TMP(POS-WRK:1)
                        ADD 1 TO POS-WRK
                        MOVE 1 TO LEN2-WRK
                    END-IF
                    MOVE OUTPUT-WRK(LEN1-WRK:1) TO
                         OUTPUT-TMP(POS-WRK:1)
           END-PERFORM.

     ‚* DIE NOCH FEHLENDEN NACHKOMMASTELLEN ÜBERNEHMEN
           MOVE     OUTPUT-WRK(LEN3-WRK + 1:) TO
                    OUTPUT-TMP(POS-WRK + 1:)

     ‚* DIE VORKOMMASTELLEN WURDEN IN UMGEKEHRTER REIHENFOLGE
     ‚* GESPEICHERT, HIER WIEDER UMDREHEN
           MOVE     OUTPUT-TMP TO OUTPUT-WRK.
           MOVE     FUNCTION REVERSE(OUTPUT-TMP(1:POS-WRK)) TO
                             OUTPUT-WRK(1:POS-WRK).
           MOVE     POS-WRK TO LEN-BEFORE-WRK.

       DEC-POS-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* FÜHRENDER NULLEN UNTERDRÜCKEN
     ‚*---------------------------------------------------------------
       SUPPRESS-ZEROES  SECTION.
       SUP-ZER-00.

     ‚* WURDE KEIN EDITIER-CODE ANGEGEBEN MUSS DER WERT OHNE
     ‚* AUFBEREITUNG ÜBERNOMMEN WERDEN
           IF       X-FMT-EDTCDE = SPACES
                    MOVE NUM-WRK TO OUTPUT-WRK
                    MOVE 1 TO SIGN-POS
                    GO TO SUP-ZER-90
           END-IF.

     ‚* NUR NULLEN UND ES DÜRFEN KEINE NULLEN ANGEZEIGT WERDEN?
           IF       NUM-WRK(1:X-FMT-PRECISION) = ZEROES
            AND     NULL-VALUES OF OPTION-REC = ZERO
                    GO TO SUP-ZER-90
           END-IF.

     ‚* WENN KEIN KOMMAZEICHEN VERWENDET WERDEN SOLL, WIRD DIE ZAHL
     ‚* ALS GANZZAHL OHNE NACHKOMMASTELLEN GELESEN
           IF       DECIMAL-SEP OF OPTION-REC = ZERO
                    MOVE X-FMT-PRECISION TO LEN-BEFORE-WRK
                    MOVE ZEROES TO LEN-DECIMAL-WRK
           END-IF.

     ‚* FÜHRENDE NULLEN ZÄHLEN
           INITIALIZE LEN1-WRK.
           INSPECT  NUM-WRK(1:LEN-BEFORE-WRK) TALLYING LEN1-WRK FOR
                    LEADING ZEROES.
           IF       LEN1-WRK = ZEROES
                    MOVE  NUM-WRK TO OUTPUT-WRK
                    GO TO SUP-ZER-90
           END-IF.

     ‚* NUR FÜHRENDE NULLEN?
           IF       LEN1-WRK = LEN-BEFORE-WRK
                    IF X-FMT-DECFMT = "J"
                       SUBTRACT 1 FROM LEN1-WRK
                    END-IF
           END-IF.
           SUBTRACT LEN1-WRK FROM LEN-BEFORE-WRK.
           IF       LEN-BEFORE-WRK = ZEROES
                    MOVE 1 TO LEN-BEFORE-WRK
           END-IF.
           COMPUTE  LEN1-WRK = (LEN-BEFORE - LEN-BEFORE-WRK) + 1
           END-COMPUTE.
           MOVE     NUM-WRK(LEN1-WRK:) TO OUTPUT-WRK.

       SUP-ZER-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* HANDLING FÜR NEGATIVE GEZONTE ZAHLEN
     ‚*---------------------------------------------------------------

           COPY     NEGNUM     OF APG-QCPYSRC.
      /
