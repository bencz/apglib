       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DURCHSUCHEN EINES STRINGS (LGPGM0923)                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  21.05.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DURCHSUCHEN EINES STRINGS                    *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

     ‚*
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0001 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  LEN                           PIC 9(5) COMP.
       01  POS                           PIC 9(5) COMP.
       01  POS2                          PIC 9(5) COMP.
       01  REC-SELEKTION-SAVE            PIC X(50).
     ‚*€01 SELEKTION-WRK                 PIC X(51).
       01  ANZ-TEILSTRINGS               PIC 9(5) COMP.
       01  TXT-LEN                       PIC 9(5) COMP.
       01  STELLE                        PIC 9(5) COMP.
       01  STP                           PIC 9(1) COMP.
       01  SELEKTION-LEER                PIC 9(1) COMP.

       01  TABELLE.
        02 TAB-REC                       OCCURS 99 TIMES.
         05 TAB-LAENGE                   PIC 9(5) COMP.
         05 TAB-STRING                   PIC X(50).
         05 BAD-CHAR-TAB OCCURS 256 TIMES PIC 9(4) COMP.

       01  NLEN                          PIC 9(4) COMP.
       01  HPOS                          PIC 9(4) COMP.
       01  NPOS                          PIC 9(4) COMP.
       01  CHAR-POS                      PIC 9(3) COMP.
       01  POS-WRK                       PIC 9(3) COMP.
       01  TEMP-1                        PIC 9(3) COMP.
       01  TEMP-2                        PIC 9(3) COMP.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-RECORD-WRK                  LIKE RECORD-WRK.
       01  X-RECORD-WRK-LEN              PIC 9(4).
       01  X-REC-SELEKTION               PIC X(50).
       01  X-RET-CODE                    PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION  USING       X-RECORD-WRK
                                       X-RECORD-WRK-LEN
                                       X-REC-SELEKTION
                                       X-RET-CODE.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* VORBELEGEN MIT "NICHT GEFUNDEN"
           MOVE     "1" TO X-RET-CODE.

     ‚* SICHERHEITSABFRAGE FÜR DIE ÜBERGEBENE LÄNGE
           IF       X-RECORD-WRK-LEN > LENGTH OF X-RECORD-WRK
                    MOVE LENGTH OF X-RECORD-WRK TO
                         X-RECORD-WRK-LEN
           END-IF.

     ‚* IST DIE ÜBERGEBENE SELEKTION NOCH NICHT NACH TEILSTRINGS
     ‚* ANALYSIERT, DURCHSUCHTLEKTION DES FELDES NACH TEILSTRINGS
           IF       X-REC-SELEKTION NOT = REC-SELEKTION-SAVE
                    MOVE X-REC-SELEKTION TO REC-SELEKTION-SAVE
                    MOVE ZEROES TO ANZ-TEILSTRINGS
                    PERFORM ANALYSIEREN-SELEKTIONSSTRING
           END-IF.

     ‚* KEINE SELEKTIONEN GETROFFEN --> FEHLER
           IF       SELEKTION-LEER = 1
                    INITIALIZE X-RET-CODE
                    GO TO ENDE
           END-IF.

     ‚* DAMIT NICHT ALLE 8192 ZEICHEN ANALYSIERT WERDEN MÜSSEN,
     ‚* WIRD HIER DIE LÄNGE DES TEXTES IM RECORD ERMITTELT
           IF       X-RECORD-WRK-LEN > ZEROES
                    MOVE X-RECORD-WRK-LEN TO TXT-LEN
           ELSE
                    MOVE     1   TO TXT-LEN
                    PERFORM  WITH TEST BEFORE
                              UNTIL  X-RECORD-WRK(TXT-LEN:) = " "
                              OR     TXT-LEN >= LENGTH OF X-RECORD-WRK
                              OR     TXT-LEN >= X-RECORD-WRK-LEN
                              ADD 32 TO TXT-LEN
                              IF  TXT-LEN > LENGTH OF X-RECORD-WRK
                                   MOVE LENGTH OF X-RECORD-WRK TO
                                        TXT-LEN
                              END-IF
                    END-PERFORM
           END-IF.
           IF       TXT-LEN < 1
                    GO TO ENDE
           END-IF.

     ‚* NORMIEREN RECORD.
     ‚* DA HIER ZWEI FUNKTIONEN (UPPER-CASSE UND CONVERT) DURCHGEFÜHRT
     ‚* WERDEN, WURDEN DIESE IN EINE SCHLEIFE ZUSAMMEN GEFASST
     ‚* ZUSÄTZLICH WIRD HIERBEI DAS WRK-FELD RECORD-WRK GEFÜLLT
           PERFORM  NORMIEREN-RECORD.

     ‚* DEN ZU DURCHSUCHENDEN TEXT NACH TEILSTRINGS DURCHSUCHEN
           MOVE     ANZ-TEILSTRINGS TO STELLE.
           INITIALIZE RET-CODE.
           PERFORM  WITH TEST BEFORE UNTIL STELLE     = ZEROES
                                     OR    RET-CODE NOT = SPACES
                    PERFORM  TEILSTRING-PRUEFEN
                    SUBTRACT 1 FROM STELLE
           END-PERFORM.
           IF       RET-CODE = SPACES
                    INITIALIZE X-RET-CODE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DER TEXT AN DIESER POSITION (POS) MIT DEM TEILSTRING
     ‚* ÜBEREINSTIMMT
     ‚*--------------------------------------------------------------
       TEILSTRING-PRUEFEN SECTION.
       T-STR-PRF-00.

     ‚* NUN PRÜFEN, OB DIE FOLGENDEN STELLEN MIT DEN TEILSTRINGS IN
     ‚* DER TABELLE ÜBEREINSTIMMEN
           MOVE     TAB-LAENGE(STELLE) TO NLEN.
           PERFORM  SEARCH-STRING.

       T-STR-PRF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANALYSIEREN/SUCHEN NACH TEILSTRINGS
     ‚*--------------------------------------------------------------
       ANALYSIEREN-SELEKTIONSSTRING SECTION.
       AN-SS-00.

           MOVE     ZEROES TO ANZ-TEILSTRINGS.

           IF       X-REC-SELEKTION = "*"
            OR      X-REC-SELEKTION = SPACES
                    MOVE 1 TO SELEKTION-LEER
                    GO TO AN-SS-90
           ELSE
                    MOVE ZEROES TO SELEKTION-LEER
           END-IF.

     ‚* ZUNÄCHST STING IN GROßBUCHSTABEN UMSETZEN
           MOVE     FUNCTION UPPER-CASE(X-REC-SELEKTION)
                          TO SELEKTION-WRK.

     ‚* DA DIE FUNKTION UPPER-CASE NICHT FÜR Ä,Ü,Ö FUNKTIONIERT, WERDEN
     ‚* DIESE ZEICHEN HIER EINZELN UMGESETZT
           INSPECT  SELEKTION-WRK CONVERTING "äöü" TO "ÄÖÜ".

     ‚* PRÜFEN AUS WIEVIELEN TEILSTRINGS DAS SELEKTIONSFELD BESTEHT

     ‚* TABELLE MIT TEILSRINGS LEEREN
           MOVE     1   TO POS.
           PERFORM  WITH TEST BEFORE UNTIL POS    > 99
                    MOVE ZEROES TO TAB-LAENGE(POS)
                    MOVE SPACES TO TAB-STRING(POS)
                    ADD  1      TO POS
           END-PERFORM.

           MOVE     1      TO POS.

       AN-SS-10.
           MOVE     ZEROES TO STP.
     ‚* SUCHEN ERSTES ZEICHEN IN SUCHSTRING
           PERFORM  WITH TEST BEFORE UNTIL STP = 1
                    IF SELEKTION-WRK(POS:1) > SPACES
                       MOVE 1 TO STP
                    ELSE
                       ADD  1 TO POS
                       IF   POS > 50
                            MOVE 1 TO STP
                       END-IF
                    END-IF
           END-PERFORM.
           IF       POS > 50
                    IF ANZ-TEILSTRINGS = ZEROES
                       MOVE 1 TO SELEKTION-LEER
                    END-IF
                    GO TO AN-SS-90
           END-IF.

     ‚* ERSTER BZW. NEUER TEILSTRING IN SUCHSTRING GEFUNDEN, NUN
     ‚* STARTPOSITION IN TABELLE ABLEGEN
           ADD      1   TO ANZ-TEILSTRINGS.
           MOVE     POS TO POS2.

     ‚* SUCHEN ENDE EINES TEILSTINGS
           MOVE     ZEROES TO STP.
           PERFORM  WITH TEST BEFORE UNTIL STP = 1
                    IF SELEKTION-WRK(POS:1) = SPACES
                       MOVE 1 TO STP
                    ELSE
                       ADD  1 TO POS
                       IF   POS > 50
                            MOVE 51 TO POS
                            MOVE 1  TO STP
                       END-IF
                    END-IF
           END-PERFORM.

     ‚* LANGE UND TEILSTRING IN TABELLE ABLEGEN
           COMPUTE  TAB-LAENGE(ANZ-TEILSTRINGS) =
                    POS - POS2
           END-COMPUTE.
           MOVE     SELEKTION-WRK(POS2:
                                  TAB-LAENGE(ANZ-TEILSTRINGS)) TO
                                  TAB-STRING(ANZ-TEILSTRINGS).

           MOVE     TAB-LAENGE(ANZ-TEILSTRINGS) TO NLEN.
           PERFORM  FUELLEN-BAD-CHAR.

     ‚* NÄCHSTEN TEILSTING BEARBEITEN
           GO TO AN-SS-10.

       AN-SS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NORMIEREN RECORD.
     ‚* DA HIER ZWEI FUNKTIONEN (UPPER-CASSE UND CONVERT) DURCHGEFÜHRT
     ‚* WERDEN, WURDEN DIESE IN EINE SCHLEIFE ZUSAMMEN GEFASST
     ‚* ZUSÄTZLICH WIRD HIERBEI DAS WRK-FELD RECORD-WRK GEFÜLLT
     ‚*--------------------------------------------------------------
       NORMIEREN-RECORD SECTION.
       NM-RC-00.

           MOVE     FUNCTION UPPER-CASE(X-RECORD-WRK(1:TXT-LEN)) TO
                    RECORD-WRK(1:TXT-LEN).
           INSPECT  RECORD-WRK(1:TXT-LEN) CONVERTING "äöü" TO "ÄÖÜ".

           GO TO    NM-RC-90.

       NM-RC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FÜLLEN DER BAD-CHARACTER TABELLE
     ‚*--------------------------------------------------------------
       FUELLEN-BAD-CHAR SECTION.
       BAD-CHR-00.

     ‚* ALLE ZEICHEN AUF DIE LÄNGE DES SUCHSTRINGS SETZEN
           INITIALIZE POS-WRK.
           PERFORM  WITH TEST BEFORE UNTIL POS-WRK = 256
                    ADD 1 TO POS-WRK
                    MOVE NLEN TO BAD-CHAR-TAB(ANZ-TEILSTRINGS, POS-WRK)
           END-PERFORM.

     ‚* FÜLLEN DER BAD-CHARACTER TABELLE
     ‚* ZUR FUNKTIONSWEISE EINFACH NACH BOOYER-MOORE TEXTSUCHE SUCHEN
           INITIALIZE POS-WRK.
           PERFORM  WITH TEST BEFORE UNTIL POS-WRK = NLEN
                    ADD 1 TO POS-WRK
                    COMPUTE CHAR-POS =
                            FUNCTION ORD(
                            TAB-STRING(ANZ-TEILSTRINGS)(POS-WRK:1)
                            )
                    END-COMPUTE
                    COMPUTE BAD-CHAR-TAB(ANZ-TEILSTRINGS, CHAR-POS) =
                            NLEN - POS-WRK
                    END-COMPUTE
           END-PERFORM.

       BAD-CHR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SUCHEN NACH BOOYER-MOORE
     ‚*--------------------------------------------------------------
       SEARCH-STRING SECTION.
       SRC-STR-00.

           MOVE     "1" TO RET-CODE.

           COMPUTE  HPOS = NLEN.
           PERFORM  WITH TEST BEFORE UNTIL HPOS > TXT-LEN
                    MOVE NLEN TO NPOS

                    PERFORM WITH TEST BEFORE UNTIL
                            RECORD-WRK(HPOS:1) NOT =
                            TAB-STRING(STELLE)(NPOS:1)
                            IF NPOS = 1
                               MOVE SPACE TO RET-CODE
                               GO TO SRC-STR-90
                            END-IF
                            SUBTRACT 1 FROM HPOS
                            SUBTRACT 1 FROM NPOS
                    END-PERFORM
                    COMPUTE TEMP-1 = ((NLEN + 1) - NPOS)
                    COMPUTE CHAR-POS = FUNCTION ORD(RECORD-WRK(HPOS:1))
                    COMPUTE TEMP-2 = BAD-CHAR-TAB(STELLE, CHAR-POS)
                    IF      TEMP-1 > TEMP-2
                            ADD TEMP-1 TO HPOS
                    ELSE
                            ADD TEMP-2 TO HPOS
                    END-IF
           END-PERFORM.

       SRC-STR-90.
           EXIT.
      /
