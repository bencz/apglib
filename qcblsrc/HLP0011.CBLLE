       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0011.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* FARBHEXWERT ERMITTELN (LGPGM0011)                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN DES OBJEKTTYPS                     *
     ‚*     X-COLOR               "GREEN"                             *
     ‚*                           "WHITE"                             *
     ‚*                           "RED"                               *
     ‚*                           "LIGHTBLUE"                         *
     ‚*                           "YELLOW"                            *
     ‚*                           "PINK"                              *
     ‚*                           "BLUE"                              *
     ‚*     X-INVERSE             "1"    = INVERS                     *
     ‚*                           SONST  = NICHTINVERS                *
     ‚*     X-BLINK               "1"    = BLINKEND                   *
     ‚*                           SONST  = NICHTBLINKEND              *
     ‚*     X-UNDERLINE           "1"    = UNTERSTRICHEN              *
     ‚*                           SONST  = NICHTUNTERSTRICHEN         *
     ‚* RÜCKGABEWERT:                                                 *
     ‚*     X-RETURN              GEWÜNSCHTES FORMAT IN HEXEDEZIMALER *
     ‚*                           DARSTELLUNG                         *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*                                                               *
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0011 ".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

     ‚* FARBEN - 5250
       01  GREEN-WRK                     PIC 9(2) VALUE 33.
       01  WHITE-WRK                     PIC 9(2) VALUE 35.
       01  RED-WRK                       PIC 9(2) VALUE 41.
       01  LIGHTBLUE-WRK                 PIC 9(2) VALUE 49.
       01  YELLOW-WRK                    PIC 9(2) VALUE 51.
       01  PINK-WRK                      PIC 9(2) VALUE 57.
       01  BLUE-WRK                      PIC 9(2) VALUE 59.

     ‚* FARBEN - ASCII
       01  GREEN-ASCII-WRK               PIC 9(2) VALUE 12.
       01  WHITE-ASCII-WRK               PIC 9(2) VALUE 14.
       01  RED-ASCII-WRK                 PIC 9(2) VALUE 16.
       01  YELLOW-ASCII-WRK              PIC 9(2) VALUE 18.

     ‚* INVERS
       01  INVERSE-WRK                   PIC 9(1) VALUE 1.

     ‚* BLINKEND
       01  BLINK-WRK                     PIC 9(1) VALUE 2.

     ‚* UNTERSTRICHEN
       01  UNDERLINE-WRK                 PIC 9(1) VALUE 4.

       01  COLOR-WRK                     PIC 9(2).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-COLOR                       PIC X(10).
       01  X-INVERSE                     PIC X(1).
       01  X-BLINK                       PIC X(1).
       01  X-UNDERLINE                   PIC X(1).
       01  X-RETURN                      PIC X(1).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-COLOR
                                X-INVERSE
                                X-BLINK
                                X-UNDERLINE
                                X-RETURN.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE X-RETURN.
           INITIALIZE COLOR-WRK.

     ‚* AUSWAHL FARBE (5250)
           EVALUATE X-COLOR
                    WHEN "GREEN"     MOVE GREEN-WRK     TO COLOR-WRK
                    WHEN "WHITE"     MOVE WHITE-WRK     TO COLOR-WRK
                    WHEN "RED"       MOVE RED-WRK       TO COLOR-WRK
                    WHEN "LIGHTBLUE" MOVE LIGHTBLUE-WRK TO COLOR-WRK
                    WHEN "YELLOW"    MOVE YELLOW-WRK    TO COLOR-WRK
                    WHEN "PINK"      MOVE PINK-WRK      TO COLOR-WRK
                    WHEN "BLUE"      MOVE BLUE-WRK      TO COLOR-WRK
                    WHEN OTHER       MOVE GREEN-WRK     TO COLOR-WRK
           END-EVALUATE.

     ‚* INVERSE DARSTELLUNG?
           IF       X-INVERSE = "1"
                    ADD INVERSE-WRK TO COLOR-WRK
           END-IF.

     ‚* BLINKENDE DARSTELLUNG?
           IF       X-BLINK = "1"
                    ADD  BLINK-WRK TO COLOR-WRK
           END-IF.

     ‚* UNTERSTRICHENE DARSTELLUNG?
           IF       X-UNDERLINE = "1"
                    ADD  UNDERLINE-WRK TO COLOR-WRK
           END-IF.

           IF       COLOR-WRK > ZEROS
                    PERFORM ERMITTELN-HEX-WERT
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* UM DIE UNABHÄNGIGKEIT VOM BETRIEBSSYSTEM ZU ERREICHEN, WIRD DER
     ‚* HEX WERT DES FELDATTRIBUTES ÜBER EINE EIGENE TABELLE ERMITTELT.
     ‚*---------------------------------------------------------------
       ERMITTELN-HEX-WERT SECTION.
       ERM-HEX-00.

     ‚* ÜBER DIE COBOL-FUNKTION FUNCTION CHAR IST DER ERMITELTE
     ‚* HEX-WERT EINE WERTIGKEIT NIEDRIGER ALS DER TATSÄCHLICHE
     ‚* HEX-WERT DER DEZIMELEN ZAHL COLOR-WRK.
           SUBTRACT 1 FROM COLOR-WRK.

     ‚* DEN HEX-WERT DES FELDATTRIBUTES IN DEN RÜCKGABEWERT SCHREIBEN.
           EVALUATE COLOR-WRK
                    WHEN "10"  MOVE X"0A" TO X-RETURN
                    WHEN "11"  MOVE X"0B" TO X-RETURN
                    WHEN "12"  MOVE X"0C" TO X-RETURN
                    WHEN "13"  MOVE X"0D" TO X-RETURN
                    WHEN "14"  MOVE X"0E" TO X-RETURN
                    WHEN "15"  MOVE X"0F" TO X-RETURN
                    WHEN "16"  MOVE X"10" TO X-RETURN
                    WHEN "17"  MOVE X"11" TO X-RETURN
                    WHEN "18"  MOVE X"12" TO X-RETURN
                    WHEN "19"  MOVE X"13" TO X-RETURN
                    WHEN "30"  MOVE X"1E" TO X-RETURN
                    WHEN "31"  MOVE X"1F" TO X-RETURN
                    WHEN "32"  MOVE X"20" TO X-RETURN
                    WHEN "33"  MOVE X"21" TO X-RETURN
                    WHEN "34"  MOVE X"22" TO X-RETURN
                    WHEN "35"  MOVE X"23" TO X-RETURN
                    WHEN "36"  MOVE X"24" TO X-RETURN
                    WHEN "37"  MOVE X"25" TO X-RETURN
                    WHEN "38"  MOVE X"26" TO X-RETURN
                    WHEN "39"  MOVE X"27" TO X-RETURN
                    WHEN "40"  MOVE X"28" TO X-RETURN
                    WHEN "41"  MOVE X"29" TO X-RETURN
                    WHEN "42"  MOVE X"2A" TO X-RETURN
                    WHEN "43"  MOVE X"2B" TO X-RETURN
                    WHEN "44"  MOVE X"2C" TO X-RETURN
                    WHEN "45"  MOVE X"2D" TO X-RETURN
                    WHEN "46"  MOVE X"2E" TO X-RETURN
                    WHEN "47"  MOVE X"2F" TO X-RETURN
                    WHEN "48"  MOVE X"30" TO X-RETURN
                    WHEN "49"  MOVE X"31" TO X-RETURN
                    WHEN "50"  MOVE X"32" TO X-RETURN
                    WHEN "51"  MOVE X"33" TO X-RETURN
                    WHEN "52"  MOVE X"34" TO X-RETURN
                    WHEN "53"  MOVE X"35" TO X-RETURN
                    WHEN "54"  MOVE X"36" TO X-RETURN
                    WHEN "55"  MOVE X"37" TO X-RETURN
                    WHEN "56"  MOVE X"38" TO X-RETURN
                    WHEN "57"  MOVE X"39" TO X-RETURN
                    WHEN "58"  MOVE X"3A" TO X-RETURN
                    WHEN "59"  MOVE X"3B" TO X-RETURN
                    WHEN "60"  MOVE X"3C" TO X-RETURN
                    WHEN "61"  MOVE X"3D" TO X-RETURN
                    WHEN "62"  MOVE X"3E" TO X-RETURN
                    WHEN "63"  MOVE X"3F" TO X-RETURN
                    WHEN "64"  MOVE X"40" TO X-RETURN
                    WHEN "65"  MOVE X"41" TO X-RETURN
                    WHEN "66"  MOVE X"42" TO X-RETURN
                    WHEN OTHER MOVE X"20" TO X-RETURN
           END-EVALUATE.

       ERM-HEX-90.
           EXIT.
      /
