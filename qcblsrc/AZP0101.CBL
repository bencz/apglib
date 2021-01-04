     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             AZP0101.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE KW-DATEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  D. MÜLLER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  11.08.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  RÜCKGABE EINER BERUFSSCHULWOCHE ALS STRING   *
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
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "AZP0101".
       01  PGM-TYP                       PIC X(8)  VALUE "AZ:VERW".

       01  PCFGDAT-WRK.
            COPY DDS-ALL-FORMATS OF PCFGDAT.

     ‚* PARAMETER FÜR DAS HLP0011
       01  COLOR-WRK                     PIC X(10).
       01  INVERS-WRK                    PIC X(1).
       01  BLINK-WRK                     PIC X(1).
       01  UNDERLINE-WRK                 PIC X(1).
       01  HEX-WRK                       PIC X(1).

       01  OCCUPIED-SIGN                 PIC X(2).
       01  FREE-SIGN                     PIC X(2).

       01  TEMP                          PIC 9(2).

     ‚* ZERO = GERADE, 1 = UNGERADE
       01  EVEN-UNEVEN                   PIC 9(1).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
     ‚* "INVERS" = BERUFSSCHULTAGE INVERS MARKIEREN
     ‚* "WITH-?" = TAGE MIT EINEM "?" MARKIEREN (?=BELIEBIGES ZEICHEN)
     ‚* "DAY"    = MONTAGE MIT M, DIENSTAGE MIT D ETC
       01  X-MODE                        PIC X(20).
       01  X-KW                          LIKE DTKW OF PCFGDAT-WRK.
       01  X-WEEKSTR                     PIC X(10).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-KW
                                X-MODE
                                X-WEEKSTR.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "AZP0101" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT-SIMPLE.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1"    TO FIRST-CALL
     ‚*             ZEICHEN ERMITTELN DIE VERWENDET WERDEN SOLLEN
                    MOVE    SPACES TO OCCUPIED-SIGN
                    MOVE    SPACES TO FREE-SIGN
                    PERFORM INIT-SIGNS
           END-IF.

     ‚* KW GERADE?
           INITIALIZE EVEN-UNEVEN.
           DIVIDE   X-KW BY 2 GIVING TEMP REMAINDER EVEN-UNEVEN.

     ‚* CONFIG LESEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "V100"            TO CFID   OF CFG-CPY.
           MOVE     X-USER            TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

     ‚* JETZT DEN STRING BEFÜLLEN
           INITIALIZE X-WEEKSTR.
           MOVE     FREE-SIGN TO X-WEEKSTR(1:1).

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
     ˆ* TODO - SOLL DIE 5 BLÖCKE ABLÖSEN !
           MOVE     1                 TO DAY-WRK.
           MOVE     CFFK01 OF CFG-CPY TO DAY-SET.
           PERFORM  GET-SIGN-FOR-DAY.
           MOVE     SIGN-WRK          TO X-WEEKSTR(1:2).

           IF       CFFK01 OF CFG-CPY = "1"
            OR      (    CFFK01 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK01 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                    IF   X-MODE = "DAY"
                         MOVE "M" TO OCCUPIED-SIGN(2:1)
                    END-IF
                    MOVE OCCUPIED-SIGN TO X-WEEKSTR(1:2)
           ELSE
                    MOVE FREE-SIGN TO X-WEEKSTR(1:2)
           END-IF.

     ‚* IN CFFK02 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK02 OF CFG-CPY = "1"
            OR      (    CFFK02 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK02 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                    IF   X-MODE = "DAY"
                         MOVE "D" TO OCCUPIED-SIGN(2:1)
                    END-IF
                    MOVE OCCUPIED-SIGN TO X-WEEKSTR(3:2)
           ELSE
                    MOVE FREE-SIGN TO X-WEEKSTR(3:2)
           END-IF.

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK03 OF CFG-CPY = "1"
            OR      (    CFFK03 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK03 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                    IF   X-MODE = "DAY"
                         MOVE "M" TO OCCUPIED-SIGN(2:1)
                    END-IF
                    MOVE OCCUPIED-SIGN TO X-WEEKSTR(5:2)
           ELSE
                    MOVE FREE-SIGN TO X-WEEKSTR(5:2)
           END-IF.

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK04 OF CFG-CPY = "1"
            OR      (    CFFK04 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK04 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                    IF   X-MODE = "DAY"
                         MOVE "D" TO OCCUPIED-SIGN(2:1)
                    END-IF
                    MOVE OCCUPIED-SIGN TO X-WEEKSTR(7:2)
           ELSE
                    MOVE FREE-SIGN TO X-WEEKSTR(7:2)
           END-IF.

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK05 OF CFG-CPY = "1"
            OR      (    CFFK05 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK05 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                    IF   X-MODE = "DAY"
                         MOVE "F" TO OCCUPIED-SIGN(2:1)
                    END-IF
                    MOVE OCCUPIED-SIGN TO X-WEEKSTR(9:2)
           ELSE
                    MOVE FREE-SIGN TO X-WEEKSTR(9:2)
           END-IF.

       ENDE.
           PERFORM  COPY-PGM-EXIT-SIMPLE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ZEICHEN ERMITTELN
     ‚*--------------------------------------------------------------
       GET-SIGN-FOR-DAY SECTION.
       GET-SFD-00.

           IF       CFFK01 OF CFG-CPY = "1"
            OR      (    CFFK01 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK01 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                    IF   X-MODE = "DAY"
                         MOVE "M" TO OCCUPIED-SIGN(2:1)
                    END-IF
                    MOVE OCCUPIED-SIGN TO X-WEEKSTR(1:2)
           ELSE
                    MOVE FREE-SIGN TO X-WEEKSTR(1:2)
           END-IF.

       GET-SFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZU VERWENDENDE ZEICHEN ERMITTELN
     ‚*--------------------------------------------------------------
       INIT-SIGNS SECTION.
       INI-SIG-00.

           EVALUATE TRUE
                    WHEN X-MODE = "INVERS"
     ‚*                  FREI:   "GRÜNES" LEERZEICHEN
     ‚*                  BELEGT: GRÜN-INVERSES KÄSTCHEN
                         MOVE "GREEN" TO COLOR-WRK
                         MOVE SPACES  TO INVERS-WRK
                         MOVE SPACES  TO BLINK-WRK
                         MOVE SPACES  TO UNDERLINE-WRK
                         CALL "HLP0011" USING COLOR-WRK
                                              INVERS-WRK
                                              BLINK-WRK
                                              UNDERLINE-WRK
                                              HEX-WRK
                         END-CALL
                         MOVE HEX-WRK TO FREE-SIGN(1:1)
                         MOVE "GREEN" TO COLOR-WRK
                         MOVE "1"     TO INVERS-WRK
                         MOVE SPACES  TO BLINK-WRK
                         MOVE SPACES  TO UNDERLINE-WRK
                         CALL "HLP0011" USING COLOR-WRK
                                              INVERS-WRK
                                              BLINK-WRK
                                              UNDERLINE-WRK
                                              HEX-WRK
                         END-CALL
                         MOVE HEX-WRK TO OCCUPIED-SIGN(1:1)
                    WHEN X-MODE(1:5) = "WITH-"
     ‚*                  FREI:   LEERZEICHEN
     ‚*                  BELEGT: ÜBERGEBENES ZEICHEN
                         MOVE X-MODE(6:1) TO OCCUPIED-SIGN(2:1)
                    WHEN X-MODE = "DAY"
     ‚*                  FREI:   "_"
     ‚*                  BELEGT: M,D,M,D,F
                         MOVE "_"         TO FREE-SIGN(2:1)
           END-EVALUATE.

       INI-SIG-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN KONFIGURATIONSDATEI
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
