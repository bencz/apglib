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

     ‚*-------------------------------------------------------------
     ‚* POPLLST: OPLFDN
     ‚*-------------------------------------------------------------
           SELECT POPLLST-DP
                  ASSIGN       TO  DATABASE-POPLLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- POPLLST: OPLFDN
       FD  POPLLST-DP
           LABEL RECORDS ARE STANDARD.
       01  POPLLST-P.
           COPY DDS-ALL-FORMATS OF POPLLST.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* CONFIG
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
       01  GREEN-RI-HEX-WRK              PIC X(1).

       01  TEMP                          PIC 9(2).

     ‚* ZERO = GERADE, 1 = UNGERADE
       01  EVEN-UNEVEN                   PIC 9(1).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
       01  X-KW                          LIKE DTKW OF PCFGDAT-WRK.
       01  X-WEEKSTR                     PIC X(6).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-KW
                                X-WEEKSTR.
     ‚*-------------------------------------------------------
       ANFANG.

           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
     ‚*             FARBWERT FÜR INVERS ERMITTELN
                    MOVE     "GREEN"         TO COLOR-WRK
                    MOVE     "1"             TO INVERS-WRK
                    MOVE     SPACES          TO BLINK-WRK
                    MOVE     SPACES          TO UNDERLINE-WRK
                    CALL     "HLP0011" USING COLOR-WRK
                                             INVERS-WRK
                                             BLINK-WRK
                                             UNDERLINE-WRK
                                             GREEN-RI-HEX-WRK
                    END-CALL
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
     ‚* 1. STELLE IST IMMER HEX-ZEICHEN GRÜN INVERS
           INITIALIZE X-WEEKSTR.
           MOVE     GREEN-RI-HEX-WRK TO X-WEEKSTR(1:1).

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK01 OF CFG-CPY = "1"
            OR      (    CFFK01 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK01 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                     MOVE SPACE TO X-WEEKSTR(2:1)
           ELSE
                    MOVE GREEN-RI-HEX-WRK TO X-WEEKSTR(2:1)
           END-IF.

     ‚* IN CFFK02 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK02 OF CFG-CPY = "1"
            OR      (    CFFK02 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK02 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                     MOVE SPACE TO X-WEEKSTR(3:1)
           ELSE
                    MOVE GREEN-RI-HEX-WRK TO X-WEEKSTR(3:1)
           END-IF.

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK03 OF CFG-CPY = "1"
            OR      (    CFFK03 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK03 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                     MOVE SPACE TO X-WEEKSTR(4:1)
           ELSE
                    MOVE GREEN-RI-HEX-WRK TO X-WEEKSTR(4:1)
           END-IF.

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK04 OF CFG-CPY = "1"
            OR      (    CFFK04 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK04 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                     MOVE SPACE TO X-WEEKSTR(5:1)
           ELSE
                    MOVE GREEN-RI-HEX-WRK TO X-WEEKSTR(5:1)
           END-IF.

     ‚* IN CFFK01 - 5 STEHEN DIE TAGE DRIN
           IF       CFFK05 OF CFG-CPY = "1"
            OR      (    CFFK05 OF CFG-CPY = "2"
                     AND EVEN-UNEVEN = ZERO     )
            OR      (    CFFK05 OF CFG-CPY = "3"
                     AND EVEN-UNEVEN NOT = ZERO )
                     MOVE SPACE TO X-WEEKSTR(6:1)
           ELSE
                    MOVE GREEN-RI-HEX-WRK TO X-WEEKSTR(6:1)
           END-IF.

       ENDE.

           GOBACK.
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
