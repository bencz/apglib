       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP0024.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ERMITTELN PROGRAMM-INFOS                                      *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  27.06.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ERMITTELN EINIGER PROGRAMM-INFOS             *
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

     ‚*-------------------------------------------------------------
     ‚* PPGMLOG: LGPGM, LGUSER
     ‚*-------------------------------------------------------------
           SELECT PPGMLOG-DP
                  ASSIGN       TO  DATABASE-PPGMLOG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*-- PPGMLOG: LGPGM, LGUSER
       FD  PPGMLOG-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLOG-P.
           COPY DDS-ALL-FORMATS OF PPGMLOG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0024".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* SCHALTER
       01  FIRST-CALL-PGMLOG             LIKE FIRST-CALL.
       01  RANK-WRK                      PIC 9(2).
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LIB                         LIKE CONST-LIB.
       01  X-RCN                         PIC X(10).
       01  X-PGM                         LIKE PGM-WRK.
       01  X-PGM-TYP                     LIKE PGM-TYP.
       01  X-DISPLAY                     LIKE DISPLAY-WRK.
       01  X-COMPILE-DATE                LIKE SYS-COMPIL.
       01  X-USER                        LIKE T-USER.
       01  X-SPRACHE                     LIKE SPRACHE-WRK.
       01  X-AKTION                      PIC X(20).
       01  X-RETURN1                     PIC X(21).
       01  X-RETURN2                     PIC X(21).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-LIB
                                X-RCN
                                X-PGM
                                X-PGM-TYP
                                X-DISPLAY
                                X-COMPILE-DATE
                                X-USER
                                X-SPRACHE
                                X-AKTION
                                X-RETURN1
                                X-RETURN2.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           EVALUATE X-AKTION
                    WHEN "COMPILE"      PERFORM GET-COMPILEDATE
                    WHEN "SPACES"       PERFORM GET-SPACES
                    WHEN "VERSION"      PERFORM GET-VERSION
                    WHEN "LEVEL"        PERFORM GET-LEVEL
                    WHEN OTHER          PERFORM GET-COMPILEDATE
           END-EVALUATE.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DATUM ZURÜCKGEBEN
     ‚*--------------------------------------------------------------
       GET-COMPILEDATE SECTION.
       GET-CDT-00.

     ‚* TEXT ERSTMAL FIX
           MOVE     X-COMPILE-DATE          TO X-RETURN1.
           MOVE     "##   COMPILEDATE   ##" TO X-RETURN2.

       GET-CDT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NUR LEERZEICHEN EINTRAGEN
     ‚*--------------------------------------------------------------
       GET-SPACES SECTION.
       GET-SPC-00.

           MOVE     SPACES                  TO X-RETURN1.
           MOVE     SPACES                  TO X-RETURN2.

       GET-SPC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSIONSNUMMER DES PROGRAMMS
     ‚*--------------------------------------------------------------
       GET-VERSION SECTION.
       GET-VER-00.

           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "CP"              TO CFID   OF CFG-CPY.
           MOVE     X-LIB             TO CFKEY  OF CFG-CPY.
           MOVE     X-RCN             TO CFKEY2 OF CFG-CPY.
           MOVE     X-PGM             TO CFKEY3 OF CFG-CPY.
           MOVE     SPACES            TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
                    INITIALIZE PCONFIGF OF CFG-CPY
           END-IF.

     ‚* STRING AUFBAUEN
     ‚*€   STRING   "V"                    DELIMITED BY SIZE
     ‚*€            CFFK38 OF CFG-CPY      DELIMITED BY SIZE
     ‚*€            " "                    DELIMITED BY SIZE
     ‚*€            "R"                    DELIMITED BY SIZE
     ‚*€            CFFK39 OF CFG-CPY      DELIMITED BY SIZE
     ‚*€            " "                    DELIMITED BY SIZE
     ‚*€            "M"                    DELIMITED BY SIZE
     ‚*€            CFFK40 OF CFG-CPY      DELIMITED BY SIZE
     ‚*€            "."                    DELIMITED BY SIZE
     ‚*€            CFNM04 OF CFG-CPY      DELIMITED BY SIZE
     ‚*€     INTO   X-RETURN1
     ‚*€   END-STRING.
           STRING   "Version: "            DELIMITED BY SIZE
                    CFFK38 OF CFG-CPY      DELIMITED BY SIZE
                    "."                    DELIMITED BY SIZE
                    CFFK39 OF CFG-CPY      DELIMITED BY SIZE
                    "."                    DELIMITED BY SIZE
                    CFFK40 OF CFG-CPY      DELIMITED BY SIZE
                    "."                    DELIMITED BY SIZE
                    CFNM04 OF CFG-CPY      DELIMITED BY SIZE
             INTO   X-RETURN1
           END-STRING.

     ‚* TEXT ERSTMAL FIX
     ‚*€   MOVE     "##     VERSION     ##" TO X-RETURN2.

       GET-VER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* "LEVEL" ERMITTELN
     ‚*--------------------------------------------------------------
       GET-LEVEL SECTION.
       GET-LVL-00.

           IF       FIRST-CALL-PGMLOG NOT = "1"
                    MOVE "1" TO FIRST-CALL-PGMLOG
                    OPEN     INPUT PPGMLOG-DP
           END-IF.

     ‚* ANZAHL DER AUFRUFE LESEN
           INITIALIZE PPGMLOGF OF PPGMLOG-P.
           MOVE     X-PGM              TO LGPGM  OF PPGMLOG-P.
           MOVE     X-USER             TO LGUSER OF PPGMLOG-P.
           READ     PPGMLOG-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PPGMLOGF OF PPGMLOG-P
                    MOVE 1  TO LGLFDN OF PPGMLOG-P
           END-IF.

           EVALUATE TRUE
                    WHEN LGLFDN OF PPGMLOG-P > 5000
                         MOVE 10        TO RANK-WRK
                         MOVE "PGM1060" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 3000
                         MOVE 9         TO RANK-WRK
                         MOVE "PGM1059" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 2500
                         MOVE 8         TO RANK-WRK
                         MOVE "PGM1058" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 2000
                         MOVE 7         TO RANK-WRK
                         MOVE "PGM1057" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 1500
                         MOVE 6         TO RANK-WRK
                         MOVE "PGM1056" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 1000
                         MOVE 5         TO RANK-WRK
                         MOVE "PGM1055" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 500
                         MOVE 4         TO RANK-WRK
                         MOVE "PGM1054" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 200
                         MOVE 3         TO RANK-WRK
                         MOVE "PGM1053" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 100
                         MOVE 2         TO RANK-WRK
                         MOVE "PGM1052" TO CPY-TXID
                    WHEN LGLFDN OF PPGMLOG-P > 50
                         MOVE 1         TO RANK-WRK
                         MOVE "PGM1051" TO CPY-TXID
                    WHEN OTHER
                         MOVE ZEROES    TO RANK-WRK
                         MOVE "PGM1050" TO CPY-TXID
           END-EVALUATE.
           IF       RANK-WRK NOT = ZEROES
            AND     RANK-WRK NOT = 10
                    MOVE    COL-BLU-RI TO X-RETURN1(1:1)
                    MOVE    COL-BLU    TO X-RETURN1(RANK-WRK * 2 + 2:1)
           END-IF.

           MOVE     X-SPRACHE          TO CPY-TXSPR.
           PERFORM  COPY-PGM-TXT.
           MOVE     CPY-TXTEXT         TO X-RETURN2.

       GET-LVL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
