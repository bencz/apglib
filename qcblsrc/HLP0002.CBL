       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN AUF GÜLTIGE E-MAIL ADRESSE                             *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN E-MAIL ADRESSE AUF GÜLTIGKEIT         *
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
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  POS                           PIC 9(4).

       01  TLD-STRING                    PIC 9(4).
       01  AT-STRING                     PIC 9(4).
       01  REST-STRING                   PIC 9(4).

       01  TLD-PART                      PIC 9(1).
       01  AT-PART                       PIC 9(1).
       01  REST-PART                     PIC 9(1).
       01  TLD-START                     PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-EMAIL                       PIC X(150).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-EMAIL
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE X-RETCODE.
           INITIALIZE TLD-PART, AT-PART, REST-PART.
           INITIALIZE TLD-STRING, AT-STRING, REST-STRING.
           INITIALIZE TLD-START.

     ‚* IST DER STRING GEFÜLLT?
           IF       X-EMAIL = SPACES
                    MOVE "1" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* BEISPIEL: APG@EHRHARDT-PARTNER.COM
     ‚*                                *** = TLD-PART
     ‚*               ****************     = AT-PART
     ‚*           ***                      = REST-PART

     ‚* PRÜFEN DES STRINGS VON HINTEN
           MOVE     1                 TO TLD-PART.
           MOVE     LENGTH OF X-EMAIL TO POS.
           PERFORM  WITH TEST BEFORE UNTIL POS = ZEROES
                    IF   TLD-PART NOT = ZEROES
     ‚*                  BEIM ERSTEN PUNKT ENDET DIE TLD
                         IF   X-EMAIL(POS:1) = "."
                              MOVE ZEROES TO TLD-PART
                              MOVE 1      TO AT-PART
                         ELSE
     ‚*                       ZÄHLEN DER ZEICHEN DER TOP-LEVEL DOMAIN
                              IF   X-EMAIL(POS:1) NOT = SPACES
                                   ADD 1  TO TLD-STRING
                                   MOVE 1 TO TLD-START
                              ELSE
     ‚*                            NACH DEM ERSTEN ZEICHEN DARF KEIN
     ‚*                            LEERZEICHEN MEHR FOLGEN
                                   IF   TLD-START NOT = ZEROES
                                        MOVE "4" TO X-RETCODE
                                        GO TO ENDE
                                   END-IF
                              END-IF
                         END-IF
                    ELSE
     ‚*                  BEFINDEN WIR UNS ZWISCHEN DER TLD UND DEM @?
                         IF   AT-PART NOT = ZEROES
                              IF   X-EMAIL(POS:1) = "@"
                                   MOVE ZEROES TO AT-PART
                                   MOVE 1      TO REST-PART
                              ELSE
     ‚*                            ZÄHLEN DER ZEICHEN
                                   IF   X-EMAIL(POS:1) NOT = SPACES
                                        ADD 1 TO AT-STRING
                                   ELSE
                                        MOVE "4" TO X-RETCODE
                                        GO TO ENDE
                                   END-IF
                              END-IF
                         ELSE
     ‚*                       ZÄHLEN DER ZEICHEN
                              IF   X-EMAIL(POS:1) NOT = SPACES
                                   ADD 1 TO REST-STRING
                              ELSE
                                   MOVE "4" TO X-RETCODE
                                   GO TO ENDE
                              END-IF
                         END-IF
                    END-IF
     ‚*             UNGÜLTIGES ZEICHEN?
                    PERFORM CHECK-ZEICHEN
                    IF   RET-CODE NOT = SPACES
                         MOVE "5" TO X-RETCODE
                         GO TO ENDE
                    END-IF
     ‚*             EINS WENIGER...
                    SUBTRACT 1 FROM POS
           END-PERFORM.

     ‚* FEHLT EIN TEIL?
           IF       TLD-PART  NOT = ZEROES
            OR      AT-PART   NOT = ZEROES
            OR      REST-PART     = ZEROES
                    MOVE "2" TO X-RETCODE
                    GO TO ENDE
           END-IF.

     ‚* PASSEN DIE LÄNGEN?
           IF       TLD-STRING   < 2
            OR      AT-STRING    < 3
            OR      REST-STRING  = ZEROES
                    MOVE "3" TO X-RETCODE
                    GO TO ENDE
           END-IF.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN AUF GÜLTIGE ZEICHEN
     ‚*--------------------------------------------------------------
       CHECK-ZEICHEN SECTION.
       CHK-ZEI-00.

           INITIALIZE RET-CODE.

       CHK-ZEI-90.
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
