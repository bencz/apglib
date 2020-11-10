     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP9068.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SUBFILETEST                                                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  19.02.2014                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TESTANZEIGE MIT SUBFILE                      *
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
     ‚* BILDSCHIRMDATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-XXD9068DE
                  ORGANIZATION IS  TRANSACTION
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF XXD9068DE.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN03                          PIC 1.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD9068DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD9068DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD9068DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD9068DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

       01  FORMAT4X.
           COPY  DDS-FMT04-O OF  XXD9068DE
           REPLACING FMT04-O BY FORMAT4
                     FMT04-O-INDIC BY FORMAT4-O-INDIC.

       01  FORMAT4Y.
           COPY  DDS-FMT04-I OF  XXD9068DE
           REPLACING FMT04-I BY FORMAT4-I
                     FMT04-I-INDIC BY FORMAT4-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP9068".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST  ".
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP9068" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   WS-DISPLAY.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT3.
           INITIALIZE F3, F12.
           MOVE     1 TO SFL-ZEILE.

       ANF010.
           MOVE     AUS TO INXX.

           PERFORM  LESE-DATEN.
           PERFORM  ANZEIGE-SUBFILE.

       ENDE.
           CLOSE                 WS-DISPLAY.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN.
     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FMT03-O-INDIC
                    WRITE   SUBFILE WS-REC FORMAT IS "FMT03"
           END-IF.

     ‚* EIN DUMMY RECORD
           INITIALIZE FORMAT2.
           MOVE     "ABC" TO OPTEXT OF FORMAT2.

           MOVE     SPACES       TO AUSW OF FORMAT2.
     ‚*€   MOVE     CORR INXX    TO FMT02-O-INDIC.
           MOVE     CORR FORMAT2 TO FMT02-O.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT02".
           MOVE     AUS TO INXX.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE FORMAT3.
     ‚* SELEKTION ANZEIGEN
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           INITIALIZE FORMAT4.
           MOVE     CORR   FORMAT4  TO  FMT04-O.
           WRITE    WS-REC FORMAT IS "FMT04".
     ‚*€   WRITE    WS-REC FORMAT IS "FMT05".

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.

           MOVE     22 TO CURY OF FORMAT3.
           MOVE     2  TO CURX OF FORMAT3.
           MOVE     AN     TO IN03  OF INXX.

           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR   INXX     TO  FMT03-O-INDIC.
           MOVE     CORR   FORMAT3  TO  FMT03-O.
           WRITE    SUBFILE WS-REC FORMAT IS "FMT03".
           MOVE     1      TO  SUBFILE-RECORD-NUMBER.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FMT03-O.
           READ     WS-DISPLAY  FORMAT IS "FMT03".
           MOVE     CORR FMT03-I      TO FORMAT3.
           MOVE     AUS  TO INXX

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FMT03-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           READ     SUBFILE WS-DISPLAY NEXT MODIFIED RECORD
                    FORMAT IS "FMT02".
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     CORR FMT02-I TO FORMAT2.

           IF       AUSW OF FORMAT2 = SPACES
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     CORR FORMAT2 TO FMT02-O.

           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           REWRITE  SUBFILE WS-REC FORMAT IS "FMT02".

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
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
