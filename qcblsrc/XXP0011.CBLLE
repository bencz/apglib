     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0011.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANZEIGE KW-DATEN                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  20.08.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ANZEIGE VON KW-DATEN                         *
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
     ‚* PAZUBER: ABKW, ABJAHR, ABUSER
     ‚*-------------------------------------------------------------
           SELECT PAZUBER-DP
                  ASSIGN       TO  DATABASE-PAZUBER
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PAZUBER: ABKW, ABJAHR, ABUSER
       FD  PAZUBER-DP
           LABEL RECORDS ARE STANDARD.
       01  PAZUBER-P.
           COPY DDS-ALL-FORMATS OF PAZUBER.
      /
       WORKING-STORAGE SECTION.

     ‚* LISTE DER TEMPORÄREN INDIKATOREN
       01  INXX.
         02 IN91                          PIC 1.
         02 IN95                          PIC 1.
         02 IN96                          PIC 1.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  XXD0011DE
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  FORMAT2Y.
           COPY  DDS-FMT02-I OF  XXD0011DE
           REPLACING FMT02-I BY FORMAT2-I
                     FMT02-I-INDIC BY FORMAT2-I-INDIC.

       01  FORMAT3X.
           COPY  DDS-FMT03-O OF  XXD0011DE
           REPLACING FMT03-O BY FORMAT3
                     FMT03-O-INDIC BY FORMAT3-O-INDIC.

       01  FORMAT3Y.
           COPY  DDS-FMT03-I OF  XXD0011DE
           REPLACING FMT03-I BY FORMAT3-I
                     FMT03-I-INDIC BY FORMAT3-I-INDIC.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0011".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

       01  AUSWAHL                       PIC X(2).
       01  KW-WRK                        LIKE ABKW   OF PAZUBER-P.
       01  DATE-WRK                      PIC 9(4).
       01  VERANTW-WRK                   PIC X(10).

       01  MAIL-ANZ                      LIKE MAIL-ANZAHL OF MAIL-EMPF.
       01  MAIL-C-ANZ                    LIKE MAIL-ANZAHL OF MAIL-EMPF.

     ‚* PARAMETER FÜR DAS HLP0022
       01  SENDER-KEY                    LIKE CFKEY  OF CFG-CPY.
       01  CHECK-ID                      LIKE CFID   OF CFG-CPY.
       01  CHECK-KEY                     LIKE CFKEY  OF CFG-CPY.
       01  MAIL-AKTION                   PIC X(20).

     ‚* PARAMETER FÜR DAS XXP0045
       01  0045-AKTION                   PIC X(20).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0011" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DISPLAY ÖFFNEN
           MOVE     "XXD0011DE" TO DISPLAY-WRK.
           PERFORM  COPY-DISPLAY-OPEN.

     ‚* DATEI ÖFFNEN
           OPEN     INPUT PAZUBER-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* WENN ICH DAS NICHT BIN, DANN EINFACH APG/BVIEW ÖFFNEN
     ‚* DANN KOMMT KEINER MEHR DURCHEINANDER
           IF       T-USER NOT = "APG"
            AND     T-USER NOT = "CFR"
            AND     T-USER NOT = "IRZ"
            AND     T-USER NOT = "DSM"
            AND     T-USER NOT = "FHY"
            AND     T-USER NOT = "THT"
            AND     T-USER NOT = "GUJ"
                    CALL "XXP0040"
                    END-CALL
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT3.
           INITIALIZE F2, F3, F12.
           MOVE     1 TO SFL-ZEILE.

     ‚* VERANTWORTLICHER VORBELEGEN
           MOVE     T-USER TO VERANTW-WRK

     ‚* DATUM ERMITTELN
           PERFORM  COPY-GET-TIME.
     ‚* KW ZUM DATUM ERMITTELN
           CALL     "CFP8007" USING DATE-8
                                    KW-WRK
           END-CALL.
           SUBTRACT 1 FROM KW-WRK.
           MOVE     DATE-8(1:4) TO DATE-WRK.

       ANF010.
           MOVE     AUS TO INXX.

           PERFORM  LESE-DATEN.
           IF       DATEN = ZEROES
                    PERFORM FEHLERMELDUNG
           ELSE
                    PERFORM ANZEIGE-SUBFILE
           END-IF.
           IF       F2 = "1"
                    GO TO ENDE
           END-IF.
           IF       F12 = "1"
                    GO TO ENDE
           END-IF.

           GO TO    ANF010.

       ENDE.
           CLOSE    PAZUBER-DP.

     ‚* DISPLAY SCHLIEßEN
           PERFORM  COPY-DISPLAY-CLOSE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

           INITIALIZE DATEN, ANZREC-WRK.
     ‚* PAZUBER LESEN
           INITIALIZE PAZUBERF OF PAZUBER-P.
           MOVE     KW-WRK              TO ABKW   OF PAZUBER-P.
           MOVE     DATE-WRK            TO ABJAHR OF PAZUBER-P.
           MOVE     SPACES              TO ABUSER OF PAZUBER-P.
           START    PAZUBER-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PAZUBER-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       ABKW   OF PAZUBER-P NOT = KW-WRK
            OR      ABJAHR OF PAZUBER-P NOT = DATE-WRK
                    GO TO LES-DAT-90
           END-IF.

     ‚* SCHON ABGEGEBEN?
           IF       ABSTAT OF PAZUBER-P     = "V"
                    GO TO LES-DAT-20
           END-IF.

     ‚* STIMMT VERANTWORTLICHER?
           IF       VERANTW-WRK       NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     "H051"              TO CFID   OF CFG-CPY
                    MOVE     "BERICHTSHEFT"      TO CFKEY  OF CFG-CPY
                    MOVE     ABUSER OF PAZUBER-P TO CFKEY2 OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
                    IF       CFG-DA OF CFG-CPY = ZEROES
                     OR      CFDAT1 OF CFG-CPY NOT = VERANTW-WRK
                             GO TO LES-DAT-20
                    END-IF
           END-IF.

     ‚* SUBFILE-CLEAR
           IF       DATEN = ZEROES
                    MOVE    1 TO  DATEN
                    INITIALIZE FORMAT3
                    MOVE    AUS     TO FORMAT3-O-INDIC
                    MOVE    ZEROES  TO SUBFILE-RECORD-NUMBER
                    MOVE    ZEROES  TO ANZREC OF FORMAT3
                    MOVE    AN      TO IN95 OF INXX
                    MOVE    AUS     TO IN91 OF INXX
                    MOVE    AUS     TO IN96 OF INXX
                    MOVE    CORR INXX  TO FORMAT3-O-INDIC
                    MOVE    "FMT03" TO FMT-WRK
                    MOVE    FORMAT3 TO WSREC-WRK
                    PERFORM COPY-DISPLAY-SFLWRITE
           END-IF.

     ‚* SUBFILE-ZEILE FÜLLEN UND SCHREIBEN
           INITIALIZE FORMAT2.
           MOVE     CORR PAZUBERF OF PAZUBER-P TO FORMAT2.
           MOVE     SPACES       TO AUSW OF FORMAT2.
     ‚*€   MOVE     CORR INXX    TO FORMAT2-O-INDIC.
           ADD      1            TO SUBFILE-RECORD-NUMBER.
           ADD      1            TO ANZREC-WRK.
           MOVE     "FMT02"      TO FMT-WRK.
           MOVE     FORMAT2      TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
     ‚*€   MOVE     AUS TO INXX.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANZEIGEN DES SUBFILES
     ‚*--------------------------------------------------------------
       ANZEIGE-SUBFILE SECTION.
       ANZ-SFL-00.

           INITIALIZE F2.
           INITIALIZE FORMAT3.
     ‚* SELEKTION ANZEIGEN
           MOVE     AUS    TO INXX.

       ANZ-SFL-20.
     ‚* AUSGABE FUSSZEILE
           MOVE     "FMT05"         TO  FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

     ‚* SUBFILEÜBERSCHRIFT ANHAND TEXTE AUS PCFGTXT FÜLLEN
           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE
           IF       SFL-ZEILE NOT > SUBFILE-RECORD-NUMBER
                    MOVE SFL-ZEILE TO  SUBFILE-RECORD-NUMBER
           END-IF.
           MOVE     VERANTW-WRK TO CFDAT1 OF FORMAT3.
           MOVE     KW-WRK   TO KW     OF FORMAT3.
           MOVE     DATE-WRK TO ABJAHR OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR OF FORMAT3.
           MOVE     ANZREC-WRK            TO ANZREC  OF FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AN     TO IN91  IN  INXX.
           MOVE     CORR INXX TO FORMAT3-O-INDIC.
           MOVE     "FMT03" TO FMT-WRK.
           MOVE     FORMAT3 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLWRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.
           MOVE     AUS  TO INXX.

     ‚* F5 AKTUALISIEREN
           IF       IN05 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F12 ABBRECHEN
           IF       IN12 IN FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO ANZ-SFL-90
           END-IF.

     ‚* F20 ERINNERUNGSMAIL
           IF       IN20 IN FORMAT3-I-INDIC = AN
                    PERFORM WRITE-ERINNERUNGSMAIL
                    GO TO ANZ-SFL-20
           END-IF.

     ‚* F24 BÖSE ERINNERUNGSMAIL
           IF       IN24 IN FORMAT3-I-INDIC = AN
                    PERFORM WRITE-BOESE-ERINNERUNGSMAIL
                    GO TO ANZ-SFL-20
           END-IF.

     ‚* ÄNDERUNG?
           IF       KW     OF FORMAT3 NOT = KW-WRK
            OR      ABJAHR OF FORMAT3 NOT = DATE-WRK
            OR      CFDAT1 OF FORMAT3 NOT = VERANTW-WRK
                    MOVE KW     OF FORMAT3 TO KW-WRK
                    MOVE ABJAHR OF FORMAT3 TO DATE-WRK
                    MOVE CFDAT1 OF FORMAT3 TO VERANTW-WRK
                    GO TO ANZ-SFL-90
           END-IF.

       ANZ-SFL-50.
     ‚* LESEN VON SUBFILE NEXT MODIFIED RECORD
           MOVE     "FMT02" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-SFLREAD.
           IF       BS-STAT NOT = ZEROES
                    MOVE "1" TO F5
                    GO TO ANZ-SFL-90
           END-IF.

           MOVE     WSREC-WRK      TO FORMAT2-I.
           MOVE     CORR FORMAT2-I TO FORMAT2.

           IF       AUSW OF FORMAT2 = SPACES
                    GO TO ANZ-SFL-50
           END-IF.

     ‚* RECORD-NUMMER SICHERN, DAMIT BEI FEHLERMELDUNG DER CURSOR
     ‚* AN DER RICHTIGEN POSITION STEHT
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.

     ‚* BERECHTIGUNG FÜR DIESE FUNKTION ÜBERPRÜFEN
           IF       AUSW OF FORMAT2 NOT = SPACES
                    MOVE  SPACES          TO RET-CODE
                    MOVE  AUSW OF FORMAT2 TO AUSWAHL
                    CALL "CFP9002" USING T-USER
                                         AUSWAHL
                                         PGM-WRK
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         MOVE     SPACES TO AUSW OF FORMAT2
                         MOVE     "FMT02" TO FMT-WRK
                         MOVE     FORMAT2 TO WSREC-WRK
                         PERFORM  COPY-DISPLAY-SFLREWRITE
                         GO TO ANZ-SFL-50
                    END-IF
           END-IF.

     ‚* ÄNDERN DES STATUS
           IF       AUSW OF FORMAT2 = "1"
            OR      AUSW OF FORMAT2 = "2"
                    IF   AUSW OF FORMAT2 = "2"
                         MOVE "NO-MAIL" TO 0045-AKTION
                    ELSE
                         MOVE SPACES    TO 0045-AKTION
                    END-IF
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
           END-IF.

     ‚* ÄNDERN DES STATUS
           IF       AUSW OF FORMAT2 = "6"
            OR      AUSW OF FORMAT2 = "5"
                    MOVE "1"         TO SFL-CHG
     ‚*             MIT ODER ODER EMAIL
                    IF   AUSW OF FORMAT2 = "6"
                         MOVE "ADD-ERROR" TO 0045-AKTION
                    ELSE
                         MOVE "MAILERROR" TO 0045-AKTION
                    END-IF
                    CALL "XXP0045" USING ABKW   OF FORMAT2
                                         ABJAHR OF FORMAT2
                                         ABUSER OF FORMAT2
                                         0045-AKTION
                    END-CALL
           END-IF.

           MOVE     SPACES TO AUSW OF FORMAT2.
           MOVE     SUBFILE-RECORD-NUMBER TO SFL-ZEILE.
           MOVE     "FMT02" TO FMT-WRK.
           MOVE     FORMAT2 TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-SFLREWRITE.

           GO TO    ANZ-SFL-50.

       ANZ-SFL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* FEHLERMELDUNG WENN KEINE SFL-ZEILEN VORHANDEN
     ‚*--------------------------------------------------------------
       FEHLERMELDUNG SECTION.
       FEH-MEL-00.

           INITIALIZE F3, F12.
           INITIALIZE FORMAT3.

     ‚* AUSGABE INFOZEILE DASS DISPLAY LEER IST
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT04" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           MOVE     SPACES  TO WSREC-WRK.
           MOVE     "FMT05" TO FMT-WRK.
           PERFORM  COPY-DISPLAY-WRITE.

           PERFORM  SFL-HEADER.

     ‚* ANZEIGE SUBFILE KOPF
           MOVE     KW-WRK   TO KW OF FORMAT3.
           MOVE     DATE-WRK TO ABJAHR OF FORMAT3.
           MOVE     VERANTW-WRK TO CFDAT1 OF FORMAT3.
           MOVE     SUBFILE-RECORD-NUMBER TO SFRECNR IN FORMAT3.
           MOVE     AUS    TO IN95  IN  INXX.
           MOVE     AN     TO IN96  IN  INXX.
           MOVE     AUS    TO IN91  IN  INXX.
           MOVE     CORR   INXX    TO  FORMAT3-O-INDIC.
           MOVE     "FMT03"        TO FMT-WRK.
           MOVE     FORMAT3        TO WSREC-WRK.
           PERFORM  COPY-DISPLAY-WRITE.
           PERFORM  COPY-DISPLAY-READ.
           MOVE     WSREC-WRK      TO FORMAT3-I.
           MOVE     CORR FORMAT3-I TO FORMAT3.

           IF       IN12 OF FORMAT3-I-INDIC = AN
                    MOVE "1" TO F12
                    GO TO FEH-MEL-90
           END-IF.

     ‚* ÄNDERUNG?
           IF       KW     OF FORMAT3 NOT = KW-WRK
            OR      ABJAHR OF FORMAT3 NOT = DATE-WRK
            OR      CFDAT1 OF FORMAT3 NOT = VERANTW-WRK
                    MOVE KW     OF FORMAT3 TO KW-WRK
                    MOVE ABJAHR OF FORMAT3 TO DATE-WRK
                    MOVE CFDAT1 OF FORMAT3 TO VERANTW-WRK
                    GO TO FEH-MEL-90
           END-IF.

       FEH-MEL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* SUBFILEÜBERSCHRIFTEN FÜLLEN
     ‚*---------------------------------------------------------------
       SFL-HEADER SECTION.
       SFL-HEA-00.

       SFL-HEA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERINNERUNGSMAIL SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-ERINNERUNGSMAIL SECTION.
       WRT-EMA-00.

           INITIALIZE MAIL-TAB.
           MOVE     ZEROES TO MAIL-ANZ, MAIL-C-ANZ.
     ‚* PAZUBER LESEN
           INITIALIZE PAZUBERF OF PAZUBER-P.
           MOVE     KW-WRK              TO ABKW   OF PAZUBER-P.
           MOVE     DATE-WRK            TO ABJAHR OF PAZUBER-P.
           MOVE     SPACES              TO ABUSER OF PAZUBER-P.
           START    PAZUBER-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WRT-EMA-80
           END-IF.

       WRT-EMA-20.
           READ     PAZUBER-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WRT-EMA-80
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       ABKW   OF PAZUBER-P NOT = KW-WRK
            OR      ABJAHR OF PAZUBER-P NOT = DATE-WRK
                    GO TO WRT-EMA-80
           END-IF.

     ‚* SCHON ABGEGEBEN?
           IF       ABSTAT OF PAZUBER-P     = "V"
                    GO TO WRT-EMA-20
           END-IF.

     ‚* PRÜFEN OB MAIL GESCHRIEBEN WERDEN SOLL
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H051"              TO CFID   OF CFG-CPY.
           MOVE     "BERICHTSHEFT"      TO CFKEY  OF CFG-CPY.
           MOVE     ABUSER OF PAZUBER-P TO CFKEY2 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
            OR      CFFK01 OF CFG-CPY = 1
                    GO TO WRT-EMA-20
           END-IF.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           IF       MAIL-ANZ < 10
                    ADD    1                TO MAIL-ANZ
                    MOVE MAIL-ANZ           TO MAIL-ANZAHL OF MAIL-EMPF
                    STRING   ABUSER OF PAZUBER-P    DELIMITED BY SPACES
                            "@ehrhardt-partner.com" DELIMITED BY SIZE
                       INTO MAIL-ADRESSE OF MAIL-EMPF-REC(MAIL-ANZ)
                    END-STRING
           ELSE
     ‚*             NUR 8 GEHEN HIER REIN DA GGF. NOCH 2 DAZUKOMMEN
                    IF   MAIL-C-ANZ < 8
                         ADD  1          TO MAIL-C-ANZ
                         MOVE MAIL-C-ANZ TO MAIL-ANZAHL OF MAIL-KOPIE
                         STRING ABUSER OF PAZUBER-P DELIMITED BY SPACES
                              "@ehrhardt-partner.com" DELIMITED BY SIZE
                            INTO MAIL-ADRESSE OF
                                 MAIL-KOPIE-REC(MAIL-C-ANZ)
                         END-STRING
                    END-IF
           END-IF.

           GO TO    WRT-EMA-20.

       WRT-EMA-80.
     ‚* SENDER ZUSAMMENBAUEN
           MOVE     "SYSEH03"        TO MAIL-HOST   OF MAIL-TAB.
           MOVE     "25"             TO MAIL-PORT   OF MAIL-TAB.
           MOVE     "APG@EHRHARDT-PARTNER.COM"
                                     TO MAIL-SENDER OF MAIL-TAB.

     ‚* MAIL-TOPIC AUFRUFEN
           STRING   PGM-WRK            DELIMITED BY SPACES
                    ": "               DELIMITED BY SIZE
                    "BERICHTSHEFT KW " DELIMITED BY SIZE
                    KW-WRK             DELIMITED BY SIZE
                    "/"                DELIMITED BY SIZE
                    DATE-WRK           DELIMITED BY SIZE
                    " fehlt noch"      DELIMITED BY SIZE
               INTO MAIL-TOPIC OF MAIL-TAB
           END-STRING.

     ‚* EMAIL BODY ALLGEMEINE INFOS - P3
           MOVE     1 TO MAIL-LEN.
           STRING   "Hallo,"                   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "mir fehlen noch die "     DELIMITED BY SIZE
                    "Berichtshefte zu der KW " DELIMITED BY SIZE
                    KW-WRK                     DELIMITED BY SIZE
                    "/"                        DELIMITED BY SIZE
                    DATE-WRK                   DELIMITED BY SIZE
                    ". Könnt ihr die mir "     DELIMITED BY SIZE
                    "bitte vorbeibringen? "    DELIMITED BY SIZE
                    "Danke."                   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "MfG,"                     DELIMITED BY SIZE
                    X"25"                      DELIMITED BY SIZE
                    T-USER                     DELIMITED BY SPACES
                    X"252525"                  DELIMITED BY SIZE
                    "------------------------" DELIMITED BY SPACES
                    X"25"                      DELIMITED BY SIZE
                    "Diese E-Mail kann in APG" DELIMITED BY SIZE
                    "/CONFIG im Schlüssel H05" DELIMITED BY SIZE
                    "1 unter dem Eintrag BERI" DELIMITED BY SIZE
                    "CHTSHEFT deaktiviert wer" DELIMITED BY SIZE
                    "den."                     DELIMITED BY SIZE
               INTO MAIL-DATA
               WITH POINTER MAIL-LEN
           END-STRING.
     ‚* ALS KOPIE IMMER MICH EINTRAGEN
           ADD      1                TO MAIL-C-ANZ.
           MOVE     MAIL-C-ANZ       TO MAIL-ANZAHL OF MAIL-KOPIE.
           STRING   "APG"                   DELIMITED BY SPACES
                    "@ehrhardt-partner.com" DELIMITED BY SIZE
               INTO MAIL-ADRESSE OF MAIL-KOPIE-REC(MAIL-C-ANZ)
           END-STRING.
     ‚* WENN DAS IRGENDWER AUßER MIR ABGESCHICKT HAT, DANN AUCH IHN
     ‚* ALS EMPFÄNGER EINTRAGEN
           IF       T-USER NOT = "APG"
                    ADD      1       TO MAIL-C-ANZ
                    MOVE     MAIL-C-ANZ    TO MAIL-ANZAHL OF MAIL-KOPIE
                    STRING  T-USER                  DELIMITED BY SPACES
                            "@ehrhardt-partner.com" DELIMITED BY SIZE
                       INTO MAIL-ADRESSE OF MAIL-KOPIE-REC(MAIL-C-ANZ)
                    END-STRING
           END-IF.

     ‚* MAILPROGRAMM AUFRUFEN
           IF       MAIL-ANZAHL OF MAIL-EMPF > ZEROES
                    CALL     "HLP0006"  USING MAIL-HOST
                                              MAIL-PORT
                                              MAIL-SENDER
                                              MAIL-EMPF
                                              MAIL-KOPIE
                                              MAIL-BKOPIE
                                              MAIL-TOPIC
                                              MAIL-DATA
                                              MAIL-FILE
                                              MAIL-RETTXT
                    END-CALL
           END-IF.

       WRT-EMA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERINNERUNGSMAIL SCHREIBEN (NICHT DEAKTIVIERBAR)
     ‚*--------------------------------------------------------------
       WRITE-BOESE-ERINNERUNGSMAIL SECTION.
       WRT-BMA-00.

           INITIALIZE MAIL-TAB.
           MOVE     ZEROES           TO MAIL-ANZAHL OF MAIL-EMPF.
     ‚* PAZUBER LESEN
           INITIALIZE PAZUBERF OF PAZUBER-P.
           MOVE     KW-WRK              TO ABKW   OF PAZUBER-P.
           MOVE     DATE-WRK            TO ABJAHR OF PAZUBER-P.
           MOVE     SPACES              TO ABUSER OF PAZUBER-P.
           START    PAZUBER-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WRT-BMA-80
           END-IF.

       WRT-BMA-20.
           READ     PAZUBER-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO WRT-BMA-80
           END-IF.

     ‚* SCHLÜSSELABFRAGE
           IF       ABKW   OF PAZUBER-P NOT = KW-WRK
            OR      ABJAHR OF PAZUBER-P NOT = DATE-WRK
                    GO TO WRT-BMA-80
           END-IF.

     ‚* SCHON ABGEGEBEN?
           IF       ABSTAT OF PAZUBER-P     = "V"
                    GO TO WRT-BMA-20
           END-IF.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           ADD      1                TO MAIL-ANZAHL OF MAIL-EMPF.
           STRING   ABUSER OF PAZUBER-P     DELIMITED BY SPACES
                    "@ehrhardt-partner.com" DELIMITED BY SIZE
               INTO MAIL-ADRESSE OF
                    MAIL-EMPF-REC(MAIL-ANZAHL OF MAIL-EMPF)
           END-STRING.

           GO TO    WRT-BMA-20.

       WRT-BMA-80.
     ‚* SENDER ZUSAMMENBAUEN
           MOVE     "SYSEH03"        TO MAIL-HOST   OF MAIL-TAB.
           MOVE     "25"             TO MAIL-PORT   OF MAIL-TAB.
           MOVE     "APG@EHRHARDT-PARTNER.COM"
                                     TO MAIL-SENDER OF MAIL-TAB.

     ‚* MAIL-TOPIC AUFRUFEN
           STRING   PGM-WRK                    DELIMITED BY SPACES
                    ": "                       DELIMITED BY SIZE
                    "BERICHTSHEFT KW "         DELIMITED BY SIZE
                    KW-WRK                     DELIMITED BY SIZE
                    "/"                        DELIMITED BY SIZE
                    DATE-WRK                   DELIMITED BY SIZE
                    " fehlt immer noch !!!"    DELIMITED BY SIZE
               INTO MAIL-TOPIC OF MAIL-TAB
           END-STRING.

     ‚* EMAIL BODY ALLGEMEINE INFOS - P3
           MOVE     1 TO MAIL-LEN.
           STRING   "Hallo,"                   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "mir fehlen noch die "     DELIMITED BY SIZE
                    "Berichtshefte zu der KW " DELIMITED BY SIZE
                    KW-WRK                     DELIMITED BY SIZE
                    "/"                        DELIMITED BY SIZE
                    DATE-WRK                   DELIMITED BY SIZE
                    ". Bitte so schnell wie "  DELIMITED BY SIZE
                    "möglich vorbeibringen!"   DELIMITED BY SIZE
                    "Danke."                   DELIMITED BY SIZE
                    X"2525"                    DELIMITED BY SIZE
                    "MfG,"                     DELIMITED BY SIZE
                    X"25"                      DELIMITED BY SIZE
                    T-USER                     DELIMITED BY SPACES
                    X"252525"                  DELIMITED BY SIZE
                    "------------------------" DELIMITED BY SPACES
                    X"25"                      DELIMITED BY SIZE
                    "Diese E-Mail kann NICHT " DELIMITED BY SIZE
                    "deaktiviert werden!!!"    DELIMITED BY SIZE
               INTO MAIL-DATA
               WITH POINTER MAIL-LEN
           END-STRING.
     ‚* ALS KOPIE IMMER MICH EINTRAGEN
           MOVE     1                TO MAIL-ANZAHL OF MAIL-KOPIE.
           STRING   "APG"                   DELIMITED BY SPACES
                    "@ehrhardt-partner.com" DELIMITED BY SIZE
               INTO MAIL-ADRESSE OF MAIL-KOPIE-REC(1)
           END-STRING.
     ‚* WENN DAS IRGENDWER AUßER MIR ABGESCHICKT HAT, DANN AUCH IHN
     ‚* ALS EMPFÄNGER EINTRAGEN
           IF       T-USER NOT = "APG"
                    ADD      1       TO MAIL-ANZAHL OF MAIL-KOPIE
                    STRING  T-USER                  DELIMITED BY SPACES
                            "@ehrhardt-partner.com" DELIMITED BY SIZE
                       INTO MAIL-ADRESSE OF MAIL-KOPIE-REC(2)
                    END-STRING
           END-IF.

     ‚* MAILPROGRAMM AUFRUFEN
           IF       MAIL-ANZAHL OF MAIL-EMPF > ZEROES
                    CALL     "HLP0006"  USING MAIL-HOST
                                              MAIL-PORT
                                              MAIL-SENDER
                                              MAIL-EMPF
                                              MAIL-KOPIE
                                              MAIL-BKOPIE
                                              MAIL-TOPIC
                                              MAIL-DATA
                                              MAIL-FILE
                                              MAIL-RETTXT
                    END-CALL
           END-IF.

       WRT-BMA-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
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
