     ‚*PROCESS  XREF.
     ‚*
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              SCP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SCHULUNGSAUFGABE COBOL-BEFEHLE                                *
     ‚*---------------------------------------------------------------*
     ‚* COPYRIGHT BY  :  EHRHARDT + PARTNER GMBH & CO. KG             *
     ‚*                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
     ‚*                  56154 BOPPARD-BUCHHOLZ                       *
     ‚*                  +49 (0) 67 42 / 87 27 0                      *
     ‚*                  INFO@EHRHARDT-PARTNER.COM                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :           P. KAMPE                            *
     ‚*                                                               *
     ‚* ERSTELLT AM   :           JUNI 05                             *
     ‚*                                                               *
     ‚* PROBLEM       :           COBOL-BEFEHLE PER DISP AUFRUFEN     *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON            GRUND DER ÄNDERUNG                  *
     ‚*                                                               *
     ‚*---------------------------------------------------------------*
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
     ‚*-   BILDSCHIRM-DATEI
     ‚*-------------------------------------------------------------
           SELECT WS-DISPLAY
                  ASSIGN       TO  WORKSTATION-SCD0002DE
                  ORGANIZATION IS  TRANSACTION
                  CONTROL-AREA IS  WS-CONTROL
                  ACCESS       IS  DYNAMIC
                  RELATIVE KEY IS  SUBFILE-RECORD-NUMBER
                  FILE STATUS  IS  BS-STAT.

      /
       DATA DIVISION.
       FILE SECTION.

     ‚*-----BILDSCHIRMDATEI -------------------------
       FD   WS-DISPLAY
            LABEL RECORDS ARE OMITTED.
       01   WS-REC.
            COPY DDS-ALL-FORMATS OF SCD0002DE.

      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* STANDARDFELDER DISPLAYHANDLING
           COPY WRKDISPLAY OF APG-QCPYSRC.

     ‚*  VORBEREITUNG FÜR DISPLAY I-O
       01  FORMAT1X.
           COPY  DDS-FMT01-O OF SCD0002DE
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT1Y.
           COPY  DDS-FMT01-I OF SCD0002DE
           REPLACING FMT01-I BY FORMAT1-I
                     FMT01-I-INDIC BY FORMAT1-I-INDIC.

     ‚* VARIABLE FÜR DEN PROGRAMMNAMEN
       01  PGM-WRK                        PIC X(10) VALUE "SCP0002".
       01  PGM-TYP                        PIC X(8)  VALUE "SCHULUNG".

     ‚*    LÄNGE DER ALPHANUMERISCHEN FELDER
           05  EINAL              PIC 9(02).
           05  AUSAL              PIC 9(02).
     ‚*    LAUFINDEX FÜR SCHLEIFEN UND FELD FÜR ZWISCHENERGEBNISSE
           05  LAUF-IDX           PIC 9(03).
           05  ZWISCHEN-HLP       PIC 9(10).
     ‚*    3 ZÄHLER FÜR LEERSTELLEN
           05  LEER1-IDX          PIC 9(02).
           05  LEER2-IDX          PIC 9(02).
           05  LEER3-IDX          PIC 9(02).
           05  LEN                PIC 9(02).
           05  ERG                PIC 9(02).
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------

     ‚*-------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "SCP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ÖFFNEN BILDSCHIRM
           OPEN     I-O   WS-DISPLAY.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ERMITTELN DER LÄNGE DER ALPHANUMMERISCHEN FELDER
           COMPUTE EINAL = LENGTH OF EINA01 OF FORMAT1.
           COMPUTE AUSAL = LENGTH OF AUSA01 OF FORMAT1.

       ANF010.
     ‚* BILDSCHIRMINHALTE LÖSCHEN IM FALLE VON F12
           INITIALIZE FORMAT1.

     ‚* SPRUNGMARKE FÜR WIEDERHOLUNG OHNE F12
       ANF020.

     ‚* DISPLAY ZEIGEN UND INPUT HOLEN
     ‚*€   MOVE     CORR INXX         TO FMT01-O-INDIC.
           MOVE     CORR FORMAT1      TO FMT01-O.
           WRITE    WS-REC     FORMAT IS "FMT01".
           READ     WS-DISPLAY FORMAT IS "FMT01".
           MOVE     CORR FMT01-I      TO FORMAT1.
     ‚*€   MOVE     AUS TO INXX.

     ‚* ABFRAGE AUF F3 UND F12
           IF       IN03 OF FMT01-I-INDIC = AN
                    GO TO ENDE
           END-IF.

           IF       IN12 OF FMT01-I-INDIC = AN
                    GO TO ANF010
           END-IF.

     ‚* AUSGABEFELDER LÖSCHEN
           PERFORM AUSGABE-INIT.

     ‚* AKTIONS-ABFRAGE-AUSWERTUNG
           EVALUATE AKTION OF FORMAT1
                    WHEN 1  PERFORM BEFEHL-ADD
                    WHEN 2  PERFORM BEFEHL-SUBTRACT
                    WHEN 3  PERFORM BEFEHL-MULTIPLY
                    WHEN 4  PERFORM BEFEHL-DIVIDE
                    WHEN 5  PERFORM BEFEHL-COMPUTE
                    WHEN 6  PERFORM BEFEHL-MOVE
                    WHEN 7  PERFORM BEFEHL-RECHTS
                    WHEN 8  PERFORM BEFEHL-LINKS
                    WHEN 9  PERFORM BEFEHL-TEIL
                    WHEN 10 PERFORM BEFEHL-INSPECT
                    WHEN 11 PERFORM BEFEHL-PERFORM
                    WHEN 12 PERFORM BEFEHL-IF
                    WHEN 20 PERFORM BEFEHL-KETTE
                    WHEN 21 PERFORM BEFEHL-KETTEPLUS
                    WHEN 22 PERFORM BEFEHL-REPLACE
                    WHEN 23 PERFORM BEFEHL-STERNSUCHE
                    WHEN 24 PERFORM BEFEHL-REVERSE
                    WHEN 25 PERFORM BEFEHL-HEXA
                    WHEN 26 PERFORM BEFEHL-UPPER
                    WHEN 27 PERFORM BEFEHL-LOWER
     ‚*€            WHEN 28 PERFORM BEFEHL-CONVERT
                    WHEN 29 PERFORM STERN-EINGABE
           END-EVALUATE.

     ‚* NORMALES SCHLEIFEN-ENDE
           GO TO ANF020.

     ‚* SPRUNGMARKE FÜR PROGRAM-ENDE IM FALL VON F3
       ENDE.
           CLOSE    WS-DISPLAY.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 1 BEFEHL ADD
     ‚*--------------------------------------------------------------
       BEFEHL-ADD SECTION.
       BEF-ADD-00.

     ‚* SCHRITTWEISES AUFADDIEREN DER EINGABEFELDER MIT
     ‚* ANZEIGE DER ZWISCHENERGEBNISSE.

     ‚* EING.1 ZUR AUSG.1 ADDIEREN.
           ADD      EINN01 OF FORMAT1 TO AUSN01 OF FORMAT1.
     ‚* EING.2 ZUR AUSG.1 ADDIEREN MIT ERGEBNISS IN AUSG.2
           ADD      EINN02 OF FORMAT1 TO AUSN01 OF FORMAT1
                    GIVING AUSN02 OF FORMAT1.
     ‚* EING.3 ZUR AUSG.2 ADDIEREN MIT ERGEBNISS IN AUSG.3
           ADD      EINN03 OF FORMAT1 TO AUSN02 OF FORMAT1
                    GIVING AUSN03 OF FORMAT1.

     ‚* KOMMENTARE SETZEN
           MOVE     "EING. 1" TO
                    KOMN01 OF FORMAT1.
           MOVE     "EING.1 + EING.2" TO
                    KOMN02 OF FORMAT1.
           MOVE     "EING.1 + EING.2 + EING.3" TO
                    KOMN03 OF FORMAT1.

       BEF-ADD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 2 BEFEHL SUBTRACT
     ‚*--------------------------------------------------------------
       BEFEHL-SUBTRACT SECTION.
       BEF-SUB-00.

     ‚* AUSG.1 = EING.1 - EING.2
     ‚* AUSG.2 = EING.1 - (EING.2 + EING.3)

     ‚* EING.2 VON EING.1 ABZIEHEN UND IN AUSG. 1 UND 2 SCHREIBEN
           SUBTRACT EINN02 OF FORMAT1 FROM EINN01 OF FORMAT1
                    GIVING AUSN01 OF FORMAT1 AUSN02 OF FORMAT1.

     ‚* EINGABE 3 VON AUSGABE 2 SUBTRAHIEREN
           SUBTRACT EINN03 OF FORMAT1 FROM
                    AUSN02 OF FORMAT1.

     ‚* KOMMENTARE SETZEN
           MOVE     "EING.1 - EING.2" TO KOMN01 OF FORMAT1.
           MOVE     "EING.1 - (EING.2 + EING.3)" TO
                    KOMN02 OF FORMAT1.

       BEF-SUB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 3 BEFEHL MULTIPLY
     ‚*--------------------------------------------------------------
       BEFEHL-MULTIPLY SECTION.
       BEF-MUL-00.

     ‚* AUSG.1 = EING.1 * EING.2
     ‚* AUSG.2 = EING.1 * EING.2 * EING.3

     ‚* AUS1 = EIN1 * EIN2
           MULTIPLY EINN01 OF FORMAT1 BY
                    EINN02 OF FORMAT1 GIVING
                    AUSN01 OF FORMAT1.

     ‚* AUS2 = AUS1 * EIN2
           MULTIPLY AUSN01 OF FORMAT1 BY
                    EINN03 OF FORMAT1 GIVING
                    AUSN02 OF FORMAT1.

     ‚* KOMMENTARE AUSGEBEN
           MOVE     "EING.1 * EING.2" TO KOMN01 OF FORMAT1.
           MOVE     "EING.1 * EING.2 * EING.3" TO KOMN02 OF FORMAT1.

       BEF-MUL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 4 BEFEHL DIVIDE
     ‚*--------------------------------------------------------------
       BEFEHL-DIVIDE SECTION.
       BEF-DIV-00.

     ‚* 2-MAL WIRD EING.1 DURCH EING.2 GETEILT. EINMAL MIT
     ‚* NACHKOMMASTELLEN UND EINMAL GANZZAHLIG MIT
     ‚* AUSGABE DES RESTES

     ‚* DEN TEILER AUF NULL ÜBERPRÜFEN.
           IF       EINN02 OF FORMAT1 NOT = ZEROES
     ‚*             EING.1 DURCH EING.2 MIT NACHKOMMASTELLEN
                    DIVIDE EINN01 OF FORMAT1 BY
                           EINN02 OF FORMAT1 GIVING
                           AUSN01 OF FORMAT1
     ‚*             EING.1 DURCH EING.2 GANZZAHLIG, DA HLP GANZZAHLIG
                    DIVIDE EINN01 OF FORMAT1 BY
                           EINN02 OF FORMAT1 GIVING
                           ZWISCHEN-HLP      REMAINDER
                           AUSN03 OF FORMAT1
     ‚*             DAS ERGEBNISS AUSGEBEN
                    MOVE   ZWISCHEN-HLP TO AUSN02 OF FORMAT1
           END-IF.

     ‚* KOMMMENTARE SETZEN
           MOVE     "EING.1 / EING.2" TO
                    KOMN01 OF FORMAT1.
           MOVE     "EING.1 / EING.2  GANZZAHLIG" TO KOMN02 OF FORMAT1.
           MOVE     "REST DER GANZZAHLIGEN DIVISION" TO
                    KOMN03 OF FORMAT1.

       BEF-DIV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 5 BEFEHL COMPUTE
     ‚*--------------------------------------------------------------
       BEFEHL-COMPUTE SECTION.
       BEF-COM-00.

     ‚* MIT 'COMPUTE' DIE GÜLTIGKEI DER REGELN FÜR KLAMMERSETZUNG UND
     ‚* PUNKT-VOR-STRICH-RECHNUNG AUCH IN COBOL BEWIESEN.

     ‚* VARIANTE1 DER KLAMMERSETZUNG : (1+2*3)
           COMPUTE  AUSN01 OF FORMAT1  =
                   (EINN01 OF FORMAT1  +
                    EINN02 OF FORMAT1  *
                    EINN03 OF FORMAT1).

     ‚* VARIANTE2 DER KLAMMERSETZUNG : (1+2)*3
           COMPUTE  AUSN02 OF FORMAT1  =
                   (EINN01 OF FORMAT1  +
                    EINN02 OF FORMAT1) *
                    EINN03 OF FORMAT1.

     ‚* VARIANTE3 DER KLAMMERSETZUNG : 1+(2*3)
           COMPUTE  AUSN03 OF FORMAT1  =
                    EINN01 OF FORMAT1  +
                   (EINN02 OF FORMAT1  *
                    EINN03 OF FORMAT1).

     ‚* KOMMENTARE SETZEN
           MOVE     "(EING.1  +  EING.2  *  EING.3)" TO
                    KOMN01 OF FORMAT1.
           MOVE     "(EING.1  +  EING.2) *  EING.3"  TO
                    KOMN02 OF FORMAT1.
           MOVE     " EING.1  + (EING.2  *  EING.3)" TO
                    KOMN03 OF FORMAT1.

       BEF-COM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 6 BEFEHL MOVE
     ‚*--------------------------------------------------------------
       BEFEHL-MOVE SECTION.
       BEF-MOV-00.

     ‚* EINFACHES VERSCHIEBEN DER EINGABEFELDER IN
     ‚* DIE AUSGABEFELDER

     ‚* NUMMERISCHE VERSCHIEBEN
           MOVE     EINN01 OF FORMAT1 TO AUSN01 OF FORMAT1.
           MOVE     EINN02 OF FORMAT1 TO AUSN02 OF FORMAT1.
           MOVE     EINN03 OF FORMAT1 TO AUSN03 OF FORMAT1.

     ‚* ALPHAS VERSCHIEBEN
           MOVE     EINA01 OF FORMAT1(1:15) TO
                    AUSA01 OF FORMAT1(20:15).
           MOVE     EINA02 OF FORMAT1 TO AUSA02 OF FORMAT1.

     ‚* KOMMENTARE SETZEN
           MOVE     "EINGABE 1(1:15) zu (20:15)" TO KOMN01 OF FORMAT1.
           MOVE     "EINGABE 2" TO KOMN02 OF FORMAT1.
           MOVE     "EINGABE 3" TO KOMN03 OF FORMAT1.

       BEF-MOV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 7 BEFEHL RECHTS
     ‚*--------------------------------------------------------------
       BEFEHL-RECHTS SECTION.
       BEF-REC-00.

     ‚* HIER WIRD VON DER UMGEDREHTE (REVERSE) ZEICHENKETTE
     ‚* DIE DANN FÜHRENDEN LEERZEICHEN GEZÄHLT. ES WERDEN ALSO
     ‚* DIE RECHTEN LEERZEICHEN GEZÄHLT. ANSCHLIEßEND WIRD DIE
     ‚* ZEICHENKETTE SO PASSEND IN DAS AUSGABE FELD GESCHOBEN,
     ‚* DAß DIE RECHTEN LEEREZEICHEN WEGFALLEN.

     ‚* ZÄHL-INDIZES NULLEN
           INITIALIZE LEER1-IDX.
           INITIALIZE LEER2-IDX.

     ‚* ROUTINE NICHT WENN EINGABE(ALPHA)1 LEER
           IF       EINA01 OF FORMAT1 NOT = SPACES

     ‚*             EINGABE1 RECHTE SPACES ZÄHLEN
                    INSPECT FUNCTION REVERSE(EINA01 OF FORMAT1)
                            TALLYING LEER1-IDX FOR
                            LEADING SPACES

     ‚*             LÄNGE DES EINGABE-FELDES OHNE RECHTE SPACES
                    COMPUTE ZWISCHEN-HLP = EINAL - LEER1-IDX

     ‚*             POSITION IM AUSGABEFELD
                    COMPUTE ZWISCHEN-HLP = AUSAL - ZWISCHEN-HLP + 1

     ‚*             NACH RECHTS VERSCHIEBEN
                    MOVE EINA01 OF FORMAT1 TO
                         AUSA01 OF FORMAT1(ZWISCHEN-HLP:)

     ‚*             INDEX AUSGEBEN
                    MOVE LEER1-IDX TO AUSN01 OF FORMAT1
           END-IF.

     ‚* ROUTINE NUR WENN EINGABE(ALPHA2) NICHT LEER
           IF       EINA02 OF FORMAT1 NOT = SPACES

     ‚*             EINGABE2 RECHTE SPACES ZÄHLEN
                    INSPECT FUNCTION REVERSE(EINA02 OF FORMAT1)
                            TALLYING LEER2-IDX FOR
                            LEADING SPACES

     ‚*             LÄNGE DES FELDES OHNE RECHTE SPACES
                    COMPUTE ZWISCHEN-HLP = EINAL - LEER2-IDX

     ‚*             POSITION IM AUSGABEFELD
                    COMPUTE ZWISCHEN-HLP = AUSAL - ZWISCHEN-HLP + 1

     ‚*             NACH RECHTS VERSCHIEBEN
                    MOVE EINA02 OF FORMAT1 TO
                         AUSA02 OF FORMAT1(ZWISCHEN-HLP:)

     ‚*             INDEX AUSGEBEN
                    MOVE LEER2-IDX TO AUSN02 OF FORMAT1
           END-IF.

     ‚* KOMMENTARE SETZEN
           MOVE     'RECHTE SPACES DER EING.1' TO
                    KOMN01 OF FORMAT1.
           MOVE     'RECHTE SPACES DER EING.2' TO
                    KOMN02 OF FORMAT1.

       BEF-REC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 8 BEFEHL LINKS
     ‚*--------------------------------------------------------------
       BEFEHL-LINKS SECTION.
       BEF-LIN-00.

     ‚* HIER WERDEN DIE FÜHRENDEN (LINKEN) LEERZEICHEN GEZÄHLT
     ‚* UND DANN WIEDER MIT HILFE DER SUBSTRING ANWEISUNG ( X(Y:Z) )
     ‚* DIE FELDER SO INS AUSGABEFELD GE'MOVE'T, DAß DIE LINKEN
     ‚* SPACES ABGESCHNITTEN WERDEN.

     ‚* ZÄHL-INDIZES NULLEN
           INITIALIZE LEER1-IDX.
           INITIALIZE LEER2-IDX.

     ‚* NUR WENN EINGABE(ALPHA1) NICHT IST
           IF       EINA01 OF FORMAT1 NOT = SPACES

     ‚*             EINGABE1 LINKE SPACES ZÄHLEN
                    INSPECT EINA01 OF FORMAT1
                            TALLYING LEER1-IDX FOR
                            LEADING SPACES

     ‚*             NACH LINKS VERSCHIEBEN
                    MOVE EINA01 OF FORMAT1(LEER1-IDX + 1:) TO
                         AUSA01 OF FORMAT1

     ‚*             INDEX AUSGEBEN
                    MOVE LEER1-IDX TO AUSN01 OF FORMAT1
           END-IF.

     ‚* ROUTINE NICHT WENN EINGABE(ALPHA)2 NUR SPACES SIND
           IF       EINA02 OF FORMAT1 NOT = SPACES

     ‚*             EINGABE2 LINKE SPACES ZÄHLEN
                    INSPECT  EINA02 OF FORMAT1
                             TALLYING LEER2-IDX FOR
                             LEADING SPACES

     ‚*             NACH LINKS VERSCHIEBEN
                    MOVE EINA02 OF FORMAT1(LEER2-IDX + 1:) TO
                         AUSA02 OF FORMAT1

     ‚*             INDEX AUSGEBEN
                    MOVE LEER2-IDX TO AUSN02 OF FORMAT1
           END-IF.

     ‚* KOMMENTARE SETZEN
           MOVE     'FÜHRENDE SPACES DER EING.1' TO
                    KOMN01 OF FORMAT1.
           MOVE     'FÜHRENDE SPACES DER EING.2' TO
                    KOMN02 OF FORMAT1.

       BEF-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 9 BEFEHL TEIL
     ‚*--------------------------------------------------------------
       BEFEHL-TEIL SECTION.
       BEF-TEI-00.

     ‚* VON DER ALPHANUMERISCHEN EINGABE 1 NEHMEN WIR DEN TEILSTRING
     ‚* DER AN POSITION (NUMERISCHE EINGABE 1) BEGINNT MIT DER
     ‚* LÄNGE DIE IN DER NUMERISCHEN EINGABE 2 STEHT UND SCHIEBEN
     ‚* DIESEN SUBSTRING IN DIE ALPHANUMERISCHE AUSGABE 1.

     ‚* SUBSTRING NUR MIT GÜLTIGEM EINGABEN 1 UND 2
           IF       EINN01 OF FORMAT1 > 0
            AND     EINN02 OF FORMAT1 > 0
            AND     EINN01 OF FORMAT1 + EINN02 OF FORMAT1
                    <= EINAL + 1
     ‚*             TEILSTRING VON EING.1 NACH AUSG.1
                    MOVE EINA01 OF FORMAT1(
                         EINN01 OF FORMAT1:
                         EINN02 OF FORMAT1) TO
                         AUSA01 OF FORMAT1

     ‚* KOMMENTAR SETZEN.
                    MOVE ' TEILSTRING VON <ALPHA-EING.1>'
                         TO KOMN01 OF FORMAT1
                    MOVE '        AB POS.: <NUM.-AUSG.1>'
                         TO KOMN02 OF FORMAT1
                    MOVE '      MIT LÄNGE: <NUM.-AUSG.2>'
                         TO KOMN03 OF FORMAT1
     ‚* FEHLERMELDUNG
           ELSE
                    MOVE '         BITTE DIE NUMERISCHEN'
                         TO KOMN01 OF FORMAT1
                    MOVE '             EINGABEFELDER MIT'
                         TO KOMN02 OF FORMAT1
                    MOVE '        GÜLTIGEN WERTEN FÜLLEN'
                         TO KOMN03 OF FORMAT1
           END-IF.

       BEF-TEI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 10 BEFEHL INSPECT
     ‚*--------------------------------------------------------------
       BEFEHL-INSPECT SECTION.
       BEF-INS-00.

     ‚* VOM EINGABEFELD 2 WERDEN DIE RECHTEN UND LINKEN
     ‚* LEERZEICHEN GEZÄHLT (UND ABGESCHNITTEN) UND DANN WIRD GESCHAUT
     ‚* WIE OFT DIESER TEILSTRING IN EING.1 AUFTAUCHT.

     ‚* ZÄHL-INDIZES NULLEN
           INITIALIZE LEER1-IDX.
           INITIALIZE LEER2-IDX.

     ‚* EINGABE 2 AUF LEERE PRÜFEN
           IF       EINA02 OF FORMAT1 NOT = SPACE
     ‚*             EINGABE2 LINKE SPACES ZÄHLEN
                    INSPECT  EINA02 OF FORMAT1
                             TALLYING LEER1-IDX FOR
                             LEADING SPACES

     ‚*             EINGABE2 RECHTE SPACES ZÄHLEN
                    INSPECT FUNCTION REVERSE(EINA02 OF FORMAT1)
                            TALLYING LEER2-IDX FOR
                            LEADING SPACES
           END-IF.

     ‚* ZÄHLERFELD (AUSN 1) NULLEN
           INITIALIZE AUSN01 OF FORMAT1.

     ‚* LÄNGE VON EING.2 OHNE RECHTE UND LINKE LEERZEICHEN
           COMPUTE  ZWISCHEN-HLP = EINAL - (LEER1-IDX + LEER2-IDX).

     ‚* WIE OFT EINA02 IN EINA01 ?
           INSPECT  EINA01 OF FORMAT1 TALLYING
                    AUSN01 OF FORMAT1 FOR ALL
                    EINA02 OF FORMAT1(LEER1-IDX + 1:ZWISCHEN-HLP).

     ‚* KOMMENTAR SETZEN
           MOVE     '- MAL IST A-EING.2 IN A-EING.1' TO
                    KOMN01 OF FORMAT1.

       BEF-INS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 11 BEFEHL PERFORM
     ‚*--------------------------------------------------------------
       BEFEHL-PERFORM SECTION.
       BEF-PER-00.

     ‚* EINE SCHLEIFE ÜBER DIE LÄNGE DER ALPHANUMERISCHEN
     ‚* EINGABEFELDER (MIT LEERZEICHEN) UND ABWECHSELNDES
     ‚* SCHIEBEN DER EINZELNEN ZEICHEN IN DIE AUSGABE1

     ‚* SCHLEIFE ÜBER GANZE EINGABEFELDER(ALPHA)
           PERFORM  VARYING LAUF-IDX FROM 1 BY 1
                    UNTIL LAUF-IDX > EINAL

     ‚*             HILFSFELD FÜLLEN (POSITION IM AUSGABEFELD)
                    COMPUTE ZWISCHEN-HLP = 2 * LAUF-IDX

     ‚*             EINGABEFELDER(1+2) IN AUSGABEFELD(1) BEWEGEN
                    MOVE EINA01 OF FORMAT1(LAUF-IDX:1) TO
                         AUSA01 OF FORMAT1(ZWISCHEN-HLP - 1:)
                    MOVE EINA02 OF FORMAT1(LAUF-IDX:1) TO
                         AUSA01 OF FORMAT1(ZWISCHEN-HLP:)

           END-PERFORM.

       BEF-PER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 12 BEFEHL IF
     ‚*--------------------------------------------------------------
       BEFEHL-IF SECTION.
       BEF-IF-00.

     ‚* ALPHANUMERISCHE FELDER AUF GLEICHHEIT ABFRAGEN
     ‚* UND ENTSPRECHENDE MITTEILUNG IN AUSGABE SCHREIBEN.

     ‚* EINGABE 1 UND 2 AUF GLEICHHEIT ABFRAGEN
           IF       EINA01 OF FORMAT1 = EINA02 OF FORMAT1
                    MOVE 'Eingabefeld 1 und 2 sind gleich' TO
                         AUSA01 OF FORMAT1
           ELSE
                    MOVE 'Eingabefeld 1 und 2 sind nicht gleich' TO
                         AUSA01 OF FORMAT1
           END-IF.

     ‚* EINGABE 2 UND 3 AUF GLEICHHEIT ABFRAGEN
           IF       EINA02 OF FORMAT1 = EINA03 OF FORMAT1
                    MOVE 'Eingabefeld 2 und 3 sind gleich' TO
                         AUSA02 OF FORMAT1
           ELSE
                    MOVE 'Eingabefeld 2 und 3 sind nicht gleich' TO
                         AUSA02 OF FORMAT1
           END-IF.

       BEF-IF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 20 BEFEHL VERKETTUNG
     ‚*--------------------------------------------------------------
       BEFEHL-KETTE SECTION.
       BEF-KET-00.

     ‚* VON DEN ALPHANUMERISCHEN EINGABEFELDER WERDEN DIE FÜHRENDEN
     ‚* LEERZEICHEN GEZÄHLT UND OHNE DIESE MIT DEM BEFEHL 'STRING'
     ‚* IN DAS AUSGABEFELD1 GESETZT. DANK DER HILFE DER STRING-
     ‚* ANWEISUNG 'DELIMITED BY' AUCH OHNE RECHTE LEERZEICHEN.

     ‚* ZÄHL-INDIZES NULLEN
           INITIALIZE LEER1-IDX.
           INITIALIZE LEER2-IDX.
           INITIALIZE LEER3-IDX.

     ‚* EINGABE1 LINKE SPACES ZÄHLEN
           INSPECT  EINA01 OF FORMAT1
                    TALLYING LEER1-IDX FOR
                    LEADING SPACES.

     ‚* EINGABE2 LINKE SPACES ZÄHLEN
           INSPECT  EINA02 OF FORMAT1
                    TALLYING LEER2-IDX FOR
                    LEADING SPACES.

     ‚* EINGABE3 LINKE SPACES ZÄHLEN
           INSPECT  EINA03 OF FORMAT1
                    TALLYING LEER3-IDX FOR
                    LEADING SPACES.

     ‚* ABGESCHNITTENES ABGESCHNITTEN ZUSAMMENFÜGEN
           STRING   EINA01 OF FORMAT1(LEER1-IDX + 1:)
                    DELIMITED BY '  '
                    EINA02 OF FORMAT1(LEER2-IDX + 1:)
                    DELIMITED BY '  '
                    EINA03 OF FORMAT1(LEER3-IDX + 1:)
                    DELIMITED BY '  '
                    INTO AUSA01 OF FORMAT1.

       BEF-KET-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 21 BEFEHL VERKETTUNG MIT PLUS
     ‚*--------------------------------------------------------------
       BEFEHL-KETTEPLUS SECTION.
       BEF-KPL-00.

     ‚* EXAKT DASSELBE WIE OBEN ('AUSWAHL 20') NUR DAS DIESMAL
     ‚* ZWISCHEN DEN SUBSTRINGS NOCH JEWEILS DAS LITERAL '+'
     ‚* EINGEFÜGT WURD.

     ‚* ZÄHL-INDIZES NULLEN
           INITIALIZE LEER1-IDX.
           INITIALIZE LEER2-IDX.
           INITIALIZE LEER3-IDX.

     ‚* EINGABE1 LINKE SPACES ZÄHLEN
           INSPECT  EINA01 OF FORMAT1
                    TALLYING LEER1-IDX FOR
                    LEADING SPACES.

     ‚* EINGABE2 LINKE SPACES ZÄHLEN
           INSPECT  EINA02 OF FORMAT1
                    TALLYING LEER2-IDX FOR
                    LEADING SPACES.

     ‚* EINGABE3 LINKE SPACES ZÄHLEN
           INSPECT  EINA03 OF FORMAT1
                    TALLYING LEER3-IDX FOR
                    LEADING SPACES.

     ‚* EINZELTEILE MIT '+' ZUSAMMENFÜGEN
           STRING   EINA01 OF FORMAT1(LEER1-IDX + 1:)
                    DELIMITED BY '  '
                    '+'
                    EINA02 OF FORMAT1(LEER2-IDX + 1:)
                    DELIMITED BY '  '
                    '+'
                    EINA03 OF FORMAT1(LEER3-IDX + 1:)
                    DELIMITED BY '  '
                    INTO AUSA01 OF FORMAT1.

       BEF-KPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 22 BEFEHL REPLACE
     ‚*--------------------------------------------------------------
       BEFEHL-REPLACE SECTION.
       BEF-REP-00.

     ‚* DIE EINGABEFELDER WERDEN IN DIE AUSGABE BEWEGT UND DANN
     ‚* WERDEN MIT HILFE DER 'INSPECT-REPLACING' ANWEISUNG
     ‚* ALLE LEERZEICHEN DURCH FRAGEZEICHEN ERSETZT.

     ‚* KOPIEREN DER EINGABE IN DIE AUSGABE
           MOVE     EINA01 OF FORMAT1 TO AUSA01 OF FORMAT1.
           MOVE     EINA02 OF FORMAT1 TO AUSA02 OF FORMAT1.

     ‚* IN EINGABE 1,2 UND 3 LEERZEICHEN DURCH '?' ERSETZEN
           INSPECT  AUSA01 OF FORMAT1 REPLACING ALL ' ' BY '?'.
           INSPECT  AUSA02 OF FORMAT1 REPLACING ALL ' ' BY '?'.

       BEF-REP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 23 BEFEHL STERNSUCHE
     ‚*--------------------------------------------------------------
       BEFEHL-STERNSUCHE SECTION.
       BEF-STS-00.

     ‚* MIT HILFE DES 'INSPECT' BEFEHLS WERDEN DIE ZEICHEN
     ‚* VOR DEM ERSTEN '*' IN EING.1 GEZÄHLT UND
     ‚* MIT HILFE DER 'UNSTRING' ANWEISUNG WIRD DER TEILSTRING
     ‚* VOR DEM ERSTEN '*' VON EING.2 IN ALPHA-AUSG.1 GESETZT.

     ‚* STELLEN VOR STERN IN EINGABE 1 ZÄHLEN
           INSPECT  EINA01 OF FORMAT1 TALLYING
                    AUSN01 OF FORMAT1 FOR
                    CHARACTERS
                    BEFORE INITIAL '*'.

     ‚* KONTROLLE OB ÜBERHAUPT STERN GEFUNDEN
           IF       AUSN01 OF FORMAT1 = 20
                    MOVE 0 TO AUSN01 OF FORMAT1
                    MOVE "KEIN STERN GEFUNDEN" TO KOMN01 OF FORMAT1
           ELSE
                    MOVE "ZEICHEN VOR DEM STERN" TO KOMN01 OF FORMAT1
           END-IF.

     ‚* KONTROLLE AUF STERN IN EING.2
           INITIALIZE ZWISCHEN-HLP.
           INSPECT  EINA02 OF FORMAT1 TALLYING
                    ZWISCHEN-HLP FOR
                    CHARACTERS BEFORE
                    INITIAL "*".

           IF       ZWISCHEN-HLP = 20
                    MOVE 'KEIN STERN GEFUNDEN' TO AUSA01 OF FORMAT1
           ELSE
     ‚*             ALLES VOR DEM STERN IN EINGABE2 IN AUSGABE1
                    UNSTRING EINA02 OF FORMAT1
                             DELIMITED BY '*'
                             INTO AUSA01 OF FORMAT1
           END-IF.

       BEF-STS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 24 BEFEHL REVERSE
     ‚*--------------------------------------------------------------
       BEFEHL-REVERSE SECTION.
       BEF-REV-00.

     ‚* 'FUNCTION REVERSE(XY)' DREHT DEN STRING XY UM.

     ‚* EINGABE 1 UMGEDREHT IN AUSGABE 1 AUSGEBEN
           MOVE     FUNCTION REVERSE(EINA01 OF FORMAT1) TO
                    AUSA01 OF FORMAT1.

     ‚* EINGABE 2 UMGEDREHT IN AUSGABE 2 AUSGEBEN
           MOVE     FUNCTION REVERSE(EINA02 OF FORMAT1) TO
                    AUSA02 OF FORMAT1.

       BEF-REV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* 25 BEFEHL HEXADEZIMAL
     ‚*--------------------------------------------------------------
       BEFEHL-HEXA SECTION.
       BEF-HEX-00.

     ‚* MIT X VOR EINEM LITERAL, DAS EIN HEXADEZIMAL-CODE ENTHÄLT,
     ‚* WIRD DIESER UMGESETZT IN DEN EBCDIC-CODE.

     ‚* DIE HEXA-WERTE ('C1' & 'C2') IN EBCDIC AUSGEBEN
           MOVE     X'C1' TO AUSA01 OF FORMAT1.
           MOVE     X'C2' TO AUSA02 OF FORMAT1.

       BEF-HEX-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 26 BEFEHL UPPER
     ‚*--------------------------------------------------------------
       BEFEHL-UPPER SECTION.
       BEF-UPP-00.

     ‚* DIE 'FUNCTION UPPER-CASE' SETZT ALLE KLEINBUCHSTABEN
     ‚* EINES ZEICHENSTRINGS IN GROßBUCHSTABEN UM.

     ‚* EINGABE 1 UND 2 GROß NACH AUSGABE 1 UND 2 UMSETZEN
           MOVE     FUNCTION UPPER-CASE(EINA01 OF FORMAT1)
                    TO AUSA01 OF FORMAT1.
           MOVE     FUNCTION UPPER-CASE(EINA02 OF FORMAT1)
                    TO AUSA02 OF FORMAT1.

       BEF-UPP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 27 BEFEHL LOWER
     ‚*--------------------------------------------------------------
       BEFEHL-LOWER SECTION.
       BEF-LOW-00.

     ‚* DIE 'FUNCTION LOWER-CASE' SETZT ALLE GROßBUCHSTABEN
     ‚* EINES ZEICHENSTRINGS IN KLEINBUCHSTABEN UM.

     ‚* EINGABE 1 UND 2 KLEIN NACH AUSGABE 1 UND 2 UMSETZEN
           MOVE     FUNCTION LOWER-CASE(EINA01 OF FORMAT1)
                    TO AUSA01 OF FORMAT1.
           MOVE     FUNCTION LOWER-CASE(EINA02 OF FORMAT1)
                    TO AUSA02 OF FORMAT1.

       BEF-LOW-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 28 BEFEHL CONVERT
     ‚*--------------------------------------------------------------
     ‚*€BEFEHL-CONVERT SECTION.
     ‚*€BEF-CON-00.
     ‚*€
     ‚* DIE ZEICHENKETTE ALPHA-EING.1 WIRD ZUR AUSGABE KOPIERT
     ‚* UND DORT WERDEN ALLE ZIFFERN DER ZEICHENKETTE DURCH
     ‚* DIE NÄCHSTHÖHERLIEGENDE ZIFFER ERSETZT.
     ‚*€
     ‚* KOPIEREN DES EINGABEFELDES IN DIE AUSGABE
     ‚*€   MOVE EINA01 OF FORMAT1 TO AUSA01 OF FORMAT1.
     ‚*€
     ‚* IM AUSGABEFELD 1 ALLE ZIFFERN UM 1 ERHÖHEN
     ‚*€   INSPECT  AUSA01 OF FORMAT1
     ‚*€            CONVERTING '1234567890' TO '2345678901'.
     ‚*€
     ‚*€BEF-CON-90.
      *    EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSWAHL 29 STERN EINGABE VORWEG
     ‚*--------------------------------------------------------------
       STERN-EINGABE SECTION.
       BEF-STE-00.

     ‚* DER EINGABEWERT MIT EINEM STERN VORWEG WIRD GEPRÜFT OB DIESER
     ‚* IN EINEM ANDEREN FELD VORKOMMT (SATZAUSWAHL)

     ‚* KOPIEREN DES EINGABEFELDES IN DIE AUSGABE
           MOVE EINA01 OF FORMAT1 TO AUSA01 OF FORMAT1.

           IF       EINA01 OF FORMAT1(1:1) = "*"
                    MOVE ZERO TO LEN
                    MOVE ZERO TO ERG
                    INSPECT EINA01 OF FORMAT1(2:) TALLYING LEN FOR
                            CHARACTERS BEFORE INITIAL SPACES
                    INSPECT EINA02 OF FORMAT1 TALLYING ERG FOR ALL
                            EINA01 OF FORMAT1(2:LEN)
                    IF   ERG NOT = ZEROES
                         MOVE EINA02 OF FORMAT1 TO AUSA02 OF FORMAT1
                         MOVE LEN               TO AUSN01 OF FORMAT1
                         MOVE ERG               TO AUSN02 OF FORMAT1
                    ELSE
                         MOVE "NICHT GEFUNDEN"  TO AUSA02 OF FORMAT1
                    END-IF
           END-IF.

       BEF-STE-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* BILDSCHIRMAUSGABEFELDER CLEAREN
     ‚*--------------------------------------------------------------
       AUSGABE-INIT SECTION.
       AUS-INI-00.

     ‚* ALLE AUSGABEFELDER LEEREN OHNE DIE EINGABE ZU LEEREN
           INITIALIZE AUSA01 OF FORMAT1.
           INITIALIZE AUSA02 OF FORMAT1.
           INITIALIZE AUSN01 OF FORMAT1.
           INITIALIZE AUSN02 OF FORMAT1.
           INITIALIZE AUSN03 OF FORMAT1.
           INITIALIZE KOMN01 OF FORMAT1.
           INITIALIZE KOMN02 OF FORMAT1.
           INITIALIZE KOMN03 OF FORMAT1.

       AUS-INI-90.
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
