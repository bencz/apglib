       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0006.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DIREKTES E-MAIL VERSENDEN ÜBER SMTP                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  U. BENNER / L. SCZECH                        *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  22.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  EMAIL ÜBER SMTP SENDEN (P3 18695)            *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
##APG‚* 14.10.2009 APG   ÄNDERN AUF APG-COPYMODULE UND ROUTINEN       *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*-------------------------------------------------------------
     ‚* PCFGMAI: MILFDN
     ‚*-------------------------------------------------------------
           SELECT PCFGMAI-DP
                  ASSIGN       TO  DATABASE-PCFGMAI
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCFGMAI: MILFDN
       FD  PCFGMAI-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGMAI-P.
           COPY DDS-ALL-FORMATS OF PCFGMAI.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0006".
       01  PGM-TYP                       PIC X(8)  VALUE "EMAIL   ".

     ‚* ANGABEN FÜR LÄNGEN
       01  LEN                 PIC 9(9).
       01  LEN-IN              PIC 9(9) BINARY.
       01  LEN-OUT             PIC 9(9) BINARY.
       01  FILE-LEN            PIC 9(9) BINARY.
       01  SEND-LEN            PIC 9(9) BINARY.

     ‚* LÄNGEN DER ÜBERGABEPARAMETER
       01  SENDER-LEN          PIC 9(2).
       01  TOPIC-LEN           PIC 9(2).
       01  DATA-LEN            PIC 9(4).

     ‚* VARIABLEN FÜR LGTCPIO
       01  WRK-RECORD          PIC X(16710680).
       01  WRK-RECORD-FILE     PIC X(16710680).
       01  TCP-SOCK            PIC S9(9) BINARY VALUE -1.
       01  TCP-ACTION          PIC X(1) VALUE " ".
       01  TCP-BUFLEN          PIC 9(9) BINARY VALUE 0.
       01  TCP-DELIM           PIC X(24) VALUE X'0D0A'.
       01  TCP-DELIMLEN        PIC 9(9) BINARY VALUE 2.
       01  TCP-TIMEOUT         PIC 9(9) BINARY VALUE ZEROES.
       01  TCP-RETTXT          PIC X(10).
     ‚* PUFFER FÜR "HOST:PORT"
       01  HOSTBUFFER          PIC X(30).
     ‚* GESAMTLÄNGE ALLER GESENDETER DATEN
       01  RECORD-SIZE         PIC 9(9).

     ‚* DUMMY-FELDER ZUM DEFINIEREN MIT LIKE
       01  ADRLEN-DUMMY        PIC X(64).
       01  LEN-DUMMY           PIC 9(3).

     ‚* VARIABLEN FÜR STRINGKONVERTIERUNG
       01  INT-LEN-IN          PIC S9(9) BINARY.
       01  INT-LEN-OUT         PIC S9(9) BINARY.
       01  BASE-LEN-IN         PIC S9(9) BINARY.
       01  BASE-LEN-OUT        PIC S9(9) BINARY.
       01  CCSID-IN            PIC 9(5).
       01  CODE-PAGE           PIC 9(5).
       01  NO-CONVERT          PIC 9(1).
       01  BASE64-AKTION       PIC X(1) VALUE "0".
       01  KEINE-ANTWORT       PIC X(1).
       01  EBC-ASCII           PIC X(1).
       01  RET-TXT             PIC X(1).
       01  FILE-WRK            PIC X(256).
       01  SENDER-OK           PIC X(1).
       01  LETZTER-DATENSATZ   PIC X(1).
       01  TCP-ERROR           PIC X(1).
       01  CLIENTNAME          PIC X(512).

     ‚* POINTER FÜR DIE INTERNEN TABELLEN
       01  POS-TAB             PIC 9(2).

     ‚* INTERNE TABELLE FÜR DIE EMPFÄNGER-ADRESSE
       01  EMPF-TAB.
        05 EMPFAENG-REC          OCCURS 10 TIMES.
          10 ADRESSE        LIKE ADRLEN-DUMMY.
          10 ADRLEN         LIKE LEN-DUMMY.
     ‚* INTERNE TABELLE FÜR DIE KOPIE-ADRESSE
       01  KOPIE-TAB.
        05 KOPIE-REC             OCCURS 10 TIMES.
          10 ADRESSE        LIKE ADRLEN-DUMMY.
          10 ADRLEN         LIKE LEN-DUMMY.
     ‚* INTERNE TABELLE FÜR DIE BLINDKOPIEKOPIE-ADRESSE
       01  BKOPIE-TAB.
        05 BKOPIE-REC            OCCURS 10 TIMES.
          10 ADRESSE        LIKE ADRLEN-DUMMY.
          10 ADRLEN         LIKE LEN-DUMMY.
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-HOST                PIC X(15).
       01  X-PORT                PIC X(5).
       01  X-SENDER              PIC X(30).
     ‚*šINFO ZU DEN ÜBERGABEPARAMTERN:                     ‚*š
     ‚*šIN X-ANZAHL STEHT DIE ANZAHL DER DATENSÄTZE FÜR DIE‚*š
     ‚*šJEWEILIGE TABELLE.                                 ‚*š
     ‚*š01 RECORD                                          ‚*š
     ‚*š  05 X-ANZAL                                       ‚*š
     ‚*š  05 X-TABELLE                                     ‚*š
     ‚*š     10 X-ADRESSE                                  ‚*š
     ‚* ÜBERNAHME DER EMPFÄNGER                             *
       01  X-EMPF.
        05 X-ANZAHL              PIC 9(2) BINARY.
        05 X-EMPF-REC            OCCURS 10 TIMES.
          10 X-ADRESSE      LIKE ADRLEN-DUMMY.
     ‚* ÜBERNAHME DER KOPIEADRESSEN
       01  X-KOPIE.
        05 X-ANZAHL              PIC 9(2) BINARY.
        05 X-KOPIE-REC           OCCURS 10 TIMES.
          10 X-ADRESSE      LIKE ADRLEN-DUMMY.
     ‚* ÜBERNAHME DER BLINDKOPIEADRESSEN
       01  X-BKOPIE.
        05 X-ANZAHL              PIC 9(2) BINARY.
        05 X-BKOPIE-REC          OCCURS 10 TIMES.
          10 X-ADRESSE      LIKE ADRLEN-DUMMY.
       01  X-TOPIC               PIC X(96).
       01  X-DATA                PIC X(5120).
       01  X-FILE           LIKE FILE-WRK.
     ‚* IM RETTXT STEHT DIE RÜCKMELDUNG DES SERVERS, FALLS
     ‚* DIESE NEGATIV WAR
       01  X-RETTXT              PIC X(256).
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING  X-HOST
                                 X-PORT
                                 X-SENDER
                                 X-EMPF
                                 X-KOPIE
                                 X-BKOPIE
                                 X-TOPIC
                                 X-DATA
                                 X-FILE
                                 X-RETTXT.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0006" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     I-O   PCFGMAI-DP.

     ‚* CLEAREN ALLER VARIABLEN / TABELLEN
           INITIALIZE LEN.
           INITIALIZE LEN-IN.
           INITIALIZE LEN-OUT.
           INITIALIZE SEND-LEN.
           INITIALIZE SENDER-LEN.
           INITIALIZE TOPIC-LEN.
           INITIALIZE DATA-LEN.
           INITIALIZE TCP-RETTXT.
           INITIALIZE HOSTBUFFER.
           INITIALIZE RECORD-SIZE.
           INITIALIZE INT-LEN-IN.
           INITIALIZE INT-LEN-OUT.
           INITIALIZE BASE-LEN-IN.
           INITIALIZE BASE-LEN-OUT.
           INITIALIZE CCSID-IN.
           INITIALIZE CODE-PAGE.
           INITIALIZE NO-CONVERT.
           INITIALIZE KEINE-ANTWORT.
           INITIALIZE EBC-ASCII.
           INITIALIZE RET-TXT.
           INITIALIZE FILE-WRK.
           INITIALIZE POS-TAB.
           INITIALIZE SENDER-OK.
           INITIALIZE LETZTER-DATENSATZ.
           INITIALIZE FILE-LEN.
           INITIALIZE TCP-ERROR.
           INITIALIZE FILE-LEN.
           PERFORM  LOESCHEN-TABELLEN.

     ‚* DATEN AUS DER ÜBERGABETABELLE IN EINE WRK-TABELLE
     ‚* ÜBERNEHMEN
           PERFORM  UEBERNEHME-ADRESSEN.

     ‚* PRÜFEN, OB ALLE BENÖTIGTEN VARIABLEN ÜBERGEBEN WURDEN
           IF       X-HOST                 = SPACES
            OR      X-PORT                 = SPACES
            OR      X-SENDER               = SPACES
            OR      ADRESSE OF EMPF-TAB(1) = SPACES
            OR      X-DATA                 = SPACES
            OR      X-TOPIC                = SPACES
                    MOVE "INVALID PARAMETERS" TO X-RETTXT
                    GO TO ENDE
           END-IF.

     ‚* LÄNGE DER ÜBERGEBENEN VARIABLEN ERMITTELN
           PERFORM  ERM-LAENGE.

     ‚* ZUSAMMENSETZEN DER HOSTADRESSE + HOST (HOST:PORT)
           MOVE     1 TO LEN.
           STRING   X-HOST               DELIMITED BY SPACE
                    ":"                  DELIMITED BY SIZE
                    X-PORT               DELIMITED BY SPACE
                    INTO HOSTBUFFER WITH POINTER LEN
           END-STRING.

     ‚* ZUM MAILSERVER VERBINDEN
           MOVE     5   TO TCP-TIMEOUT.
           MOVE     "0" TO TCP-ACTION.
           MOVE     LEN TO TCP-BUFLEN.
           INITIALIZE TCP-RETTXT.
           CALL     PROCEDURE "TCPIO"  USING TCP-SOCK
                                               TCP-ACTION
                                               HOSTBUFFER
                                               TCP-BUFLEN
                                               TCP-DELIM
                                               TCP-DELIMLEN
                                               TCP-TIMEOUT
                                               TCP-RETTXT
           END-CALL.
           IF       TCP-RETTXT > SPACE
                    MOVE TCP-RETTXT TO X-RETTXT
                    GO TO ENDE
           END-IF.

     ‚* MAILSERVER IDENTIFIZIEREN
     ‚* DAZU CLIENTNAMEN ERMITTELN
           CALL PROCEDURE "GETHOST" USING CLIENTNAME.
           MOVE     1 TO SEND-LEN.
           STRING   "EHLO "    DELIMITED BY SIZE
                    "'"        DELIMITED BY SIZE
                    CLIENTNAME DELIMITED BY " "
                    "'"        DELIMITED BY SIZE
                    INTO WRK-RECORD  WITH POINTER SEND-LEN
           END-STRING.
           PERFORM  SENDEN-STRING.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* EMAIL SENDER
           MOVE     1 TO SEND-LEN.
           STRING   "MAIL FROM: "           DELIMITED BY SIZE
                    "<"                     DELIMITED BY SIZE
                    X-SENDER(1:SENDER-LEN)  DELIMITED BY SIZE
                    ">"                     DELIMITED BY SIZE
                    INTO WRK-RECORD WITH POINTER SEND-LEN
           END-STRING.
           PERFORM  SENDEN-STRING.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* IN DIESER SECTION WERDEN ALLE ANGEGEBENEN EMPFÄNGER AN DEN
     ‚* MAILSERVER ÜBERGEBEN
           PERFORM  ANGABE-EMPFAENGER.
           IF       SENDER-OK = SPACES
                    GO TO ANF090
           END-IF.

     ‚* EMAIL BODY EINLEITEN
           MOVE     "DATA" TO WRK-RECORD(1:4).
           MOVE     4 TO SEND-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* AB HIER ERHALTEN WIR KEINE ANTWORT MEHR VOM SERVER,
     ‚* ERST NACH ABSCHLUSS DES BODIES GIBT ES WIEDER RÜCKMELDUNG.
           MOVE     1 TO KEINE-ANTWORT.

     ‚* SENDER IM DATA-BEREICH ANGEBEN
           MOVE     1 TO SEND-LEN.
           STRING   "FROM: "                DELIMITED BY SIZE
                    X-SENDER(1:SENDER-LEN)  DELIMITED BY SIZE
                    INTO WRK-RECORD WITH POINTER SEND-LEN
           END-STRING.
           PERFORM  SENDEN-STRING.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* DIE EMPFÄNGER / KOPIEN MÜSSEN NOCHMALS
     ‚* IM DATA-BEREICH ANGEGEBEN WERDEN, SODASS DIESE BEIM EMPFÄNGER
     ‚* IN DER EMAIL STEHEN
           PERFORM  AUSGABE-EMPFAENGER.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* EMAIL BETREFF
           MOVE     "SUBJECT: "          TO WRK-RECORD(1:9).
           MOVE     X-TOPIC(1:TOPIC-LEN) TO WRK-RECORD(10:).
           COMPUTE  SEND-LEN = 9 + TOPIC-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* MIME-VERSION ANGEBEN
           MOVE     "MIME-VERSION: 1.0"  TO WRK-RECORD(1:17).
           MOVE     17 TO SEND-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* MIME PARAMETER ANGEBEN
           MOVE     1 TO SEND-LEN.
           STRING   'CONTENT-TYPE: MULTIPART/MIXED; ' DELIMITED BY SIZE
                    'BOUNDARY="XBODY"'                DELIMITED BY SIZE
                     INTO WRK-RECORD WITH POINTER SEND-LEN
           END-STRING.
           PERFORM  SENDEN-STRING.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* NACH DEM HEADER EINE LEERZEILE AUSGEBEN
           PERFORM  LEERZEILE.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* BODY ERÖFFNEN
           MOVE     "--XBODY" TO WRK-RECORD(1:7).
           MOVE     7 TO SEND-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* CHARSET ANGEBEN
     ‚* CHARSET=ISO-8859-1
           MOVE     "CONTENT-TYPE: TEXT/PLAIN; CHARSET=ISO-8859-1" TO
                    WRK-RECORD(1:44).
           MOVE     44 TO SEND-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* VOR DEM INHALT EINE LEERZEILE AUSGEBEN
           PERFORM  LEERZEILE.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* EMAIL INHALT
           MOVE     X-DATA(1:DATA-LEN) TO WRK-RECORD(1:DATA-LEN).
           MOVE     DATA-LEN TO SEND-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* NACH DEM INHALT EINE LEERZEILE AUSGEBEN
           PERFORM  LEERZEILE.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* WURDE EINE DATEI ANGEGEBEN?
           IF       X-FILE > SPACES
                    MOVE     "--XBODY" TO WRK-RECORD(1:7)
                    MOVE     7 TO SEND-LEN
                    PERFORM  SENDEN
                    IF       TCP-ERROR > SPACES
                             GO TO ANF095
                    END-IF
                    PERFORM SENDE-DATEI
           END-IF.

     ‚* NACH DEM DATENSATZ EINE ELERZEILE AUSGEBEN
           PERFORM  LEERZEILE.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* BODY SCHLIEßEN
           MOVE     "--XBODY--" TO WRK-RECORD(1:10).
           MOVE     9 TO SEND-LEN.
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

     ‚* AB HIER WIRD WIEDER AUF DEN HOST GEHÖRT
           INITIALIZE KEINE-ANTWORT.

     ‚* EMAIL ABSCHLIESSEN
           MOVE     "." TO WRK-RECORD(1:1).
           MOVE     1 TO SEND-LEN
           PERFORM  SENDEN.
           IF       TCP-ERROR > SPACES
                    GO TO ANF095
           END-IF.

       ANF090.
     ‚* IST EIN RETTXT VORHANDEN DATEN RESETTEN!
           IF       X-RETTXT > SPACES
                    MOVE "RESET" TO WRK-RECORD(1:5)
                    MOVE 5 TO SEND-LEN
                    PERFORM SENDEN
           END-IF.

     ‚* SMTP-SERVER WIEDER VERLASSEN
           MOVE     "QUIT" TO WRK-RECORD(1:4).
           MOVE     4 TO SEND-LEN
           MOVE     1 TO LETZTER-DATENSATZ.
           PERFORM  SENDEN.

     ‚* VERBINDUNG WIRD GESCHLOSSEN
       ANF095.
           PERFORM  VERBINDUNG-SCHLIESSEN.

     ‚* PROTOKOLLIEREN DER MAIL
           PERFORM  SAVE-MAIL.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           CLOSE                 PCFGMAI-DP.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÄNGE DER ÜBERGEBENEN VARIABLEN ERMITTELN
     ‚*--------------------------------------------------------------
       ERM-LAENGE SECTION.
       ERM-LEN-00.

     ‚* SENDER
           COMPUTE  SENDER-LEN = FUNCTION LENGTH(X-SENDER).
           PERFORM  WITH TEST BEFORE VARYING SENDER-LEN
                    FROM SENDER-LEN BY -1
                    UNTIL SENDER-LEN = ZEROES OR
                    X-SENDER(SENDER-LEN:1) NOT = SPACES
                    CONTINUE
           END-PERFORM.

     ‚* LÄNGE FÜR ALLE EMPFÄNGER ERMITTELN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-EMPF
                    COMPUTE ADRLEN OF EMPF-TAB(POS-TAB) =
                            LENGTH OF ADRLEN-DUMMY
                    PERFORM WITH TEST BEFORE
                            VARYING ADRLEN  OF EMPF-TAB(POS-TAB)
                            FROM    ADRLEN  OF EMPF-TAB(POS-TAB) BY -1
                            UNTIL   ADRLEN  OF EMPF-TAB(POS-TAB)
                                    = ZEROES
                             OR
                                    ADRESSE OF EMPF-TAB(POS-TAB)
                                   (ADRLEN  OF EMPF-TAB(POS-TAB):1)
                                    NOT = SPACES
                            CONTINUE
                    END-PERFORM
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* LÄNGE FÜR KOPIEN ERMITTELN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-KOPIE
                    COMPUTE ADRLEN OF KOPIE-TAB(POS-TAB) =
                            LENGTH OF ADRLEN-DUMMY
                    PERFORM WITH TEST BEFORE
                            VARYING ADRLEN  OF KOPIE-TAB(POS-TAB)
                            FROM    ADRLEN  OF KOPIE-TAB(POS-TAB) BY -1
                            UNTIL   ADRLEN  OF KOPIE-TAB(POS-TAB)
                                    = ZEROES
                             OR
                                    ADRESSE OF KOPIE-TAB(POS-TAB)
                                   (ADRLEN  OF KOPIE-TAB(POS-TAB):1)
                                    NOT = SPACES
                            CONTINUE
                    END-PERFORM
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* LÄNGE FÜR BLINDKOPIEN ERMITTELN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-BKOPIE
                    COMPUTE ADRLEN OF BKOPIE-TAB(POS-TAB) =
                            LENGTH OF ADRLEN-DUMMY
                    PERFORM WITH TEST BEFORE
                            VARYING ADRLEN OF BKOPIE-TAB(POS-TAB)
                            FROM    ADRLEN OF BKOPIE-TAB(POS-TAB) BY -1
                            UNTIL   ADRLEN OF BKOPIE-TAB(POS-TAB)
                                    = ZEROES
                             OR
                                    ADRESSE OF BKOPIE-TAB(POS-TAB)
                                   (ADRLEN  OF BKOPIE-TAB(POS-TAB):1)
                                    NOT = SPACES
                            CONTINUE
                    END-PERFORM
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* BETREFF
           COMPUTE  TOPIC-LEN = FUNCTION LENGTH(X-TOPIC).
           PERFORM  WITH TEST BEFORE VARYING TOPIC-LEN
                    FROM TOPIC-LEN BY -1
                    UNTIL TOPIC-LEN = ZEROES OR
                    X-TOPIC(TOPIC-LEN:1) NOT = SPACES
                    CONTINUE
           END-PERFORM.

     ‚* TEXT
           COMPUTE  DATA-LEN = FUNCTION LENGTH(X-DATA).
           PERFORM  WITH TEST BEFORE VARYING DATA-LEN
                    FROM DATA-LEN BY -1
                    UNTIL DATA-LEN = ZEROES OR
                    X-DATA(DATA-LEN:1) NOT = SPACES
                    CONTINUE
           END-PERFORM.

       ERM-LEN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN WRK-RECORD (VORHER LÄNGE - 1, BEI LÄNGEN DIE MIT
     ‚* "STRING" ERMITTELT WURDEN)
     ‚*--------------------------------------------------------------
       SENDEN-STRING SECTION.
       SND-STR-00.

           SUBTRACT 1 FROM SEND-LEN.
           PERFORM  SENDEN.

       SND-STR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN WRK-RECORD
     ‚*--------------------------------------------------------------
       SENDEN SECTION.
       SEN-DAT-00.

           INITIALIZE TCP-ERROR.

     ‚* STRING IN ASCII KONVERTIEREN
           IF       NO-CONVERT = ZEROES
                    MOVE     "1" TO EBC-ASCII
                    PERFORM  KONVERT-STRING
           END-IF.

     ‚* INT-LEN-OUT+1, DAMIT DAS ENDE NICHT ÜBERSCHRIEBEN WIRD
           MOVE     TCP-DELIM TO WRK-RECORD(SEND-LEN + 1:2).
           ADD      2 TO SEND-LEN.

     ‚* SENDEN DES DATENSATZES
           MOVE     "2"      TO TCP-ACTION.
           MOVE     5        TO TCP-TIMEOUT.
           MOVE     SEND-LEN TO TCP-BUFLEN.
           INITIALIZE TCP-RETTXT.
           CALL     PROCEDURE "TCPIO" USING TCP-SOCK
                                              TCP-ACTION
                                              WRK-RECORD(1:SEND-LEN)
                                              TCP-BUFLEN
                                              TCP-DELIM
                                              TCP-DELIMLEN
                                              TCP-TIMEOUT
                                              TCP-RETTXT
           END-CALL.

     ‚* VERBINDUNG ERFOLGREICH?
           IF       TCP-RETTXT > SPACES
                    MOVE "1" TO TCP-ERROR
                    MOVE TCP-RETTXT TO X-RETTXT
                    GO TO SEN-DAT-90
           END-IF.

     ‚* NICHT BEI JEDEM SENDEVORGANG WIRD EINE ANTWORT ERWARTET.
     ‚* IN DER DATEN-SEKTION ERFOLGT GENERELL KEINE RÜCKMELDUNG
           IF       KEINE-ANTWORT > SPACES
                    GO TO SEN-DAT-90
           END-IF.

     ‚* DER PUFFER WIRD SO LANGE GELESEN, BIS EIN TIMEOUT
     ‚* VORHANDEN IST (DANN IST DER PUFFER LEER)
       SEN-DAT-20.
     ‚* ÜBERPRÜFEN, OB DER HOST EINE RÜCKMELDUNG GEGEBEN HAT
           MOVE     "3"               TO TCP-ACTION.
           MOVE     1                 TO TCP-TIMEOUT.
           COMPUTE  TCP-BUFLEN = LENGTH OF WRK-RECORD.
           INITIALIZE TCP-RETTXT.
           CALL     PROCEDURE "TCPIO"  USING TCP-SOCK
                                               TCP-ACTION
                                               WRK-RECORD
                                               TCP-BUFLEN
                                               TCP-DELIM
                                               TCP-DELIMLEN
                                               TCP-TIMEOUT
                                               TCP-RETTXT
           END-CALL.

     ‚* WIRD EIN TIMEOUT ZURÜCKGEGEBEN GIBT ES KEINE NACHRICHTEN MEHR
           IF       TCP-RETTXT = "*TIMEOUT"
            OR      TCP-BUFLEN = ZEROES
                    GO TO SEN-DAT-90
           END-IF.

     ‚* DER STRING VOM HOST MUSS ERST KONVERTIERT WERDEN
           MOVE     TCP-BUFLEN TO SEND-LEN.
           MOVE     SPACES TO EBC-ASCII.
           PERFORM  KONVERT-STRING.

     ‚* PRÜFEN, OB EIN FEHLER AUFGETRETEN IST
           IF       WRK-RECORD > SPACES
            AND    (WRK-RECORD(1:1) = "4"
            OR      WRK-RECORD(1:1) = "5")
                    MOVE LENGTH OF X-RETTXT TO SEND-LEN
                    MOVE WRK-RECORD(1:SEND-LEN) TO X-RETTXT
           ELSE
                    INITIALIZE X-RETTXT
           END-IF.

     ‚* AUSGABE DER RÜCKMELDUNG (FÜR DEN TEST AUSSTERNEN)
     ‚*€   DISPLAY  WRK-RECORD(1:SEND-LEN).

     ‚* WURDE DIE VERBINDUNG BEENDET? DANN NICHT WEITER AUF DEN PORT
     ‚* LISTEN
           IF       LETZTER-DATENSATZ > SPACES
                    GO TO SEN-DAT-90
           END-IF.

     ‚* ERNEUT DEN PUFFER LESEN
           GO TO SEN-DAT-20.

       SEN-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WRK-RECORD IN ASCII KONVERTIEREN
     ‚*--------------------------------------------------------------
       KONVERT-STRING SECTION.
       KON-STR-00.

           INITIALIZE RET-CODE.
           INITIALIZE INT-LEN-IN.
           INITIALIZE INT-LEN-OUT.

     ‚* SOLL VON ASCII VON ODER NACH ASCII KONVERTIERT WERDEN?
           IF       EBC-ASCII = SPACES
                    MOVE     819 TO CCSID-IN
                    MOVE     0   TO CODE-PAGE
           ELSE
                    MOVE     ZEROES  TO CCSID-IN
                    MOVE     819 TO CODE-PAGE
           END-IF.

           MOVE     SEND-LEN TO INT-LEN-IN.
           COMPUTE  INT-LEN-OUT = FUNCTION LENGTH (WRK-RECORD).

     ‚* LGPGM0818 KONVERTIERT DEN WRK-RECORD
           CALL     PROCEDURE  "CCP0818" USING
                                        WRK-RECORD(1:INT-LEN-IN)
                                        WRK-RECORD(1:INT-LEN-IN)
                                        INT-LEN-IN
                                        INT-LEN-OUT
                                        CCSID-IN
                                        CODE-PAGE
                                        RET-CODE
           END-CALL.

     ‚* WAR DAS KONVERTIEREN ERFOLGREICH
           IF       RET-CODE NOT = SPACES
                    GO TO KON-STR-90
           END-IF.

           MOVE     INT-LEN-OUT TO SEND-LEN.

       KON-STR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER DATEI
     ‚*--------------------------------------------------------------
       SENDE-DATEI SECTION.
       SND-FIL-00.

     ‚* INHALT DER ZU SENDENDEN DATEI LESEN
           COMPUTE  LEN-IN = LENGTH OF WRK-RECORD-FILE.
           CALL     PROCEDURE "GETIFS" USING X-FILE
                                               WRK-RECORD-FILE
                                               LEN-IN
                                               FILE-LEN
                                               RET-CODE
           END-CALL.

     ‚* KONNTE KEIN INHALT ERMITTELT WERDEN? BEENDEN
           IF       FILE-LEN = ZEROES
                    GO TO SND-FIL-90
           END-IF

     ‚* DEN DATEINAMEN AUS DEM IFS-VERZEICHNIS ERMITTELN
     ‚* LEN-IN    LEN-OUT
     ‚* V---------V
     ‚*           V-------V
     ‚* /HOME/USR/DATEI.TXT
     ‚* ZUNÄCHST DIE POSITION NACH DEM LETZTEN "/" ERMITTELN
           MOVE     LENGTH OF X-FILE TO LEN-IN.
           PERFORM  WITH TEST BEFORE UNTIL
                    LEN-IN = ZEROES
            OR      X-FILE(LEN-IN:1) = "/"
                    SUBTRACT 1 FROM LEN-IN
           END-PERFORM.
           ADD      1 TO LEN-IN.

     ‚* WIRD WIRD DIE POSITION VOM "/" BIZ ZUM NÄCHSTEN
     ‚* LEERZEICHEN ERMITTELT
           MOVE     LEN-IN TO LEN-OUT.
           PERFORM  WITH TEST BEFORE UNTIL
                    X-FILE(LEN-OUT:1) = SPACES
                    ADD 1 TO LEN-OUT
           END-PERFORM.
           MOVE     X-FILE(LEN-IN:LEN-OUT) TO FILE-WRK.

     ‚* NEUEN BODY ERÖFFNEN
           MOVE     "--XBODY" TO WRK-RECORD(1:7).
           MOVE     7 TO SEND-LEN.
           PERFORM  SENDEN.

     ‚* DATEI ANGEBEN
     ‚* ALS DATEITYP WIRD PAUSCHAL "MODEL" ANGEGEBEN.
           MOVE     1 TO SEND-LEN.
           STRING   "CONTENT-TYPE: MODEL; " DELIMITED BY SIZE
                    'NAME="'                DELIMITED BY SIZE
                    FILE-WRK                DELIMITED BY SIZE
                    '"'                     DELIMITED BY SIZE
                    INTO WRK-RECORD WITH POINTER SEND-LEN
           END-STRING.
           PERFORM  SENDEN-STRING.

     ‚* CODIERUNG DER DATEI ANGEBEN (IN UNSEREM FALL BASE64)
           MOVE     "CONTENT-TRANSFER-ENCODING: BASE64" TO
                    WRK-RECORD(1:33).
           MOVE     33 TO SEND-LEN.
           PERFORM  SENDEN

     ‚* AUCH HIER WIEDER VOR DEM INHALT EINE LEERZEILE AUSGEBEN
           PERFORM  LEERZEILE.

     ‚* DER TEXT MUSS ZUM SENDEN NACH BASE64 KONVERTIERT WERDEN
           COMPUTE  BASE-LEN-OUT = LENGTH OF WRK-RECORD-FILE.
           MOVE     FILE-LEN TO BASE-LEN-IN.
           CALL     PROCEDURE "BASE64" USING BASE64-AKTION
                                         WRK-RECORD-FILE(1:FILE-LEN)
                                         WRK-RECORD
                                         BASE-LEN-IN
                                         BASE-LEN-OUT
                                         RET-CODE
           END-CALL
           IF       RET-CODE NOT = SPACES
                    GO TO SND-FIL-90
           END-IF

     ‚* DATEI SENDEN
           MOVE     1 TO NO-CONVERT.
           MOVE     BASE-LEN-OUT TO SEND-LEN.
           PERFORM  SENDEN.
           MOVE     0 TO NO-CONVERT.

       SND-FIL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EMPFÄNGER ANGEBEN
     ‚*--------------------------------------------------------------
       ANGABE-EMPFAENGER SECTION.
       ANG-EMP-00.

     ‚* DIE ANGEGEBENEN EMPFÄNGER WERDEN ÜBERNOMMEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-EMPF
                    MOVE   1 TO SEND-LEN
                    STRING "RCPT TO: " DELIMITED BY SIZE
                           "<"         DELIMITED BY SIZE
                           ADRESSE OF EMPF-TAB(POS-TAB)
                                      (1:ADRLEN OF EMPF-TAB(POS-TAB))
                                       DELIMITED BY SIZE
                           ">"         DELIMITED BY SIZE
                           INTO WRK-RECORD WITH POINTER SEND-LEN
                    END-STRING
                    PERFORM  SENDEN-STRING
                    IF   X-RETTXT  = SPACES
                     AND TCP-ERROR = SPACES
                         MOVE "1" TO SENDER-OK
                    END-IF
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* WURDE MINDESTENS 1 GÜLTIGER EMPFÄNGER ANGEGEBEN?
           IF       SENDER-OK = SPACES
                    GO TO ANG-EMP-90
           END-IF.

     ‚* DIE ANGEGEBENEN KOPIEN WERDEN ÜBERNOMMEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-KOPIE
                    MOVE   1 TO SEND-LEN
                    STRING "RCPT TO: " DELIMITED BY SIZE
                           "<"         DELIMITED BY SIZE
                           ADRESSE OF KOPIE-TAB(POS-TAB)
                                     (1:ADRLEN OF KOPIE-TAB(POS-TAB))
                                      DELIMITED BY SIZE
                           ">"        DELIMITED BY SIZE
                           INTO WRK-RECORD WITH POINTER SEND-LEN
                    END-STRING
                    PERFORM  SENDEN-STRING
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* DIE ANGEGEBENEN BLINDKOPIEN WERDEN ÜBERNOMMEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-BKOPIE
                    MOVE   1 TO SEND-LEN
                    STRING "RCPT TO: " DELIMITED BY SIZE
                           "<"         DELIMITED BY SIZE
                           ADRESSE OF BKOPIE-TAB(POS-TAB)
                                      (1:ADRLEN OF BKOPIE-TAB(POS-TAB))
                                       DELIMITED BY SIZE
                           ">"         DELIMITED BY SIZE
                           INTO WRK-RECORD WITH POINTER SEND-LEN
                    END-STRING
                    PERFORM  SENDEN-STRING
                    ADD 1 TO POS-TAB
           END-PERFORM.

       ANG-EMP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CLEAREN ALLER TABELLEN
     ‚*--------------------------------------------------------------
       LOESCHEN-TABELLEN SECTION.
       LOE-TAB-00.

     ‚* LÖSCHEN EMPFÄNGERTABELLE
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE
                    UNTIL ADRESSE OF EMPF-TAB(POS-TAB) = SPACES
                    MOVE SPACES TO ADRESSE OF EMPF-TAB(POS-TAB)
                    MOVE ZEROES TO ADRLEN  OF EMPF-TAB(POS-TAB)
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* LÖSCHEN KOPIENTABLLE
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE
                    UNTIL ADRESSE OF KOPIE-TAB(POS-TAB) = SPACES
                    MOVE SPACES TO ADRESSE OF KOPIE-TAB(POS-TAB)
                    MOVE ZEROES TO ADRLEN  OF KOPIE-TAB(POS-TAB)
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* LÖSCHEN BLINDKOPIENTABELLE
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE
                    UNTIL ADRESSE OF BKOPIE-TAB(POS-TAB) = SPACES
                    MOVE SPACES TO ADRESSE OF BKOPIE-TAB(POS-TAB)
                    MOVE ZEROES TO ADRLEN  OF BKOPIE-TAB(POS-TAB)
                    ADD 1 TO POS-TAB
           END-PERFORM.

       LOE-TAB-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ÜBERNAHME ADRESSDATEN AUS DEM CL-PROGRAMM
     ‚*--------------------------------------------------------------
       UEBERNEHME-ADRESSEN SECTION.
       UEB-ADR-00.

     ‚* ÜBERNAHME ALLER EMPFÄNGER
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE
                    UNTIL POS-TAB > X-ANZAHL OF X-EMPF
                    MOVE X-ADRESSE OF X-EMPF(POS-TAB) TO
                           ADRESSE OF EMPF-TAB(POS-TAB)
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* ÜBERNAHME ALLER KOPIEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE
                    UNTIL POS-TAB > X-ANZAHL OF X-KOPIE
                    MOVE X-ADRESSE OF X-KOPIE(POS-TAB) TO
                           ADRESSE OF KOPIE-TAB(POS-TAB)
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* ÜBERNAHME ALLER BLINDKOPIEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE
                    UNTIL POS-TAB > X-ANZAHL OF X-BKOPIE
                    MOVE X-ADRESSE OF X-BKOPIE(POS-TAB) TO
                           ADRESSE OF BKOPIE-TAB(POS-TAB)
                    ADD 1 TO POS-TAB
           END-PERFORM.

       UEB-ADR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSGABE EMPFÄNGER / KOPIEN IM DATA-BEREICH
     ‚*--------------------------------------------------------------
       AUSGABE-EMPFAENGER SECTION.
       AUS-EMP-00.

     ‚* DIE ANGEGEBENEN EMPFÄNGER WERDEN ÜBERNOMMEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-EMPF
                    MOVE   1 TO SEND-LEN
                    STRING "TO: " DELIMITED BY SIZE
                           ADRESSE OF EMPF-TAB(POS-TAB)
                                      (1:ADRLEN OF EMPF-TAB(POS-TAB))
                                       DELIMITED BY SIZE
                           INTO WRK-RECORD WITH POINTER SEND-LEN
                    END-STRING
                    PERFORM  SENDEN-STRING
                    ADD 1 TO POS-TAB
           END-PERFORM.

     ‚* DIE ANGEGEBENEN KOPIEN WERDEN ÜBERNOMMEN
           MOVE     1 TO POS-TAB.
           PERFORM  WITH TEST BEFORE UNTIL
                    POS-TAB > X-ANZAHL OF X-KOPIE
                    MOVE   1 TO SEND-LEN
                    STRING "CC: " DELIMITED BY SIZE
                           ADRESSE OF KOPIE-TAB(POS-TAB)
                                    (1:ADRLEN OF KOPIE-TAB(POS-TAB))
                                     DELIMITED BY SIZE
                           INTO WRK-RECORD WITH POINTER SEND-LEN
                    END-STRING
                    PERFORM  SENDEN-STRING
                    ADD 1 TO POS-TAB
           END-PERFORM.

       AUS-EMP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LEERZEILE AUSGEBEN
     ‚*--------------------------------------------------------------
       LEERZEILE SECTION.
       AUS-LER-00.

     ‚* AUSGABE EINER LEERZEILE
           MOVE     SPACES TO WRK-RECORD(1:1)
           MOVE     1 TO SEND-LEN.
           PERFORM  SENDEN.

       AUS-LER-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERBINDUNG SCHLIEßEN
     ‚*--------------------------------------------------------------
       VERBINDUNG-SCHLIESSEN SECTION.
       VER-SCH-00.

     ‚* VERBINDUNG SCHLIESSEN
           MOVE     5   TO TCP-TIMEOUT.
           MOVE     "1" TO TCP-ACTION.
           INITIALIZE TCP-RETTXT.
           CALL     PROCEDURE "TCPIO"  USING TCP-SOCK
                                               TCP-ACTION
                                               RECORD-SIZE
                                               TCP-BUFLEN
                                               TCP-DELIM
                                               TCP-DELIMLEN
                                               TCP-TIMEOUT
                                               TCP-RETTXT
           END-CALL.

       VER-SCH-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DIE MAIL IN DIE DATEI ABLEGEN
     ‚*--------------------------------------------------------------
       SAVE-MAIL SECTION.
       SAV-MAI-00.

           INITIALIZE PCFGMAIF OF PCFGMAI-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PCFGMAI" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK          TO MILFDN OF PCFGMAI-P.
     ‚* DATEN FÜLLEN
           MOVE     X-TOPIC           TO MITOPC OF PCFGMAI-P.
           MOVE     X-EMPF            TO MITO   OF PCFGMAI-P.
           MOVE     X-KOPIE           TO MICC   OF PCFGMAI-P.
           MOVE     X-BKOPIE          TO MIBCC  OF PCFGMAI-P.
           MOVE     X-DATA            TO MIDATA OF PCFGMAI-P.
           MOVE     X-FILE            TO MIFILE OF PCFGMAI-P.
           MOVE     X-HOST            TO MIHOST OF PCFGMAI-P.
           MOVE     X-PORT            TO MIPORT OF PCFGMAI-P.
           MOVE     X-SENDER          TO MISNDR OF PCFGMAI-P.
     ‚* ANLAGEDATEN FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER            TO MIAUSR OF PCFGMAI-P.
           MOVE     T-TERM            TO MIABS  OF PCFGMAI-P.
           MOVE     PGM-WRK           TO MIAPGM OF PCFGMAI-P.
           MOVE     DATE-8            TO MIADTA OF PCFGMAI-P.
           MOVE     TIME-6            TO MIATIA OF PCFGMAI-P.
           WRITE    PCFGMAI-P.

       SAV-MAI-90.
           EXIT.
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
