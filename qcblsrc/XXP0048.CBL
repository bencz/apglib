     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0048.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* GEBURTSTAGSMAIL SCHREIBEN                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.09.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SCHREIBEN DER GEBURTSTAGSMAIL                *
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
     ‚* PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCONFIGL2: XXDAY, XXMON
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DL
                  ASSIGN       TO  DATABASE-PCONFIGL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PCFGDAT: DTLFDN
     ‚*-------------------------------------------------------------
           SELECT PCFGDAT-DP
                  ASSIGN       TO  DATABASE-PCFGDAT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCONFIG: CFID, CFKEY, CFKEY2, CFKEY3, CFKEY4
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
     ‚*--- PCONFIGL2: XXDAY, XXMON
       FD  PCONFIG-DL
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-L.
           COPY DDS-ALL-FORMATS OF PCONFIGL2.
      /
     ‚*--- PCFGDAT: DTLFDN
       FD  PCFGDAT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGDAT-P.
           COPY DDS-ALL-FORMATS OF PCFGDAT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0048".
       01  PGM-TYP                       PIC X(8)  VALUE "TEST".

     ‚* VARIABLEN FÜR DIE ALTERSBERECHNUNG
       01  YEAR-ACT                      PIC 9(4).
       01  YEAR-GEB                      PIC 9(4).
       01  DSP-AGE-NUM                   PIC 9(4).
       01  DSP-AGE                       PIC X(4).

       01  TEST-MODE                     PIC X(1).
       01  TODAY-TOMORROW                PIC 9(1).
       01  HEADER-WRITTEN                PIC 9(1).
       01  DATE-WRK                      LIKE DATE-8.
       01  MAIL-ANZ                      LIKE MAIL-ANZAHL OF MAIL-EMPF.
       01  MAIL-C-ANZ                    LIKE MAIL-ANZAHL OF MAIL-EMPF.

       01  BETREFF-WRK                   PIC X(50).
       01  BETREFF-LEN                   PIC 9(4).

     ‚* PARAMETER FÜR DAS HLP0022
       01  SENDER-KEY                    LIKE CFKEY  OF CFG-CPY.
       01  CHECK-ID                      LIKE CFID   OF CFG-CPY.
       01  CHECK-KEY                     LIKE CFKEY  OF CFG-CPY.
       01  MAIL-AKTION                   PIC X(20).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-DATE                        LIKE DATE-8.
       01  X-TEST-FLAG                   PIC X(1).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-DATE
                                X-TEST-FLAG.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0048" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     INPUT PCONFIG-DL
                          PCONFIG-DP
                          PCFGDAT-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE DATEN.
           INITIALIZE TEST-MODE.
           INITIALIZE HEADER-WRITTEN.
           INITIALIZE MAIL-TAB.
           MOVE     X-DATE TO DATE-WRK.
           MOVE     1      TO MAIL-LEN.
     ‚* ERMITTELN DATUM/ZEIT
           IF       X-DATE = ZEROES
                    PERFORM  COPY-GET-TIME
                    MOVE     DATE-8 TO X-DATE
           END-IF.

     ‚* TESTMODUS?
           IF       ADDRESS OF X-TEST-FLAG NOT = NULL
                    MOVE X-TEST-FLAG TO TEST-MODE
           END-IF.

           INITIALIZE BETREFF-WRK.
           MOVE     1 TO BETREFF-LEN.
     ‚* SOLL DIE MAIL ÜBERHAUPT GESENDET WERDEN?
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A400"            TO CFID   OF CFG-CPY.
           MOVE     "GEBURTSTAG"      TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
            AND     CFFK01 OF CFG-CPY = 1
     ‚*             HEUTIGE GEBURTSTAGE
                    MOVE    ZEROES TO TODAY-TOMORROW
                    PERFORM CHECK-BIRTHDAY-USER
                    IF   DATEN NOT = ZEROES
     ‚*                  MORGIGE GEBURTSTAGEE
                         PERFORM GET-NEXT-DAY
                         MOVE    ZEROES TO DATEN
                         MOVE    1      TO TODAY-TOMORROW
     ‚*                  ZUSÄTZLICHE LEERZEILEN ERSTELLEN
                         STRING X"2525" DELIMITED BY SIZE
                                X"2525" DELIMITED BY SIZE
                           INTO MAIL-DATA
                           WITH POINTER MAIL-LEN
                         END-STRING
     ‚*                  MAILTEXT NUN AUFBAUEN
                         PERFORM CHECK-BIRTHDAY-USER
     ‚*                  MAIL SCHREIBEN
                         PERFORM WRITE-MAIL
                    END-IF
           END-IF.

       ENDE.
           CLOSE    PCONFIG-DL
                    PCONFIG-DP
                    PCFGDAT-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ALLE USER MIT DIESEM DATUM HERAUSLESEN
     ‚*--------------------------------------------------------------
       CHECK-BIRTHDAY-USER SECTION.
       CHK-BDU-00.

     ‚* MIT TAG UND MONAT AUFSETZEN
           INITIALIZE PCONFIGF OF PCONFIG-L.
           MOVE     X-DATE(7:2) TO XXDAY  OF PCONFIG-L.
           MOVE     X-DATE(5:2) TO XXMON  OF PCONFIG-L.
           MOVE     ZEROES      TO XXYEAR OF PCONFIG-L.
           START    PCONFIG-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-BDU-90
           END-IF.

       CHK-BDU-20.
           READ     PCONFIG-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-BDU-90
           END-IF.

           IF       XXDAY  OF PCONFIG-L NOT = X-DATE(7:2)
            OR      XXMON  OF PCONFIG-L NOT = X-DATE(5:2)
                    GO TO CHK-BDU-90
           END-IF.

     ‚* JETZT DIE PERSON ANHÄNGEN
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "H050"              TO CFID   OF CFG-CPY.
           MOVE     CFKEY  OF PCONFIG-L TO CFKEY  OF CFG-CPY.
           MOVE     CFKEY2 OF PCONFIG-L TO CFKEY2 OF CFG-CPY.
           MOVE     CFKEY3 OF PCONFIG-L TO CFKEY3 OF CFG-CPY.
           MOVE     CFKEY4 OF PCONFIG-L TO CFKEY4 OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = 1
                    PERFORM  ADD-TO-MAIL
           END-IF.

           GO TO    CHK-BDU-20.

       CHK-BDU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL AUFBAUEN
     ‚*--------------------------------------------------------------
       ADD-TO-MAIL SECTION.
       ADD-TOM-00.

     ‚* HEADER GESCHRIEBEN?
           IF       HEADER-WRITTEN = ZEROES
                    MOVE 1 TO HEADER-WRITTEN
     ‚*             EINLEITUNG DER MAIL
                    STRING   "Hallo."                 DELIMITED BY SIZE
                             X"2525"                  DELIMITED BY SIZE
                      INTO MAIL-DATA
                      WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

     ‚* EMAIL BODY ALLGEMEINE INFOS
           EVALUATE DATEN ALSO TODAY-TOMORROW
               WHEN ZEROES ALSO ZEROES
                    MOVE 1 TO DATEN
     ‚*             EINLEITUNG DER MAIL
                    STRING   "Folgende Personen "     DELIMITED BY SIZE
                             "haben heute Geburtstag" DELIMITED BY SIZE
                             ":"                      DELIMITED BY SIZE
                             X"25"                    DELIMITED BY SIZE
                             "======================" DELIMITED BY SIZE
                             "======================" DELIMITED BY SIZE
                             X"2525"                  DELIMITED BY SIZE
                      INTO MAIL-DATA
                      WITH POINTER MAIL-LEN
                    END-STRING
               WHEN ZEROES ALSO 1
                    MOVE 1 TO DATEN
     ‚*             EINLEITUNG DER MAIL
                    STRING   "Folgende Personen "     DELIMITED BY SIZE
                             "haben morgen "          DELIMITED BY SIZE
                             "Geburtstag:"            DELIMITED BY SIZE
                             X"25"                    DELIMITED BY SIZE
                             "======================" DELIMITED BY SIZE
                             "======================" DELIMITED BY SIZE
                             X"2525"                  DELIMITED BY SIZE
                      INTO MAIL-DATA
                      WITH POINTER MAIL-LEN
                    END-STRING
           END-EVALUATE.

     ‚* DATUM IN ANZEIGEFORMAT BRINGEN
           IF       CFIN04 OF CFG-CPY(1:8) IS NUMERIC
                    MOVE     CFIN04 OF CFG-CPY(1:8) TO DATE-DB
                    MOVE     "TO-DSP"               TO DATUM-CONVERT
                    PERFORM  COPY-CONVERT-DATE
           ELSE
                    MOVE     ZEROES                 TO DATE-DB
           END-IF.

     ‚* ALTER BERECHNEN
           MOVE     DATE-DB(1:4) TO YEAR-GEB.
           MOVE     X-DATE(1:4)  TO YEAR-ACT.
           COMPUTE  DSP-AGE-NUM = YEAR-ACT - YEAR-GEB
           END-COMPUTE.

     ‚* ALTER AUFBEREITEN
           MOVE     DSP-AGE-NUM TO DSP-AGE.
           INSPECT  DSP-AGE REPLACING LEADING ZEROES BY SPACES.

     ‚* JETZT TEXT FÜR DEN USER
           STRING   CFKEY  OF CFG-CPY(1:10)
                    " "
                    DATUM-ALPHA-10
                    " "
                    DSP-AGE
                    " "
                    "Jahre"
                    "   "
                    "Büro: "
                    CFIN26 OF CFG-CPY(1:10)
                    "/ "
                    FUNCTION TRIM(CFIN21 OF CFG-CPY)
                    " "
                    FUNCTION TRIM(CFIN22 OF CFG-CPY)
                    X"25"                      DELIMITED BY SIZE
               INTO MAIL-DATA
               WITH POINTER MAIL-LEN
           END-STRING.

     ‚* DEN BETREFF AUCH ANPASSEN
           IF       BETREFF-WRK = SPACES
                    STRING   CFKEY  OF CFG-CPY(1:10) DELIMITED BY SPACE
                      INTO BETREFF-WRK
                      WITH POINTER BETREFF-LEN
                    END-STRING
           ELSE
                    STRING   ", "                    DELIMITED BY SIZE
                             CFKEY  OF CFG-CPY(1:10) DELIMITED BY SPACE
                      INTO BETREFF-WRK
                      WITH POINTER BETREFF-LEN
                    END-STRING
           END-IF.

       ADD-TOM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL VORBEREITEN UND SENDEN
     ‚*--------------------------------------------------------------
       WRITE-MAIL SECTION.
       WRT-MAI-00.

     ‚* MAIL-TOPIC AUFRUFEN
           MOVE     DATE-WRK               TO DATE-DB.
           MOVE     "TO-DSP"               TO DATUM-CONVERT.
           PERFORM  COPY-CONVERT-DATE.

           STRING   "GEBURTSTAG "      DELIMITED BY SIZE
                    DATUM-ALPHA-10     DELIMITED BY SIZE
               INTO MAIL-TOPIC OF MAIL-TAB
           END-STRING.

     ‚* JETZT ALLE EMPFÄNGER FÜLLEN
           PERFORM  FILL-RECEIVER.

     ‚* MAILPROGRAMM AUFRUFEN
           MOVE     "GEBURTSTAG"   TO SENDER-KEY.
           MOVE     "H051"         TO CHECK-ID.
           MOVE     "GEBURTSTAG"   TO CHECK-KEY.
           CALL     "HLP0022"  USING SENDER-KEY
                                     CHECK-ID
                                     CHECK-KEY
                                     MAIL-AKTION
                                     T-USER
                                     PGM-WRK
                                     MAIL-EMPF
                                     MAIL-KOPIE
                                     MAIL-BKOPIE
                                     MAIL-TOPIC
                                     MAIL-DATA
                                     MAIL-FILE
                                     MAIL-RETTXT
           END-CALL.

       WRT-MAI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EMPFÄNGER FÜLLEN
     ‚*--------------------------------------------------------------
       FILL-RECEIVER SECTION.
       FIL-RCV-00.

           MOVE     ZEROES TO MAIL-ANZ, MAIL-C-ANZ.
     ‚* PCONFIG LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "H051"       TO CFID   OF PCONFIG-P.
           MOVE     "GEBURTSTAG" TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES       TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES       TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES       TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO FIL-RCV-90
           END-IF.

       FIL-RCV-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO FIL-RCV-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "H051"
            OR      CFKEY  OF PCONFIG-P NOT = "GEBURTSTAG"
                    GO TO FIL-RCV-90
           END-IF.

     ‚* KOPFSATZ ODER MAIL DEAKTIVIERT?
           IF       CFFK01 OF PCONFIG-P = 1
            OR      CFKEY2 OF PCONFIG-P = SPACES
                    GO TO FIL-RCV-20
           END-IF.

     ‚* IM TESTMODUS NUR AN MICH SENDEN
           IF       TEST-MODE               = "1"
            AND     CFKEY2 OF PCONFIG-P NOT = "APG"
                    GO TO FIL-RCV-20
           END-IF.

     ‚* EMPFÄNGER ZUSAMMENSETZEN
           IF       MAIL-ANZ < 10
                    ADD  1        TO MAIL-ANZ
                    MOVE MAIL-ANZ TO MAIL-ANZAHL OF MAIL-EMPF
                    MOVE CFKEY2 OF PCONFIG-P TO
                         MAIL-ADRESSE OF MAIL-EMPF-REC(MAIL-ANZ)
           ELSE
     ‚*             NUR 8 GEHEN HIER REIN DA GGF. NOCH 2 DAZUKOMMEN
                    IF   MAIL-C-ANZ < 8
                         ADD  1          TO MAIL-C-ANZ
                         MOVE MAIL-C-ANZ TO MAIL-ANZAHL OF MAIL-KOPIE
                         MOVE CFKEY2 OF PCONFIG-P TO
                              MAIL-ADRESSE OF MAIL-KOPIE-REC(MAIL-ANZ)
                    END-IF
           END-IF.

           GO TO    FIL-RCV-20.

       FIL-RCV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* NÄCHSTEN TAG ERMITTELN
     ‚*--------------------------------------------------------------
       GET-NEXT-DAY SECTION.
       GET-NXD-00.

     ‚* MIT TAG UND MONAT AUFSETZEN
           INITIALIZE PCFGDATF OF PCFGDAT-P.
           MOVE     X-DATE      TO DTLFDN OF PCFGDAT-P.
           START    PCFGDAT-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-NXD-90
           END-IF.

       GET-NXD-20.
           READ     PCFGDAT-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO GET-NXD-90
           END-IF.

     ‚* SOLANGE WEITERLESEN BIS DTLFDN GRÖßER ALS HEUTE IST
           IF       DTLFDN OF PCFGDAT-P <= X-DATE
                    GO TO GET-NXD-20
           END-IF.

     ‚* DEN TAG MERKEN
           MOVE     DTLFDN OF PCFGDAT-P TO X-DATE.

       GET-NXD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CONVERT-DATE: DATUMSFELDER AUFBEREITEN/UMSETZEN
     ‚*---------------------------------------------------------------

           COPY     CNVDATE    OF APG-QCPYSRC.
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
