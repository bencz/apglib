       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0022.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PROJEKTNUMMER AUFBEREITEN                                     *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.02.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  AUFRUFPROGRAMM FÜR MAIL "HLP0006"            *
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
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0022".
       01  PGM-TYP                       PIC X(8)  VALUE "EMAIL".

       01  ADRLEN-DUMMY                  PIC X(64).

     ‚* SCHALTER
       01  MAIL-AKTIV                    LIKE CFFK01 OF CFG-CPY.
       01  ADD-TOPIC-PGM                 LIKE CFFK02 OF CFG-CPY.
       01  CHECK-KEY-DA                  LIKE CFFK03 OF CFG-CPY.

       01  CNT                           PIC 9(4).
       01  RCV-CNT                       LIKE CNT.
       01  CRV-CNT                       LIKE CNT.
       01  BCRV-CNT                      LIKE CNT.
       01  CHECK-ADDRESS                 LIKE ADRLEN-DUMMY.
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-SENDER-KEY                  LIKE CFKEY  OF CFG-CPY.
       01  X-CHECK-ID                    LIKE CFID   OF CFG-CPY.
       01  X-CHECK-KEY                   LIKE CFKEY  OF CFG-CPY.
       01  X-AKTION                      PIC X(20).
       01  X-USER                        LIKE T-USER.
       01  X-PGM                         LIKE PGM-WRK.
     ‚* ÜBERNAHME DER EMPFÄNGER                             *
       01  X-EMPF.
        05 X-ANZAHL                      PIC 9(2) BINARY.
        05 X-EMPF-REC                    OCCURS 10 TIMES.
          10 X-ADRESSE                   LIKE ADRLEN-DUMMY.
     ‚* ÜBERNAHME DER KOPIEADRESSEN
       01  X-KOPIE.
        05 X-ANZAHL                      PIC 9(2) BINARY.
        05 X-KOPIE-REC                   OCCURS 10 TIMES.
          10 X-ADRESSE                   LIKE ADRLEN-DUMMY.
     ‚* ÜBERNAHME DER BLINDKOPIEADRESSEN
       01  X-BKOPIE.
        05 X-ANZAHL                      PIC 9(2) BINARY.
        05 X-BKOPIE-REC                  OCCURS 10 TIMES.
          10 X-ADRESSE                   LIKE ADRLEN-DUMMY.
       01  X-TOPIC                       LIKE MAIL-TOPIC  OF MAIL-TAB.
       01  X-DATA                        LIKE MAIL-DATA   OF MAIL-TAB.
       01  X-FILE                        LIKE MAIL-FILE   OF MAIL-TAB.
     ‚* IM RETTXT STEHT DIE RÜCKMELDUNG DES SERVERS, FALLS
     ‚* DIESE NEGATIV WAR
       01  X-RETTXT                      LIKE MAIL-RETTXT OF MAIL-TAB.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING  X-SENDER-KEY
                                 X-CHECK-ID
                                 X-CHECK-KEY
                                 X-AKTION
                                 X-USER
                                 X-PGM
                                 X-EMPF
                                 X-KOPIE
                                 X-BKOPIE
                                 X-TOPIC
                                 X-DATA
                                 X-FILE
                                 X-RETTXT.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           INITIALIZE MAIL-TAB.
     ‚* ERMITTELN HOSTNAME, PORT UND SENDER
           INITIALIZE PCONFIGF OF CFG-CPY.
           MOVE     "A400"            TO CFID   OF CFG-CPY.
           MOVE     X-SENDER-KEY      TO CFKEY  OF CFG-CPY.
           PERFORM  COPY-CFG-CPY.
           IF       CFG-DA OF CFG-CPY = ZEROES
     ‚*             DEFAULTWERTE ERMITTELN
                    MOVE "SYSEH03"    TO MAIL-HOST OF MAIL-TAB
                    MOVE "25"         TO MAIL-PORT OF MAIL-TAB
                    MOVE "APG@ehrhardt-partner.com" TO
                         MAIL-SENDER OF MAIL-TAB
                    MOVE ZEROES       TO ADD-TOPIC-PGM
                    MOVE 1            TO MAIL-AKTIV
                    MOVE ZEROES       TO CHECK-KEY-DA
           ELSE
     ‚*             WERTE AUS SCHLÜSSEL ÜBERNEHMEN
                    MOVE CFIN21 OF CFG-CPY TO MAIL-HOST   OF MAIL-TAB
                    MOVE CFNM05 OF CFG-CPY TO MAIL-PORT   OF MAIL-TAB
                    MOVE CFIN22 OF CFG-CPY TO MAIL-SENDER OF MAIL-TAB
                    MOVE CFFK01 OF CFG-CPY TO MAIL-AKTIV
                    MOVE CFFK02 OF CFG-CPY TO ADD-TOPIC-PGM
                    MOVE CFFK03 OF CFG-CPY TO CHECK-KEY-DA
           END-IF.
     ‚* WIRD ÜBER DIESE EINSTELLUNG MAILS GESENDET?
           IF       MAIL-AKTIV = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB DIE EMPFÄNGER DENN ALLE EINE MAIL WOLLEN
           PERFORM  CHECK-RECEIVER.
           IF       RCV-CNT = ZEROES
            AND     CRV-CNT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* WERTE ÜBERNEHMEN
           MOVE     X-DATA  TO MAIL-DATA  OF MAIL-TAB.

     ‚* JETZT GGF. DEN PROGRAMMNAMEN IM BETREFF ANHÄNGEN
           IF       ADD-TOPIC-PGM NOT = ZEROES
                    PERFORM CHANGE-TOPIC-ADD-PGM
           ELSE
                    MOVE X-TOPIC TO MAIL-TOPIC OF MAIL-TAB
           END-IF.

     ‚* JETZT GGF. DEN SCHLÜSSEL ERWÄHNEN MIT DEM DIE MAIL
     ‚* DEAKTIVIERT WERDEN KANN
           IF       X-CHECK-ID  NOT = SPACES
            AND     X-CHECK-KEY NOT = SPACES
                    PERFORM CHANGE-DATA-ADD-KEY
           END-IF.

     ‚* VERSCHIEDENE USER IN DIE BLINDKOPIE EINTRAGEN
           PERFORM  ADD-RECEIVER-BCC.

     ‚* DAS "@EHRHARDT-PARTNER.COM" ANHÄNGEN
           PERFORM  ADD-RECEIVER-MAIL.

     ‚* DIE ANZAHL DER MAILUSER EINTRAGEN
           MOVE     RCV-CNT   TO MAIL-ANZAHL OF MAIL-EMPF.
           MOVE     CRV-CNT   TO MAIL-ANZAHL OF MAIL-KOPIE.
           MOVE     BCRV-CNT  TO MAIL-ANZAHL OF MAIL-BKOPIE.

     ‚* ZUM SCHLUSS DIE MAIL AN DAS SENDEPROGRAMM ÜBERGEBEN
           PERFORM  SEND-MAIL.

       ENDE.
           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN DER MAIL
     ‚*--------------------------------------------------------------
       SEND-MAIL SECTION.
       SND-MAI-00.

     ‚* MAILPROGRAMM AUFRUFEN
           CALL     "HLP0026"  USING MAIL-HOST
                                     MAIL-PORT
                                     MAIL-SENDER
                                     MAIL-EMPF
                                     MAIL-KOPIE
                                     MAIL-BKOPIE
                                     MAIL-TOPIC
                                     MAIL-DATA
                                     MAIL-FILE
                                     MAIL-RETTXT
           END-CALL.

       SND-MAI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINIGE USER ALS BLINDKOPIE EINTRAGEN
     ‚*--------------------------------------------------------------
       ADD-RECEIVER-BCC SECTION.
       ADD-RBC-00.

     ‚* MICH IMMER EINTRAGEN
             IF     BCRV-CNT < 10
                    ADD    1 TO BCRV-CNT
                    MOVE   "APG" TO
                           MAIL-ADRESSE OF MAIL-BKOPIE-REC(BCRV-CNT)
             END-IF.

     ‚* ABSENDER IMMER EINTRAGEN
             IF     BCRV-CNT   < 10
              AND   X-USER NOT = "APG"
              AND   X-USER NOT = "QUSER"
              AND   X-USER NOT = "QSECOFR"
                    ADD    1 TO BCRV-CNT
                    MOVE   X-USER TO
                           MAIL-ADRESSE OF MAIL-BKOPIE-REC(BCRV-CNT)
             END-IF.

       ADD-RBC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAILADRESSE VERVOLLSTÄNDIGEN
     ‚*--------------------------------------------------------------
       ADD-RECEIVER-MAIL SECTION.
       ADD-RML-00.

           INITIALIZE CNT.
     ‚* ALLE EMPFÄNGER BEARBEITEN
           PERFORM  RCV-CNT TIMES
                    ADD 1 TO CNT
                    MOVE    MAIL-ADRESSE OF MAIL-EMPF-REC(CNT) TO
                            CHECK-ADDRESS
                    PERFORM ADD-MAIL
                    MOVE    CHECK-ADDRESS TO
                            MAIL-ADRESSE OF MAIL-EMPF-REC(CNT)
           END-PERFORM.

           INITIALIZE CNT.
     ‚* ALLE KOPIE-EMPFÄNGER BEARBEITEN
           PERFORM  CRV-CNT TIMES
                    ADD 1 TO CNT
                    MOVE    MAIL-ADRESSE OF MAIL-KOPIE-REC(CNT) TO
                            CHECK-ADDRESS
                    PERFORM ADD-MAIL
                    MOVE    CHECK-ADDRESS TO
                            MAIL-ADRESSE OF MAIL-KOPIE-REC(CNT)
           END-PERFORM.

           INITIALIZE CNT.
     ‚* ALLE BLINDKOPIE-EMPFÄNGER BEARBEITEN
           PERFORM  BCRV-CNT TIMES
                    ADD 1 TO CNT
                    MOVE    MAIL-ADRESSE OF MAIL-BKOPIE-REC(CNT) TO
                            CHECK-ADDRESS
                    PERFORM ADD-MAIL
                    MOVE    CHECK-ADDRESS TO
                            MAIL-ADRESSE OF MAIL-BKOPIE-REC(CNT)
           END-PERFORM.

       ADD-RML-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAILADRESSE IN DER FORM <USER>@EHRHARDT-PARTNER.COM SCHREIBEN
     ‚*--------------------------------------------------------------
       ADD-MAIL SECTION.
       ADD-MAI-00.

     ‚*€   IF       CHECK-ADDRESS NOT = SPACES
     ‚*€            STRING CHECK-ADDRESS           DELIMITED BY SPACES
     ‚*€                   "@ehrhardt-partner.com" DELIMITED BY SIZE
     ‚*€              INTO CHECK-ADDRESS
     ‚*€            END-STRING
     ‚*€   END-IF.

       ADD-MAI-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PROGRAMMNAMEN ZUM BETREFF HINZUFÜGEN
     ‚*--------------------------------------------------------------
       CHANGE-TOPIC-ADD-PGM SECTION.
       CHG-TAP-00.

     ‚* PROGRAMM UND BETREFF ZUSAMMENBAUEN
           STRING   X-PGM                    DELIMITED BY SPACES
                    ": "                     DELIMITED BY SIZE
                    X-TOPIC                  DELIMITED BY SIZE
             INTO   MAIL-TOPIC OF MAIL-TAB
           END-STRING.

       CHG-TAP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ZUM TEXT NOCH DIE ZEILE HINZUFÜGEN, DASS DIE MAIL MIT
     ‚* EINEM SCHLÜSSEL DEAKTIVIERT WERDEN KANN
     ‚*--------------------------------------------------------------
       CHANGE-DATA-ADD-KEY SECTION.
       CHG-DAK-00.

     ‚* EMAIL BODY ALLGEMEINE INFOS - P3
           STRING   FUNCTION TRIM(MAIL-DATA)   DELIMITED BY SIZE
                    X"252525"                  DELIMITED BY SIZE
                    "------------------------" DELIMITED BY SIZE
                    X"25"                      DELIMITED BY SIZE
                    "Diese E-Mail kann in APG" DELIMITED BY SIZE
                    "/CONFIG im Schlüssel "    DELIMITED BY SIZE
                    FUNCTION TRIM(X-CHECK-ID)  DELIMITED BY SIZE
                    " unter dem Eintrag "      DELIMITED BY SIZE
                    FUNCTION TRIM(X-CHECK-KEY) DELIMITED BY SIZE
                    " deaktiviert wer"         DELIMITED BY SIZE
                    "den."                     DELIMITED BY SIZE
               INTO MAIL-DATA
           END-STRING.

       CHG-DAK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIE EMPFÄNGER DIE MAIL AUCH WOLLEN
     ‚*--------------------------------------------------------------
       CHECK-RECEIVER SECTION.
       CHK-RCV-00.

     ‚* EMPFÄNGERANZAHL INITIALISIEREN
           INITIALIZE RCV-CNT, CRV-CNT, BCRV-CNT.

           INITIALIZE CNT.
     ‚* ALLE EMPFÄNGER PRÜFEN
           PERFORM  10 TIMES
                    ADD 1 TO CNT
                    MOVE X-ADRESSE OF X-EMPF-REC(CNT) TO CHECK-ADDRESS
                    PERFORM CHECK-MAIL-RCV
                    IF   RET-CODE = SPACES
     ‚*                  WILL DIE MAIL HABEN, ALSO HINZUFÜGEN
                         ADD 1 TO RCV-CNT
                         MOVE X-ADRESSE OF X-EMPF-REC(CNT) TO
                              MAIL-ADRESSE OF MAIL-EMPF-REC(RCV-CNT)
                    END-IF
           END-PERFORM.

           INITIALIZE CNT.
     ‚* ALLE KOPIE-EMPFÄNGER PRÜFEN
           PERFORM  10 TIMES
                    ADD 1 TO CNT
                    MOVE X-ADRESSE OF X-KOPIE-REC(CNT) TO CHECK-ADDRESS
                    PERFORM CHECK-MAIL-RCV
                    IF   RET-CODE = SPACES
     ‚*                  WILL DIE MAIL HABEN, ALSO HINZUFÜGEN
                         ADD 1 TO CRV-CNT
                         MOVE X-ADRESSE OF X-KOPIE-REC(CNT) TO
                              MAIL-ADRESSE OF MAIL-KOPIE-REC(CRV-CNT)
                    END-IF
           END-PERFORM.

           INITIALIZE CNT.
     ‚* ALLE BLINDKOPIE-EMPFÄNGER PRÜFEN
           PERFORM  10 TIMES
                    ADD 1 TO CNT
                    MOVE X-ADRESSE OF X-BKOPIE-REC(CNT) TO
                         CHECK-ADDRESS
                    PERFORM CHECK-MAIL-RCV
                    IF   RET-CODE = SPACES
     ‚*                  WILL DIE MAIL HABEN, ALSO HINZUFÜGEN
                         ADD 1 TO BCRV-CNT
                         MOVE X-ADRESSE OF X-BKOPIE-REC(CNT) TO
                              MAIL-ADRESSE OF MAIL-BKOPIE-REC(BCRV-CNT)
                    END-IF
           END-PERFORM.

       CHK-RCV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DER USER DIE MAIL HABEN WILL
     ‚*--------------------------------------------------------------
       CHECK-MAIL-RCV SECTION.
       CHK-MRV-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB DER USER DIE MAIL GGF. DEAKTIVIERT HAT
           IF       X-CHECK-ID    NOT = SPACES
            AND     X-CHECK-KEY   NOT = SPACES
            AND     CHECK-ADDRESS NOT = SPACES
                    INITIALIZE PCONFIGF OF CFG-CPY
                    MOVE     X-CHECK-ID        TO CFID   OF CFG-CPY
                    MOVE     X-CHECK-KEY       TO CFKEY  OF CFG-CPY
                    MOVE     CHECK-ADDRESS     TO CFKEY2 OF CFG-CPY
                    PERFORM  COPY-CFG-CPY
     ‚*             IST CHECK-KEY-DA = 1, SO SOLL AUCH EINE MAIL
     ‚*             GESENDET WERDEN WENN DER EINTRAG NICHT EXISTIERT.
     ‚*             NUR EIN VORHANDENER EINTRAG KANN DAS VERSENDEN
     ‚*             EINER MAIL VERHINDERN
                    IF   (    CFG-DA OF CFG-CPY = ZEROES
                          AND CHECK-KEY-DA      = ZEROES)
                     OR  CFFK01 OF CFG-CPY = 1
                         MOVE "1" TO RET-CODE
                    END-IF
           ELSE
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-MRV-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CFG-CPY: LESEN DER KONFIGURATION
     ‚*---------------------------------------------------------------

           COPY     CFGCPY     OF APG-QCPYSRC.
      /
