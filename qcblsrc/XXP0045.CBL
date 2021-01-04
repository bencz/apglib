     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             XXP0045.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ABSCHLIEßEN KW FÜR USER                                       *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.07.2011                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ABSCHLIEßEN EINER KW UND MAIL SENDEN         *
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

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH CFG-CPY
           COPY WRKCFGCPY  OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP0045".
       01  PGM-TYP                       PIC X(8)  VALUE "PLANUNG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  SBMJOB-REC                    TYPE SBMJOB-TYPE.

     ‚* PARAMETER FÜR DAS HLP0022
       01  SENDER-KEY                    LIKE CFKEY  OF CFG-CPY.
       01  CHECK-ID                      LIKE CFID   OF CFG-CPY.
       01  CHECK-KEY                     LIKE CFKEY  OF CFG-CPY.
       01  MAIL-AKTION                   PIC X(20).
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-ABKW                        LIKE ABKW   OF PAZUBER-P.
       01  X-ABJAHR                      LIKE ABJAHR OF PAZUBER-P.
       01  X-ABUSER                      LIKE ABUSER OF PAZUBER-P.
       01  X-AKTION                      PIC X(20).
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING X-ABKW
                                X-ABJAHR
                                X-ABUSER
                                X-AKTION.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP0045" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEI ÖFFNEN
           OPEN     I-O PAZUBER-DP.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* DATENSATZ BEARBEITEN UND MAIL SCHICKEN
           EVALUATE X-AKTION
                    WHEN SPACES
                    WHEN "NO-MAIL"
                         PERFORM  UPDATE-PAZUBER
                         IF   X-AKTION NOT = "NO-MAIL"
                              PERFORM WRITE-MAIL
                         END-IF
                    WHEN "ADD-ERROR"
                         PERFORM  ADD-ERROR-PAZUBER
                    WHEN "RESET-STATUS"
                         PERFORM  RESET-STATUS-PAZUBER
                    WHEN "MAILERROR"
                         PERFORM  ADD-ERROR-PAZUBER
                         IF   X-AKTION = "MAILERROR"
                              PERFORM WRITE-MAIL
                         END-IF
                    WHEN "ER-MAIL"
                         PERFORM  WRITE-MAIL
                    WHEN "PR-MAIL"
                         PERFORM  WRITE-MAIL
                    WHEN "AH-MAIL"
                         PERFORM  WRITE-MAIL
           END-EVALUATE.

       ENDE.
           CLOSE    PAZUBER-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEI UPDATEN
     ‚*--------------------------------------------------------------
       UPDATE-PAZUBER SECTION.
       UPD-AZU-00.

     ‚* ÄNDERN DES STATUS
           INITIALIZE PAZUBERF OF PAZUBER-P
           MOVE     X-ABKW            TO ABKW   OF PAZUBER-P
           MOVE     X-ABJAHR          TO ABJAHR OF PAZUBER-P
           MOVE     X-ABUSER          TO ABUSER OF PAZUBER-P
           READ     PAZUBER-DP
           IF       FILE-STATUS = ZEROES
                    MOVE    "V"      TO ABSTAT OF PAZUBER-P
                    PERFORM COPY-GET-TIME
                    MOVE    T-USER   TO ABMSER OF PAZUBER-P
                    MOVE    T-TERM   TO ABMBS  OF PAZUBER-P
                    MOVE    PGM-WRK  TO ABMPGM OF PAZUBER-P
                    MOVE    DATE-8   TO ABMDTA OF PAZUBER-P
                    MOVE    TIME-6   TO ABMTIA OF PAZUBER-P
                    REWRITE PAZUBER-P
           END-IF.

       UPD-AZU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DATEI UPDATEN
     ‚*--------------------------------------------------------------
       ADD-ERROR-PAZUBER SECTION.
       UPD-AZU-00.

     ‚* FEHLER HINZUFÜGEN
           INITIALIZE PAZUBERF OF PAZUBER-P
           MOVE     X-ABKW            TO ABKW   OF PAZUBER-P
           MOVE     X-ABJAHR          TO ABJAHR OF PAZUBER-P
           MOVE     X-ABUSER          TO ABUSER OF PAZUBER-P
           READ     PAZUBER-DP
           IF       FILE-STATUS = ZEROES
                    ADD     1        TO ABERR  OF PAZUBER-P
                    PERFORM COPY-GET-TIME
                    MOVE    T-USER   TO ABMSER OF PAZUBER-P
                    MOVE    T-TERM   TO ABMBS  OF PAZUBER-P
                    MOVE    PGM-WRK  TO ABMPGM OF PAZUBER-P
                    MOVE    DATE-8   TO ABMDTA OF PAZUBER-P
                    MOVE    TIME-6   TO ABMTIA OF PAZUBER-P
                    REWRITE PAZUBER-P
           END-IF.

       UPD-AZU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* RESET STATUS TO "NOT FINISHED"
     ‚*--------------------------------------------------------------
       RESET-STATUS-PAZUBER SECTION.
       RST-STS-00.

     ‚* RESET STATUS TO "NOT FINISHED" IF CONTROLLER HAS DONE
     ‚* A MISTAKE BY SETTING THE REPORT INTO "FINISHED".
           INITIALIZE PAZUBERF OF PAZUBER-P.
           MOVE     X-ABKW   TO ABKW   OF PAZUBER-P
           MOVE     X-ABJAHR TO ABJAHR OF PAZUBER-P
           MOVE     X-ABUSER TO ABUSER OF PAZUBER-P
           READ     PAZUBER-DP
           IF       FILE-STATUS = ZEROES
                    INITIALIZE ABSTAT OF PAZUBER-P
                    PERFORM COPY-GET-TIME
                    MOVE T-USER  TO ABMSER OF PAZUBER-P
                    MOVE T-TERM  TO ABMBS  OF PAZUBER-P
                    MOVE PGM-WRK TO ABMPGM OF PAZUBER-P
                    MOVE DATE-8  TO ABMDTA OF PAZUBER-P
                    MOVE TIME-6  TO ABMTIA OF PAZUBER-P
                    REWRITE PAZUBER-P
           END-IF.

       RST-STS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* MAIL VORBEREITEN UND SENDEN
     ‚*--------------------------------------------------------------
       WRITE-MAIL SECTION.
       WRT-MAI-00.

           INITIALIZE MAIL-TAB.

     ‚* EMAIL FÜR KORREKTES BERICHTSHEFT
           IF       X-AKTION = SPACES
     ‚*             MAIL-TOPIC AUFRUFEN
                    STRING X-ABUSER                 DELIMITED BY SPACES
                           " - BERICHTSHEFT KW "      DELIMITED BY SIZE
                           X-ABKW                     DELIMITED BY SIZE
                           "/"                        DELIMITED BY SIZE
                           X-ABJAHR                   DELIMITED BY SIZE
                           " UNTERSCHRIEBEN"          DELIMITED BY SIZE
                           INTO MAIL-TOPIC OF MAIL-TAB
                    END-STRING
     ‚*             EMAIL BODY ALLGEMEINE INFOS
                    MOVE 1 TO MAIL-LEN
                    STRING "Hallo,"                   DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "das Berichtsheft ist "    DELIMITED BY SIZE
                           "unterschrieben und kann " DELIMITED BY SIZE
                           "eingeheftet werden."      DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "MfG,"                     DELIMITED BY SIZE
                           X"25"                      DELIMITED BY SIZE
                           T-USER                   DELIMITED BY SPACES
                           INTO MAIL-DATA
                           WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

     ‚* EMAIL FÜR FEHLERHAFTES BERICHTSHEFT
           IF       X-AKTION = "MAILERROR"
     ‚*             MAIL-TOPIC AUFRUFEN
                    STRING X-ABUSER                 DELIMITED BY SPACES
                           " - BERICHTSHEFT KW "      DELIMITED BY SIZE
                           X-ABKW                     DELIMITED BY SIZE
                           "/"                        DELIMITED BY SIZE
                           X-ABJAHR                   DELIMITED BY SIZE
                           " BITTE NACHBESSERN"       DELIMITED BY SIZE
                           INTO MAIL-TOPIC OF MAIL-TAB
                    END-STRING
     ‚*             EMAIL BODY ALLGEMEINE INFOS
                    MOVE 1 TO MAIL-LEN
                    STRING "Hallo,"                   DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "das Berichtsheft kann "   DELIMITED BY SIZE
                           "zum Nachbessern "         DELIMITED BY SIZE
                           "abgeholt werden."         DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "MfG,"                     DELIMITED BY SIZE
                           X"25"                      DELIMITED BY SIZE
                           T-USER                   DELIMITED BY SPACES
                           INTO MAIL-DATA
                           WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

     ‚* ERINNERUNGS-EMAIL
           IF       X-AKTION = "ER-MAIL"
     ‚*             MAIL-TOPIC AUFRUFEN
                    STRING X-ABUSER                 DELIMITED BY SPACES
                           " - ERINNERUNG AN DIE "    DELIMITED BY SIZE
                           "ABGABE DES "              DELIMITED BY SIZE
                           "BERICHTSHEFTES "          DELIMITED BY SIZE
                           X-ABKW                     DELIMITED BY SIZE
                           "/"                        DELIMITED BY SIZE
                           X-ABJAHR                   DELIMITED BY SIZE
                           INTO MAIL-TOPIC OF MAIL-TAB
                    END-STRING
     ‚*             EMAIL BODY ALLGEMEINE INFOS
                    MOVE 1 TO MAIL-LEN
                    STRING "Hallo,"                   DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "bitte reichen Sie das "   DELIMITED BY SIZE
                           "noch nicht unterschrieb"  DELIMITED BY SIZE
                           "ene Berichtsheft "        DELIMITED BY SIZE
                           X-ABKW                     DELIMITED BY SIZE
                           "/"                        DELIMITED BY SIZE
                           X-ABJAHR                   DELIMITED BY SIZE
                           " zur "                    DELIMITED BY SIZE
                           "Kontrolle bei Ihrem Aus"  DELIMITED BY SIZE
                           "bilder ein."              DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "MfG,"                     DELIMITED BY SIZE
                           X"25"                      DELIMITED BY SIZE
                           T-USER                   DELIMITED BY SPACES
                           INTO MAIL-DATA
                           WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

     ‚* AZUBI SOLL PRUEFEN, OB ER DIESE BERICHTSHEFT SCHON ABGEHEFTET
     ‚* HAT
           IF       X-AKTION = "PR-MAIL"
     ‚*             MAIL-TOPIC AUFRUFEN
                    STRING X-ABUSER                 DELIMITED BY SPACES
                           " - BITTE PRUEFEN, OB "    DELIMITED BY SIZE
                           "DAS BERICHTSHEFT "        DELIMITED BY SIZE
                           X-ABKW                     DELIMITED BY SIZE
                           "/"                        DELIMITED BY SIZE
                           X-ABJAHR                   DELIMITED BY SIZE
                           " SCHON ABGEHEFTET IST"    DELIMITED BY SIZE
                           INTO MAIL-TOPIC OF MAIL-TAB
                    END-STRING
     ‚*             EMAIL BODY ALLGEMEINE INFOS
                    MOVE 1 TO MAIL-LEN
                    STRING "Hallo,"                   DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "bitte prüfen Sie, ob "    DELIMITED BY SIZE
                           "das Berichtsheft "        DELIMITED BY SIZE
                           X-ABKW                     DELIMITED BY SIZE
                           "/"                        DELIMITED BY SIZE
                           X-ABJAHR                   DELIMITED BY SIZE
                           " bereits unterschrieben " DELIMITED BY SIZE
                           "und abgeheftet ist."      DELIMITED BY SIZE
                           X"25"                      DELIMITED BY SIZE
                           "Bitte geben Sie mir "     DELIMITED BY SIZE
                           "zeitnah eine Rückmeldung" DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "MfG,"                     DELIMITED BY SIZE
                           X"25"                      DELIMITED BY SIZE
                           T-USER                   DELIMITED BY SPACES
                           INTO MAIL-DATA
                           WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.

     ‚* AZUBI ERHÄLT EMAIL, WEIL ER SEINE KORRIGIERTEN BERICHTSHEFTE
     ‚* SEIT LÄNGEREM NICHT ABGEHOLT HAT.
           IF       X-AKTION = "AH-MAIL"
     ‚*             MAIL-TOPIC AUFRUFEN
                    STRING X-ABUSER                 DELIMITED BY SPACES
                           " - "                      DELIMITED BY SIZE
                           "SIE HABEN NOCH KORRIGIER" DELIMITED BY SIZE
                           "TE AUSBILDUNGSNACHWEISE " DELIMITED BY SIZE
                           "BEI IHREM AUSBILDER "     DELIMITED BY SIZE
                           "LIEGEN."                  DELIMITED BY SIZE
                           INTO MAIL-TOPIC OF MAIL-TAB
                    END-STRING
     ‚*             EMAIL BODY ALLGEMEINE INFOS
                    MOVE 1 TO MAIL-LEN
                    STRING "Hallo,"                   DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "bitte holen Sie Ihre "    DELIMITED BY SIZE
                           "korrigierten Berichts"    DELIMITED BY SIZE
                           "hefte bei Ihrem Ausbild"  DELIMITED BY SIZE
                           "er ab."                   DELIMITED BY SIZE
                           X"2525"                    DELIMITED BY SIZE
                           "MfG,"                     DELIMITED BY SIZE
                           X"25"                      DELIMITED BY SIZE
                           T-USER                   DELIMITED BY SPACES
                           INTO MAIL-DATA
                           WITH POINTER MAIL-LEN
                    END-STRING
           END-IF.
           MOVE     "BERICHTSHEFT" TO SENDER-KEY.
           MOVE     "H051"         TO CHECK-ID.
           MOVE     "BERICHTSHEFT" TO CHECK-KEY.
           MOVE     X-ABUSER       TO MAIL-ADRESSE OF MAIL-EMPF-REC(1).

     ‚* MAILPROGRAMM AUFRUFEN
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
