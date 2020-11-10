       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN DER BERECHTIGUNGEN FÜR PROGRAMM ODER PROGRAMMGRUPPE    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  13.11.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BERECHTIGUNGSPRÜFUNG OHNE AUSFÜHRUNG         *
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
     ‚* PHYSISCHE DATEI KONFIGURATION
     ‚*-------------------------------------------------------------
           SELECT PCFGAUT-DP
                  ASSIGN       TO  DATABASE-PCFGAUT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEXTE
       FD  PCFGAUT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGAUT-P.
           COPY DDS-ALL-FORMATS OF PCFGAUT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9010".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  PGM-ERLAUBT                   LIKE CFFK01 OF PCFGAUT-P.
       01  TYP-ERLAUBT                   LIKE CFFK01 OF PCFGAUT-P.
       01  BEENDEN-MODUS-PGM             LIKE CFFK02 OF PCFGAUT-P.
       01  BEENDEN-MODUS-TYP             LIKE CFFK02 OF PCFGAUT-P.
       01  END-MODE                      LIKE CFFK02 OF PCFGAUT-P.

       01  AUFRUF-LIB                    LIKE ACTLIB-WRK.
       01  AUFRUF-PGM                    LIKE ACTPGM-WRK.
       01  DEPTH-WRK                     LIKE ACT-DEPTH.

       01  DATEI-OPEN                    PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
       01  X-PGM                         LIKE PGM-WRK.
       01  X-TYP                         PIC X(8).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-PGM
                                X-TYP
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9010 " TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     ZEROES TO DATEI-OPEN.
     ‚* IST DER X-USER <> T-USER VERSUCHT VIELLEICHT JEMAND MITTELS
     ‚* DEBUGGER DAS SYSTEM ZU UMGEHEN ...
           IF       X-USER NOT = T-USER
                    PERFORM POSSIBLE-BREAK
           END-IF.

     ‚* WENN DIE DATEI IN DER FALSCHEN LIB LIEGT, DANN AUCH ENDE
           PERFORM  CHECK-DATEI.

           OPEN     I-O   PCFGAUT-DP.
           MOVE     1     TO DATEI-OPEN.

           INITIALIZE X-RETCODE.
           INITIALIZE PGM-ERLAUBT.
           INITIALIZE TYP-ERLAUBT.
           INITIALIZE BEENDEN-MODUS-PGM.
           INITIALIZE BEENDEN-MODUS-TYP.
     ‚* PRÜFEN OB DER USER DAS PGM BENUTZEN DARF
           IF       X-PGM NOT = SPACES
                    PERFORM CHECK-PROGRAMM
           ELSE
                    MOVE ZEROES TO PGM-ERLAUBT
           END-IF.

     ‚* EINIGE PGM HABEN KEINE GRUPPE, DANN DORT DAVON AUSGEHEN
     ‚* DAS MAN DARF
           IF       X-TYP NOT = SPACES
                    PERFORM CHECK-PROGRAMMGRUPPE
           ELSE
                    MOVE 1      TO TYP-ERLAUBT
           END-IF.

     ‚* PRÜFEN OB DER USER DAS PGM BENUTZEN DARF
           EVALUATE PGM-ERLAUBT ALSO TYP-ERLAUBT
     ‚*             WEDER PGM NOCH TYP ERLAUBT => NICHT ERLAUBT
                    WHEN ZEROES ALSO ZEROES
                         MOVE "1" TO X-RETCODE
     ‚*             TYP, ABER NICHT PGM ERLAUBT => NICHT ERLAUBT
                    WHEN ZEROES ALSO 1
                         MOVE "1" TO X-RETCODE
     ‚*             PGM, ABER NICHT TYP ERLAUBT => NICHT ERLAUBT
                    WHEN 1      ALSO ZEROES
                         MOVE "1" TO X-RETCODE
     ‚*             PGM UND TYP ERLAUBT => ERLAUBT
                    WHEN 1      ALSO 1
                         INITIALIZE X-RETCODE
           END-EVALUATE.

       ENDE.
           IF       DATEI-OPEN NOT = ZEROES
                    CLOSE PCFGAUT-DP
           END-IF.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DER USER DAS PGM BENUTZEN DARF
     ‚*--------------------------------------------------------------
       CHECK-PROGRAMM SECTION.
       CHK-PGM-00.

           INITIALIZE PGM-ERLAUBT.
     ‚* PRÜFEN DER BERECHTIGUNGEN FÜR EIN PROGRAMM
           INITIALIZE PCFGAUTF OF PCFGAUT-P.
           MOVE     "CHK-PGM"      TO CFID   OF PCFGAUT-P.
           MOVE     X-PGM          TO CFKEY  OF PCFGAUT-P.
           MOVE     X-USER         TO CFKEY2 OF PCFGAUT-P.
           MOVE     SPACES         TO CFKEY3 OF PCFGAUT-P.
           MOVE     SPACES         TO CFKEY4 OF PCFGAUT-P.
           READ     PCFGAUT-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
     ‚*             JETZT FÜR ALLE USER PRÜFEN
                    INITIALIZE PCFGAUTF OF PCFGAUT-P
                    MOVE "CHK-PGM" TO CFID   OF PCFGAUT-P
                    MOVE X-PGM     TO CFKEY  OF PCFGAUT-P
                    MOVE "*ALL"    TO CFKEY2 OF PCFGAUT-P
                    MOVE SPACES    TO CFKEY3 OF PCFGAUT-P
                    MOVE SPACES    TO CFKEY4 OF PCFGAUT-P
                    READ PCFGAUT-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
     ‚*                  ES GIBT KEINEN EINTRAG? DANN EINEN
     ‚*                  EINTRAG FÜR ALLE USER ANLEGEN
                         PERFORM ANLEGEN-STATUS-PGM
                         MOVE 1 TO PGM-ERLAUBT
                         GO TO CHK-PGM-90
                    END-IF
           END-IF.

     ‚* JETZT PRÜFEN OB DER USER DARF
           MOVE     CFFK01 OF PCFGAUT-P TO PGM-ERLAUBT.
           MOVE     CFFK02 OF PCFGAUT-P TO BEENDEN-MODUS-PGM.

       CHK-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIE PROGRAMMGRUPPE ERLAUBT IST
     ‚*--------------------------------------------------------------
       CHECK-PROGRAMMGRUPPE SECTION.
       CHK-TYP-00.

           INITIALIZE TYP-ERLAUBT.
     ‚* PRÜFEN DER BERECHTIGUNGEN FÜR EIN PROGRAMM
           INITIALIZE PCFGAUTF OF PCFGAUT-P.
           MOVE     "CHK-TYP"      TO CFID   OF PCFGAUT-P.
           MOVE     X-TYP          TO CFKEY  OF PCFGAUT-P.
           MOVE     X-USER         TO CFKEY2 OF PCFGAUT-P.
           MOVE     SPACES         TO CFKEY3 OF PCFGAUT-P.
           MOVE     SPACES         TO CFKEY4 OF PCFGAUT-P.
           READ     PCFGAUT-DP WITH NO LOCK.
           IF       FILE-STATUS NOT = ZEROES
     ‚*             JETZT FÜR ALLE USER PRÜFEN
                    INITIALIZE PCFGAUTF OF PCFGAUT-P
                    MOVE "CHK-TYP" TO CFID   OF PCFGAUT-P
                    MOVE X-TYP     TO CFKEY  OF PCFGAUT-P
                    MOVE "*ALL"    TO CFKEY2 OF PCFGAUT-P
                    MOVE SPACES    TO CFKEY3 OF PCFGAUT-P
                    MOVE SPACES    TO CFKEY4 OF PCFGAUT-P
                    READ PCFGAUT-DP WITH NO LOCK
                    IF   FILE-STATUS NOT = ZEROES
     ‚*                  ES GIBT KEINEN EINTRAG? DANN EINEN
     ‚*                  EINTRAG FÜR ALLE USER ANLEGEN
                         PERFORM ANLEGEN-STATUS-TYP
                         MOVE 1 TO TYP-ERLAUBT
                         GO TO CHK-TYP-90
                    END-IF
           END-IF.

     ‚* JETZT PRÜFEN OB DER USER DARF
           MOVE     CFFK01 OF PCFGAUT-P TO TYP-ERLAUBT.
           MOVE     CFFK02 OF PCFGAUT-P TO BEENDEN-MODUS-TYP.

       CHK-TYP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-DATEI SECTION.
       CHK-DAT-00.

     ‚* PRÜFEN OB OBJEKT IN DER RICHTIGEN LIB VORHANDEN IST
           INITIALIZE RTVOBJD-REC.
           MOVE     "PCFGAUT"    TO OBJ     OF RTVOBJD-REC.
           MOVE     "*LIBL"      TO OBJLIB  OF RTVOBJD-REC.
           MOVE     "*FILE"      TO OBJTYPE OF RTVOBJD-REC.
           CALL     "CLP0006" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE              NOT = SPACES
            OR      RTNLIB OF RTVOBJD-REC NOT = CONST-LIB
     ‚*             DANN NACHRICHT SENDEN
                    INITIALIZE SNDMSG-REC
     ‚*             PGM SOWIE DOPPELPUNKT
                    MOVE    PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10)
                    MOVE    ":"          TO MSGTXT OF SNDMSG-REC(11:1)
     ‚*             USER EINTRAGEN
                    MOVE    "PCFGAUT"    TO MSGTXT OF SNDMSG-REC(13:10)
     ‚*             USER AN DEM DIE NACHRICHT GEHT
                    MOVE    "APG"        TO USER OF SNDMSG-REC
     ‚*             SENDEN DER NACHRICHT
                    CALL    "CLP0010" USING SNDMSG-REC
                                            RET-CODE
                    END-CALL
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY "APG"
                    END-CALL
           END-IF.

       CHK-TED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN DES STATUS FÜR DEN TYP
     ‚*--------------------------------------------------------------
       ANLEGEN-STATUS-PGM SECTION.
       ANL-STP-00.

     ‚* SCHREIBEN DES DATENSATZES
           INITIALIZE PCFGAUTF OF PCFGAUT-P.
           MOVE     "CHK-PGM"              TO CFID   OF PCFGAUT-P.
           MOVE     X-PGM                  TO CFKEY  OF PCFGAUT-P.
           MOVE     "*ALL"                 TO CFKEY2 OF PCFGAUT-P.
           MOVE     SPACES                 TO CFKEY3 OF PCFGAUT-P.
           MOVE     SPACES                 TO CFKEY4 OF PCFGAUT-P.
     ‚* WIR ERLAUBEN ERSTMAL
           MOVE     1                      TO CFFK01 OF PCFGAUT-P.
           MOVE     1                      TO CFFK02 OF PCFGAUT-P.

           MOVE     T-USER                 TO CFAUSR OF PCFGAUT-P.
           MOVE     T-TERM                 TO CFABS  OF PCFGAUT-P.
           MOVE     PGM-WRK                TO CFAPGM OF PCFGAUT-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8                 TO CFADTA OF PCFGAUT-P.
           MOVE     TIME-6                 TO CFATIA OF PCFGAUT-P.

           WRITE    PCFGAUT-P.

       ANL-STP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANLEGEN DES STATUS FÜR DEN TYP
     ‚*--------------------------------------------------------------
       ANLEGEN-STATUS-TYP SECTION.
       ANL-STT-00.

     ‚* SCHREIBEN DES DATENSATZES
           INITIALIZE PCFGAUTF OF PCFGAUT-P.
           MOVE     "CHK-TYP"              TO CFID   OF PCFGAUT-P.
           MOVE     X-TYP                  TO CFKEY  OF PCFGAUT-P.
           MOVE     "*ALL"                 TO CFKEY2 OF PCFGAUT-P.
           MOVE     SPACES                 TO CFKEY3 OF PCFGAUT-P.
           MOVE     SPACES                 TO CFKEY4 OF PCFGAUT-P.
     ‚* WIR ERLAUBEN ERSTMAL
           MOVE     1                      TO CFFK01 OF PCFGAUT-P.
           MOVE     1                      TO CFFK02 OF PCFGAUT-P.

           MOVE     T-USER                 TO CFAUSR OF PCFGAUT-P.
           MOVE     T-TERM                 TO CFABS  OF PCFGAUT-P.
           MOVE     PGM-WRK                TO CFAPGM OF PCFGAUT-P.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.
           MOVE     DATE-8                 TO CFADTA OF PCFGAUT-P.
           MOVE     TIME-6                 TO CFATIA OF PCFGAUT-P.

           WRITE    PCFGAUT-P.

       ANL-STT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* VERSUCHT JEMAND DAS SYSTEM ZU UMGEHEN?
     ‚*--------------------------------------------------------------
       POSSIBLE-BREAK SECTION.
       POS-BRK-00.

           INITIALIZE RET-CODE.
     ‚* SENDEN DER NACHRICHT
           INITIALIZE SNDMSG-REC.
     ‚* PGM SOWIE DOPPELPUNKT
           MOVE     PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10).
           MOVE     ":"          TO MSGTXT OF SNDMSG-REC(11:1).
     ‚* USER EINTRAGEN
           MOVE     T-USER       TO MSGTXT OF SNDMSG-REC(13:10).
           MOVE     "/"          TO MSGTXT OF SNDMSG-REC(24:1).
           MOVE     X-USER       TO MSGTXT OF SNDMSG-REC(26:10).
     ‚* USER AN DEM DIE NACHRICHT GEHT
           MOVE     "APG"        TO USER OF SNDMSG-REC.

           CALL     "CLP0010" USING SNDMSG-REC
                                    RET-CODE
           END-CALL.

     ‚* JETZT ABBRECHEN
           CALL     "CFP9107" IN LIBRARY "APG"
           END-CALL.

       POS-BRK-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-GET-TIME: DATUM UND UHRZEIT ERMITTELN
     ‚*---------------------------------------------------------------

           COPY     GETTIME    OF APG-QCPYSRC.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
