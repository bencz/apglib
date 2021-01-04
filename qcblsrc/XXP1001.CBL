       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              XXP1001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* ANMELDUNG IN AMU/KINO                                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  03.06.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  KINOANMELDUNG AUSFÜHREN                      *
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
     ‚* PHYSISCHE DATEI FILME
     ‚*-------------------------------------------------------------
           SELECT PMOVIE-DP
                  ASSIGN       TO  DATABASE-PMOVIE
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI FILM-ANMELDUNGEN
     ‚*-------------------------------------------------------------
           SELECT PANMELD-DP
                  ASSIGN       TO  DATABASE-PANMELD
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI KONFIGURATION
     ‚*-------------------------------------------------------------
           SELECT PCONFIG-DP
                  ASSIGN       TO  DATABASE-PCONFIG
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- FILME
       FD  PMOVIE-DP
           LABEL RECORDS ARE STANDARD.
       01  PMOVIE-P.
           COPY DDS-ALL-FORMATS OF AMU-PMOVIE.
      /
     ‚*--- ANMELDUNGEN
       FD  PANMELD-DP
           LABEL RECORDS ARE STANDARD.
       01  PANMELD-P.
           COPY DDS-ALL-FORMATS OF AMU-PANMELD.
      /
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF APG-PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER FÜR PROGRAMMTEXTE
           COPY WRKTXT     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH LOG-CPY
           COPY WRKLOGCPY  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "XXP1001".
       01  PGM-TYP                       PIC X(8)  VALUE "ANMELDEN".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  SNDMSG-REC                    TYPE SNDMSG-TYPE.
       01  ADDLIBLE-REC                  TYPE ADDLIBLE-TYPE.
       01  RMVLIBLE-REC                  TYPE RMVLIBLE-TYPE.

       01  ANMELDE-USER                  LIKE T-USER.
       01  BENACHRICHTIGEN-USER          LIKE CFDAT1 OF PCONFIG-P.
       01  ANMELDE-TYP                   LIKE CFFK02 OF PCONFIG-P.
       01  MESSAGE-WRK                   LIKE CFTXT2 OF PCONFIG-P.
       01  LEN                           PIC 9(4).
       01  POS                           PIC 9(4).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "XXP1001" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* AMU-BIBLIOTHEK VORSCHALTEN
           PERFORM  ADD-AMU-LIB.

     ‚* PRÜFEN OB DIE AMU-BIBLIOTHEK GESETZT UND DIE DATEI
     ‚* VORHANDEN IST
           PERFORM  CHECK-OBJECTS.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÖFFNEN DER DATEIEN
           OPEN     INPUT PMOVIE-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGSTSF OF LOG-CPY
                    MOVE ":EXECUTE"             TO STFILE OF LOG-CPY
                    MOVE "ERROR"                TO STKEY  OF LOG-CPY
                    MOVE "ERR0009"              TO STTID  OF LOG-CPY
                    MOVE "Open failed: PMOVIE"  TO STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.
           OPEN     I-O   PANMELD-DP.
           IF       FILE-STATUS NOT = ZEROES
                    INITIALIZE PCFGSTSF OF LOG-CPY
                    MOVE ":EXECUTE"             TO STFILE OF LOG-CPY
                    MOVE "ERROR"                TO STKEY  OF LOG-CPY
                    MOVE "ERR0009"              TO STTID  OF LOG-CPY
                    MOVE "Open failed: PANMELD" TO STSTS  OF LOG-CPY
                    PERFORM COPY-WRITE-LOG-CPY
                    GO TO ENDE
           END-IF.
           OPEN     INPUT PCONFIG-DP.

     ‚* LESEN DER OFFENEN FILME UND HINZUFÜGEN EINES EINTRAGS
           PERFORM  LESE-DATEN.

     ‚* AMU-BIBLIOTHEK LÖSCHEN
           PERFORM  DEL-AMU-LIB.

     ‚* DATEIEN SCHLIEßEN
           CLOSE    PANMELD-DP
                    PCONFIG-DP
                    PMOVIE-DP.

       ENDE.
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEIEN AUCH GIBT
     ‚*--------------------------------------------------------------
       CHECK-OBJECTS SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "PMOVIE"     TO OBJ     OF CHKOBJ-REC.
           MOVE     "*LIBL"      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "PANMELD"    TO OBJ     OF CHKOBJ-REC.
           MOVE     "*LIBL"      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "2" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

       CHK-OBJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER OFFENEN FILME
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* PMOVIE LESEN
           INITIALIZE PMOVIEF OF PMOVIE-P.
           MOVE     ZEROES              TO MOLFDN OF PMOVIE-P.
           START    PMOVIE-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PMOVIE-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

     ‚* NUR NICHT ABGELAUFENE FILME
           IF       MODAT  OF PMOVIE-P < DATE-8
                    GO TO LES-DAT-20
           END-IF.

     ‚* NUR AKTIVE FILME LESEN
           IF       MOAKTI OF PMOVIE-P = ZEROES
                    GO TO LES-DAT-20
           END-IF.

     ‚* PRÜFEN OB ALLE USER, DIE GERNE ANGEMELDET WERDEN MÖCHTEN
     ‚* AUCH SCHON ANGEMELDET SIND UND WENN NICHT, DANN DIESE
     ‚* JETZT ANMELDEN
           PERFORM  ANMELDEN-USER.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ANMELDEN DES USER
     ‚*--------------------------------------------------------------
       ANMELDEN-USER SECTION.
       ANM-USR-00.

           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "K100"    TO CFID   OF PCONFIG-P.
           MOVE     SPACES    TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES    TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES    TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES    TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ANM-USR-90
           END-IF.

       ANM-USR-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ANM-USR-90
           END-IF.

     ‚* PASST DER KEY
           IF       CFID   OF PCONFIG-P   NOT = "K100"
                    GO TO ANM-USR-90
           END-IF.

     ‚* USER GEFÜLLT?
           IF       CFKEY  OF PCONFIG-P(1:10) = SPACES
                    GO TO ANM-USR-20
           END-IF.

     ‚* ANMELDUNG AKTIV?
           IF       CFFK01 OF PCONFIG-P       = ZEROES
                    GO TO ANM-USR-20
           END-IF.

     ‚* WORK-VARIABLEN SICHERN
           MOVE     CFKEY  OF PCONFIG-P(1:10) TO ANMELDE-USER.
           MOVE     CFFK02 OF PCONFIG-P       TO ANMELDE-TYP.
           MOVE     CFDAT1 OF PCONFIG-P       TO BENACHRICHTIGEN-USER.
           MOVE     CFTXT2 OF PCONFIG-P       TO MESSAGE-WRK.

     ‚* ANMELDUNG PRÜFEN UND GGF. ANMELDEN
           PERFORM  CHECK-ANMELDUNG.
           IF       RET-CODE = SPACES
     ‚*             KINO ANMELDUNG DURCHFÜHREN
                    PERFORM  KINO-ANMELDEN
     ‚*             NACHRICHT AN DEN USER SCHICKEN
                    PERFORM  SENDEN-NACHRICHT
     ‚*             SOLL JEMAND INFORMIERT WERDEN? HIER IST
     ‚*             ES ZU 99% AMU ;-)
                    IF   BENACHRICHTIGEN-USER NOT = SPACES
                         PERFORM  SENDEN-NACHRICHT-AMU
                    END-IF
           END-IF.

           GO TO    ANM-USR-20.

       ANM-USR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN DER ANMELDUNG
     ‚*--------------------------------------------------------------
       CHECK-ANMELDUNG SECTION.
       CHK-ANM-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB USER ZUM FILM SCHON ANGEMELDET
           INITIALIZE PANMELDF OF PANMELD-P.
           MOVE     MOLFDN OF PMOVIE-P  TO ANZUSL OF PANMELD-P.
           MOVE     ANMELDE-USER        TO ANAUSR OF PANMELD-P.
           READ     PANMELD-DP WITH NO LOCK.
           IF       FILE-STATUS = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-ANM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SCHREIBEN DES DATENSATZES
     ‚*--------------------------------------------------------------
       KINO-ANMELDEN SECTION.
       KIN-ANM-00.

     ‚* DATUM/UHRZEIT
           PERFORM  COPY-GET-TIME.

     ‚* DATENSATZ SCHREIBEN
           INITIALIZE PANMELDF OF PANMELD-P.
           MOVE     MOLFDN OF PMOVIE-P TO ANZUSL OF PANMELD-P.
           MOVE     ANMELDE-USER       TO ANAUSR OF PANMELD-P.
     ‚* ANMELDUNG IST DEM SCHLÜSSEL HINTERLEGT, IMMER FÜR EINE PERSON
     ‚*  0 = ABMELDEN
     ‚*  1 = ANMELDEN
     ‚*  2 = UNTER VORBEHALT
           MOVE     ANMELDE-TYP        TO ANSTS  OF PANMELD-P.
           MOVE     1                  TO ANANZ  OF PANMELD-P.
     ‚* KOMMENTAR FÜLLEN "ANGEMELDET DURCH ANMELDESRKIPT"
           IF       MESSAGE-WRK = SPACES
                    MOVE     "PGM1000"     TO CPY-TXID
                    MOVE     SPRACHE-WRK   TO CPY-TXSPR
                    PERFORM  COPY-PGM-TXT
                    MOVE     CPY-TXTEXT    TO ANKOMM OF PANMELD-P
           ELSE
                    MOVE     MESSAGE-WRK   TO ANKOMM OF PANMELD-P
           END-IF.
     ‚* USER/TERMINAL/ETC
           MOVE     PGM-WRK            TO ANPGM  OF PANMELD-P.
           MOVE     T-USER             TO ANUSER OF PANMELD-P.
           MOVE     T-TERM             TO ANBS   OF PANMELD-P.
     ‚* DATU/UHRZEIT
           MOVE     JAHRHUNDERT        TO ANMJH  OF PANMELD-P.
           MOVE     JAHR    OF DATUM   TO ANMJA  OF PANMELD-P.
           MOVE     MONAT   OF DATUM   TO ANMMO  OF PANMELD-P.
           MOVE     TAG     OF DATUM   TO ANMTA  OF PANMELD-P.
           MOVE     STUNDE  OF UHRZEIT TO ANMST  OF PANMELD-P.
           MOVE     MINUTE  OF UHRZEIT TO ANMMI  OF PANMELD-P.
           MOVE     SEKUNDE OF UHRZEIT TO ANMSE  OF PANMELD-P.
           WRITE    PANMELD-P.

       KIN-ANM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER NACHRICHT, DASS ZUM FILM ANGEMELDET WURDE
     ‚*--------------------------------------------------------------
       SENDEN-NACHRICHT SECTION.
       SND-NAC-00.

           INITIALIZE RET-CODE, LEN.
     ‚* ERMITTELN DER LÄNGE DES FILMNAMEN
           MOVE     LENGTH OF MOVIE  OF PMOVIE-P TO LEN.
           PERFORM  UNTIL LEN = ZEROES
                     OR MOVIE OF PMOVIE-P(LEN:1) NOT = SPACES
                    SUBTRACT 1 FROM LEN
           END-PERFORM.

     ‚* SENDEN DER NACHRICHT
           INITIALIZE SNDMSG-REC.
     ‚* PGM SOWIE DOPPELPUNKT
           MOVE     PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10).
           MOVE     ":"          TO MSGTXT OF SNDMSG-REC(11:1).
     ‚* FILMNAME
           MOVE     MOVIE  OF PMOVIE-P(1:LEN) TO
                    MSGTXT OF SNDMSG-REC(13:LEN).
           MOVE     ","          TO MSGTXT OF SNDMSG-REC(13 + LEN:1).
     ‚* DATUM
           MOVE     "TO-DSP"           TO DATUM-CONVERT.
           MOVE     MODAT  OF PMOVIE-P TO DATE-DB.
           PERFORM  COPY-CONVERT-DATE.
           MOVE     DATUM-ALPHA-10     TO
                    MSGTXT OF SNDMSG-REC(13 + LEN + 2:).
     ‚* USER AN DEN DIE NACHRICHT GEHT
           MOVE     ANMELDE-USER TO USER OF SNDMSG-REC.

           CALL     "CLP0010" USING SNDMSG-REC
                                    RET-CODE
           END-CALL.

       SND-NAC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER NACHRICHT DAS ANGEMELDET WURDE
     ‚*--------------------------------------------------------------
       SENDEN-NACHRICHT-AMU SECTION.
       SND-NAM-00.

           INITIALIZE RET-CODE, LEN, POS.
     ‚* ERMITTELN DER LÄNGE DES FILMNAMEN
           MOVE     LENGTH OF MOVIE  OF PMOVIE-P TO LEN.
           PERFORM  UNTIL LEN = ZEROES
                     OR MOVIE OF PMOVIE-P(LEN:1) NOT = SPACES
                    SUBTRACT 1 FROM LEN
           END-PERFORM.

     ‚* SENDEN DER NACHRICHT
           INITIALIZE SNDMSG-REC.
     ‚* PGM SOWIE DOPPELPUNKT
           MOVE     1                  TO POS.
           MOVE     PGM-WRK            TO MSGTXT OF SNDMSG-REC(POS:10).
           ADD      10                 TO POS.
           MOVE     ":"                TO MSGTXT OF SNDMSG-REC(POS:1).
           ADD      2                  TO POS.

     ‚* FILMNAME
           MOVE     MOVIE  OF PMOVIE-P(1:LEN) TO
                    MSGTXT OF SNDMSG-REC(POS:LEN).
           ADD      LEN                TO POS.
           MOVE     ","                TO MSGTXT OF SNDMSG-REC(POS:1).
           ADD      2                  TO POS.

     ‚* DATUM
           MOVE     "TO-DSP"           TO DATUM-CONVERT.
           MOVE     MODAT  OF PMOVIE-P TO DATE-DB.
           PERFORM  COPY-CONVERT-DATE.
           MOVE     DATUM-ALPHA-10     TO MSGTXT OF SNDMSG-REC(POS:10).
           ADD      11                 TO POS.

     ‚* USER ANHÄNGEN DER SICH ANGEMELDET HAT
           MOVE     "=> "              TO MSGTXT OF SNDMSG-REC(POS:3).
           ADD      3                  TO POS.
           MOVE     ANMELDE-USER       TO MSGTXT OF SNDMSG-REC(POS:10).
           ADD      10                 TO POS.

     ‚* USER AN DEN DIE NACHRICHT GEHT
           MOVE     BENACHRICHTIGEN-USER        TO USER OF SNDMSG-REC.

           CALL     "CLP0010" USING SNDMSG-REC
                                    RET-CODE
           END-CALL.

       SND-NAM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AMU-BIBLIOTHEK VORSCHALTEN
     ‚*--------------------------------------------------------------
       ADD-AMU-LIB SECTION.
       ADD-AMU-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE ADDLIBLE-REC.
           MOVE     "AMU"        TO LIB     OF ADDLIBLE-REC.
           MOVE     "*LAST"      TO LIBPOS  OF ADDLIBLE-REC.
           CALL     "CLP0007" USING ADDLIBLE-REC
                                    RET-CODE
           END-CALL.

       ADD-AMU-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AMU-BIBLIOTHEK ENTFERNEN
     ‚*--------------------------------------------------------------
       DEL-AMU-LIB SECTION.
       DEL-AMU-00.

     ‚* "AMU" BIBLIOTHEK HINZUFÜGEN
           INITIALIZE RMVLIBLE-REC.
           MOVE     "AMU"        TO LIB     OF RMVLIBLE-REC.
           CALL     "CLP0009" USING RMVLIBLE-REC
           END-CALL.

       DEL-AMU-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-WRITE-LOG-CPY: SCHREIBEN EINES LOG-SATZES
     ‚*---------------------------------------------------------------

           COPY     LOGCPY     OF APG-QCPYSRC.
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
     ‚* COPY-PGM-TXT: PROGRAMMTEXTE LESEN
     ‚*---------------------------------------------------------------

           COPY     PGMTXT     OF APG-QCPYSRC.
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
