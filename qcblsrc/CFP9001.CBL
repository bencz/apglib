       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9001.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN DER BERECHTIGUNGEN FÜR PROGRAMM ODER PROGRAMMGRUPPE    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  BERECHTIGUNGSPRÜFUNG                         *
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
     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI GÜLTIGE PGM-TYPEN
     ‚*-------------------------------------------------------------
           SELECT PCFGPGM-DP
                  ASSIGN       TO  DATABASE-PCFGPGM
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI GÜLTIGE PGM-TYPEN/PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PCFGPGT-DP
                  ASSIGN       TO  DATABASE-PCFGPGT
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- BERECHTIGUNGEN
       FD  PCFGAUT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGAUT-P.
           COPY DDS-ALL-FORMATS OF PCFGAUT.
      /
     ‚*--- GÜLTIGE PGM-TYPEN
       FD  PCFGPGM-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGPGM-P.
           COPY DDS-ALL-FORMATS OF PCFGPGM.
      /
     ‚*--- GÜLTIGE PGM-TYPEN/PROGRAMME
       FD  PCFGPGT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGPGT-P.
           COPY DDS-ALL-FORMATS OF PCFGPGT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

     ‚* WORKBEREICH LOG-CPY
           COPY WRKLOGCPY   OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9001".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  SNDMSG-REC                    TYPE SNDMSG-TYPE.
       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.

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
           MOVE     "CFP9001 " TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     ZEROES TO DATEI-OPEN.

     ‚* IST DER X-USER <> T-USER VERSUCHT VIELLEICHT JEMAND MITTELS
     ‚* DEBUGGER DAS SYSTEM ZU UMGEHEN ...
           IF       X-USER NOT = T-USER
                    PERFORM POSSIBLE-BREAK
           END-IF.

     ‚* WENN X-PGM NICHT GLEICH DEM RICHTIGEN PROGRAMM LAUT AUFRUF-
     ‚* STAPEL IST, DANN HAT DERJENIGE VERSUCHT DAS PROGRAMM IM
     ‚* DEBUGGER ZU ÄNDERN. JETZT ALSO DEN AUFRUFSTAPEL ERMITTELN UND
     ‚* SO DAS PROGRAMM KRIEGEN
           PERFORM  CHECK-PGM-AUFRUFSTAPEL.

     ‚* WENN DIE DATEI IN DER FALSCHEN LIB LIEGT, DANN AUCH ENDE
           PERFORM  CHECK-DATEI.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           OPEN     I-O   PCFGAUT-DP.
           OPEN     INPUT PCFGPGM-DP.
           OPEN     INPUT PCFGPGT-DP.
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
     ‚* DAS MAN NICHT DARF
           IF       X-TYP NOT = SPACES
                    PERFORM CHECK-PROGRAMMGRUPPE
           ELSE
                    MOVE ZEROES TO TYP-ERLAUBT
           END-IF.

     ‚* JETZT WIRD GEPRÜFT, OB DER NAME MIT DEM TYP ÜBEREINSTIMMT
           PERFORM  CHECK-PROGRAMMTYP.
           IF       RET-CODE NOT = SPACES
                    MOVE ZEROES TO PGM-ERLAUBT
                    MOVE ZEROES TO TYP-ERLAUBT
     ‚*             FESTER ABBRUCHMODUS
                    MOVE 3 TO BEENDEN-MODUS-PGM
                    MOVE 3 TO BEENDEN-MODUS-TYP
           END-IF.

     ‚* PRÜFEN OB DER USER DAS PGM BENUTZEN DARF
           EVALUATE PGM-ERLAUBT ALSO TYP-ERLAUBT
     ‚*             WEDER PGM NOCH TYP ERLAUBT      => NICHT ERLAUBT
                    WHEN ZEROES ALSO ZEROES
                         MOVE "1" TO X-RETCODE
     ‚*             TYP, ABER NICHT PGM ERLAUBT     => NICHT ERLAUBT
                    WHEN ZEROES ALSO 1
                         MOVE "1" TO X-RETCODE
     ‚*             PGM, ABER NICHT TYP ERLAUBT     => NICHT ERLAUBT
                    WHEN 1      ALSO ZEROES
                         MOVE "1" TO X-RETCODE
     ‚*             PGM UND TYP ERLAUBT             => ERLAUBT
                    WHEN 1      ALSO 1
                         INITIALIZE X-RETCODE
           END-EVALUATE.

     ‚* SOLL DIES AUF EINE BESONDERE WEISE QUITTIERT WERDEN?
     ‚*  1 = SIMPLES WINDOW
     ‚*  2 = "BLUESCREEN"-IMITAT
     ‚*  3 = "BLUESCREEN"-IMITAT + ABBRUCH
     ‚*  4 = "GO TO ENDE" IM COPY-MODUL
     ‚*  5 = "GOBACK" IM COPY-MODUL
     ‚*  7 = ABBRUCH
     ‚*  9 = SIGNOFF
           IF       (    BEENDEN-MODUS-PGM NOT = ZEROES
                     OR  BEENDEN-MODUS-TYP NOT = ZEROES)
            AND     X-RETCODE NOT = SPACES
     ‚*             DER HÖHERE WERT SCHLÄGT DEN KLEINEREN
                    IF   BEENDEN-MODUS-PGM > BEENDEN-MODUS-TYP
                         MOVE BEENDEN-MODUS-PGM TO END-MODE
                    ELSE
                         MOVE BEENDEN-MODUS-TYP TO END-MODE
                    END-IF
     ‚*             ENDE-PROGRAMM AUFRUFEN
                    CALL "CFP9100" IN LIBRARY CONST-LIB
                                   USING X-PGM
                                         X-TYP
                                         END-MODE
                    END-CALL
     ‚*             4 UND 5 WERDEN HIER GEHANDHABT
                    IF   END-MODE = 4
                         MOVE "4" TO X-RETCODE
                    END-IF
                    IF   END-MODE = 5
                         MOVE "5" TO X-RETCODE
                    END-IF
           END-IF.

       ENDE.
           IF       DATEI-OPEN NOT = ZEROES
                    CLOSE PCFGAUT-DP
                    CLOSE PCFGPGM-DP
                    CLOSE PCFGPGT-DP
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
           CALL     "CLP0006" IN LIBRARY CONST-LIB
                              USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE              NOT = SPACES
            OR      RTNLIB OF RTVOBJD-REC NOT = CONST-LIB
     ‚*             DANN NACHRICHT SENDEN
                    INITIALIZE SNDMSG-REC
     ‚*             PGM SOWIE DOPPELPUNKT
                    MOVE    PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10)
                    MOVE    ":"          TO MSGTXT OF SNDMSG-REC(11:1)
                    MOVE    "WRONG FILE" TO MSGTXT OF SNDMSG-REC(13:10)
     ‚*             USER EINTRAGEN
                    MOVE    "PCFGAUT"    TO MSGTXT OF SNDMSG-REC(24:10)
                    PERFORM WRITE-TO-LOG
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY CONST-LIB
                    END-CALL
           END-IF.

     ‚* PRÜFEN OB OBJEKT IN DER RICHTIGEN LIB VORHANDEN IST
           INITIALIZE RTVOBJD-REC.
           MOVE     "PCFGPGM"    TO OBJ     OF RTVOBJD-REC.
           MOVE     "*LIBL"      TO OBJLIB  OF RTVOBJD-REC.
           MOVE     "*FILE"      TO OBJTYPE OF RTVOBJD-REC.
           CALL     "CLP0006" IN LIBRARY CONST-LIB
                              USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE              NOT = SPACES
            OR      RTNLIB OF RTVOBJD-REC NOT = CONST-LIB
     ‚*             DANN NACHRICHT SENDEN
                    INITIALIZE SNDMSG-REC
     ‚*             PGM SOWIE DOPPELPUNKT
                    MOVE    PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10)
                    MOVE    ":"          TO MSGTXT OF SNDMSG-REC(11:1)
                    MOVE    "WRONG FILE" TO MSGTXT OF SNDMSG-REC(13:10)
     ‚*             USER EINTRAGEN
                    MOVE    "PCFGPGM"    TO MSGTXT OF SNDMSG-REC(24:10)
                    PERFORM WRITE-TO-LOG
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY CONST-LIB
                    END-CALL
           END-IF.

     ‚* PRÜFEN OB OBJEKT IN DER RICHTIGEN LIB VORHANDEN IST
           INITIALIZE RTVOBJD-REC.
           MOVE     "PCFGPGT"    TO OBJ     OF RTVOBJD-REC.
           MOVE     "*LIBL"      TO OBJLIB  OF RTVOBJD-REC.
           MOVE     "*FILE"      TO OBJTYPE OF RTVOBJD-REC.
           CALL     "CLP0006" IN LIBRARY CONST-LIB
                              USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE              NOT = SPACES
            OR      RTNLIB OF RTVOBJD-REC NOT = CONST-LIB
     ‚*             DANN NACHRICHT SENDEN
                    INITIALIZE SNDMSG-REC
     ‚*             PGM SOWIE DOPPELPUNKT
                    MOVE    PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10)
                    MOVE    ":"          TO MSGTXT OF SNDMSG-REC(11:1)
                    MOVE    "WRONG FILE" TO MSGTXT OF SNDMSG-REC(13:10)
     ‚*             USER EINTRAGEN
                    MOVE    "PCFGPGT"    TO MSGTXT OF SNDMSG-REC(24:10)
                    PERFORM WRITE-TO-LOG
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY CONST-LIB
                    END-CALL
           END-IF.

     ‚* ALLES IN ORDNUNG
           INITIALIZE RET-CODE.

       CHK-TED-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DES PROGRAMMS ANHAND DES AUFRUFSTAPELS
     ‚*--------------------------------------------------------------
       CHECK-PGM-AUFRUFSTAPEL SECTION.
       CHK-PGA-00.

           INITIALIZE RET-CODE.
     ‚* ERMITTELN DES AUFRUFSTAPELS
           MOVE     SPACES TO AUFRUF-LIB, AUFRUF-PGM.
           MOVE     1      TO DEPTH-WRK.
           CALL     "CFP9007" IN LIBRARY CONST-LIB
                              USING PGM-WRK
                                    DEPTH-WRK
                                    AUFRUF-LIB
                                    AUFRUF-PGM
           END-CALL.
     ‚* SIND WIR IN DER RICHTIGEN LIB, ABER DAS FALSCHE PGM?
           IF       AUFRUF-PGM NOT = X-PGM
            AND     AUFRUF-LIB     = CONST-LIB
                    INITIALIZE SNDMSG-REC
                    STRING   PGM-WRK         DELIMITED BY SIZE
                             ":"             DELIMITED BY SIZE
                             "WRONG PGM: "   DELIMITED BY SIZE
                             AUFRUF-PGM      DELIMITED BY SPACES
                             "/"             DELIMITED BY SIZE
                             X-PGM           DELIMITED BY SPACES
                    INTO     SNDMSG-REC
                    PERFORM WRITE-TO-LOG
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY CONST-LIB
                    END-CALL
           END-IF.

     ‚* RICHTIGES PGM, ABER FALSCHE LIB?
           IF       AUFRUF-PGM     = X-PGM
            AND     AUFRUF-LIB NOT = CONST-LIB
                    INITIALIZE SNDMSG-REC
                    STRING   PGM-WRK         DELIMITED BY SIZE
                             ":"             DELIMITED BY SIZE
                             "WRONG LIB: "   DELIMITED BY SIZE
                             AUFRUF-LIB      DELIMITED BY SPACES
                             "/"             DELIMITED BY SIZE
                             CONST-LIB       DELIMITED BY SPACES
                    INTO     SNDMSG-REC
                    PERFORM WRITE-TO-LOG
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY CONST-LIB
                    END-CALL
           END-IF.


       CHK-PGA-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DIES EINE GÜLTIGE PROGRAMMGRUPPE IST
     ‚*--------------------------------------------------------------
       CHECK-PROGRAMMTYP SECTION.
       CHK-PTP-00.

           INITIALIZE RET-CODE.
     ‚* GIBT ES DEN EINTRAG?
           INITIALIZE PCFGPGMF OF PCFGPGM-P.
           MOVE     X-TYP          TO VTTYP  OF PCFGPGM-P.
           MOVE     X-PGM(1:2)     TO VTKEY  OF PCFGPGM-P.
           READ     PCFGPGM-DP.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

     ‚* FIXE ZUORDNUNG ZU EINEM TYP? MUSS DANN ÜBEREINSTIMMEN!
           INITIALIZE PCFGPGTF OF PCFGPGT-P.
           MOVE     X-PGM          TO FPPGM  OF PCFGPGT-P.
           READ     PCFGPGT-DP.
           IF       FILE-STATUS             = ZEROES
            AND     FPTYP  OF PCFGPGT-P NOT = X-TYP
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-PTP-90.
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
           STRING   PGM-WRK         DELIMITED BY SIZE
                    ":"             DELIMITED BY SIZE
                    "WRONG USER: "  DELIMITED BY SIZE
                    T-USER          DELIMITED BY SPACES
                    " / "           DELIMITED BY SIZE
                    X-USER          DELIMITED BY SIZE
           INTO     SNDMSG-REC.

     ‚* PROTOKOLLIEREN
           PERFORM  WRITE-TO-LOG.

     ‚* JETZT ABBRECHEN
           CALL     "CFP9107" IN LIBRARY CONST-LIB
           END-CALL.

       POS-BRK-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LOGGEN DER MELDUNG
     ‚*--------------------------------------------------------------
       WRITE-TO-LOG SECTION.
       WRT-TOL-00.

     ‚* USER AN DEM DIE NACHRICHT GEHT
           MOVE     "APG"        TO USER OF SNDMSG-REC.
           CALL     "CLP0010" USING SNDMSG-REC
                                    RET-CODE
           END-CALL.

     ‚* PROTOKOLLIEREN
           INITIALIZE PCFGSTSF OF LOG-CPY.
           MOVE     ":ACCESS"  TO STFILE OF LOG-CPY.
           MOVE     "ERROR"    TO STKEY  OF LOG-CPY.
           MOVE     "ERR0008"  TO STTID  OF LOG-CPY.
           MOVE     SNDMSG-REC TO STSTS  OF LOG-CPY.
           PERFORM  COPY-WRITE-LOG-CPY.

       WRT-TOL-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-WRITE-LOG-CPY: SCHREIBEN EINES LOG-SATZES
     ‚*---------------------------------------------------------------

           COPY     LOGCPY     OF APG-QCPYSRC.
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
