     ˆ* BAUSTELLE 50%
     ‚*PROCESS  XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             CFP0011.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* BEFEHL PARSEN UND IN MAPDTA SPEICHERN                         *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.07.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PARSEN EINES BEFEHLS                         *
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
     ‚* COMMAND-AUFBAU
     ‚*-------------------------------------------------------------
           SELECT PCFGCMD-DP
                  ASSIGN       TO  DATABASE-PCFGCMD
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
     ‚*
       DATA DIVISION.
       FILE SECTION.
     ‚*--- COMMAND-AUFBAU
       FD  PCFGCMD-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGCMD-P.
           COPY DDS-ALL-FORMATS OF PCFGCMD.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP0011".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  MAPDTA-REC                    TYPE MAPDTA-TYPE.
       01  PTRMAP-REC                    TYPE PTRMAP-TYPE.

       01  CMD-POS                       PIC 9(4).
       01  CMD-LEN                       LIKE CMD-POS.
       01  CMD-START                     LIKE CMD-POS.

       01  COMMAND-WRK                   LIKE CMCMD  OF PCFGCMD-P.
       01  PARAM-WRK                     LIKE CMPARM OF PCFGCMD-P.
       01  PARAMETER-FOUND               PIC 9(1).

       01  VAL-START                     LIKE CMD-POS.
       01  VAL-POS                       LIKE CMD-POS.
       01  VALUE-WRK                     LIKE CMDFTA OF PCFGCMD-P.
      /
     ‚*-------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------
       01  X-NAMESPACE                   LIKE CMNSPC OF PCFGCMD-P.
       01  X-COMMAND                     PIC X(1024).
       01  X-RET-PTR                     USAGE POINTER.
       01  X-ERRFLD                      LIKE ERRFLD-WRK.
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING          X-NAMESPACE
                                         X-COMMAND
                                         X-RET-PTR
                                         X-ERRFLD.
     ‚*-------------------------------------------------------
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CFP0011" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* ALLE DATEIEN ÖFFNEN
           IF       FIRST-CALL NOT = "1"
                    MOVE "1" TO FIRST-CALL
                    OPEN     INPUT PCFGCMD-DP
           END-IF.

           INITIALIZE X-ERRFLD.
           SET      X-RET-PTR TO NULL.
     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* KEIN TEXT VORHANDEN?
           IF       X-COMMAND = SPACES
                    GO TO ENDE
           END-IF.

     ‚* POSITION WIRD IN ALLEN SECTIONS WEITERGEZÄHLT
           MOVE     1 TO CMD-POS.

     ‚* LEERZEICHEN ÜBERLESEN
           PERFORM  WITH TEST BEFORE UNTIL
                        CMD-POS > LENGTH OF X-COMMAND
                     OR X-COMMAND(CMD-POS:1) NOT = SPACES
                    ADD 1 TO CMD-POS
           END-PERFORM.

     ‚* ERMITTELN DES COMMAND IN DER TABELLE
           PERFORM  GET-COMMAND.
           IF       COMMAND-WRK = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB DER BEFEHL AKTIV IST
           PERFORM  CHECK-COMMAND.
           IF       ERRFLD-WRK NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DIE PARAMETER AUFBAUEN
           PERFORM  GET-PARAMETER.

       ENDE.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DEN BEFEHL SELBST AUSLESEN
     ‚*--------------------------------------------------------------
       GET-COMMAND SECTION.
       GET-CMD-00.

           MOVE     CMD-POS TO CMD-START.
           MOVE     ZEROES  TO CMD-LEN.
     ‚* SOLANGE HOCHZÄHLEN BIS EIN SPACE KOMMT, DANN HÖRT DER BEFEHL
     ‚* AUF BEISPIEL:
     ‚*  NMAKE -F MAKEFILE.MAK
           PERFORM  WITH TEST BEFORE UNTIL
                        CMD-POS > LENGTH OF X-COMMAND
                     OR X-COMMAND(CMD-POS:)  = SPACES
                     OR X-COMMAND(CMD-POS:1) = SPACES
                    ADD 1 TO CMD-LEN
                    ADD 1 TO CMD-POS
           END-PERFORM.
           IF       CMD-POS >= LENGTH OF X-COMMAND
                    GO TO GET-CMD-90
           END-IF.

     ‚* BEFEHL AUSLESEN
           MOVE     X-COMMAND(CMD-START:CMD-LEN) TO COMMAND-WRK.

       GET-CMD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER PARAMETER
     ‚*--------------------------------------------------------------
       GET-PARAMETER SECTION.
       GET-PAR-00.

           MOVE     CMD-POS TO CMD-START.
           MOVE     ZEROES  TO CMD-LEN.
     ‚* SOLANGE HOCHZÄHLEN BIS EIN "-" KOMMT, DANN FÄNGT DER PARAMETER
     ‚* ERST AN, EIN "-" BEDEUTET ES KOMMT 1 ZEICHEN, "--" BEDEUTET ES
     ‚* KOMMT EIN LÄNGERER PARAMETER
           PERFORM  WITH TEST BEFORE UNTIL
                        CMD-POS > LENGTH OF X-COMMAND
                     OR X-COMMAND(CMD-POS:)  = SPACES
                     OR X-COMMAND(CMD-POS:1) = "-"
                    ADD 1 TO CMD-LEN
                    ADD 1 TO CMD-POS
           END-PERFORM.
           IF       CMD-POS >= LENGTH OF X-COMMAND
                    GO TO GET-PAR-90
           END-IF.

     ‚* JETZT GIBT ES VIER MÖGLICHKEITEN:
     ‚*  1: -K       = PARAMETER "K"
     ‚*  2: -KR      = PARAMETER "K" UND "R"
     ‚*  3: --HELP   = HILFE
     ‚*  4: --       = ENDE DER PARAMETER
           ADD      1 TO CMD-POS.
           IF       CMD-POS > LENGTH OF X-COMMAND
            OR      X-COMMAND(CMD-POS:) = SPACES
                    GO TO GET-PAR-90
           END-IF.

           MOVE     ZEROES  TO PARAMETER-FOUND.
     ‚* IST DAS NÄCHSTE ZEICHEN AUCH "-"?
           IF       X-COMMAND(CMD-POS:1) = "-"
     ‚*             WENN EIN "--" KOMMT HEIßT DIES, ES KOMMEN KEINE
     ‚*             PARAMETER MEHR
                    IF   X-COMMAND(CMD-POS:) = SPACES
                         GO TO GET-PAR-90
                    ELSE
                         PERFORM GET-LONG-PARM
                    END-IF
           ELSE
                    MOVE    1 TO PARAMETER-FOUND
                    PERFORM GET-SHORT-PARM
           END-IF.
     ‚* SOLANGE WEITERMACHEN, BIS DER STRING FERTIG IST
           IF       CMD-POS <= LENGTH OF X-COMMAND
                    GO TO GET-PAR-00
           END-IF.

       GET-PAR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN EINES KURZEN PARAMETERS + EVENTUELLEN WERT
     ‚*--------------------------------------------------------------
       GET-SHORT-PARM SECTION.
       GET-SPR-00.

           MOVE     ZEROES TO VAL-START, VAL-POS.
     ‚* WIR STEHEN JETZT AUF DEM PARAMETER, JETZT ALLE PARAMETER
     ‚* EINTRAGEN BIS EIN LEERZEICHEN KOMMT, DANN KÖNNEN ENTWEDER
     ‚* WIEDER PARAMETER ODER DER WERT ZU EINEM PARAMETER FOLGEN
           PERFORM  WITH TEST BEFORE UNTIL
                        CMD-POS > LENGTH OF X-COMMAND
                     OR X-COMMAND(CMD-POS:)  = SPACES
     ‚*             SOBALD EIN LEERZEICHEN KOMMT, WURDEN DIE PARAMETER
     ‚*             BEENDET, ES KÖNNTE MAX. NOCH EIN WERT ZUM LETZTEN
     ‚*             ERSTELLTEN PARAMETER FOLGEN
                    IF   X-COMMAND(CMD-POS:1) = SPACES
                         MOVE ZEROES  TO PARAMETER-FOUND
                         MOVE CMD-POS TO VAL-START
                    END-IF
     ‚*             SOBALD EIN PARAMETER GEFUNDEN WURDE, HIER VERLASSEN
     ‚*             UND DIE ENTSCHEIDUNG DER HAUPTROUTINE ÜBERLASSEN
                    IF   X-COMMAND(CMD-POS:1) = "-"
                         GO TO GET-SPR-90
                    END-IF
     ‚*             PARAMETER AUFBAUEN UND HINZUFÜGEN
                    IF   PARAMETER-FOUND = 1
                         MOVE "-"                  TO PARAM-WRK(1:1)
                         MOVE X-COMMAND(CMD-POS:1) TO PARAM-WRK(2:1)
                         PERFORM CHECK-PARAMETER
                    END-IF
                    ADD 1 TO CMD-POS
     ‚*             NUR ZÄHLEN WENN BEGONNEN WURDE
                    IF   VAL-START NOT = ZEROES
                         ADD 1 TO VAL-POS
                    END-IF
           END-PERFORM.
           IF       CMD-POS > LENGTH OF X-COMMAND
                    GO TO GET-SPR-90
           END-IF.

       GET-SPR-90.
     ‚* WERT AN PARAMETER HÄNGEN
           IF       VAL-START NOT = ZEROES
                    MOVE X-COMMAND(VAL-START + 1:VAL-POS) TO VALUE-WRK
                    PERFORM ADD-VALUE-TO-LAST-PARM
           END-IF.

           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LANGE PARAMETER UND WERTE ERMITTELN
     ‚*--------------------------------------------------------------
       GET-LONG-PARM SECTION.
       GET-LPR-00.

           INITIALIZE RET-CODE.

       GET-LPR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEFAULT-WERT EINES PARAMETERS ERMITTELN
     ‚*--------------------------------------------------------------
       GET-DEFAULT-VALUE SECTION.
       GET-DFV-00.

           INITIALIZE ERRFLD-WRK.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PCFGCMDF OF PCFGCMD-P.
           MOVE     X-NAMESPACE         TO CMNSPC OF PCFGCMD-P.
           MOVE     COMMAND-WRK         TO CMCMD  OF PCFGCMD-P.
           MOVE     PARAM-WRK           TO CMPARM OF PCFGCMD-P.
           READ     PCFGCMD-DP.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "ERR0002" TO ERRFLD-WRK
           ELSE
     ‚*             ALPHANUMERISCH ODER NUMERISCH?
                    IF   CMPTYP OF PCFGCMD-P = ZEROES
                         MOVE CMDFTA OF PCFGCMD-P TO VALUE-WRK
                    ELSE
                         MOVE CMDFTN OF PCFGCMD-P TO VALUE-WRK
                    END-IF
           END-IF.

       GET-DFV-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DEM LETZTEN PARAMETER NOCH EINEN WERT EINTRAGEN
     ‚*--------------------------------------------------------------
       ADD-VALUE-TO-LAST-PARM SECTION.
       ADD-VLP-00.

     ‚* JETZT "SCHREIBEN", DA DAMIT DER ALTE SATZ ÜBERSCHRIEBEN WIRD
           INITIALIZE MAPDTA-REC.
           IF       PARAM-WRK NOT = SPACES
                    MOVE PARAM-WRK     TO MAP-KEY  OF MAPDTA-REC
           ELSE
                    MOVE "*REST"       TO MAP-KEY  OF MAPDTA-REC
           END-IF.
           MOVE     VALUE-WRK          TO MAP-REC  OF MAPDTA-REC.
           MOVE     MAP-WRITE          TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-RET-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-RET-PTR TO MAP-PTR OF MAPDTA-REC.

       ADD-VLP-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       CHECK-COMMAND SECTION.
       CHK-CMD-00.

           INITIALIZE ERRFLD-WRK.
     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PCFGCMDF OF PCFGCMD-P.
           MOVE     X-NAMESPACE         TO CMNSPC OF PCFGCMD-P.
           MOVE     COMMAND-WRK         TO CMCMD  OF PCFGCMD-P.
           MOVE     SPACES              TO CMPARM OF PCFGCMD-P.
           READ     PCFGCMD-DP.
           IF       FILE-STATUS NOT = ZEROES
                    MOVE "ERR0002" TO ERRFLD-WRK
                    GO TO CHK-CMD-90
           END-IF.

           IF       CMAKT  OF PCFGCMD-P = ZEROES
                    MOVE "ERR0003" TO ERRFLD-WRK
                    GO TO CHK-CMD-90
           END-IF.

     ‚* ANSONSTEN SCHREIBEN WIR NUN DEN POINTER
           INITIALIZE MAPDTA-REC.
           MOVE     "*COMMAND"         TO MAP-KEY  OF MAPDTA-REC.
           MOVE     COMMAND-WRK        TO MAP-REC  OF MAPDTA-REC.
           MOVE     MAP-WRITE          TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-RET-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-RET-PTR TO MAP-PTR OF MAPDTA-REC.

       CHK-CMD-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* ANZEIGE ALLER DATEN IM SUBFILE
     ‚*---------------------------------------------------------------
       CHECK-PARAMETER SECTION.
       CHK-PRM-00.

     ‚* FÜLLEN DES KEYS ZUM LESEN DER DATEI
           INITIALIZE PCFGCMDF OF PCFGCMD-P.
           MOVE     X-NAMESPACE         TO CMNSPC OF PCFGCMD-P.
           MOVE     COMMAND-WRK         TO CMCMD  OF PCFGCMD-P.
           MOVE     PARAM-WRK           TO CMPARM OF PCFGCMD-P.
           READ     PCFGCMD-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHK-PRM-90
           END-IF.

     ‚* PARAMETER AKTIV?
           IF       CMAKT  OF PCFGCMD-P = ZEROES
                    GO TO CHK-PRM-90
           END-IF.

           INITIALIZE VALUE-WRK.
     ‚* JETZT SCHREIBEN WIR DEN PARAMETER
           INITIALIZE MAPDTA-REC.
           MOVE     PARAM-WRK          TO MAP-KEY  OF MAPDTA-REC.
           IF       CMPVAL OF PCFGCMD-P NOT = ZEROES
                    PERFORM GET-DEFAULT-VALUE
           END-IF.
           MOVE     VALUE-WRK          TO MAP-REC  OF MAPDTA-REC.
           MOVE     MAP-WRITE          TO MAP-ACT  OF MAPDTA-REC.
           SET      MAP-PTR OF MAPDTA-REC TO X-RET-PTR.
           PERFORM  COPY-CALL-MAPDTA.
           SET      X-RET-PTR TO MAP-PTR OF MAPDTA-REC.

       CHK-PRM-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-CALL-MAPDTA: MAPDTA AUSFÜHREN
     ‚*---------------------------------------------------------------

           COPY     MAPDTA     OF APG-QCPYSRC.
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
