     ‚*PROCESS   XREF.
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             DRP0002.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LABEL DRUCKEN                                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  02.03.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  DRUCKEN EINES LABELS                         *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
          SOURCE-COMPUTER. IBM-AS400.
          OBJECT-COMPUTER. IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
     ‚*---------------------------------------------------------------*
     ‚*    LAYOUTDATEI
     ‚*---------------------------------------------------------------*
           SELECT PETIKLBL-DATP
                  ASSIGN        TO DATABASE-PETIKLBL
                  ORGANIZATION  IS SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FILE-STATUS.
     ‚*---------------------------------------------------------------*
     ‚*    PRINTERFILE
     ‚*---------------------------------------------------------------*
           SELECT DRUCK-DAT
                  ASSIGN        TO FORMATFILE-DRF0002
                  ORGANIZATION  IS SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL.
      /
       DATA DIVISION.
        FILE SECTION.
     ‚*-----DRUCKDATEN----
       FD  PETIKLBL-DATP
           LABEL RECORD STANDARD.
       01  PETIKLBL-RECP.
           COPY DDS-ALL-FORMATS OF PETIKLBL.
      /
     ‚*------DRUCKAUSGABE----
       FD  DRUCK-DAT
           LABEL RECORDS ARE OMITTED.
       01  DRUCK-REC.
            COPY DDS-ALL-FORMATS OF DRF0002.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "DRP0002".
       01  PGM-TYP                       PIC X(8)  VALUE "PRINT".

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF DRF0001
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-INDIC.

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  DELOVR-REC                    TYPE DELOVR-TYPE.
       01  OVRDBF-REC                    TYPE OVRDBF-TYPE.
       01  OVRPRTF-REC                   TYPE OVRPRTF-TYPE.

       01  OVR-OUTQ                      PIC X(10).
       01  TYP-WRK                       PIC X(1).

       01  ABSCHLUSSZEICHEN              PIC X(10).
       01  CONVERT-ZEILE                 LIKE AUSTEXT OF FORMAT1.
       01  POS                           PIC 9(4).
       01  POS-END                       PIC 9(4).
       01  POSITION-VAR                  PIC 9(4).
       01  DUMMY-WRK                     PIC 9(2).

     ‚* PARAMETER FÜR DAS HLP0013
       01  ERSETZEN-STRING               PIC X(250).
       01  FROM-STRING                   PIC X(100).
       01  TO-STRING                     PIC X(100).
       01  MODE-WRK                      PIC X(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-ZIEL-LIB                    PIC X(10).
       01  X-ZIEL-FILE                   PIC X(10).
       01  X-ZIEL-MBR                    PIC X(10).
       01  X-TYP                         PIC X(1).
       01  X-OUTQ                        PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING        X-ZIEL-LIB
                                       X-ZIEL-FILE
                                       X-ZIEL-MBR
                                       X-TYP
                                       X-OUTQ.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "DRP0002" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE OVR-OUTQ, TYP-WRK.
     ‚* OUTQ SETZEN
           IF       ADDRESS OF X-OUTQ = NULL
                    MOVE "PRT01" TO OVR-OUTQ
           ELSE
                    IF   X-OUTQ = SPACES
                         MOVE "PRT01"    TO OVR-OUTQ
                    ELSE
                         MOVE X-OUTQ     TO OVR-OUTQ
                    END-IF
           END-IF.

     ‚* TYP PRÜFEN
           IF       ADDRESS OF X-TYP = NULL
     ‚*             WURDE KEINE TEILDATEI ANGEGEBEN?
                    IF   ADDRESS OF X-ZIEL-MBR = NULL
     ‚*                  AUCH KEINE LIB ODER KEIN FILE?
                         IF   ADDRESS OF X-ZIEL-LIB  = NULL
                          AND ADDRESS OF X-ZIEL-FILE = NULL
     ‚*                       KEIN OVR AUSFÜHREN
                              MOVE "3" TO TYP-WRK
                         ELSE
     ‚*                       OVR AUF EINE PETIK AUSFÜHREN
                              MOVE "1" TO TYP-WRK
                         END-IF
                    ELSE
     ‚*                  OVR MIT MBR AUF LAYOUTDATEI DURCHFÜHREN
                         MOVE "2" TO TYP-WRK
                    END-IF
           ELSE
                    MOVE X-TYP TO TYP-WRK
           END-IF.

           INITIALIZE RET-CODE.
     ‚* OVERWRITE PRINTERFILE AUSFÜHREN
           INITIALIZE OVRPRTF-REC.
           MOVE     "DRF0002" TO FROMFILE   OF OVRPRTF-REC.
           MOVE     OVR-OUTQ  TO TOOUTQ     OF OVRPRTF-REC.
           CALL     "CLP0041" USING OVRPRTF-REC
                                    RET-CODE
           END-CALL.

     ‚* OVERWRITE AUSFÜHREN?
           EVALUATE TYP-WRK
     ‚*             AUF EINE ANDERE PETIK ÜBERSCHREIBEN
                    WHEN "1"
                         INITIALIZE RET-CODE
     ‚*                  OVERWRITE AUSFÜHREN
                         INITIALIZE OVRDBF-REC
                         MOVE "PETIKLBL"  TO FROMFILE OF OVRDBF-REC
                         MOVE X-ZIEL-LIB  TO TOLIB    OF OVRDBF-REC
                         MOVE X-ZIEL-FILE TO TOFILE   OF OVRDBF-REC
                         MOVE "*FIRST"    TO TOMBR    OF OVRDBF-REC
                         MOVE "*NO"       TO LVLCHK   OF OVRDBF-REC
                         CALL "CLP0011" USING OVRDBF-REC
                                              RET-CODE
                         END-CALL
     ‚*             AUF EINE LAYOUT-DATEI ÜBERSCHREIBEN
                    WHEN "2"
                    WHEN "4"
                         INITIALIZE RET-CODE
     ‚*                  OVERWRITE AUSFÜHREN
                         INITIALIZE OVRDBF-REC
                         MOVE "PETIKLBL"  TO FROMFILE OF OVRDBF-REC
                         MOVE X-ZIEL-LIB  TO TOLIB    OF OVRDBF-REC
                         MOVE X-ZIEL-FILE TO TOFILE   OF OVRDBF-REC
                         MOVE X-ZIEL-MBR  TO TOMBR    OF OVRDBF-REC
                         MOVE "*NO"       TO LVLCHK   OF OVRDBF-REC
                         CALL "CLP0011" USING OVRDBF-REC
                                              RET-CODE
                         END-CALL
     ‚*             KEINE ÜBERSCHREIBUNG DURCHFÜHREN
                    WHEN "3"
                         INITIALIZE RET-CODE
                    WHEN OTHER
                         GO TO ENDE
           END-EVALUATE.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRINTERFILE ÖFFNEN
           OPEN     INPUT  PETIKLBL-DATP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           OPEN     OUTPUT DRUCK-DAT.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

           INITIALIZE FORMAT1.
           INITIALIZE PETIKF OF PETIKLBL-RECP.

       ANF020.
           READ     PETIKLBL-DATP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
            AND     FILE-STATUS NOT = "04"
                    GO TO ENDE
           END-IF.

     ‚* BEI EINER SOURCE MUSS DIE ZEILENNUMMER UND DAS DATUM WEG-
     ‚* GELASSEN WERDEN
           EVALUATE TYP-WRK
                    WHEN "2"
                         MOVE PKPARM OF PETIKLBL-RECP(13:) TO
                              AUSTEXT OF FORMAT1
                    WHEN "4"
     ‚*                  PETIK MIT KYOCERA-STEUERZEICHEN IN FMT-
     ‚*                  SCHREIBWEISE, Z.B.:
     ‚*                  "FMT01 006MAP 13.3,2.5;"
                         MOVE PKPARM OF PETIKLBL-RECP(22:) TO
                              AUSTEXT OF FORMAT1
                    WHEN OTHER
                         MOVE PKPARM OF PETIKLBL-RECP TO
                              AUSTEXT OF FORMAT1
           END-EVALUATE.

           MOVE     AUSTEXT OF FORMAT1 TO CONVERT-ZEILE.
     ‚* JE NACH TYP MUSS EVTL. DAS ABSCHLUSSZEICHEN HINZUGEFÜGT WERDEN
           PERFORM  CHECKVAR.
           IF       POSITION-VAR > 1
     ‚*             LÄNGE DES STRINGS ERMITTELN
                    PERFORM GET-LENGTH
     ‚*             ABSCHLUSSZEICHEN ERMITTELN
                    PERFORM GET-ABSCHLUSSZEICHEN
                    IF   ABSCHLUSSZEICHEN > SPACES
                     AND POS-END          > ZEROES
                         MOVE ABSCHLUSSZEICHEN TO
                              AUSTEXT OF FORMAT1(POS-END + 1:)
                    END-IF
           END-IF.

     ‚* ERSETZEN DES <ESC> DURCH DAS ENTSPRECHENDE HEX-ZEICHEN
     ‚* ASCII: 1B, EBCDIC: 27
           IF       X-ZIEL-MBR = "PETKD8793O"
                    INITIALIZE MODE-WRK
                    MOVE    "<ESC>"   TO FROM-STRING
                    MOVE    X"27"     TO TO-STRING
                    MOVE    AUSTEXT OF FORMAT1 TO ERSETZEN-STRING
                    CALL    PROCEDURE "HLP0013" USING ERSETZEN-STRING
                                                      FROM-STRING
                                                      TO-STRING
                                                      MODE-WRK
                    END-CALL
                    MOVE ERSETZEN-STRING TO AUSTEXT OF FORMAT1
           END-IF.

           MOVE     CORR FORMAT1     TO FMT01-O.
           WRITE    DRUCK-REC FORMAT IS "FMT01".

     ‚* NÄCHSTE ZEILE LESEN
           GO TO    ANF020.

       ENDE.
           CLOSE                 DRUCK-DAT
                                 PETIKLBL-DATP.

     ‚* OVERWRITE LÖSCHEN
           INITIALIZE DELOVR-REC.
           MOVE     "DRF0002"  TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

           INITIALIZE DELOVR-REC.
           MOVE     "PETIKLBL" TO FROMFILE OF DELOVR-REC.
           CALL     "CLP0012" USING DELOVR-REC
           END-CALL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*---------------------------------------------------------------
     ‚* CHECK VARIABLENLÄNGE
     ‚*---------------------------------------------------------------
       CHECKVAR SECTION.
       CHE-VAR-00.

     ‚* POSITIONSBESTIMMUNG VOR "$$"-VARIABLEN-/PGMTXT-MARKIERUNG
           INITIALIZE POS.
           INSPECT  CONVERT-ZEILE TALLYING POS FOR CHARACTERS
                    BEFORE INITIAL "$$".

     ‚* "$$"-VARIABLEN-/PGMTXT-MARKIERUNG VORHANDEN?
     ‚* POSITIONSFEHLERVERMEIDUNG: POS NICHT > LÄNGE - $PGM####
           IF       POS > LENGTH OF CONVERT-ZEILE - 8
                    INITIALIZE POSITION-VAR
                    GO TO CHE-VAR-90
           END-IF.

     ‚* SICHERUNG POSITION VON "$$"
           ADD      1 TO POS.
           MOVE     POS TO POSITION-VAR.

       CHE-VAR-90.
           EXIT.
      /
     ‚* --------------------------------------------------------------
     ‚* ERMITTEN DES ABSCHLUSSZEICHEN, WELCHES GGFLS AN DIE
     ‚* DRUCKZEILE ANGEHÄNGT WERDEN SOLL(MUSS
     ‚* --------------------------------------------------------------
       GET-ABSCHLUSSZEICHEN SECTION.
       GET-ABS-00.

           MOVE     SPACES TO ABSCHLUSSZEICHEN.

     ‚* FESTSTELLEN FÜR WELCHEN DRUCKER DIE LAYOUTDATEI ERSTELLT IST
     ‚* ZEBRA    : VOR DER VARIABLE STEHT DIE KONSTANTE ^FD
     ‚*                                           ODER  ^XG
     ‚*                                           ODER  :>8
     ‚*                                           ODER  ;>8
     ‚* MP104    : VOR DER VARIABLE STEHT DIE KONSTANTE "
     ‚* INTERMEC : VOR DER VARIABLE STEHT DIE KONSTANTE TX>
     ‚*                                           ODER  B>1
     ‚* PRINTRON.: VOR DER VARIABLE STEHT DIE KONSTANTE *
     ‚* M+N 3000 : KEINE DER O.A. ZEICHEN


     ‚* MARKPOINT-DRUCKER ?
           IF       POSITION-VAR > 1
            AND     CONVERT-ZEILE(POSITION-VAR - 1:1) = """"
                    MOVE """"  TO ABSCHLUSSZEICHEN(1:1)
           END-IF.
     ‚* METO-DRUCKER ?
           IF       POSITION-VAR > 1
            AND     CONVERT-ZEILE(POSITION-VAR - 1:1) = """"
            AND     CONVERT-ZEILE(1:1)                = "["
                    MOVE """]"  TO ABSCHLUSSZEICHEN(1:2)
           END-IF.
     ‚* ZEBRA-DRUCKER ?
           IF       POSITION-VAR > 3
             AND    (    CONVERT-ZEILE(POSITION-VAR - 3:3) = "^FD"
                     OR  CONVERT-ZEILE(POSITION-VAR - 5:3) = "^FD"
                     OR  CONVERT-ZEILE(POSITION-VAR - 3:3) = ":>8"
                     OR  CONVERT-ZEILE(POSITION-VAR - 5:3) = ";>8")
                    MOVE "^FS"  TO ABSCHLUSSZEICHEN
           END-IF.
           IF       (    POSITION-VAR > 3
                     AND CONVERT-ZEILE(POSITION-VAR - 3:3) = "^XG")
             OR     (    POSITION-VAR > 5
                     AND CONVERT-ZEILE(POSITION-VAR - 5:3) = "^XG")
                    MOVE ",1,1^FS" TO ABSCHLUSSZEICHEN
           END-IF.

           MOVE     ZEROES TO DUMMY-WRK.
           INSPECT  CONVERT-ZEILE TALLYING DUMMY-WRK FOR ALL ";d3,".
     ‚* INTERMEC-DRUCKER ?
           IF       POSITION-VAR > 3
            AND     (    CONVERT-ZEILE(POSITION-VAR - 3:3) = "d3,"
                     OR  CONVERT-ZEILE(POSITION-VAR - 3:3) = "B>1"
                     OR  CONVERT-ZEILE(POSITION-VAR - 3:3) = "TX>"
                     OR  CONVERT-ZEILE(POSITION-VAR - 2:3) = "B&G"
                     OR  DUMMY-WRK                 NOT= 0)
                    MOVE "<ETX>" TO ABSCHLUSSZEICHEN
           END-IF.
     ‚* PRINTONICS-RUCKER ?
           IF       POSITION-VAR > 1
            AND     CONVERT-ZEILE(POSITION-VAR - 1:1) = "*"
                    MOVE "*" TO ABSCHLUSSZEICHEN
           END-IF.

       GET-ABS-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER LÄNGE
     ‚*--------------------------------------------------------------
       GET-LENGTH SECTION.
       GET-LEN-00.

           MOVE     LENGTH OF AUSTEXT OF FORMAT1 TO POS-END.
     ‚* LÄNGE DES STRINGS ERMITTELN
           PERFORM  UNTIL POS-END                           = ZEROES
                     OR   AUSTEXT OF FORMAT1(POS-END:1) NOT = SPACE
                    SUBTRACT 1 FROM POS-END
           END-PERFORM.

       GET-LEN-90.
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
