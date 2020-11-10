       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.             HLP0026.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* DIREKTES E-MAIL VERSENDEN ÜBER SMTP                           *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  03.06.2016                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TRIGGER PMS EMAIL MESSAGE                    *
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
     ‚* TEMPORARY FILE PTMPMAI
     ‚*-------------------------------------------------------------
           SELECT PTMPMAI-DP
                  ASSIGN       TO  DATABASE-PTMPMAI
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEMPORARY PTMPMAI
       FD  PTMPMAI-DP
           LABEL RECORDS ARE STANDARD.
       01  PTMPMAI-P.
           COPY DDS-ALL-FORMATS OF PTMPMAI.
      /
       WORKING-STORAGE SECTION.
     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM  OF APG-QCPYSRC.

     ‚* STANDARDFELDER DTAQ
           COPY WRKDTAQ     OF APG-QCPYSRC.

       01  PGM-WRK                        PIC X(10) VALUE "HLP0026".
       01  PGM-TYP                        PIC X(8)  VALUE "EMAIL   ".

       01  CRTPF-REC                 TYPE CRTPF-TYPE.
       01  CPYTOSTMF-REC             TYPE CPYTOSTMF-TYPE.

     ‚* DUMMY-FELDER ZUM DEFINIEREN MIT LIKE
       01  ADRLEN-DUMMY                   PIC X(64).

     ‚* LENGTH AND POS VARIABLES
       01  LEN                            PIC 9(4).
       01  POS                       LIKE LEN.
       01  MAX-LEN                   LIKE LEN.
       01  START-POS                 LIKE LEN.

     ‚* OTHER VARIABLES
       01  FILENAME-WRK                   PIC X(26).
       01  STREAM-REC                     PIC X(326).

     ‚* ARRAY STRUCTURE
       01  ARRAY-VAR.
        05 CNT-VAR                        PIC 9(2) BINARY.
        05 MAIL-REC                       OCCURS 10 TIMES.
          10 MAIL-NAME               LIKE ADRLEN-DUMMY.
       01  IDX-WRK                        PIC 9(2).

     ‚* PARAMETER FOR QSNDDTAQ
       01  DTAQ-WRK                       PIC X(10) VALUE "PMSMAILAPI".
       01  LIBL-WRK                       PIC X(10) VALUE "PRODATV2".
       01  LEN-WRK                        PIC S9(5) COMP-3 VALUE 300.

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       01  X-HOST                PIC X(15).
       01  X-PORT                PIC X(5).
       01  X-SENDER              PIC X(30).
     ‚* ÜBERNAHME DER EMPFÄNGER
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
       01  X-FILE                PIC X(256).
     ‚* IM RETTXT STEHT DIE RÜCKMELDUNG DES SERVERS, FALLS
     ‚* DIESE NEGATIV WAR
       01  X-RETTXT              PIC X(256).
     ‚*-------------------------------------------------------
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

     ‚* CALL STANDARD INIT-FUNCTION
           PERFORM  COPY-PGM-INIT.

           PERFORM  COPY-GET-TIME.

     ‚* CREATE FILENAME WITH DATE AND TIME
           INITIALIZE FILENAME-WRK.
           STRING   DATE-8 "_" TIME-6 "_" T-USER
                    DELIMITED BY SIZE
             INTO   FILENAME-WRK
           END-STRING.

     ‚* CHECK IF IFS FILE EXISTS
           PERFORM  CHECK-FILE-EXISTS.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

           INITIALIZE RET-CODE.
     ‚* ERSTELLEN DER "PTMPJOB"
           INITIALIZE CRTPF-REC.
           MOVE     "QTEMP"         TO TOLIB    OF CRTPF-REC.
           MOVE     "PTMPMAI"       TO TOPGM    OF CRTPF-REC.
           MOVE     326             TO RECSIZE  OF CRTPF-REC.
           CALL     "CLP0031" USING CRTPF-REC
                                    RET-CODE
           END-CALL.

     ‚* OPEN TEMPORARY FILE
           OPEN     OUTPUT PTMPMAI-DP.

     ‚* WRITE STREAM-FILE TO PMS FOLDER
           PERFORM  CREATE-STREAM-FILE.

     ‚* SEND TRIGGER TO PMS DTAQ "PMSMAILAPI"
           PERFORM  SEND-TRIGGER.

       ENDE.
     ‚*‚CALL STANDARD EXIT-FUNCTION
           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* CREATE STREAM FILE AND FILL RECORDS
     ‚*--------------------------------------------------------------
       CREATE-STREAM-FILE SECTION.
       CRT-STM-00.

     ‚* TOPIC
           MOVE     X-TOPIC  TO STREAM-REC.
           PERFORM  WRITE-LINE.

     ‚* TO
           MOVE     X-EMPF   TO ARRAY-VAR.
           PERFORM  CONVERT-ARRAY-TO-STRING.
           PERFORM  WRITE-LINE.

     ‚* CC
           MOVE     X-KOPIE  TO ARRAY-VAR.
           PERFORM  CONVERT-ARRAY-TO-STRING.
           PERFORM  WRITE-LINE.

     ‚* BCC
           MOVE     X-BKOPIE TO ARRAY-VAR.
           PERFORM  CONVERT-ARRAY-TO-STRING.
           PERFORM  WRITE-LINE.

     ‚* ATTACHMENT LOCATION
           MOVE     X-FILE   TO STREAM-REC.
           PERFORM  WRITE-LINE.

     ‚* NOW PROCESS MAIL-DATA, A NEW LINE MUST BE ADDED AFTER A "X25"
           COMPUTE  MAX-LEN = FUNCTION LENGTH(FUNCTION TRIMR(X-DATA)).
           MOVE     1      TO START-POS.
           MOVE     1      TO POS.
           MOVE     ZEROES TO LEN.

           PERFORM  UNTIL POS         >= MAX-LEN
                       OR X-DATA(POS:) = SPACES
     ‚*             IST DIES DAS TRENNZEICHEN? DANN JETZT DEN ZÄHLER
     ‚*             HOCHZÄHLEN UND ALLE ZEICHEN ÜBERNEHMEN
                    IF   X-DATA(POS:1) = X"25"
                         PERFORM ADD-VALUE-TO-FILE
     ‚*                  DIE ZEICHEN WERDEN ERST NACH DEM TRENNZEICHEN
     ‚*                  ÜBERNOMMEN, DAHER HIER +1 RECHNEN
                         COMPUTE START-POS = POS + 1
                         MOVE    ZEROES TO LEN
                    ELSE
                         ADD 1 TO LEN
                    END-IF
     ‚*             POSITION SETZEN
                    ADD 1 TO POS
           END-PERFORM.
     ‚* DER LETZTE WIRD NICHT MIT EINEM TRENNZEICHEN BEENDET
           IF       X-DATA(START-POS:) NOT = SPACES
                    COMPUTE LEN = MAX-LEN - START-POS
                    PERFORM ADD-VALUE-TO-FILE
           END-IF.

     ‚* CLOSE FILE AND SEND TRIGGER
           CLOSE    PTMPMAI-DP.

     ‚* NOW COPY THE TEMPORARY FILE TO IFS
           INITIALIZE CPYTOSTMF-REC.
           MOVE     "/QSYS.LIB/QTEMP.LIB/PTMPMAI.FILE/PTMPMAI.MBR" TO
                    FROMMBR  OF CPYTOSTMF-REC.
           STRING   "/EUPPROV2/PROTMPV2/DTAQMAIL/"
                    FUNCTION TRIM(FILENAME-WRK)
                    DELIMITED BY SIZE
             INTO   TOFILE  OF CPYTOSTMF-REC
           END-STRING.
           MOVE     "1252"     TO CCSID   OF CPYTOSTMF-REC.
           MOVE     "*CRLF"    TO LINFMT  OF CPYTOSTMF-REC.
           MOVE     "*REPLACE" TO CPYMODE OF CPYTOSTMF-REC.
           CALL     "CLP0073" USING CPYTOSTMF-REC
                                    RET-CODE
           END-CALL.

       CRT-STM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WRITE LINE TO STREAM FILE
     ‚*--------------------------------------------------------------
       WRITE-LINE SECTION.
       WRT-LIN-00.

           INITIALIZE PTMPMAI-P.
           MOVE     STREAM-REC TO PTMPMAI-P.
           WRITE    PTMPMAI-P.
           INITIALIZE STREAM-REC.

       WRT-LIN-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SEND TRIGGER TO PMS
     ‚*--------------------------------------------------------------
       SEND-TRIGGER SECTION.
       SND-TRG-00.

           MOVE     DATE-8            TO D-DATE    OF DTAQ-REC.
           MOVE     TIME-6            TO D-TIME    OF DTAQ-REC.
           MOVE     T-TERM            TO D-TERM    OF DTAQ-REC.
           MOVE     T-USER            TO D-USER    OF DTAQ-REC.
           MOVE     T-JBNR            TO D-JBNR    OF DTAQ-REC.
           MOVE     PGM-WRK           TO D-PGM     OF DTAQ-REC.
           MOVE     SPRACHE-WRK       TO D-SPRACHE OF DTAQ-REC.
           MOVE     "V1"              TO D-DTAID   OF DTAQ-REC.
           MOVE     SPACES            TO D-ACODE   OF DTAQ-REC.
           MOVE     DATE-8            TO D-PARM1   OF DTAQ-REC.
           MOVE     TIME-6            TO D-PARM2   OF DTAQ-REC.
           MOVE     T-USER            TO D-PARM3   OF DTAQ-REC.

           CALL     "QSNDDTAQ" USING DTAQ-WRK
                                     LIBL-WRK
                                     LEN-WRK
                                     DTAQ-REC
           END-CALL.

       SND-TRG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ADD RECORD TO THE FILE (PARTIAL OR EMPTY LINE)
     ‚*--------------------------------------------------------------
       ADD-VALUE-TO-FILE SECTION.
       ADD-VTF-00.

     ‚* LEERE ELEMENTE KÖNNEN VORKOMMEN, WENN ZWISCHEN DEN TRENNZEICHEN
     ‚* KEIN PLATZ MEHR IST Z.B. "XXX;;;AA;"
           IF       LEN > ZEROES
                    MOVE X-DATA(START-POS:LEN) TO STREAM-REC
           ELSE
                    MOVE SPACES                TO STREAM-REC
           END-IF.
           PERFORM  WRITE-LINE.

       ADD-VTF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CHECK IF FILE ALREADY EXISTS IN IFS - USUALLY NOT POSSIBLE
     ‚* BECAUSE DATE/TIME ARE PART OF THE FILENAME
     ‚*--------------------------------------------------------------
       CHECK-FILE-EXISTS SECTION.
       CHK-FIL-00.

           INITIALIZE RET-CODE.

       CHK-FIL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* CONVERT ARRAY TO STRING
     ‚* RESULT MUST BE: USER,USER,USER,USER (WITHOUT WHITEPACES)
     ‚*--------------------------------------------------------------
       CONVERT-ARRAY-TO-STRING SECTION.
       CNV-ATS-00.

           INITIALIZE IDX-WRK.
           MOVE     1 TO LEN.

           PERFORM  CNT-VAR OF ARRAY-VAR TIMES
                    ADD 1 TO IDX-WRK
     ‚*             ADD COMMA AFTER THE FIRST ITEM
                    IF   IDX-WRK > 1
                         STRING "," DELIMITED BY SIZE
                           INTO STREAM-REC
                           WITH POINTER LEN
                         END-STRING
                    END-IF
     ‚*             ADD USER NAME TO THE TEXT
                    STRING FUNCTION TRIM(
                            MAIL-NAME OF ARRAY-VAR(IDX-WRK))
                           DELIMITED BY SIZE
                      INTO STREAM-REC
                      WITH POINTER LEN
                    END-STRING
           END-PERFORM.

       CNV-ATS-90.
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
