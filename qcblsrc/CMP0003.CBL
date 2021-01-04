       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB IRGENDWELCHE PROGRAMME VON ANDEREN GEWANDELT WURDEN *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN CRTUSER FÜR ALLE PROGRAMME            *
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
     ‚* PHYSISCHE DATEI EINSTELLUNGEN
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
     ‚*--- KONFIGURATION
       FD  PCONFIG-DP
           LABEL RECORDS ARE STANDARD.
       01  PCONFIG-P.
           COPY DDS-ALL-FORMATS OF PCONFIG.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0003".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

       01  SNDBRKMSG-REC                 TYPE SNDBRKMSG-TYPE.
       01  RTVOBJD-REC                   TYPE RTVOBJD-TYPE.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* ERMITTELN JOB-INFORMATIONEN
           MOVE     "INIT" TO COPY-AKTION.
           CALL     PROCEDURE "CFP9000" USING COPY-AKTION
                                              JOBINFO
                                              PGM-WRK
           END-CALL.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     INPUT PCONFIG-DP.

     ‚* PRÜFEN ALLER PROGRAMME
           PERFORM  LESE-DATEN.

       ENDE.
           CLOSE                 PCONFIG-DP.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER TAG-DATEN ANHAND DER SELEKTION
     ‚*--------------------------------------------------------------
       LESE-DATEN SECTION.
       LES-DAT-00.

     ‚* PCONFIG LESEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "CP"                TO CFID   OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY  OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY2 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY3 OF PCONFIG-P.
           MOVE     SPACES              TO CFKEY4 OF PCONFIG-P.
           START    PCONFIG-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

       LES-DAT-20.
           READ     PCONFIG-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-DAT-90
           END-IF.

           IF       CFID   OF PCONFIG-P NOT = "CP"
                    GO TO LES-DAT-90
           END-IF.

     ‚* STANDARDEINTRÄGE WEGLASSEN
           IF       CFKEY  OF PCONFIG-P(1:1) = "*"
            OR      CFKEY2 OF PCONFIG-P(1:1) = "*"
            OR      CFKEY3 OF PCONFIG-P(1:1) = "*"
                    GO TO LES-DAT-20
           END-IF.

     ‚* ERMITTELN OBJEKTINFORMATIONEN
           PERFORM  GET-OBJD.
           IF       CRTUSER OF RTVOBJD-REC NOT = "APG"
            AND     CRTUSER OF RTVOBJD-REC NOT = SPACES
                    PERFORM SND-BREAK-MESSAGE
           END-IF.

           GO TO    LES-DAT-20.

       LES-DAT-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER OBJEKTBESCHREIBUNG
     ‚*--------------------------------------------------------------
       GET-OBJD SECTION.
       GET-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* OBJEKTDATEN ERMITTELN
           INITIALIZE RTVOBJD-REC.
           MOVE     CFKEY  OF PCONFIG-P TO OBJLIB   OF RTVOBJD-REC.
           MOVE     CFKEY3 OF PCONFIG-P TO OBJ      OF RTVOBJD-REC.
           CALL     "HLP0010" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.

       GET-OBJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SENDEN EINER BREAK-MESSAGE AN EINE SITZUNG
     ‚*--------------------------------------------------------------
       SND-BREAK-MESSAGE SECTION.
       SND-BRK-00.

           INITIALIZE SNDBRKMSG-REC, RET-CODE.
           STRING   CFKEY  OF PCONFIG-P DELIMITED BY SPACES
                    "/"                 DELIMITED BY SIZE
                    CFKEY2 OF PCONFIG-P DELIMITED BY SPACES
                    "/"                 DELIMITED BY SIZE
                    CFKEY3 OF PCONFIG-P DELIMITED BY SPACES
           INTO     MSG    OF SNDBRKMSG-REC.
           MOVE     T-TERM    TO TERM   OF SNDBRKMSG-REC.
           CALL     "CLP0017" USING SNDBRKMSG-REC
                                    RET-CODE
           END-CALL.

       SND-BRK-90.
           EXIT.
      /
