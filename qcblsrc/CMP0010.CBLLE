       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0010.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* AUFNEHMEN PROGRAMM IN COMPILEREINSTELLUNGEN                   *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  07.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SCHREIBEN COMPILERSATZ                       *
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
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0010".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  RTVMBRD-REC                   TYPE RTVMBRD-TYPE.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CPLIB                       PIC X(10).
       01  X-CPFILE                      PIC X(10).
       01  X-CPMBR                       PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CPLIB
                                X-CPFILE
                                X-CPMBR.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0010" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCONFIG-DP.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CPLIB  = SPACES
            OR      X-CPFILE = SPACES
            OR      X-CPMBR  = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB EINTRAG SCHON VORHANDEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "CP"                  TO CFID   OF PCONFIG-P.
           MOVE     X-CPLIB               TO CFKEY  OF PCONFIG-P.
           MOVE     X-CPFILE              TO CFKEY2 OF PCONFIG-P.
           MOVE     X-CPMBR               TO CFKEY3 OF PCONFIG-P.
           READ     PCONFIG-DP.
           IF       FILE-STATUS = ZEROES
                    PERFORM  GET-MEMBER-DATEN
                    MOVE     MBRTEXT   OF RTVMBRD-REC  TO
                             CFTXT1 OF PCONFIG-P
                    REWRITE PCONFIG-P
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB ES DIE TEILDATEI GIBT
           PERFORM  CHECK-TEILDATEI.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* KEIN DATENSATZ VORHANDEN, ALSO ANLEGEN
           INITIALIZE PCONFIGF OF PCONFIG-P.
           MOVE     "CP"                    TO CFID   OF PCONFIG-P.
           MOVE     X-CPLIB                 TO CFKEY  OF PCONFIG-P.
           MOVE     X-CPFILE                TO CFKEY2 OF PCONFIG-P.
           MOVE     X-CPMBR                 TO CFKEY3 OF PCONFIG-P.
     ‚* MEMBERDATEN ERMITTELN
           PERFORM  GET-MEMBER-DATEN.
     ‚* DEBUGGER AKTIV
           MOVE     1                       TO CFFK01 OF PCONFIG-P.
     ‚* SOURCE-TYP, Z.B. CBLLE
           MOVE     SRCTYPE OF RTVMBRD-REC  TO CFIN01 OF PCONFIG-P.
     ‚* BERECHTIGUNG?
           MOVE     "*USER"                 TO CFIN02 OF PCONFIG-P.
     ‚* RELEASE?
           MOVE     "*CURRENT"              TO CFIN03 OF PCONFIG-P.
     ‚* AKTIVIERUNGSGRUPPE?
           MOVE     "QILE"                  TO CFIN04 OF PCONFIG-P.
     ‚* OPTIMIERUNGSSTUFE?
           MOVE     "*NONE"                 TO CFIN05 OF PCONFIG-P.
     ‚* BINDERVERZEICHNIS
           MOVE     "APGGUI"                TO CFIN06 OF PCONFIG-P.
     ‚* SOURCE-BESCHREIBUNGSTEXT
           MOVE     MBRTEXT OF RTVMBRD-REC  TO CFTXT1 OF PCONFIG-P.
     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                  TO CFAUSR OF PCONFIG-P.
           MOVE     T-TERM                  TO CFABS  OF PCONFIG-P.
           MOVE     PGM-WRK                 TO CFAPGM OF PCONFIG-P.
     ‚* DATUM/UHRZEIT
           MOVE     DATE-8                  TO CFADTA OF PCONFIG-P.
           MOVE     TIME-6                  TO CFATIA OF PCONFIG-P.
           WRITE    PCONFIG-P.

       ENDE.
           CLOSE                 PCONFIG-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER MEMBER-DATEN
     ‚*--------------------------------------------------------------
       GET-MEMBER-DATEN SECTION.
       GET-MBR-00.

           INITIALIZE RTVMBRD-REC, RET-CODE.
           MOVE     X-CPFILE     TO FILENAME OF RTVMBRD-REC.
           MOVE     X-CPMBR      TO MBRNAME  OF RTVMBRD-REC.
           MOVE     X-CPLIB      TO OBJLIB   OF RTVMBRD-REC.
           CALL     "CLP0002" USING RTVMBRD-REC
                                    RET-CODE
           END-CALL.

       GET-MBR-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DIE DATEI GIBT
     ‚*--------------------------------------------------------------
       CHECK-TEILDATEI SECTION.
       CHK-TED-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-CPFILE     TO OBJ     OF CHKOBJ-REC.
           MOVE     X-CPMBR      TO MBR     OF CHKOBJ-REC.
           MOVE     X-CPLIB      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-TED-90
           END-IF.

       CHK-TED-90.
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
