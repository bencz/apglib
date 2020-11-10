       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CMP0005.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* FÜLLEN PCFGFFD                                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  29.10.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  FÜLLEN PCFGFFD NACH CP                       *
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
     ‚* PHYSISCHE DATEI PCFGFFD
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DP
                  ASSIGN       TO  DATABASE-PCFGFFD
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI PCFGFFD NACH LIB/FILE
     ‚*-------------------------------------------------------------
           SELECT PCFGFFD-DL
                  ASSIGN       TO  DATABASE-PCFGFFDL1
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* TEMPORÄRE DATEI PTMPFFD
     ‚*-------------------------------------------------------------
           SELECT PTMPFFD-DP
                  ASSIGN       TO  DATABASE-PTMPFFD
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PCFGFFD
       FD  PCFGFFD-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-P.
           COPY DDS-ALL-FORMATS OF PCFGFFD.
      /
     ‚*--- PCFGFFD NACH LIB/FILE
       FD  PCFGFFD-DL
           LABEL RECORDS ARE STANDARD.
       01  PCFGFFD-L.
           COPY DDS-ALL-FORMATS OF PCFGFFDL1.
      /
     ‚*--- TEMPORÄRE PTMPFFD
       FD  PTMPFFD-DP
           LABEL RECORDS ARE STANDARD.
       01  PTMPFFD-P.
           COPY DDS-ALL-FORMATS OF PTMPFFD.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CMP0005".
       01  PGM-TYP                       PIC X(8)  VALUE "COMPILER".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  DSPFFD-REC                    TYPE DSPFFD-TYPE.
       01  DLTF-REC                      TYPE DLTF-TYPE.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-CPLIB                       PIC X(10).
       01  X-CPFILE                      PIC X(10).
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-CPLIB
                                X-CPFILE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CMP0005" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           OPEN     I-O   PCFGFFD-DP.
           OPEN     INPUT PCFGFFD-DL.

     ‚* ALLE PARAMETER VORHANDEN?
           IF       X-CPLIB  = SPACES
            OR      X-CPFILE = SPACES
                    GO TO ENDE
           END-IF.

     ‚* PRÜFEN OB ES DAS OBJEKT GIBT
           PERFORM  CHECK-OBJECT.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT ERSTMAL ALLE WERTE LÖSCHEN
           PERFORM  DELETE-XMLFFD.

     ‚* JETZT DEN NEUEN DSPFFD AUSFÜHREN; BEI EINEM FEHLER IN DSPFFD
     ‚* SIND DANN ZWAR ALLE EINTRÄGE WEG, ABER SO WEIß MAN EHER DAS
     ‚* HIER ETWAS SCHIEF GEGANGEN IST, ALSO WENN DIE ALTEN UNGÜLTIGEN
     ‚* EINTRÄGE STEHEN BLEIBEN
           PERFORM  CREATE-DSPFFD.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* JETZT DIE NEUE DATEI ÖFFNEN
           OPEN     INPUT PTMPFFD-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* UND JETZT DIE NEUE TEMPORÄRE DATEI ÖFFNEN UND IN DIE
     ‚* PCFGFFD SCHREIBEN
           PERFORM  UPDATE-XMLFFD.

           CLOSE    PTMPFFD-DP.

     ‚* JETZT LÖSCHEN DER PTMPFFD
           PERFORM  DELETE-TMPFFD.

       ENDE.
           CLOSE                 PCFGFFD-DP
                                 PCFGFFD-DL.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER BISHERIGEN PCFGFFD-EINTRÄGE
     ‚*--------------------------------------------------------------
       DELETE-XMLFFD SECTION.
       DEL-FFD-00.

           INITIALIZE PCFGFFDF OF PCFGFFD-L.
           MOVE     X-CPLIB             TO WHLIB  OF PCFGFFD-L.
           MOVE     X-CPFILE            TO WHFILE OF PCFGFFD-L.
           MOVE     SPACES              TO WHNAME OF PCFGFFD-L.
           MOVE     SPACES              TO WHFLDE OF PCFGFFD-L.
           START    PCFGFFD-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-FFD-90
           END-IF.

       DEL-FFD-20.
           READ     PCFGFFD-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO DEL-FFD-90
           END-IF.

           IF       WHLIB  OF PCFGFFD-L NOT = X-CPLIB
            OR      WHFILE OF PCFGFFD-L NOT = X-CPFILE
                    GO TO DEL-FFD-90
           END-IF.

           INITIALIZE PCFGFFDF OF PCFGFFD-P.
           MOVE     WHLFDN OF PCFGFFD-L TO WHLFDN OF PCFGFFD-P.
           READ     PCFGFFD-DP.
           IF       FILE-STATUS = ZEROES
                    DELETE PCFGFFD-DP
           END-IF.

           GO TO    DEL-FFD-20.

       DEL-FFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TEMPORÄREN DATEI
     ‚*--------------------------------------------------------------
       DELETE-TMPFFD SECTION.
       DEL-TMP-00.

           INITIALIZE RET-CODE.
     ‚* DLTF AUSFÜHREN
           INITIALIZE DLTF-REC.
           MOVE     "PTMPFFD"       TO FILENAME OF DLTF-REC.
           MOVE     "QTEMP"         TO LIBNAME  OF DLTF-REC.
           CALL     "CLP0044" USING DLTF-REC
                                    RET-CODE
           END-CALL.

       DEL-TMP-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* AUSFÜHREN DES DSPFFD IN DIE QTEMP
     ‚*--------------------------------------------------------------
       CREATE-DSPFFD SECTION.
       CRT-FFD-00.

           INITIALIZE RET-CODE.
     ‚* DSPFFD AUSFÜHREN
           INITIALIZE DSPFFD-REC.
           MOVE     X-CPFILE        TO FILENAME OF DSPFFD-REC.
           MOVE     X-CPLIB         TO LIBNAME  OF DSPFFD-REC.
           MOVE     "PTMPFFD"       TO OUTFILE  OF DSPFFD-REC.
           MOVE     "QTEMP"         TO OUTLIB   OF DSPFFD-REC.
           CALL     "CLP0043" USING DSPFFD-REC
                                    RET-CODE
           END-CALL.

       CRT-FFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* JETZT DIE NEUEN PCFGFFD-EINTRÄGE SCHREIBEN
     ‚*--------------------------------------------------------------
       UPDATE-XMLFFD SECTION.
       UPD-FFD-00.

           INITIALIZE QWHDRFFD OF PTMPFFD-P.

       UPD-FFD-20.
           READ     PTMPFFD-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO UPD-FFD-90
           END-IF.

           IF       WHLIB  OF PTMPFFD-P NOT = X-CPLIB
            OR      WHFILE OF PTMPFFD-P NOT = X-CPFILE
                    GO TO UPD-FFD-20
           END-IF.

     ‚* JETZT DEN DATENSATZ SCHREIBEN
           PERFORM  WRITE-XMLFFD.

           GO TO    UPD-FFD-20.

       UPD-FFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* EINEN DATENSATZ SCHREIBEN
     ‚*--------------------------------------------------------------
       WRITE-XMLFFD SECTION.
       WRI-FFD-00.

     ‚* KEIN DATENSATZ VORHANDEN
           INITIALIZE PCFGFFDF OF PCFGFFD-P.
     ‚* KEY FÜLLEN
           INITIALIZE LFDN-ID, LFDN-WRK.
           MOVE     "PCFGFFD" TO LFDN-ID.
           CALL     "CFP0020" USING LFDN-ID
                                    LFDN-WRK
           END-CALL.
           MOVE     LFDN-WRK                TO WHLFDN OF PCFGFFD-P.
     ‚* DATEN AUS DER PTMPFFD FÜLLEN
           MOVE     CORR QWHDRFFD OF PTMPFFD-P TO
                         PCFGFFDF OF PCFGFFD-P.
     ‚* USER FÜLLEN
           PERFORM  COPY-GET-TIME.
           MOVE     T-USER                  TO WHAUSR OF PCFGFFD-P.
           MOVE     T-TERM                  TO WHABS  OF PCFGFFD-P.
           MOVE     PGM-WRK                 TO WHAPGM OF PCFGFFD-P.
     ‚* DATUM/UHRZEIT
           MOVE     DATE-8                  TO WHADTA OF PCFGFFD-P.
           MOVE     TIME-6                  TO WHATIA OF PCFGFFD-P.
           WRITE    PCFGFFD-P.

       WRI-FFD-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB ES DAS OBJEKT GIBT
     ‚*--------------------------------------------------------------
       CHECK-OBJECT SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     X-CPFILE     TO OBJ     OF CHKOBJ-REC.
           MOVE     X-CPLIB      TO LIB     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
                    GO TO CHK-OBJ-90
           END-IF.

       CHK-OBJ-90.
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
