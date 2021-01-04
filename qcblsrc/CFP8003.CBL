       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP8003.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN OB USER ANGEMELDET IST                                 *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  28.09.2009                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  PRÜFEN OB USER ANGEMELDET IST                *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON   GRUND DER ÄNDERUNG                           *
     ‚*                                                               *
     ‚*****************************************************************
     ‚*
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚* TEMPORÄRE DATEI PTMPJOB
     ‚*-------------------------------------------------------------
           SELECT PTMPJOB-DP
                  ASSIGN       TO  DATABASE-PTMPJOB
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- TEMPORÄRE PTMPJOB
       FD  PTMPJOB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTMPJOB-P.
           COPY DDS-ALL-FORMATS OF PTMPJOB.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM  OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP8003".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CPYSPLF-REC                   TYPE CPYSPLF-TYPE.
       01  CRTPF-REC                     TYPE CRTPF-TYPE.
       01  DLTF-REC                      TYPE DLTF-TYPE.
       01  WRKUSRJOB-REC                 TYPE WRKUSRJOB-TYPE.

     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
     ‚* SPACES = USER NICHT ANGEMELDET
     ‚* "1"    = USER ANGEMELDET
       01  X-STATUS                      LIKE RET-CODE.
     ‚*-------------------------------------------------------------

     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-STATUS.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP8003" TO PGM-WRK.

           INITIALIZE X-STATUS.
     ‚* ERSTMAL PRÜFEN ODER DER USER GÜLTIG IST
           IF       X-USER = SPACES
                    GO TO ENDE
           END-IF.

     ‚* VORSICHTSHALBER LÖSCHEN, FALLS EIN VORHERIGER DURCHGANG NICHT
     ‚* SAUBER BEENDET WURDE
           PERFORM  DELETE-TMPJOB.

     ‚* ERSTELLEN DER TEMP. DATEI FÜR DEN ZU KOPIERENDEN SPOOL
           PERFORM  CREATE-TEMPFILE.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* WRKUSRJOB AUSFÜHREN
           PERFORM  CREATE-WRKUSRJOB.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* DURCH WRKUSRJOB WURDE EIN SPOOLFILE ERSTELLT, DIESES JETZT
     ‚* IN EINE TEMPORÄRE PHYSISCHE DATEI KOPIEREN, "QPDSPSBJ"
           PERFORM  COPY-SPOOL.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* NACHDEM ES ERFOLGREICH KOPIERT WURDE, JETZT DIE DATEI ÖFFNEN
     ‚* UND DEN INHALT PRÜFEN
           OPEN     INPUT PTMPJOB-DP.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO ENDE
           END-IF.

     ‚* LESEN DES SPOOLFILES
           PERFORM  READ-SPOOLFILE.

     ‚* DATEI SCHLIEßEN UND ANSCHLIEßEND LÖSCHEN
           CLOSE    PTMPJOB-DP.

     ‚* JETZT LÖSCHEN DER PTMPJOB
           PERFORM  DELETE-TMPJOB.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* ERSTELLEN DER TEMP. DATEI
     ‚*--------------------------------------------------------------
       CREATE-TEMPFILE SECTION.
       CRT-TMF-00.

           INITIALIZE RET-CODE.
     ‚* ERSTELLEN DER "PTMPJOB"
           INITIALIZE CRTPF-REC.
           MOVE     "QTEMP"         TO TOLIB    OF CRTPF-REC.
           MOVE     "PTMPJOB"       TO TOPGM    OF CRTPF-REC.
           MOVE     132             TO RECSIZE  OF CRTPF-REC.
           CALL     "CLP0031" USING CRTPF-REC
                                    RET-CODE
           END-CALL.

       CRT-TMF-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* WRKUSRJOB AUSFÜHREN UND SPOOL ERSTELLEN
     ‚*--------------------------------------------------------------
       CREATE-WRKUSRJOB SECTION.
       CRT-WUJ-00.

           INITIALIZE RET-CODE.
     ‚* JETZT MITTELS WRKUSRJOB ERMITTELN OB DER USER ANGEMELDET IST
           INITIALIZE WRKUSRJOB-REC.
           MOVE     X-USER        TO TUSER     OF WRKUSRJOB-REC.
           MOVE     "*ACTIVE"     TO JOBSTATUS OF WRKUSRJOB-REC.
           MOVE     "*PRINT"      TO OUTOPT    OF WRKUSRJOB-REC.
           MOVE     "*INTERACT"   TO JOBTYPE   OF WRKUSRJOB-REC.
           CALL     "CLP0018" USING WRKUSRJOB-REC
                                    RET-CODE
           END-CALL.

       CRT-WUJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* SPOOL IN DIE TEMPORÄRE DATEI KOPIEREN
     ‚*--------------------------------------------------------------
       COPY-SPOOL SECTION.
       COP-SPL-00.

           INITIALIZE RET-CODE.
     ‚* CPYSPLF AUSFÜHREN
           INITIALIZE CPYSPLF-REC.
           MOVE     "QPDSPSBJ"    TO SPOOLNAME OF CPYSPLF-REC.
           MOVE     "QTEMP"       TO TOLIB     OF CPYSPLF-REC.
           MOVE     "PTMPJOB"     TO TOFILE    OF CPYSPLF-REC.
           MOVE     "*REPLACE"    TO MBROPT    OF CPYSPLF-REC.
           CALL     "CLP0046" USING CPYSPLF-REC
                                    RET-CODE
           END-CALL.

       COP-SPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DES SPOOLFILES
     ‚*--------------------------------------------------------------
       READ-SPOOLFILE SECTION.
       LES-SPL-00.

           INITIALIZE PTMPJOB-P.

       LES-SPL-20.
           READ     PTMPJOB-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-SPL-90
           END-IF.

     ‚* AN DER 30. STELLE STEHT DIE JOB-NUMMER DES AKTIVEN JOBS
     ‚* SOLLTE ES ALSO EINE JOB-NUMMER GEBEN, REICHT MIR DAS
           IF       PTMPJOB-P(30:6) IS NUMERIC
                    MOVE "1" TO X-STATUS
                    GO TO LES-SPL-90
           END-IF.

           GO TO    LES-SPL-20.

       LES-SPL-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LÖSCHEN DER TEMPORÄREN DATEI
     ‚*--------------------------------------------------------------
       DELETE-TMPJOB SECTION.
       DEL-TMP-00.

           INITIALIZE RET-CODE.
     ‚* DLTF AUSFÜHREN
           INITIALIZE DLTF-REC.
           MOVE     "PTMPJOB"       TO FILENAME OF DLTF-REC.
           MOVE     "QTEMP"         TO LIBNAME  OF DLTF-REC.
           CALL     "CLP0044" USING DLTF-REC
                                    RET-CODE
           END-CALL.

       DEL-TMP-90.
           EXIT.
      /
