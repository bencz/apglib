       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              HLP0017.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* HERMES AKTUALISIEREN                                          *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  09.04.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  HERMES MIT NEUER LIB AKTUALISIEREN           *
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
     ‚* PHYSISCHE DATEI PROGRAMME
     ‚*-------------------------------------------------------------
           SELECT PPGMLIB-DP
                  ASSIGN       TO  DATABASE-PPGMLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DP
                  ASSIGN       TO  DATABASE-PTAGLIB
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* PHYSISCHE DATEI TEXTE
     ‚*-------------------------------------------------------------
           SELECT PPGMLST-DP
                  ASSIGN       TO  DATABASE-PPGMLST
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.

     ‚*-------------------------------------------------------------
     ‚* LOGISCHE DATEI TAGS
     ‚*-------------------------------------------------------------
           SELECT PTAGLIB-DL
                  ASSIGN       TO  DATABASE-PTAGLIBL2
                  ORGANIZATION IS  INDEXED
                  RECORD KEY   IS  EXTERNALLY-DESCRIBED-KEY
                                   WITH DUPLICATES
                  ACCESS       IS  DYNAMIC
                  FILE STATUS  IS  FILE-STATUS.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- PPGMLIB: PLLIB, PLFILE
       FD  PPGMLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLIB-P.
           COPY DDS-ALL-FORMATS OF PPGMLIB.
      /
     ‚*--- PTAGLIB: PTLFDN
       FD  PTAGLIB-DP
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-P.
           COPY DDS-ALL-FORMATS OF PTAGLIB.
      /
     ‚*--- PTAGLIBL2: PTLIB, PTFILE, PTTAG
       FD  PTAGLIB-DL
           LABEL RECORDS ARE STANDARD.
       01  PTAGLIB-L.
           COPY DDS-ALL-FORMATS OF PTAGLIBL2.
      /
     ‚*--- PPGMLST: PSLIB, PSFILE, PSLFPO
       FD  PPGMLST-DP
           LABEL RECORDS ARE STANDARD.
       01  PPGMLST-P.
           COPY DDS-ALL-FORMATS OF PPGMLST.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* WORKBEREICH ÜBERGABEPARAMETER
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "HLP0017".
       01  PGM-TYP                       PIC X(8)  VALUE "HILFE".

       01  NEW-LIB                       LIKE PLLIB  OF PPGMLIB-P.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-OLD-VERSION                 PIC X(1).
       01  X-NEW-VERSION                 PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-OLD-VERSION
                                X-NEW-VERSION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "HLP0017" TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

     ‚* DATEIEN ÖFFNEN
           OPEN     I-O   PPGMLIB-DP
                          PTAGLIB-DP
                          PPGMLST-DP.
           OPEN     INPUT PTAGLIB-DL.

     ‚* DARF DIESES PROGRAMM BENUTZT WERDEN?
           PERFORM  COPY-PERMISSION.
           IF       RET-CODE NOT = SPACES
                    GO TO ENDE
           END-IF.

     ‚* ÄNDENR DER DATEN
           IF       X-OLD-VERSION NOT = SPACES
            AND     X-NEW-VERSION NOT = SPACES
                    PERFORM LESEN-PGMLIB
           END-IF.

       ENDE.
           CLOSE    PPGMLIB-DP
                    PTAGLIB-DP
                    PTAGLIB-DL
                    PPGMLST-DP.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN ALLER PROGRAMME UND ÄNDERN DER DATEN
     ‚*--------------------------------------------------------------
       LESEN-PGMLIB SECTION.
       LES-PGM-00.

     ‚* PROGRAMME LESEN
           INITIALIZE PPGMLIBF OF PPGMLIB-P.
           MOVE     SPACES              TO PLLIB  OF PPGMLIB-P.
           MOVE     SPACES              TO PLFILE OF PPGMLIB-P.
           START    PPGMLIB-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-PGM-90
           END-IF.

       LES-PGM-20.
           READ     PPGMLIB-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO LES-PGM-90
           END-IF.

     ‚* NUR LFS-BIBLIOTHEKEN ÄNDERN
           IF       PLLIB  OF PPGMLIB-P(1:3) NOT = "LFS"
            OR      PLLIB  OF PPGMLIB-P(8:1) NOT = X-OLD-VERSION
                    REWRITE PPGMLIB-P
                    GO TO LES-PGM-20
           END-IF.

     ‚* NEUEN BIBLIOTHEKSNAMEN ZUSAMMENBAUEN
           INITIALIZE NEW-LIB.
           MOVE     PLLIB  OF PPGMLIB-P TO NEW-LIB.
           MOVE     X-NEW-VERSION       TO NEW-LIB(8:1).
           PERFORM  GET-OBJDATEN.

     ‚* ERST IN DEN ANDEREN DATEIEN ÄNDENR
           PERFORM  CHANGE-PTAGLIB.
           PERFORM  CHANGE-PPGMLST.

     ‚* ALS LETZES VERSION IN DER PGMLIB ÄNDERN
           DELETE   PPGMLIB-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE  NEW-LIB TO PLLIB  OF PPGMLIB-P
                    IF   OBJTEXT OF RTVOBJD-REC NOT = SPACES
                         MOVE OBJATR  OF RTVOBJD-REC TO
                              PLSEU2  OF PPGMLIB-P
                         MOVE OBJTEXT OF RTVOBJD-REC TO
                              PLMTXT  OF PPGMLIB-P
                    END-IF
                    WRITE PPGMLIB-P
           END-IF.

           GO TO    LES-PGM-20.

       LES-PGM-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DER DAZUGEHÖRENDEN TAGS
     ‚*--------------------------------------------------------------
       CHANGE-PTAGLIB SECTION.
       CHG-TAG-00.

           INITIALIZE PTAGLIBF OF PTAGLIB-L.
           MOVE     PLLIB  OF PPGMLIB-P TO PTLIB  OF PTAGLIB-L.
           MOVE     PLFILE OF PPGMLIB-P TO PTFILE OF PTAGLIB-L.
           MOVE     SPACES              TO PTTAG  OF PTAGLIB-L.
           START    PTAGLIB-DL KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-TAG-90
           END-IF.

       CHG-TAG-20.
           READ     PTAGLIB-DL NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-TAG-90
           END-IF.

           IF       PTLIB  OF PTAGLIB-L NOT = PLLIB  OF PPGMLIB-P
            OR      PTFILE OF PTAGLIB-L NOT = PLFILE OF PPGMLIB-P
                    GO TO CHG-TAG-90
           END-IF.

     ‚* JETZT ÜBER DIE PHYSISCHE ÄNDERN
           INITIALIZE PTAGLIBF OF PTAGLIB-P.
           MOVE     PTLFDN OF PTAGLIB-L TO PTLFDN OF PTAGLIB-P.
           READ     PTAGLIB-DP
           IF       FILE-STATUS = ZEROES
                    MOVE  NEW-LIB TO PTLIB  OF PTAGLIB-P
                    REWRITE PTAGLIB-P
           END-IF.

           GO TO    CHG-TAG-20.

       CHG-TAG-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* LESEN DER DAZUGEHÖRENDEN TEXTE
     ‚*--------------------------------------------------------------
       CHANGE-PPGMLST SECTION.
       CHG-LST-00.

           INITIALIZE PPGMLSTF OF PPGMLST-P.
           MOVE     PLLIB  OF PPGMLIB-P TO PSLIB  OF PPGMLST-P.
           MOVE     PLFILE OF PPGMLIB-P TO PSFILE OF PPGMLST-P.
           MOVE     ZEROES              TO PSLFPO OF PPGMLST-P.
           START    PPGMLST-DP KEY NOT < EXTERNALLY-DESCRIBED-KEY.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-LST-90
           END-IF.

       CHG-LST-20.
           READ     PPGMLST-DP NEXT RECORD.
           IF       FILE-STATUS NOT = ZEROES
                    GO TO CHG-LST-90
           END-IF.

           IF       PSLIB  OF PPGMLST-P NOT = PLLIB  OF PPGMLIB-P
            OR      PSFILE OF PPGMLST-P NOT = PLFILE OF PPGMLIB-P
                    REWRITE PPGMLST-P
                    GO TO CHG-LST-90
           END-IF.

     ‚* ALS LETZES VERSION IN DER PGMLIB ÄNDERN
           DELETE   PPGMLST-DP.
           IF       FILE-STATUS = ZEROES
                    MOVE  NEW-LIB TO PSLIB  OF PPGMLST-P
                    WRITE PPGMLST-P
           END-IF.

           GO TO    CHG-LST-20.

       CHG-LST-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ERMITTELN DER OBJEKTDATEN
     ‚*--------------------------------------------------------------
       GET-OBJDATEN SECTION.
       GET-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* ERMITTELN DES PROGRAMMTEXTES
           INITIALIZE RTVOBJD-REC.
           MOVE     NEW-LIB             TO OBJLIB   OF RTVOBJD-REC.
           MOVE     PLFILE OF PPGMLIB-P TO OBJ      OF RTVOBJD-REC.
           MOVE     "*PGM"              TO OBJTYPE  OF RTVOBJD-REC.
           CALL     "CLP0006" USING RTVOBJD-REC
                                    RET-CODE
           END-CALL.
           IF       RET-CODE NOT = SPACES
                    INITIALIZE RTVOBJD-REC
                    MOVE NEW-LIB             TO OBJLIB   OF RTVOBJD-REC
                    MOVE PLFILE OF PPGMLIB-P TO OBJ      OF RTVOBJD-REC
                    MOVE "*MODULE"           TO OBJTYPE  OF RTVOBJD-REC
                    CALL "CLP0006" USING RTVOBJD-REC
                                         RET-CODE
                    END-CALL
                    IF   RET-CODE NOT = SPACES
                         INITIALIZE RTVOBJD-REC
                         MOVE NEW-LIB        TO OBJLIB   OF RTVOBJD-REC
                         MOVE PLFILE OF PPGMLIB-P TO
                              OBJ      OF RTVOBJD-REC
                         MOVE "*FILE"        TO OBJTYPE  OF RTVOBJD-REC
                         CALL "CLP0006" USING RTVOBJD-REC
                                              RET-CODE
                         END-CALL
                         IF   RET-CODE NOT = SPACES
                              INITIALIZE RTVOBJD-REC
                         END-IF
                    END-IF
           END-IF.

       GET-OBJ-90.
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
