       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CFP9012.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* PRÜFEN DER ROLLE                                              *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  15.06.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  ROLLEN-PRÜFUNG                               *
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
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*--- BERECHTIGUNGEN
       FD  PCFGAUT-DP
           LABEL RECORDS ARE STANDARD.
       01  PCFGAUT-P.
           COPY DDS-ALL-FORMATS OF PCFGAUT.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD      OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CFP9012".
       01  PGM-TYP                       PIC X(8)  VALUE "CONFIG".

       01  DATEI-OPEN                    PIC 9(1).
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-USER                        LIKE T-USER.
       01  X-ROLE                        PIC X(10).
       01  X-ROLE-CHECKSUM               PIC X(32).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING X-USER
                                X-ROLE
                                X-ROLE-CHECKSUM
                                X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK
           MOVE     "CFP9012 " TO PGM-WRK.
           PERFORM  COPY-PGM-INIT.

           MOVE     ZEROES TO DATEI-OPEN.
     ‚* IST DER X-USER <> T-USER VERSUCHT VIELLEICHT JEMAND MITTELS
     ‚* DEBUGGER DAS SYSTEM ZU UMGEHEN ...
           IF       X-USER NOT = T-USER
                    PERFORM POSSIBLE-BREAK
           END-IF.

     ‚* WENN DIE DATEI IN DER FALSCHEN LIB LIEGT, DANN AUCH ENDE
           PERFORM  CHECK-DATEI.

           OPEN     INPUT PCFGAUT-DP.
           MOVE     1     TO DATEI-OPEN.

           INITIALIZE X-RETCODE.

       ENDE.
           IF       DATEI-OPEN NOT = ZEROES
                    CLOSE PCFGAUT-DP
           END-IF.

           PERFORM  COPY-PGM-EXIT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB DER USER DAS PGM BENUTZEN DARF
     ‚*--------------------------------------------------------------
       CHECK-PROGRAMM SECTION.
       CHK-PGM-00.

     ‚* PRÜFEN DER BERECHTIGUNGEN FÜR EIN PROGRAMM
           INITIALIZE PCFGAUTF OF PCFGAUT-P.
           MOVE     "CHK-PGM"      TO CFID   OF PCFGAUT-P.
           MOVE     "TEST"         TO CFKEY  OF PCFGAUT-P.
           MOVE     X-USER         TO CFKEY2 OF PCFGAUT-P.
           MOVE     SPACES         TO CFKEY3 OF PCFGAUT-P.
           MOVE     SPACES         TO CFKEY4 OF PCFGAUT-P.
           READ     PCFGAUT-DP.
           IF       FILE-STATUS NOT = ZEROES
     ‚*             JETZT FÜR ALLE USER PRÜFEN
                    INITIALIZE PCFGAUTF OF PCFGAUT-P
                    MOVE "CHK-PGM" TO CFID   OF PCFGAUT-P
                    MOVE "TEST"    TO CFKEY  OF PCFGAUT-P
                    MOVE "*ALL"    TO CFKEY2 OF PCFGAUT-P
                    MOVE SPACES    TO CFKEY3 OF PCFGAUT-P
                    MOVE SPACES    TO CFKEY4 OF PCFGAUT-P
                    READ PCFGAUT-DP
                    IF   FILE-STATUS NOT = ZEROES
                         GO TO CHK-PGM-90
                    END-IF
           END-IF.

       CHK-PGM-90.
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
     ‚*             USER EINTRAGEN
                    MOVE    "PCFGAUT"    TO MSGTXT OF SNDMSG-REC(13:10)
     ‚*             USER AN DEM DIE NACHRICHT GEHT
                    MOVE    "APG"        TO USER OF SNDMSG-REC
     ‚*             SENDEN DER NACHRICHT
                    CALL    "CLP0010" USING SNDMSG-REC
                                            RET-CODE
                    END-CALL
     ‚*             JETZT ABBRECHEN
                    CALL    "CFP9107" IN LIBRARY CONST-LIB
                    END-CALL
           END-IF.

       CHK-TED-90.
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
     ‚* PGM SOWIE DOPPELPUNKT
           MOVE     PGM-WRK      TO MSGTXT OF SNDMSG-REC(1:10).
           MOVE     ":"          TO MSGTXT OF SNDMSG-REC(11:1).
     ‚* USER EINTRAGEN
           MOVE     T-USER       TO MSGTXT OF SNDMSG-REC(13:10).
           MOVE     "/"          TO MSGTXT OF SNDMSG-REC(24:1).
           MOVE     X-USER       TO MSGTXT OF SNDMSG-REC(26:10).
     ‚* USER AN DEM DIE NACHRICHT GEHT
           MOVE     "APG"        TO USER OF SNDMSG-REC.

           CALL     "CLP0010" USING SNDMSG-REC
                                    RET-CODE
           END-CALL.

     ‚* JETZT ABBRECHEN
           CALL     "CFP9107" IN LIBRARY CONST-LIB
           END-CALL.

       POS-BRK-90.
           EXIT.
      /
     ‚*---------------------------------------------------------------
     ‚* COPY-PGM-INIT/COPY-PGM-EXIT: STANDARDROUTINEN START UND ENDE
     ‚*---------------------------------------------------------------

           COPY     PGMWRK     OF APG-QCPYSRC.
      /
