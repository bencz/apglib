       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0040.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* TEMPORÄRE ISEU-DATEI ERSTELLEN                                *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  12.01.2012                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  TEMP. DATEI PTMPSEU ERSTELLEN                *
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

      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0040".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".

     ‚* PARAMETER FÜR CL-PROGRAMME
       01  CHKOBJ-REC                    TYPE CHKOBJ-TYPE.
       01  CRTPF-REC                     TYPE CRTPF-TYPE.

       01  TEMP-NAME                     PIC X(10) VALUE "PTMPSEU".
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* PRÜFEN OB DAS OBJEKT SCHON VORHANDEN IST
           PERFORM  CHECK-OBJECT.
           IF       RET-CODE NOT = SPACES
                    PERFORM CREATE-OBJECT
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DANN JETZT DAS OBJEKT ERSTELLEN
     ‚*--------------------------------------------------------------
       CREATE-OBJECT SECTION.
       CRT-OBJ-00.

           INITIALIZE CRTPF-REC.
           MOVE     "QTEMP"        TO TOLIB   OF CRTPF-REC.
           MOVE     TEMP-NAME      TO TOPGM   OF CRTPF-REC.
           MOVE     "004096"       TO RECSIZE OF CRTPF-REC.
           CALL     "CLP0031" USING CRTPF-REC
                                    RET-CODE
           END-CALL.

       CRT-OBJ-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* PRÜFEN OB PTMPSEU EXISTIERT
     ‚*--------------------------------------------------------------
       CHECK-OBJECT SECTION.
       CHK-OBJ-00.

           INITIALIZE RET-CODE.
     ‚* PRÜFEN OB OBJEKT VORHANDEN IST
           INITIALIZE CHKOBJ-REC.
           MOVE     "QTEMP"      TO LIB     OF CHKOBJ-REC.
           MOVE     TEMP-NAME    TO OBJ     OF CHKOBJ-REC.
           MOVE     "*FILE"      TO OBJTYPE OF CHKOBJ-REC.
           CALL     "CLP0004" USING CHKOBJ-REC
           END-CALL.
           IF       OBJ-EXIST OF CHKOBJ-REC = ZEROES
                    MOVE "1" TO RET-CODE
           END-IF.

       CHK-OBJ-90.
           EXIT.
      /
