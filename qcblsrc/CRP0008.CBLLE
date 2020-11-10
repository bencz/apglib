       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              CRP0008.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* SPERREN/ENTSPERREN EINER SOURCE                               *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  A. PIEGER                                    *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  08.01.2010                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  SPERREN/ENTSPERREN EINER SOURCE              *
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
       WORKING-STORAGE SECTION.

     ‚* STANDARDFELDER
           COPY WRKSTD     OF APG-QCPYSRC.

     ‚* STANDARDFELDER CL-PROGRAMME
           COPY WRKPGMPARM OF APG-QCPYSRC.

       01  PGM-WRK                       PIC X(10) VALUE "CRP0008".
       01  PGM-TYP                       PIC X(8)  VALUE "SEU   ".
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
       01  X-LCK-LIB                     PIC X(10).
       01  X-LCK-RCN                     PIC X(10).
       01  X-LCK-MBR                     PIC X(10).
       01  X-LCK-AKT                     PIC 9(1).
       01  X-RETCODE                     PIC X(1).
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION USING          X-LCK-LIB
                                         X-LCK-RCN
                                         X-LCK-MBR
                                         X-LCK-AKT
                                         X-RETCODE.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* FÜLLEN PGM-WRK UND AUSFÜHREN STANDARDROUTINEN
           MOVE     "CRP0008" TO PGM-WRK.

           INITIALIZE X-RETCODE.
     ‚* JE NACH AKTION ANDERES AUSFÜHREN
           EVALUATE X-LCK-AKT
                    WHEN ZEROES
                         PERFORM UNLOCK-SOURCE
                    WHEN 1
                         PERFORM LOCK-SOURCE
           END-EVALUATE.
           IF       RET-CODE NOT = SPACES
                    MOVE "1" TO X-RETCODE
           END-IF.

       ENDE.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* SOURCE SPERREN
     ‚*--------------------------------------------------------------
       LOCK-SOURCE SECTION.
       LCK-SRC-00.

           INITIALIZE RET-CODE.
     ‚* ALCOBJ ZUM SPERREN AUFRUFEN
           INITIALIZE ALCOBJ-REC.
           MOVE     X-LCK-LIB TO LIBNAME  OF ALCOBJ-REC.
           MOVE     X-LCK-RCN TO FILENAME OF ALCOBJ-REC.
           MOVE     X-LCK-MBR TO MBRNAME  OF ALCOBJ-REC.
           MOVE     "*FILE"   TO TYPNAME  OF ALCOBJ-REC.
           MOVE     "*EXCL"   TO SPRSTS   OF ALCOBJ-REC.
           MOVE     ZEROES    TO TIMEWAIT OF ALCOBJ-REC.
           CALL     "CLP0051" USING ALCOBJ-REC
                                    RET-CODE
           END-CALL.

       LCK-SRC-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* ENTSPERREN DER SOURCE
     ‚*--------------------------------------------------------------
       UNLOCK-SOURCE SECTION.
       ULK-SRC-00.

           INITIALIZE RET-CODE.
     ‚* DLCOBJ ZUM ENTSPERREN AUFRUFEN
           INITIALIZE DLCOBJ-REC.
           MOVE     X-LCK-LIB TO LIBNAME  OF DLCOBJ-REC.
           MOVE     X-LCK-RCN TO FILENAME OF DLCOBJ-REC.
           MOVE     X-LCK-MBR TO MBRNAME  OF DLCOBJ-REC.
           MOVE     "*FILE"   TO TYPNAME  OF DLCOBJ-REC.
           MOVE     "*EXCL"   TO SPRSTS   OF DLCOBJ-REC.
           CALL     "CLP0052" USING DLCOBJ-REC
                                    RET-CODE
           END-CALL.


       ULK-SRC-90.
           EXIT.
      /
