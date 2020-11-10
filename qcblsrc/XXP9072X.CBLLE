      *PROCESS  XREF.
       IDENTIFICATION DIVISION.
      *
         PROGRAM-ID.              LGSTDTRG.
      *
     ‚*****************************************************************
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* COPYRIGHT BY  :  EHRHARDT + PARTNER  GMBH & CO. KG            *
     ‚*                  PLANUNG + REALISIERUNG EDV-GESTÜTZTER        *
     ‚*                  BETRIEBSABLÄUFE UND PROGRAMMIERUNG           *
     ‚*                  56154 BOPPARD-BUCHHOLZ                       *
     ‚*                  TEL 06742 / 87270                            *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :           B. GERLICH                          *
     ‚*                                                               *
     ‚* ERSTELLT AM   :           JULI 2003                           *
     ‚*                                                               *
     ‚*                                                               *
     ‚* PROBLEM       :           TRIGGERPROGRAMM FÜR DATEI           *
     ‚*                           PSTDLOG                             *
     ‚*                                                               *
     ‚* ÄNDERUNGEN:                                                   *
     ‚* DATUM      VON            GRUND DER ÄNDERUNG                  *
     ‚*                                                               *
     ‚*****************************************************************
      *
       ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
         SOURCE-COMPUTER.         IBM-AS400.
         OBJECT-COMPUTER.         IBM-AS400.
        SPECIAL-NAMES. , DECIMAL-POINT IS COMMA.
      /
       INPUT-OUTPUT SECTION.
         FILE-CONTROL.

     ‚*-------------------------------------------------------------
     ‚*    LOGDATEI
     ‚*-------------------------------------------------------------
           SELECT PDLTLOG-DATP
                  ASSIGN       TO  DATABASE-PDLTLOG
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS       IS  SEQUENTIAL
                  FILE STATUS  IS  FILE-STATUS.

      /

       DATA DIVISION.
       FILE SECTION.

     ‚*-----LOGDATEI--------------
       FD   PDLTLOG-DATP.
       01   PDLTLOG-RECP.
            COPY DDS-ALL-FORMATS OF PDLTLOG.
      /
       WORKING-STORAGE SECTION.

     ‚*-----Statusinformationen für die im Programm def. Dateien-----
       77   FILE-STATUS                  PIC XX.

       01  BUF-PTR USAGE IS POINTER.

       01  PSTDLOG-WRK.
           COPY  DDS-PSTDLOGF OF PSTDLOG.

       01   UHRZEIT.
          05  STUNDE                      PIC 9(2).
          05  MINUTE                      PIC 9(2).
          05  SEKUNDE                     PIC 9(2).
       01   UHRZEIT-RED REDEFINES UHRZEIT PIC 9(6).

       01   DATUM.
          05  JAHR                        PIC 9(2).
          05  MONAT                       PIC 9(2).
          05  TAG                         PIC 9(2).
       01   DATUM-RED REDEFINES DATUM     PIC 9(6).

     ‚*-------------------------------------------------------
       LINKAGE SECTION.
       COPY TRGBUF OF QSYSINC-QCBLLESRC.
              01  TRG-PARM2 PIC S9(00009) BINARY.
              01  OLDPSTDLOG.
                  COPY DDS-ALL-FORMATS OF PSTDLOG.
              01  NEWPSTDLOG.
                  COPY DDS-ALL-FORMATS OF PSTDLOG.
     ‚*-------------------------------------------------------
     ‚*-------------------------------------------------------
       PROCEDURE DIVISION USING  QDB-TRIGGER-BUFFER
                                 TRG-PARM2.
     ‚*-------------------------------------------------------
       STEUER SECTION.
       ANFANG.

           OPEN     EXTEND PDLTLOG-DATP.

     ‚* BERECHNEN DER ADRESSE DES ALT-RECORD-PUFFERS
     ‚* AUS ADRESSE DES ERSTEN PARMS (=BEGINN DER STATISCHEN INFO)
     ‚* + DEM OFFSET AUF OLD-RECORD
           SET      BUF-PTR TO ADDRESS OF QDB-TRIGGER-BUFFER.
           SET      BUF-PTR UP BY OLD-RECORD-OFFSET.
           SET      ADDRESS OF OLDPSTDLOG TO BUF-PTR.
     ‚* ENTSPRECHEND FÜR NEWPPTVANM
           SET      BUF-PTR TO ADDRESS OF QDB-TRIGGER-BUFFER.
           SET      BUF-PTR UP BY NEW-RECORD-OFFSET.
           SET      ADDRESS OF NEWPSTDLOG TO BUF-PTR.

     ‚* UMSETZUNG IN FELDDEFINIERTEN WORKBEREICH
           MOVE     CORR PSTDLOGF OF OLDPSTDLOG TO
                         PDLTLOGF OF PDLTLOG-RECP.

           ACCEPT   DATUM   FROM DATE.
           ACCEPT   UHRZEIT FROM TIME.

           MOVE     STUNDE  OF UHRZEIT TO SDDSTD OF PDLTLOG-RECP.
           MOVE     MINUTE  OF UHRZEIT TO SDDMIN OF PDLTLOG-RECP.
           MOVE     SEKUNDE OF UHRZEIT TO SDDSEK OF PDLTLOG-RECP.
           MOVE     JAHR    OF DATUM   TO SDDJA  OF PDLTLOG-RECP.
           MOVE     MONAT   OF DATUM   TO SDDMO  OF PDLTLOG-RECP.
           MOVE     TAG     OF DATUM   TO SDDTA  OF PDLTLOG-RECP.
           IF       JAHR    OF DATUM > 80
                    MOVE  "19"               TO SDDJH  OF PDLTLOG-RECP
           ELSE
                    MOVE  "20"               TO SDDJH  OF PDLTLOG-RECP
           END-IF.

           CALL     "RTVTERM" USING       SDDBS  OF PDLTLOG-RECP
                                          SDDUSR OF PDLTLOG-RECP
           END-CALL.
           WRITE    PDLTLOG-RECP.

           CLOSE               PDLTLOG-DATP.
           ENDE.
