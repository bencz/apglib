     ˆ* BAUSTELLE
       IDENTIFICATION DIVISION.
     ‚*
         PROGRAM-ID.              SCPGM031.
           COPY COPYRIGHT  OF APG-QCPYSRC.
     ‚*
     ‚*---------------------------------------------------------------*
     ‚* LAGERFÜHRUNGSSYSTEM  LFS 400                                  *
     ‚*---------------------------------------------------------------*
     ‚* COPYRIGHT BY  :  EHRHARDT + PARTNER GMBH & CO. KG             *
     ‚*                  SOFTWARE-SYSTEME FÜR WAREHOUSE-LOGISTIK      *
     ‚*                  56154 BOPPARD-BUCHHOLZ                       *
     ‚*                  +49 (0) 67 42 / 87 27 0                      *
     ‚*                  INFO@EHRHARDT-PARTNER.COM                    *
     ‚*---------------------------------------------------------------*
     ‚*                                                               *
     ‚* AUTOR         :  XXXXXXXXXX                                   *
     ‚*                                                               *
     ‚* ERSTELLT AM   :  XXXXX JJJJ                                   *
     ‚*                                                               *
     ‚* FUNKTION      :  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
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
     ‚*-   INFORMATIONSBLATT
     ‚*-------------------------------------------------------------
           SELECT DRUCK-DAT
                  ASSIGN       TO  FORMATFILE-SCPRT031
                  ORGANIZATION IS  SEQUENTIAL
                  ACCESS MODE  IS  SEQUENTIAL.
      /
       DATA DIVISION.
       FILE SECTION.
     ‚*----PRINTERFILE
       FD  DRUCK-DAT
           LABEL RECORDS ARE OMITTED.
       01  DRUCK-REC.
           COPY DDS-ALL-FORMATS OF SCPRT031.
      /
       WORKING-STORAGE SECTION.

       01  FORMAT1X.
           COPY  DDS-FMT01-O OF  SCPRT031
           REPLACING FMT01-O BY FORMAT1
                     FMT01-O-INDIC BY FORMAT1-O-INDIC.

       01  FORMAT2X.
           COPY  DDS-FMT02-O OF  SCPRT031
           REPLACING FMT02-O BY FORMAT2
                     FMT02-O-INDIC BY FORMAT2-O-INDIC.

       01  OVRPRTF.
        05  OVR-1                        PIC X(31)
            VALUE "OVRPRTF FILE(SCPRT031) TOFILE(".
        05  OVR-FILE                     PIC X(10).
        05  OVR-3                        PIC X(2) VALUE ") ".
        05  OVR-4                        PIC X(6) VALUE "OUTQ(".
        05  OVR-OUTQ                     PIC X(10).
        05  OVR-6                        PIC X(1) VALUE ")".

       01  OVR-LAENGE                    PIC 9(10)V9(5) COMP-3.
      /
     ‚*-------------------------------------------------------------
       LINKAGE SECTION.
     ‚*-------------------------------------------------------------
       PROCEDURE DIVISION.
     ‚*-------------------------------------------------------------
       STEUER SECTION.
       ANFANG.

     ‚* OUTQ ÄNDERN
           MOVE      "SCPRT031"  TO OVR-FILE OF OVRPRTF.
     ‚* BITTE IMMER MIT TEAM IAW ABSTIMMEN OB DIE OUTQ GENUTZT
     ‚* WERDEN KANN, GGF. AUF PRTLAN06 AUSWEICHEN
           MOVE      "PRTWELS01" TO OVR-OUTQ OF OVRPRTF.
           MOVE      60          TO OVR-LAENGE.
           CALL      "QCMDEXC"  USING   OVRPRTF
                                        OVR-LAENGE
           END-CALL.

     ‚* ALLE DATEIEN ÖFFNEN
           OPEN     OUTPUT DRUCK-DAT.

     ‚* ALLES INITIALISIEREN
           INITIALIZE FORMAT1.
           INITIALIZE FORMAT2.

     ‚* ERSTES FORMAT DRUCKEN
           PERFORM  DRUCKE-FORMAT1.

     ‚* ZWEITES FORMAT DRUCKEN
           PERFORM  DRUCKE-FORMAT2.

       ENDE.
           CLOSE                 DRUCK-DAT.

           GOBACK.
      /
     ‚*--------------------------------------------------------------
     ‚* DRUCKE FORMAT1
     ‚*--------------------------------------------------------------
       DRUCKE-FORMAT1 SECTION.
       DRU-FM1-00.

           INITIALIZE FORMAT1.
           MOVE       CORR FORMAT1 TO FMT01-O.
           WRITE      DRUCK-REC FORMAT IS "FMT01".

       DRU-FM1-90.
           EXIT.
      /
     ‚*--------------------------------------------------------------
     ‚* DRUCKE FORMAT2
     ‚*--------------------------------------------------------------
       DRUCKE-FORMAT2 SECTION.
       DRU-FM2-00.

           INITIALIZE FORMAT2.
           MOVE       921003118889 TO LFDN OF FORMAT2.
           MOVE       CORR FORMAT2 TO FMT02-O.
           WRITE      DRUCK-REC FORMAT IS "FMT02".

       DRU-FM2-90.
           EXIT.
      /
