             PGM

             DCL        VAR(&LIBL) TYPE(*CHAR) LEN(250)
             DCL        VAR(&CMD) TYPE(*CHAR) LEN(2760)

             MONMSG     MSGID(CPF0000)

             RTVSYSVAL  SYSVAL(QUSRLIBL) RTNVAR(&LIBL)
             CHGVAR     VAR(&CMD) VALUE('CHGLIBL (' *CAT &LIBL *TCAT +
                          ')')
             CALL       PGM(QCMDEXC) PARM(&CMD 2760)

             DLTDTAARA  DTAARA(QTEMP/LFSSPRACHE)
             /*RCLACTGRP  ACTGRP(*ELIGIBLE)*/
             RCLRSC     LVL(*CALLER)
             RCLRSC     LVL(*)

             RCLACTGRP  ACTGRP(QILE)

/*‚CCSID UND LANGID LAUT USRPRF SETZEN                               */
             CHGJOB     SRTSEQ(*USRPRF) LANGID(*USRPRF) +
                          CNTRYID(*USRPRF) CCSID(*USRPRF)

             ENDPGM
