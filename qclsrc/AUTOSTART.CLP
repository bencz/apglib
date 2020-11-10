             PGM

             MONMSG     MSGID(CPF0000)

/* STARTPROGRAMM FÜR SYSEH02 AUFRUFEN                                */
             CALL       PGM(EHRHARDT/STARTEUP)

/* BREITER DEBUGGER                                                  */
             ADDENVVAR  ENVVAR(ILE_DEBUGGER_1) +
                          VALUE(ALLOW_WIDE_SCREEN) LEVEL(*JOB) +
                          REPLACE(*YES)

             ENDPGM
