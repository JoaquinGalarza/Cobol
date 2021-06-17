 000001        IDENTIFICATION DIVISION.
 000002       *                                                        *
 000003        PROGRAM-ID PGMDB233.
 000004       **********************************************************
 000005       *                                                        *
 000006       *                                                        *
 000007       *                                                        *
 000008       **********************************************************
 000009       *      MANTENIMIENTO DE PROGRAMA                         *
 000010       **********************************************************
 000011       *  FECHA   *    DETALLE        * COD *
 000012       **************************************
 000013       *          *                   *     *
 000014       *          *                   *     *
 000015       **************************************
 000016        ENVIRONMENT DIVISION.
 000017        CONFIGURATION SECTION.
 000018        SPECIAL-NAMES.
 000019            DECIMAL-POINT IS COMMA.
 000020
 000021        INPUT-OUTPUT SECTION.
 000022        FILE-CONTROL.
 000023
 000024              SELECT PERSONA ASSIGN DDPERSO
 000025              ORGANIZATION IS INDEXED
 000026              ACCESS MODE IS SEQUENTIAL
 000027              RECORD KEY IS PER-KEY
 000028                     FILE STATUS IS WS-PER-CODE.
 000029
 000030        DATA DIVISION.
 000031        FILE SECTION.
 000032        FD PERSONA.
 000033
 000034        01 REG-PERSO.
 000035           03 PER-KEY.
 000036              05 PER-TIP-DOC PIC X(02).
 000037              05 PER-NRO-DOC PIC 9(11).
 000038           03 FILLER       PIC X(147).
 000039
 000040       **************************************
 000041        WORKING-STORAGE SECTION.
 000042       **************************************
 000043        77  FILLER         PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.
 000044
 000045        01 WS-STATUS-CODES.
 000046           03  WS-PER-CODE         PIC XX          VALUE SPACES.
 000047           03  WS-SQLCODE          PIC S9(03)      VALUE ZEROS.
 000048
 000049        01 WS-COMP-NRO-DOC         PIC S9(11)V  COMP-3 VALUE ZEROS.
 000050
 000051        01 WS-STATUS-FIN           PIC X        VALUE SPACES.
 000052           88  WS-FIN-LECTURA                   VALUE 'Y'.
 000053           88  WS-NO-FIN-LECTURA                VALUE 'N'.
 000054
 000055        01 WS-CONTADORES.
 000056           03  WS-DISPLAYADOS       PIC 9(03)    VALUE ZEROS.
 000057           03  WS-LEIDOS-VSAM       PIC 9(03)    VALUE ZEROS.
 000058           03  WS-LEIDOS-SQL        PIC 9(03)    VALUE ZEROS.
 000059           03  WS-NOTFOUND          PIC 9(03)    VALUE ZEROS.
 000060
 000061        01  REG-PERSONA.
 000062            03  KEY-TIP-DOC            PIC X(02).
 000063            03  KEY-NRO-DOC            PIC 9(11).
 000064            03  FILLER                 PIC X(147).
 000065
 000066        77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.
 000067
 000068             EXEC SQL
 000069               INCLUDE SQLCA
 000070             END-EXEC.
 000071
 000072             EXEC SQL
 000073               INCLUDE TBPERSO
 000074             END-EXEC.
 000075
 000076        77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'.
 000077
 000078       ***************************************************************.
 000079        PROCEDURE DIVISION.
 000080       **************************************
 000081       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000082       **************************************
 000083        MAIN-PROGRAM.
 000084
 000085            PERFORM 1000-I-INICIO   THRU
 000086                    1000-F-INICIO.
 000087
 000088            PERFORM 2000-I-PROCESO  THRU
 000089                    2000-F-PROCESO        UNTIL WS-FIN-LECTURA.
 000090
 000091            PERFORM 9999-I-FINAL    THRU
 000092                    9999-F-FINAL.
 000093
 000094        F-MAIN-PROGRAM. GOBACK.
 000095
 000096       **************************************
 000097       *  CUERPO INICIO APERTURA ARCHIVOS   *
 000098       **************************************
 000099        1000-I-INICIO.
 000100
 000101            SET WS-NO-FIN-LECTURA TO TRUE.
 000102
 000103            OPEN INPUT PERSONA.
 000104            IF WS-PER-CODE    IS NOT EQUAL '00'
 000105               DISPLAY '* ERROR EN OPEN PERSONA = ' WS-PER-CODE
 000106               MOVE 9999 TO RETURN-CODE
 000107               SET  WS-FIN-LECTURA TO TRUE
 000108            END-IF.
 000109
 000110
 000111            IF SQLCODE NOT EQUAL ZEROS
 000112               MOVE SQLCODE   TO WS-SQLCODE
 000113               DISPLAY '* ERROR OPEN CURSOR      = ' WS-SQLCODE
 000114               MOVE 9999 TO RETURN-CODE
 000115               SET  WS-FIN-LECTURA TO TRUE
 000116            END-IF.
 000117
 000118        1000-F-INICIO. EXIT.
 000119
 000120       **************************************
 000121       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000122       **************************************
 000123        2000-I-PROCESO.
 000124
 000125            PERFORM 3000-I-LEER-VSAM THRU 3000-F-LEER-VSAM.
 000126
 000127            PERFORM 3500-I-LEER-SQL  THRU 3500-F-LEER-SQL.
 000128
 000129            IF SQLCODE EQUAL ZEROES
 000130
 000131              PERFORM 4000-I-DISPLAY   THRU 4000-F-DISPLAY
 000132
 000133            END-IF.
 000134
 000135        2000-F-PROCESO. EXIT.
 000136
 000137       **************************************
 000138       *           LEER CURSOR              *
 000139       **************************************
 000140        3000-I-LEER-VSAM.
 000141
 000142            READ PERSONA INTO REG-PERSONA.
 000143
 000144
 000145            EVALUATE WS-PER-CODE
 000146
 000147            WHEN '00'
 000148               ADD 1 TO WS-LEIDOS-VSAM
 000149               MOVE KEY-TIP-DOC TO PER-TIP-DOC
 000150               MOVE KEY-NRO-DOC TO WS-COMP-NRO-DOC
 000151
 000152            WHEN '10'
 000153               SET WS-FIN-LECTURA TO TRUE
 000154
 000155            WHEN OTHER
 000156               DISPLAY '* ERROR LEER VSAM = ' WS-PER-CODE
 000157               MOVE 9999 TO RETURN-CODE
 000158               SET WS-FIN-LECTURA TO TRUE
 000159            END-EVALUATE.
 000160
 000161        3000-F-LEER-VSAM. EXIT.
 000162
 000163        3500-I-LEER-SQL.
 000164
 000165            EXEC SQL
 000166
 000167              SELECT PER_TIP_DOC, PER_NRO_DOC,
 000168                     PER_CLI_NRO, PER_NOMAPE,
 000169                     PER_CLI_AAAAMMDD, PER_DIRECCION,
 000170                     PER_LOCALIDAD, PER_EMAIL,
 000171                     PER_TELEFONO, PER_SEXO
 000172                   INTO :DCLPERSO.SQL-PER-TIP-DOC,
 000173                        :DCLPERSO.SQL-PER-NRO-DOC,
 000174                        :DCLPERSO.SQL-PER-CLI-NRO,
 000175                        :DCLPERSO.SQL-PER-NOMAPE,
 000176                        :DCLPERSO.SQL-PER-CLI-AAAAMMDD,
 000177                        :DCLPERSO.SQL-PER-DIRECCION,
 000178                        :DCLPERSO.SQL-PER-LOCALIDAD,
 000179                        :DCLPERSO.SQL-PER-EMAIL,
 000180                        :DCLPERSO.SQL-PER-TELEFONO,
 000181                        :DCLPERSO.SQL-PER-SEXO
 000182                   FROM ITPFBIO.TBPERSO
 000183                   WHERE PER_TIP_DOC =:KEY-TIP-DOC
 000184                     AND PER_NRO_DOC =:WS-COMP-NRO-DOC
 000185
 000186            END-EXEC.
 000187
 000188            EVALUATE SQLCODE
 000189
 000190            WHEN ZEROS
 000191               ADD 1 TO WS-LEIDOS-SQL
 000192
 000193            WHEN +100
 000194               ADD 1 TO WS-NOTFOUND
 000195
 000196            WHEN OTHER
 000197               MOVE SQLCODE   TO WS-SQLCODE
 000198               DISPLAY '* ERROR LEER SQL = ' WS-SQLCODE
 000199               MOVE 9999 TO RETURN-CODE
 000200               SET WS-FIN-LECTURA TO TRUE
 000201            END-EVALUATE.
 000202
 000203        3500-F-LEER-SQL. EXIT.
 000204
 000205        4000-I-DISPLAY.
 000206
 000207            DISPLAY 'NOMBRE Y APELLIDO: ' SQL-PER-NOMAPE.
 000208
 000209            DISPLAY 'TIPO DE DOCUMENTO: ' SQL-PER-TIP-DOC.
 000210
 000211            DISPLAY 'NRO DE DOCUMENTO: ' SQL-PER-NRO-DOC.
 000212
 000213            DISPLAY 'NRO DE CLIENTE: ' SQL-PER-CLI-NRO.
 000214
 000215            DISPLAY 'FECHA: ' SQL-PER-CLI-AAAAMMDD.
 000216
 000217            DISPLAY 'DIRECCION: ' SQL-PER-DIRECCION.
 000218
 000219            DISPLAY 'LOCALIDAD: ' SQL-PER-LOCALIDAD.
 000220
 000221            DISPLAY 'EMAIL: ' SQL-PER-EMAIL.
 000222
 000223            DISPLAY 'TELEFONO: ' SQL-PER-TELEFONO.
 000224
 000225            DISPLAY 'SEXO: ' SQL-PER-SEXO.
 000226
 000227            DISPLAY 'FIN DE REGISTRO.'.
 000228
 000229            ADD 1 TO WS-DISPLAYADOS.
 000230
 000231        4000-F-DISPLAY. EXIT.
 000232
 000233       **************************************
 000234       *  CUERPO FINAL CIERRE DE FILES      *
 000235       **************************************
 000236        9999-I-FINAL.
 000237
 000238            CLOSE PERSONA
 000239               IF WS-PER-CODE  IS NOT EQUAL '00'
 000240                 DISPLAY '* ERROR EN CLOSE PERSONA = ' WS-PER-CODE
 000241                 MOVE 9999 TO RETURN-CODE
 000242              END-IF.
 000243
 000244       **************************************
 000245       *   MOSTRAR TOTALES DE CONTROL
 000246       **************************************
 000247
 000248            DISPLAY 'CANTIDAD LEIDOS VSAM: ' WS-LEIDOS-VSAM.
 000249            DISPLAY 'CANTIDAD LEIDOS SQL: '  WS-LEIDOS-SQL.
 000250            DISPLAY 'CANTIDAD DISPLAYADOS: ' WS-DISPLAYADOS.
 000251            DISPLAY 'CANTIDAD DE NO ENCONTRADOS: ' WS-NOTFOUND.
 000252
 000253        9999-F-FINAL.
 000254            EXIT.
 000255       *