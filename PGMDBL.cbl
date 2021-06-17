 000001        IDENTIFICATION DIVISION.
 000002         PROGRAM-ID PGMDBL33.
 000003        ENVIRONMENT DIVISION.
 000004        CONFIGURATION SECTION.
 000005        SPECIAL-NAMES.
 000006            DECIMAL-POINT IS COMMA.
 000007        INPUT-OUTPUT SECTION.
 000008        FILE-CONTROL.
 000009              SELECT SALIDA ASSIGN SALIDA
 000010                     FILE STATUS IS FS-SALIDA.
 000011
 000012        DATA DIVISION.
 000013        FILE SECTION.
 000014        FD SALIDA
 000015             BLOCK CONTAINS 0 RECORDS
 000016             RECORDING MODE IS F.
 000017
 000018        01 REG-SALIDA PIC X(132).
 000019       ************************************
 000020       *             WORKING              *
 000021       ************************************
 000022        WORKING-STORAGE SECTION.
 000023       ************************************
 000024       *           FILE STATUS            *
 000025       ************************************
 000026        01  FS-FILE-STATUS.
 000027            05  FS-CLIENTE      PIC X     VALUE SPACES.
 000028                88  FS-CLIENTE-OK         VALUE 'Y'.
 000029                88  FS-CLIENTE-EOF        VALUE 'N'.
 000030            05  FS-CUENTA       PIC X     VALUE SPACES.
 000031                88  FS-CUENTA-OK          VALUE 'Y'.
 000032                88  FS-CUENTA-EOF         VALUE 'N'.
 000033            05  FS-SALIDA       PIC XX    VALUE SPACES.
 000034                88  FS-SALIDA-OK          VALUE '00'.
 000035                88  FS-SALIDA-EOF         VALUE '10'.
 000036       ************************************
 000037       *          CONTADORES              *
 000038       ************************************
 000039        01 CN-CONTADORES.
 000040            05 CN-NOVEDADES-FD       PIC 9(05)  VALUE ZEROS.
 000041            05 CN-NOVEDADES-NFD      PIC 9(05)  VALUE ZEROS.
 000042            05 CN-NOVEDADES-ER       PIC 9(05)  VALUE ZEROS.
 000043            05 CN-NOVEDADES-GRABADAS PIC 9(05)  VALUE ZEROS.
 000044            05 CN-CUENTA-LINEA       PIC 9(05)  VALUE ZEROS.
 000045            05 CN-CUENTAS            PIC 9(05)  VALUE ZEROS.
 000046       ************************************
 000047       *          CONSTANTES              *
 000048       ************************************
 000049        01 CT-CONSTANTES.
 000050            05 CT-1000               PIC 9(04)  VALUE 1000.
 000051       ************************************
 000052       *          NUMERO DE ALU           *
 000053       ************************************
 000054 CIELO  01  WS-NRO-ALU.
 000055            05  WS-ALU               PIC 9(05)         VALUE ZEROS.
 000056        01  CR-NRO.
 000057            05  CR-NRO-CLI           PIC S9(5)V USAGE COMP-3.
 000058            05  CR-ALU-H             PIC S9(5)V USAGE COMP-3.
 000059            05  CR-ALU-L             PIC S9(5)V USAGE COMP-3.
 000060       ************************************
 000061       *         FECHA DE PROCESO         *
 000062       ************************************
 000063        01  WS-AREA.
 000064            05  WS-AREA-AA       PIC 9(04)         VALUE ZEROS.
 000065            05  WS-AREA-MM       PIC 9(02)         VALUE ZEROS.
 000066            05  WS-AREA-DD       PIC 9(02)         VALUE ZEROS.
 000067
 000068        01  WS-FECHA.
 000069            05  WS-FECHA-AA      PIC 9(04)         VALUE ZEROS.
 000070            05  WS-SEP1          PIC X(01)         VALUE '-'.
 000071            05  WS-FECHA-MM      PIC 9(02)         VALUE ZEROS.
 000072            05  WS-SEP2          PIC X(01)         VALUE '-'.
 000073            05  WS-FECHA-DD      PIC 9(02)         VALUE ZEROS.
 000074       ************************************
 000075       *       VARIABLES IMPRESION *
 000076       ************************************
 000077        01  WS-REG-CLIENTE.
 000078            05  FILLER    PIC X(2)    VALUE SPACES.
 000079            05  WS-CLI-NOM    PIC X(30)    VALUE SPACES.
 000080            05  FILLER    PIC X(2)    VALUE SPACES.
 000081            05  FILLER    PIC X(01)    VALUE '|'.
 000082            05  FILLER    PIC X(2)    VALUE SPACES.
 000083            05  WS-CLI-APE    PIC X(30)    VALUE SPACES.
 000084            05  FILLER    PIC X(2)    VALUE SPACES.
 000085            05  FILLER    PIC X(01)    VALUE '|'.
 000086            05  FILLER    PIC X(2)    VALUE SPACES.
 000087            05  WS-CLI-NRO-CLI    PIC X(05)    VALUE SPACES.
 000088            05  FILLER    PIC X(8)    VALUE SPACES.
 000089            05  FILLER    PIC X(01)    VALUE '|'.
 000090            05  FILLER    PIC X(2)    VALUE SPACES.
 000091            05  WS-CLI-TIP-DOC    PIC X(02)    VALUE SPACES.
 000092            05  FILLER    PIC X(14)    VALUE SPACES.
 000093            05  FILLER    PIC X(01)    VALUE '|'.
 000094            05  FILLER    PIC X(2)    VALUE SPACES.
 000095            05  WS-CLI-NRO-DOC    PIC X(11)    VALUE SPACES.
 000096            05  FILLER    PIC X(4)    VALUE SPACES.
 000097            05  FILLER    PIC X(01)    VALUE '|'.
 000098            05  FILLER    PIC X(2)    VALUE SPACES.
 000099            05  WS-CLI-FECHA-ALTA    PIC X(10)    VALUE SPACES.
 000100            05  FILLER    PIC X(2)    VALUE SPACES.
 000101            05  FILLER    PIC X(01)    VALUE '|'.
 000102            05  FILLER    PIC X(2)    VALUE SPACES.
 000103            05  WS-CLI-FECHA-BAJA    PIC X(10)    VALUE SPACES.
 000104            05  FILLER    PIC X(2)    VALUE SPACES.
 000105            05  FILLER    PIC X(01)    VALUE '|'.
 000106
 000107
 000108        01  WS-REG-CUENTA.
 000109            05  FILLER    PIC X(2)    VALUE SPACES.
 000110            05  WS-CUE-TIP-CUE    PIC X(02)    VALUE SPACES.
 000111            05  FILLER    PIC X(11)    VALUE SPACES.
 000112            05  FILLER    PIC X(01)    VALUE '|'.
 000113            05  FILLER    PIC X(2)    VALUE SPACES.
 000114            05  WS-CUE-NRO-CUE    PIC X(15)    VALUE SPACES.
 000115            05  FILLER    PIC X(2)    VALUE SPACES.
 000116            05  FILLER    PIC X(01)    VALUE '|'.
 000117            05  FILLER    PIC X(2)    VALUE SPACES.
 000118            05  WS-CUE-MONEDA    PIC X(02)    VALUE SPACES.
 000119            05  FILLER    PIC X(6)    VALUE SPACES.
 000120            05  FILLER    PIC X(01)    VALUE '|'.
 000121            05  FILLER    PIC X(2)    VALUE SPACES.
 000122            05  WS-CUE-CBU    PIC X(11)    VALUE SPACES.
 000123            05  FILLER    PIC X(2)    VALUE SPACES.
 000124            05  FILLER    PIC X(01)    VALUE '|'.
 000125            05  FILLER    PIC X(2)    VALUE SPACES.
 000126            05  WS-CUE-NRO-CLI    PIC X(05)    VALUE SPACES.
 000127            05  FILLER    PIC X(8)    VALUE SPACES.
 000128            05  FILLER    PIC X(01)    VALUE '|'.
 000129            05  FILLER    PIC X(2)    VALUE SPACES.
 000130            05  WS-CUE-SALDO-ACT    PIC -ZZZ.ZZZ.999,99 VALUE ZEROS.
 000131            05  FILLER    PIC X(2)    VALUE SPACES.
 000132            05  FILLER    PIC X(01)    VALUE '|'.
 000133            05  FILLER    PIC X(2)    VALUE SPACES.
 000134            05  WS-CUE-FECHA-ACT    PIC X(10)    VALUE SPACES.
 000135            05  FILLER    PIC X(4)    VALUE SPACES.
 000136            05  FILLER    PIC X(01)    VALUE '|'.
 000137            05  FILLER    PIC X(2)    VALUE SPACES.
 000138            05  WS-CUE-FECHA-ULT    PIC X(10)    VALUE SPACES.
 000139            05  FILLER    PIC X(11)    VALUE SPACES.
 000140            05  FILLER    PIC X(01)    VALUE '|'.
 000141       ************************************
 000142       *            IMPRESION             *
 000143       ************************************
 000144        01  IP-TITULO.
 000145            05  FILLER      PIC X(20) VALUE  SPACES.
 000146            05  FILLER      PIC X(29) VALUE
 000147             'LISTADO DE CLIENTES Y CUENTAS'.
 000148            05  FILLER      PIC X(05) VALUE  SPACES.
 000149            05  FILLER      PIC X(07) VALUE  'FECHA: '.
 000150            05  IP-FECHA    PIC X(10) VALUE  SPACES.
 000151            05  FILLER      PIC X(05) VALUE  SPACES.
 000152            05  FILLER      PIC X(05) VALUE  'ALU: '.
 000153            05  IP-ALU      PIC X(04) VALUE  SPACES.
 000154
 000155        01  IP-SUBTITULO.
 000156            05  IP-BARRA.
 000157                10  FILLER        PIC X(54) VALUE
 000158                '------------------------------------------------------'.
 000159                10  FILLER        PIC X(54) VALUE
 000160                '------------------------------------------------------'.
 000161            05  IP-INTRODUCCION-CLIENTE.
 000162                10  FILLER    PIC X(38)    VALUE SPACES.
 000163                10  FILLER    PIC X(07)    VALUE 'CLIENTE'.
 000164            05  IP-INTRODUCCION-CUENTA.
 000165                10  FILLER    PIC X(38)    VALUE SPACES.
 000166                10  FILLER    PIC X(07)    VALUE 'CUENTAS'.
 000167            05  IP-NO-CUENTAS.
 000168                10  FILLER    PIC X(38)    VALUE SPACES.
 000169                10  FILLER    PIC X(19)    VALUE 'CLIENTE SIN CUENTAS'.
 000170            05  IP-CLIENTE.
 000171                10  FILLER    PIC X(14)    VALUE SPACES.
 000172                10  FILLER    PIC X(6)    VALUE 'NOMBRE'.
 000173                10  FILLER    PIC X(14)    VALUE SPACES.
 000174                10  FILLER    PIC X(01)    VALUE  '|'.
 000175                10  FILLER    PIC X(13)    VALUE SPACES.
 000176                10  FILLER    PIC X(8)    VALUE 'APELLIDO'.
 000177                10  FILLER    PIC X(13)    VALUE SPACES.
 000178                10  FILLER    PIC X(01)    VALUE  '|'.
 000179                10  FILLER    PIC X(2)    VALUE SPACES.
 000180                10  FILLER    PIC X(11)    VALUE 'NRO CLIENTE'.
 000181                10  FILLER    PIC X(2)    VALUE SPACES.
 000182                10  FILLER    PIC X(01)    VALUE  '|'.
 000183                10  FILLER    PIC X(2)    VALUE SPACES.
 000184                10  FILLER    PIC X(14)    VALUE 'TIPO DOCUMENTO'.
 000185                10  FILLER    PIC X(2)    VALUE SPACES.
 000186                10  FILLER    PIC X(01)    VALUE  '|'.
 000187                10  FILLER    PIC X(2)    VALUE SPACES.
 000188                10  FILLER    PIC X(13)    VALUE 'NRO DOCUMENTO'.
 000189                10  FILLER    PIC X(2)    VALUE SPACES.
 000190                10  FILLER    PIC X(01)    VALUE  '|'.
 000191                10  FILLER    PIC X(2)    VALUE SPACES.
 000192                10  FILLER    PIC X(10)    VALUE 'FECHA ALTA'.
 000193                10  FILLER    PIC X(2)    VALUE SPACES.
 000194                10  FILLER    PIC X(01)    VALUE  '|'.
 000195                10  FILLER    PIC X(2)    VALUE SPACES.
 000196                10  FILLER    PIC X(10)    VALUE 'FECHA BAJA'.
 000197                10  FILLER    PIC X(2)    VALUE SPACES.
 000198                10  FILLER    PIC X(01)    VALUE  '|'.
 000199            05  IP-CUENTA.
 000200                10  FILLER    PIC X(2)    VALUE SPACES.
 000201                10  FILLER    PIC X(11)    VALUE 'TIPO CUENTA'.
 000202                10  FILLER    PIC X(2)    VALUE SPACES.
 000203                10  FILLER    PIC X(01)    VALUE  '|'.
 000204                10  FILLER    PIC X(4)    VALUE SPACES.
 000205                10  FILLER    PIC X(10)    VALUE 'NRO CUENTA'.
 000206                10  FILLER    PIC X(5)    VALUE SPACES.
 000207                10  FILLER    PIC X(01)    VALUE  '|'.
 000208                10  FILLER    PIC X(2)    VALUE SPACES.
 000209                10  FILLER    PIC X(6)    VALUE 'MONEDA'.
 000210                10  FILLER    PIC X(2)    VALUE SPACES.
 000211                10  FILLER    PIC X(01)    VALUE  '|'.
 000212                10  FILLER    PIC X(6)    VALUE SPACES.
 000213                10  FILLER    PIC X(3)    VALUE 'CBU'.
 000214                10  FILLER    PIC X(6)    VALUE SPACES.
 000215                10  FILLER    PIC X(01)    VALUE  '|'.
 000216                10  FILLER    PIC X(2)    VALUE SPACES.
 000217                10  FILLER    PIC X(11)    VALUE 'NRO CLIENTE'.
 000218                10  FILLER    PIC X(2)    VALUE SPACES.
 000219                10  FILLER    PIC X(01)    VALUE  '|'.
 000220                10  FILLER    PIC X(4)    VALUE SPACES.
 000221                10  FILLER    PIC X(12)    VALUE 'SALDO ACTUAL'.
 000222                10  FILLER    PIC X(5)    VALUE SPACES.
 000223                10  FILLER    PIC X(01)    VALUE '|'.
 000224                10  FILLER    PIC X(2)    VALUE SPACES.
 000225                10  FILLER    PIC X(12)    VALUE 'FECHA ACTUAL'.
 000226                10  FILLER    PIC X(2)    VALUE SPACES.
 000227                10  FILLER    PIC X(01)    VALUE  '|'.
 000228                10  FILLER    PIC X(2)    VALUE SPACES.
 000229                10  FILLER    PIC X(19)    VALUE 'FECHA ULTIMO CIERRE'.
 000230                10  FILLER    PIC X(2)    VALUE SPACES.
 000231                10  FILLER    PIC X(01)    VALUE  '|'.
 000232       ************************************
 000233       *             SQL CODE             *
 000234       ************************************
 000235        77  FS-SQLCODE               PIC -999          VALUE ZEROS.
 000236       ************************************
 000237       *              SQL                 *
 000238       ************************************
 000239             EXEC SQL
 000240               INCLUDE SQLCA
 000241             END-EXEC.
 000242       *
 000243             EXEC SQL
 000244               INCLUDE TBCLIENT
 000245             END-EXEC.
 000246       *
 000247             EXEC SQL
 000248               INCLUDE TBCUENT
 000249             END-EXEC.
 000250       *
 000251             EXEC SQL
 000252               DECLARE CLIENTE_CURSOR CURSOR
 000253               FOR
 000254                SELECT TIPO_DOCUMENTO, NRO_DOCUMENTO,
 000255                       NRO_CLIENTE, NOMBRE_CLIENTE,
 000256                       APELLIDO_CLIENTE, DOMICILIO,
 000257                       CIUDAD, CODIGO_POSTAL,
 000258                       NACIONALIDAD, FECHA_DE_ALTA,
 000259                       FECHA_DE_BAJA, ESTADO_CIVIL,
 000260                       SEXO, CORREO_ELECTRONICO,
 000261                       FECCHA_NACIMIENTO
 000262       *
 000263                       FROM ITPFBIO.TBCLIENT
 000264       *
 000265                       WHERE NRO_CLIENTE > :CR-ALU-L AND
 000266                             NRO_CLIENTE < :CR-ALU-H
 000267       *
 000268             END-EXEC.
 000269       *
 000270             EXEC SQL
 000271               DECLARE CUENTA_CURSOR CURSOR
 000272               FOR
 000273                SELECT TIPO_CUENTA, NRO_CUENTA,
 000274                       MONEDA, CBU, NRO_CLIENTE,
 000275                       SALDO_ACTUAL, FECHA_ACTUAL,
 000276                       FECHA_ULTIMO_CIERRE
 000277       *
 000278                       FROM ITPFBIO.TBCUENTAS
 000279       *
 000280                       WHERE NRO_CLIENTE = :CR-NRO-CLI
 000281             END-EXEC.
 000282       **************************************
 000283        PROCEDURE DIVISION.
 000284       **************************************
 000285       *  CUERPO PRINCIPAL DEL PROGRAMA     *
 000286       **************************************
 000287        MAIN-PROGRAM.
 000288            PERFORM 1000-I-INICIO
 000289               THRU 1000-F-INICIO
 000290       *
 000291            PERFORM 2000-I-PROCESO
 000292               THRU 2000-F-PROCESO
 000293               UNTIL FS-CLIENTE-EOF
 000294       *
 000295            PERFORM 9999-I-FINAL
 000296               THRU 9999-F-FINAL
 000297            .
 000298        F-MAIN-PROGRAM. GOBACK.
 000299       **************************************
 000300       *  CUERPO INICIO APERTURA ARCHIVOS   *
 000301       **************************************
 000302        1000-I-INICIO.
 000303 TIERRA     ACCEPT WS-NRO-ALU FROM SYSIN
 000304       *
 000305            MULTIPLY WS-ALU BY CT-1000 GIVING CR-ALU-L
 000306       *
 000307            ADD 1 TO WS-ALU
 000308       *
 000309            MULTIPLY WS-ALU BY CT-1000 GIVING CR-ALU-H
 000310
 000311            MOVE 33000 TO CR-ALU-L
 000312            MOVE 34000 TO CR-ALU-H
 000313       *
 000314            SUBTRACT 1 FROM WS-ALU
 000315       *
 000316            PERFORM 7000-I-FECHA
 000317               THRU 7000-F-FECHA
 000318       *
 000319            OPEN OUTPUT SALIDA
 000320       *
 000321            IF NOT FS-SALIDA-OK
 000322               DISPLAY '* ERROR EN OPEN SALIDA = ' FS-SALIDA
 000323               MOVE 9999 TO RETURN-CODE
 000324               SET  FS-CLIENTE-EOF TO TRUE
 000325            END-IF
 000326       *
 000327            PERFORM 9000-I-TITULO
 000328               THRU 9000-F-TITULO
 000329       *
 000330            PERFORM 2500-I-OPEN-CLIENTE
 000331               THRU 2500-F-OPEN-CLIENTE
 000332       *
 000333            PERFORM 3000-I-LEER-CLIENTE
 000334               THRU 3000-F-LEER-CLIENTE
 000335            .
 000336        1000-F-INICIO. EXIT.
 000337       **************************************
 000338       *  CUERPO PRINCIPAL DE PROCESOS      *
 000339       **************************************
 000340        2000-I-PROCESO.
 000341            PERFORM 2600-I-OPEN-CUENTA
 000342               THRU 2600-F-OPEN-CUENTA
 000343       *
 000344            PERFORM 8500-I-SEP-CUENTA
 000345               THRU 8500-F-SEP-CUENTA
 000346       *
 000347            PERFORM 8200-I-LAYOUT-CUENTA
 000348               THRU 8200-F-LAYOUT-CUENTA
 000349       *
 000350            PERFORM 3500-I-LEER-CUENTA
 000351               THRU 3500-F-LEER-CUENTA
 000352               UNTIL FS-CUENTA-EOF
 000353       *
 000354            PERFORM 6500-I-CLOSE-CUENTA
 000355               THRU 6500-F-CLOSE-CUENTA
 000356       *
 000357            PERFORM 3000-I-LEER-CLIENTE
 000358               THRU 3000-F-LEER-CLIENTE
 000359       *
 000360            SET FS-CUENTA-OK TO TRUE
 000361            .
 000362        2000-F-PROCESO. EXIT.
 000363       **************************************
 000364       *         OPEN CURSOR CLIENTE        *
 000365       **************************************
 000366        2500-I-OPEN-CLIENTE.
 000367            EXEC SQL
 000368               OPEN CLIENTE_CURSOR
 000369            END-EXEC
 000370       *
 000371            IF SQLCODE NOT EQUAL ZEROS
 000372               MOVE SQLCODE   TO FS-SQLCODE
 000373               DISPLAY '* ERROR OPEN CURSOR CLIENTE   = ' FS-SQLCODE
 000374               MOVE 9999 TO RETURN-CODE
 000375               SET  FS-CLIENTE-EOF TO TRUE
 000376            END-IF
 000377            .
 000378        2500-F-OPEN-CLIENTE. EXIT.
 000379       **************************************
 000380       *         OPEN CURSOR CUENTA         *
 000381       **************************************
 000382        2600-I-OPEN-CUENTA.
 000383            EXEC SQL
 000384               OPEN CUENTA_CURSOR
 000385            END-EXEC
 000386       *
 000387            IF SQLCODE NOT EQUAL ZEROS
 000388               MOVE SQLCODE   TO FS-SQLCODE
 000389               DISPLAY '* ERROR OPEN CURSOR CUENTA    = ' FS-SQLCODE
 000390               MOVE 9999 TO RETURN-CODE
 000391               SET  FS-CLIENTE-EOF TO TRUE
 000392            ELSE
 000393               INITIALIZE CN-CUENTAS
 000394            END-IF
 000395            .
 000396        2600-F-OPEN-CUENTA. EXIT.
 000397       **************************************
 000398       *            LEER CLIENTE            *
 000399       **************************************
 000400        3000-I-LEER-CLIENTE.
 000401            EXEC SQL
 000402                 FETCH  CLIENTE_CURSOR
 000403                 INTO  :DCLCLIEN.TIPO-DOCUMENTO,
 000404                       :DCLCLIEN.NRO-DOCUMENTO,
 000405                       :DCLCLIEN.CLI-NRO-CLIENTE,
 000406                       :DCLCLIEN.NOMBRE-CLIENTE,
 000407                       :DCLCLIEN.APELLIDO-CLIENTE,
 000408                       :DCLCLIEN.DOMICILIO,
 000409                       :DCLCLIEN.CIUDAD,
 000410                       :DCLCLIEN.CODIGO-POSTAL,
 000411                       :DCLCLIEN.NACIONALIDAD,
 000412                       :DCLCLIEN.FECHA-DE-ALTA,
 000413                       :DCLCLIEN.FECHA-DE-BAJA,
 000414                       :DCLCLIEN.ESTADO-CIVIL,
 000415                       :DCLCLIEN.SEXO,
 000416                       :DCLCLIEN.CORREO-ELECTRONICO,
 000417                       :DCLCLIEN.FECCHA-NACIMIENTO
 000418            END-EXEC
 000419       *
 000420            EVALUATE SQLCODE
 000421       *
 000422            WHEN ZEROS
 000423               MOVE CLI-NRO-CLIENTE TO CR-NRO-CLI
 000424       *
 000425               ADD 1 TO CN-NOVEDADES-FD
 000426       *
 000427               PERFORM 8000-I-SEP-CLIENTE
 000428                  THRU 8000-F-SEP-CLIENTE
 000429       *
 000430               PERFORM 8100-I-LAYOUT-CLIENTE
 000431                  THRU 8100-F-LAYOUT-CLIENTE
 000432       *
 000433            WHEN +100
 000434               SET FS-CLIENTE-EOF TO TRUE
 000435       *
 000436            WHEN OTHER
 000437               ADD 1 TO CN-NOVEDADES-ER
 000438               MOVE SQLCODE   TO FS-SQLCODE
 000439               DISPLAY '* ERROR LEER CURSOR CLIENTE = ' FS-SQLCODE
 000440               MOVE 9999 TO RETURN-CODE
 000441               SET FS-CLIENTE-EOF TO TRUE
 000442            END-EVALUATE
 000443            .
 000444        3000-F-LEER-CLIENTE. EXIT.
 000445       **************************************
 000446       *            LEER CUENTA             *
 000447       **************************************
 000448        3500-I-LEER-CUENTA.
 000449            EXEC SQL
 000450                 FETCH CUENTA_CURSOR
 000451                 INTO  :DCLCUEN.TIPO-CUENTA,
 000452                       :DCLCUEN.NRO-CUENTA,
 000453                       :DCLCUEN.MONEDA,
 000454                       :DCLCUEN.CBU,
 000455                       :DCLCUEN.CUE-NRO-CLIENTE,
 000456                       :DCLCUEN.SALDO-ACTUAL,
 000457                       :DCLCUEN.FECHA-ACTUAL,
 000458                       :DCLCUEN.FECHA-ULTIMO-CIERRE
 000459            END-EXEC
 000460       *
 000461            EVALUATE SQLCODE
 000462       *
 000463            WHEN ZEROS
 000464               ADD 1 TO CN-NOVEDADES-FD
 000465               PERFORM 5500-I-GRABAR-CUENTA
 000466                  THRU 5500-F-GRABAR-CUENTA
 000467       *
 000468               ADD 1 TO CN-CUENTAS
 000469       *
 000470            WHEN +100
 000471               SET FS-CUENTA-EOF TO TRUE
 000472       *
 000473               IF CN-CUENTAS EQUAL TO ZERO
 000474                 WRITE REG-SALIDA  FROM IP-NO-CUENTAS
 000475       *
 000476                 IF NOT FS-SALIDA-OK
 000477                   DISPLAY '* ERROR EN GRABAR NO CUENTAS = ' FS-SALIDA
 000478                   MOVE 9999 TO RETURN-CODE
 000479                   SET  FS-CLIENTE-EOF TO TRUE
 000480                 ELSE
 000481                   ADD 1 TO CN-CUENTA-LINEA
 000482                 END-IF
 000483
 000484               END-IF
 000485       *
 000486            WHEN OTHER
 000487               MOVE SQLCODE   TO FS-SQLCODE
 000488               DISPLAY '* ERROR LEER CURSOR CUENTA = ' FS-SQLCODE
 000489               MOVE 9999 TO RETURN-CODE
 000490               SET FS-CLIENTE-EOF TO TRUE
 000491            END-EVALUATE
 000492            .
 000493        3500-F-LEER-CUENTA. EXIT.
 000494       **************************************
 000495       *           MOVER CLIENTE            *
 000496       **************************************
 000497        4000-I-MOVER-CLIENTE.
 000498       *
 000499            MOVE TIPO-DOCUMENTO            TO WS-CLI-TIP-DOC
 000500       *
 000501            MOVE NRO-DOCUMENTO             TO WS-CLI-NRO-DOC
 000502       *
 000503            MOVE CLI-NRO-CLIENTE           TO WS-CLI-NRO-CLI
 000504       *
 000505            MOVE NOMBRE-CLIENTE            TO WS-CLI-NOM
 000506       *
 000507            MOVE APELLIDO-CLIENTE          TO WS-CLI-APE
 000508       *
 000509            MOVE FECHA-DE-ALTA             TO WS-CLI-FECHA-ALTA
 000510       *
 000511            MOVE FECHA-DE-BAJA             TO WS-CLI-FECHA-BAJA
 000512            .
 000513        4000-F-MOVER-CLIENTE. EXIT.
 000514       **************************************
 000515       *           MOVER CUENTA             *
 000516       **************************************
 000517        4500-I-MOVER-CUENTA.
 000518       *
 000519            MOVE TIPO-CUENTA               TO WS-CUE-TIP-CUE
 000520       *
 000521            MOVE NRO-CUENTA                TO WS-CUE-NRO-CUE
 000522       *
 000523            MOVE MONEDA                    TO WS-CUE-MONEDA
 000524       *
 000525            MOVE CBU                       TO WS-CUE-CBU
 000526       *
 000527            MOVE CUE-NRO-CLIENTE           TO WS-CUE-NRO-CLI
 000528       *
 000529            MOVE SALDO-ACTUAL              TO WS-CUE-SALDO-ACT
 000530       *
 000531            MOVE FECHA-ACTUAL              TO WS-CUE-FECHA-ACT
 000532       *
 000533            MOVE FECHA-ULTIMO-CIERRE       TO WS-CUE-FECHA-ULT
 000534            .
 000535        4500-F-MOVER-CUENTA. EXIT.
 000536       **************************************
 000537       *             GRABAR CLIENTE         *
 000538       **************************************
 000539        5000-I-GRABAR-CLIENTE.
 000540            IF CN-CUENTA-LINEA GREATER 60
 000541                PERFORM 9000-I-TITULO THRU 9000-F-TITULO
 000542            END-IF
 000543       *
 000544            PERFORM 4000-I-MOVER-CLIENTE
 000545               THRU 4000-F-MOVER-CLIENTE
 000546       *
 000547            WRITE REG-SALIDA   FROM WS-REG-CLIENTE
 000548       *
 000549            IF NOT FS-SALIDA-OK
 000550              DISPLAY '* ERROR EN GRABAR SALIDA CLIENTE = ' FS-SALIDA
 000551              MOVE 9999 TO RETURN-CODE
 000552              SET  FS-CLIENTE-EOF TO TRUE
 000553            ELSE
 000554              ADD 1                         TO  CN-NOVEDADES-GRABADAS
 000555              ADD 1                         TO CN-CUENTA-LINEA
 000556            END-IF
 000557            .
 000558        5000-F-GRABAR-CLIENTE. EXIT.
 000559       **************************************
 000560       *             GRABAR CUENTA          *
 000561       **************************************
 000562        5500-I-GRABAR-CUENTA.
 000563            IF CN-CUENTA-LINEA GREATER 60
 000564                PERFORM 9000-I-TITULO THRU 9000-F-TITULO
 000565            END-IF
 000566       *
 000567            PERFORM 4500-I-MOVER-CUENTA
 000568               THRU 4500-F-MOVER-CUENTA
 000569       *
 000570            WRITE REG-SALIDA   FROM WS-REG-CUENTA
 000571       *
 000572            IF NOT FS-SALIDA-OK
 000573              DISPLAY '* ERROR EN GRABAR SALIDA CUENTA = ' FS-SALIDA
 000574              MOVE 9999 TO RETURN-CODE
 000575              SET  FS-CLIENTE-EOF TO TRUE
 000576            ELSE
 000577              ADD 1                         TO  CN-NOVEDADES-GRABADAS
 000578              ADD 1                         TO CN-CUENTA-LINEA
 000579            END-IF
 000580            .
 000581        5500-F-GRABAR-CUENTA. EXIT.
 000582       **************************************
 000583       *         CLOSE CURSOR CLIENTE       *
 000584       **************************************
 000585        6000-I-CLOSE-CLIENTE.
 000586            EXEC SQL
 000587               CLOSE CLIENTE_CURSOR
 000588            END-EXEC
 000589       *
 000590            IF SQLCODE NOT EQUAL ZEROS
 000591               MOVE SQLCODE TO FS-SQLCODE
 000592               DISPLAY '* ERROR CLOSE CURSOR CLIENTE = ' FS-SQLCODE
 000593               MOVE 9999 TO RETURN-CODE
 000594            END-IF
 000595            .
 000596        6000-F-CLOSE-CLIENTE. EXIT.
 000597       **************************************
 000598       *         CLOSE CURSOR CUENTA        *
 000599       **************************************
 000600        6500-I-CLOSE-CUENTA.
 000601            EXEC SQL
 000602               CLOSE CUENTA_CURSOR
 000603            END-EXEC
 000604       *
 000605            IF SQLCODE NOT EQUAL ZEROS
 000606               MOVE SQLCODE TO FS-SQLCODE
 000607               DISPLAY '* ERROR CLOSE CURSOR CUENTA = ' FS-SQLCODE
 000608               MOVE 9999 TO RETURN-CODE
 000609            END-IF
 000610            .
 000611        6500-F-CLOSE-CUENTA. EXIT.
 000612       **************************************
 000613       *           TOMAR FECHA              *
 000614       **************************************
 000615        7000-I-FECHA.
 000616            ACCEPT WS-AREA FROM DATE YYYYMMDD
 000617       *
 000618            MOVE WS-AREA-AA TO WS-FECHA-AA
 000619       *
 000620            MOVE WS-AREA-MM TO WS-FECHA-MM
 000621       *
 000622            MOVE WS-AREA-DD TO WS-FECHA-DD
 000623            .
 000624        7000-F-FECHA. EXIT.
 000625       **************************************
 000626       *          SEPARADOR CLIENTE         *
 000627       **************************************
 000628        8000-I-SEP-CLIENTE.
 000629            WRITE REG-SALIDA FROM IP-BARRA
 000630       *
 000631            IF NOT FS-SALIDA-OK
 000632              DISPLAY '* ERROR EN GRABAR SEPARADOR BARRA = ' FS-SALIDA
 000633              MOVE 9999 TO RETURN-CODE
 000634              SET  FS-CLIENTE-EOF TO TRUE
 000635            ELSE
 000636              ADD 1 TO CN-CUENTA-LINEA
 000637            END-IF
 000638       *
 000639            WRITE REG-SALIDA FROM IP-INTRODUCCION-CLIENTE
 000640       *
 000641            IF NOT FS-SALIDA-OK
 000642              DISPLAY '* ERROR EN GRABAR SEPARADOR CLIENTE = ' FS-SALIDA
 000643              MOVE 9999 TO RETURN-CODE
 000644              SET  FS-CLIENTE-EOF TO TRUE
 000645            ELSE
 000646              ADD 1 TO CN-CUENTA-LINEA
 000647            END-IF
 000648            .
 000649        8000-F-SEP-CLIENTE. EXIT.
 000650       **************************************
 000651       *          LAYOUT CLIENTE            *
 000652       **************************************
 000653        8100-I-LAYOUT-CLIENTE.
 000654            WRITE REG-SALIDA FROM IP-CLIENTE
 000655       *
 000656            IF NOT FS-SALIDA-OK
 000657              DISPLAY '* ERROR EN GRABAR IPCLIENTE = ' FS-SALIDA
 000658              MOVE 9999 TO RETURN-CODE
 000659              SET  FS-CLIENTE-EOF TO TRUE
 000660            ELSE
 000661              ADD 1                         TO CN-CUENTA-LINEA
 000662              PERFORM  5000-I-GRABAR-CLIENTE
 000663                 THRU  5000-F-GRABAR-CLIENTE
 000664            END-IF
 000665            .
 000666        8100-F-LAYOUT-CLIENTE. EXIT.
 000667       **************************************
 000668       *          LAYOUT CUENTA             *
 000669       **************************************
 000670        8200-I-LAYOUT-CUENTA.
 000671            WRITE REG-SALIDA FROM IP-CUENTA
 000672       *
 000673            IF NOT FS-SALIDA-OK
 000674              DISPLAY '* ERROR EN GRABAR IPCUENTA = ' FS-SALIDA
 000675              MOVE 9999 TO RETURN-CODE
 000676              SET  FS-CLIENTE-EOF TO TRUE
 000677            ELSE
 000678              ADD 1                         TO CN-CUENTA-LINEA
 000679            END-IF
 000680            .
 000681        8200-F-LAYOUT-CUENTA. EXIT.
 000682       **************************************
 000683       *          SEPARADOR CUENTA          *
 000684       **************************************
 000685        8500-I-SEP-CUENTA.
 000686            WRITE REG-SALIDA FROM IP-INTRODUCCION-CUENTA
 000687       *
 000688            IF NOT FS-SALIDA-OK
 000689              DISPLAY '* ERROR EN GRABAR SEPARADOR CUENTA = ' FS-SALIDA
 000690              MOVE 9999 TO RETURN-CODE
 000691              SET  FS-CLIENTE-EOF TO TRUE
 000692            ELSE
 000693              ADD 1 TO CN-CUENTA-LINEA
 000694            END-IF
 000695            .
 000696        8500-F-SEP-CUENTA. EXIT.
 000697       **************************************
 000698       *           GRABAR TITULO            *
 000699       **************************************
 000700        9000-I-TITULO.
 000701            MOVE WS-FECHA TO IP-FECHA
 000702       *
 000703            MOVE WS-ALU   TO IP-ALU
 000704       *
 000705            WRITE  REG-SALIDA    FROM IP-TITULO AFTER PAGE
 000706       *
 000707            IF NOT FS-SALIDA-OK
 000708               DISPLAY '* ERROR EN IMPRIMIR TITULO = ' FS-SALIDA
 000709               MOVE 9999 TO RETURN-CODE
 000710               SET  FS-CLIENTE-EOF TO TRUE
 000711            ELSE
 000712               MOVE 1 TO CN-CUENTA-LINEA
 000713            END-IF
 000714            .
 000715        9000-F-TITULO. EXIT.
 000716       **************************************
 000717       *  CUERPO FINAL CIERRE DE FILES      *
 000718       **************************************
 000719        9999-I-FINAL.
 000720            CLOSE SALIDA
 000721               IF NOT FS-SALIDA-OK
 000722                 DISPLAY '* ERROR EN CLOSE SALIDA =  ' FS-SALIDA
 000723                 MOVE 9999 TO RETURN-CODE
 000724                 SET FS-SALIDA-EOF TO TRUE
 000725               END-IF
 000726       *
 000727            PERFORM 6000-I-CLOSE-CLIENTE
 000728               THRU 6000-F-CLOSE-CLIENTE
 000729       *
 000730            DISPLAY 'NOVEDADES ENCONTRADAS: ' CN-NOVEDADES-FD
 000731            DISPLAY 'NOVEDADES NO ENCONTRADAS: ' CN-NOVEDADES-NFD
 000732            DISPLAY 'NOVEDADES ERRONEAS: ' CN-NOVEDADES-ER
 000733            DISPLAY 'REGISTROS GRABADOS: ' CN-NOVEDADES-GRABADAS
 000734            .
 000735        9999-F-FINAL. EXIT.