 PROCEDURE APLICA_PAGO (IN_REG_PAGO IN MPENGES.SPG_PAGO%ROWTYPE,
                         IN_USUARIO IN VARCHAR2,
                         OUT_COD_RESPUESTA OUT NUMBER,
                         OUT_MENSAJE OUT VARCHAR2) IS

   l_error                VARCHAR2(2000);

   l_reg_pago                 MPENGES.SPG_PAGO%ROWTYPE;
   l_reg_cuenta_por_pagar     MPENGES.SPG_CUENTA_POR_PAGAR%ROWTYPE;
   l_reg_solicitud            mpenges.spg_solicitud%ROWTYPE;
   l_fuente_financiamiento    mnompen.Nomp_Fuente_Financiamiento.valor%TYPE;
   l_nitPagadorPreli          MPENGES.SPG_CUENTA_POR_PAGAR.NIT_PAGADOR%type;
   l_respuestaMotor           VARCHAR2(3000);
   l_validacionMotorOk        NUMBER;
   l_ejecutarMotor            VARCHAR2(2);
   l_sysdate_pago             date;
   l_nominaPensionadoId       MNOMPEN.NOMP_NOMINA_PENSIONADO.NOMINA_PENSIONADO_ID%TYPE;
   l_rta_val_gpm              BOOLEAN;
   l_cod_rta_val_gpm          VARCHAR2(2);
  BEGIN
    --DBMS_OUTPUT.PUT_LINE('------- ENTRO EJECUTAR APLICA PAGO LIQUIDACION -------');

    OUT_COD_RESPUESTA := 0;
    l_reg_pago := IN_REG_PAGO;
    l_nitFsp               := TO_NUMBER(NVL(OBTENER_VALOR_GEN_PARAMETRO(C_FONDO_PENSION_OBLIG,C_MODULO_PENS_GESTION,C_PARAMETRO_NIT_FSP),0));
    l_nombreFsp            := NVL(OBTENER_VALOR_GEN_PARAMETRO(C_FONDO_PENSION_OBLIG,C_MODULO_PENS_GESTION,C_PARAMETRO_NOMBRE_FSP),'');

     -- SE OBTIENE EL REGISTRO DE LA CUENTA POR PAGAR
     BEGIN
        SELECT CXP.CUENTA_POR_PAGAR_ID,
               CXP.SOLICITUD_ID,
               CXP.CONSECUTIVO_ID,
               CXP.BENEFICIARIO_ID,
               CXP.PERSONA_ID,
               CXP.ESTADO_CXP_ID,
               CXP.VALOR_PESOS,
               CXP.NIT_PAGADOR,
               CXP.TIPO_PAGO_ID,
               CXP.SOLICITUD_ID_BENEFICIO,
               CXP.CUENTA_POR_COBRAR_ID
         INTO  l_reg_cuenta_por_pagar.CUENTA_POR_PAGAR_ID,
               l_reg_cuenta_por_pagar.SOLICITUD_ID,
               l_reg_cuenta_por_pagar.CONSECUTIVO_ID,
               l_reg_cuenta_por_pagar.BENEFICIARIO_ID,
               l_reg_cuenta_por_pagar.PERSONA_ID,
               l_reg_cuenta_por_pagar.ESTADO_CXP_ID,
               l_reg_cuenta_por_pagar.VALOR_PESOS,
               l_reg_cuenta_por_pagar.NIT_PAGADOR,
               l_reg_cuenta_por_pagar.TIPO_PAGO_ID,
               l_reg_cuenta_por_pagar.SOLICITUD_ID_BENEFICIO,
               l_reg_cuenta_por_pagar.CUENTA_POR_COBRAR_ID
        FROM MPENGES.SPG_CUENTA_POR_PAGAR CXP
        WHERE CXP.CUENTA_POR_PAGAR_ID = l_reg_pago.CUENTA_POR_PAGAR_ID;
     EXCEPTION WHEN OTHERS THEN
       l_error := 'CUENTA_POR_PAGAR_ID = ' || l_reg_pago.CUENTA_POR_PAGAR_ID || ', ' || '[CONSULTANDO_CUENTA_POR_PAGAR] Error[' || SQLERRM ||
      '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
       RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

     IF(l_reg_pago.eps_id IS NOT NULL)
     THEN
         BEGIN
           SELECT E.NIT,E.DESCRIPCION
           INTO  l_nitEps,l_nombreEps
           FROM MPENGES.SPG_EPS E
           WHERE E.EPS_ID=l_reg_pago.eps_id;
         EXCEPTION WHEN OTHERS  THEN
            l_nitEps :=0;
            l_error := 'EPS_ID = ' || l_reg_pago.eps_id || ', ' || '[CONSULTANDO_EPS] Error[' || SQLERRM ||
          '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         END;
     ELSE
       l_nitEps :=0;
     END IF;

     IF(l_reg_pago.CAJA_COMPENSACION_ID IS NOT NULL)
     THEN
         BEGIN
           SELECT CA.NIT,CA.DESCRIPCION
           INTO  l_nitCaja,l_nombreCaja
           FROM MPENGES.SPG_CAJA_COMPENSACION CA
           WHERE CA.CAJA_COMPENSACION_ID = l_reg_pago.CAJA_COMPENSACION_ID;

         EXCEPTION WHEN OTHERS  THEN
            l_nitCaja :=0;
            l_error := 'CAJA_COMPENSACION_ID = ' || l_reg_pago.CAJA_COMPENSACION_ID || ', ' || '[CONSULTANDO_CAJA] Error[' || SQLERRM ||
          '] Traza[' || dbms_utility.format_error_backtrace || ']' ;

         END;
     ELSE
       l_nitCaja :=0;
     END IF;

     BEGIN
       SELECT S.SOLICITUD_ID,
              S.CUENTA_AFILIADO,
              S.TIPO_RECLAMACION_ID,
              S.ESTADO_RECLAMACION_ID,
              S.SOLICITUD_ID_BENEFICIO,
              S.CODIGO_RADICACION_REC
       INTO   l_reg_solicitud.SOLICITUD_ID,
              l_reg_solicitud.CUENTA_AFILIADO,
              l_reg_solicitud.TIPO_RECLAMACION_ID,
              l_reg_solicitud.ESTADO_RECLAMACION_ID,
              l_reg_solicitud.SOLICITUD_ID_BENEFICIO,
              l_reg_solicitud.CODIGO_RADICACION_REC
       FROM MPENGES.SPG_SOLICITUD S
       WHERE S.SOLICITUD_ID = l_reg_cuenta_por_pagar.solicitud_id;
     EXCEPTION WHEN OTHERS THEN
        l_error := 'SOLICITUD_ID = ' || l_reg_cuenta_por_pagar.solicitud_id || ', ' || '[CONSULTANDO_SOLICITUD] Error[' || SQLERRM ||
      '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
       RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

     BEGIN
          SELECT NP.NOMINA_PENSIONADO_ID INTO l_nominaPensionadoId
          FROM MPENGES.SPG_PAGO P ,
              MPENGES.SPG_SOLICITUD S,
              MPENGES.SPG_BENEFICIARIO B,
              MPENGES.SPG_CUENTA_POR_PAGAR C,
              MNOMPEN.NOMP_NOMINA_PENSIONADO NP
          WHERE P.SOLICITUD_ID_BENEFICIO = S.SOLICITUD_ID
          AND C.SOLICITUD_ID = S.SOLICITUD_ID
          AND C.BENEFICIARIO_ID = B.BENEFICIARIO_ID
          AND NP.SOLICITUD_ID = S.SOLICITUD_ID
          AND NP.BENEFICIARIO_ID = B.BENEFICIARIO_ID
          AND P.PAGO_ID = IN_REG_PAGO.pago_id
          AND C.CUENTA_POR_PAGAR_ID = IN_REG_PAGO.cuenta_por_pagar_id;
     EXCEPTION WHEN OTHERS THEN
         l_nominaPensionadoId := 0;
     END;

   ---  DBMS_OUTPUT.PUT_LINE('------- VALIDA ESTADO DE LA CXP -------');

     IF(l_reg_cuenta_por_pagar.ESTADO_CXP_ID NOT IN ('ANULADA','PAGADA'))
     THEN
         l_ejecutarMotor := OBTENER_VALOR_GEN_PARAMETRO(1,2400,'NOM_PARAM_EJEC_MOTOR_LIQ');
         --Ejecuci¿¿n del Motor de validaciones de nomina
         --DBMS_OUTPUT.PUT_LINE(IN_REG_PAGO.PAGO_ID||'-------ENTRO EJECUTAR MOTOR='||to_char(SYSDATE, 'YYYY-MM-DD HH24:MI.SS'));
         IF (l_ejecutarMotor IN ('S')) THEN
             MNOMPEN.NOMP_MOTOR_NOM_PENSIONADOS_PCK.INVOCAR_PROCESO(l_reg_solicitud.SOLICITUD_ID,
                                                                    l_reg_cuenta_por_pagar.TIPO_PAGO_ID,
                                                                    IN_REG_PAGO.Periodo,
                                                                    'LIQ',
                                                                    l_respuestaMotor);
         --DBMS_OUTPUT.PUT_LINE(IN_REG_PAGO.PAGO_ID||'-------FIN EJECUTAR MOTOR='||to_char(SYSDATE, 'YYYY-MM-DD HH24:MI.SS'));
         ELSE
             l_respuestaMotor := 'BIEN';
         END IF;

       ---  DBMS_OUTPUT.PUT_LINE('------- RESPUESTA MOTOR -------' || l_respuestaMotor);
         IF (l_respuestaMotor = 'BIEN') THEN

            BEGIN
              ---validar beneficiario nomina pensionado existe
              SELECT COUNT(N.NOMINA_PENSIONADO_ID)
                INTO l_validacionMotorOk
                FROM MNOMPEN.NOMP_NOMINA_PENSIONADO N
               WHERE N.ESTADO_NOMINA = 'ACTIVO'
                 AND EXISTS
               (SELECT R.Nomina_Pensionado_Id
                        FROM MNOMPEN.NOMP_RESPUESTA_VALIDACION R, MNOMPEN.NOMP_DETALLE_RTA_VALIDACION D
                       WHERE R.NOMINA_PENSIONADO_ID = N.NOMINA_PENSIONADO_ID
                         AND R.RESPUESTA_VALIDACION_ID= D.RESPUESTA_VALIDACION_ID
                         AND R.VALIDACION_SUSPENDIDO = 'N'
                         AND D.ESTADO_VALIDACION = 'ACTIVO'
                         AND D.CONFIGURACION_VALIDACION_ID IN (SELECT CONF.CONFIGURACION_VALIDACION_ID
                                                               FROM MNOMPEN.NOMP_CONFIGURACION_VALIDACION CONF
                                                               WHERE CONF.VAL_LIQUIDACION = 'S'))
                 AND N.SOLICITUD_ID = l_reg_solicitud.SOLICITUD_ID
                 AND N.NOMINA_PENSIONADO_ID = l_nominaPensionadoId
                 AND rownum = 1;
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                l_validacionMotorOk := 0;
            END;

           ---  DBMS_OUTPUT.PUT_LINE('------- VALIDO RESUALRADO DEL MOTOR --------- ');

              IF(l_validacionMotorOk = 0)
              THEN
                   -- SE OBTIENE EL NIT DEL FONDO Y SOCIEDAD
                  l_nitFondo             := TO_NUMBER(NVL(OBTENER_VALOR_GEN_PARAMETRO(C_FONDO_PENSION_OBLIG,C_MODULO_PENS_GESTION,C_PARAMETRO_FONDO),0));
                  l_nitSociedad          := TO_NUMBER(NVL(OBTENER_VALOR_GEN_PARAMETRO(C_FONDO_PENSION_OBLIG,C_MODULO_PENS_GESTION,C_PARAMETRO_SOCIEDAD),0));

                  -- Reinicia la variable cada vez que pase
                  l_periodo_suspendido := TRUE;

DECLARE
   TYPE t_estados_tab IS TABLE OF VARCHAR2(30);
   l_estados t_estados_tab;
BEGIN
   SELECT DP.ESTADO
   BULK COLLECT INTO l_estados
   FROM MPENGES.SPG_DETALLE_PAGO DP
   WHERE DP.PAGO_ID = IN_REG_PAGO.pago_id
     AND DP.PERIODO = IN_REG_PAGO.Periodo;

   FOR i IN 1 .. l_estados.COUNT LOOP
     IF l_estados(i) != 'SUSPENDIDO' THEN
        l_periodo_suspendido := FALSE;
        EXIT;
     END IF;
   END LOOP;
END;

                   IF l_periodo_suspendido = FALSE THEN

                   -- Se valida si es GPM y se realiza la apropiaci¿¿¿¿n si es verdadero
                   MNOMPEN.NOMP_PRELIQUIDACION_NOMINA_PCK.VAL_LIQUIDA_GPM(l_reg_solicitud.SOLICITUD_ID,
                                                                          C_COD_OPER_RET_PROGRAMADO,
                                                                          l_nominaPensionadoId,
                                                                          IN_REG_PAGO.Periodo,
                                                                          'N',
                                                                          l_rta_val_gpm,
                                                                          l_cod_rta_val_gpm);

                    --   DBMS_OUTPUT.PUT_LINE('------- VALIDA GPM -------' || l_cod_rta_val_gpm);

                   ELSE
                    IF  l_reg_cuenta_por_pagar.nit_pagador = l_nitSociedad THEN
                        l_cod_rta_val_gpm:= '05';
                    ELSE
                        l_cod_rta_val_gpm:= '00';
                    END IF;
                   END IF;

                   BEGIN

                       SELECT F.VALOR
                       INTO   l_fuente_financiamiento
                       FROM MNOMPEN.NOMP_PRELIQUIDACION_PAGO P,
                            MNOMPEN.NOMP_FUENTE_FINANCIAMIENTO F
                       WHERE F.FUENTE_FINANCIAMIENTO_ID = P.FUENTE_FINANCIAMIENTO
                       AND   P.PAGO_ID = l_reg_pago.pago_id;

                       l_nitPagadorPreli := to_number(l_fuente_financiamiento);

                   EXCEPTION
                      WHEN NO_DATA_FOUND THEN
                          l_error := '-PAGO_ID['||IN_REG_PAGO.PAGO_ID||']. PAGO NO EXIXTE EN PRELIQUIDACION NOMINA , VER LOGS PARA MAS INFO.';
                          RAISE_APPLICATION_ERROR (-20001, l_error);
                   END;


                   IF (l_cod_rta_val_gpm IS NOT NULL
                       AND l_cod_rta_val_gpm NOT IN ('00','02','05','08')) THEN

                       l_error := 'SOLICITUD_ID ['||l_reg_solicitud.SOLICITUD_ID||']-PAGO_ID['||IN_REG_PAGO.PAGO_ID||']. CASO GPM CON RESPUESTA DE APROPIACION ERRADA ['||l_cod_rta_val_gpm||'], VER LOGS PARA MAS INFO.';
                       RAISE_APPLICATION_ERROR (-20001, l_error);

                   ELSIF ((l_cod_rta_val_gpm IS NULL
                           AND l_reg_cuenta_por_pagar.NIT_PAGADOR != l_nitSociedad)
                           OR l_cod_rta_val_gpm  IN ('00','02','08'))
                     THEN

                    --  DBMS_OUTPUT.PUT_LINE('------- APLICA PAGO FONDO-------');

                       APLICA_PAGO_FONDO(l_reg_solicitud,
                                         l_reg_pago,
                                         l_reg_cuenta_por_pagar,
                                         l_nominaPensionadoId,
                                         IN_USUARIO,
                                         OUT_COD_RESPUESTA,
                                         OUT_MENSAJE);

                       IF l_reg_cuenta_por_pagar.nit_pagador != l_nitFondo
                          THEN

                             UPDATE MPENGES.SPG_CUENTA_POR_PAGAR C
                             SET    C.NIT_PAGADOR = l_nitFondo,
                                    C.USUARIO_ULTIMA_MODIFICACION = 'APLICA_PAGO_NOMINA',
                                    C.FECHA_ULTIMA_MODIFICACION = l_sysdate_pago
                             WHERE C.CUENTA_POR_PAGAR_ID = l_reg_cuenta_por_pagar.cuenta_por_pagar_id;

                        END IF;

                        IF l_nitPagadorPreli != l_nitFondo
                           THEN

                               UPDATE MNOMPEN.NOMP_PRELIQUIDACION_PAGO P
                               SET    P.FUENTE_FINANCIAMIENTO = 3,
                                      P.USUARIO_ULTIMA_MODIFICACION = 'APLICA_PAGO_NOMINA',
                                      P.FECHA_ULTIMA_MODIFICACION = l_sysdate_pago
                                WHERE P.PAGO_ID = l_reg_pago.pago_id;

                         END IF;



                   ELSIF (l_cod_rta_val_gpm IS NULL OR l_cod_rta_val_gpm = '05')
                     THEN

                  --   DBMS_OUTPUT.PUT_LINE('------- APLICA SOCIDAD-------');

                      APLICA_PAGO_SOCIEDAD (l_reg_solicitud,
                                           l_reg_pago,
                                           l_reg_cuenta_por_pagar,
                                           l_nominaPensionadoId,
                                           IN_USUARIO,
                                           OUT_COD_RESPUESTA,
                                           OUT_MENSAJE);

                        IF l_reg_cuenta_por_pagar.nit_pagador != l_nitSociedad
                          THEN

                             UPDATE MPENGES.SPG_CUENTA_POR_PAGAR C
                             SET    C.NIT_PAGADOR = l_nitSociedad,
                                    C.USUARIO_ULTIMA_MODIFICACION = 'APLICA_PAGO',
                                    C.FECHA_ULTIMA_MODIFICACION = l_sysdate_pago
                             WHERE C.CUENTA_POR_PAGAR_ID = l_reg_cuenta_por_pagar.cuenta_por_pagar_id;

                        END IF;

                        IF l_nitPagadorPreli != l_nitSociedad
                           THEN

                               UPDATE MNOMPEN.NOMP_PRELIQUIDACION_PAGO P
                               SET    P.FUENTE_FINANCIAMIENTO = 4,
                                      P.USUARIO_ULTIMA_MODIFICACION = 'APLICA_PAGO_NOMINA',
                                      P.FECHA_ULTIMA_MODIFICACION = l_sysdate_pago
                                WHERE P.PAGO_ID = l_reg_pago.pago_id;

                         END IF;
                   END IF;
              ELSE
                  l_error := 'SOLICITUD_ID ['||l_reg_solicitud.SOLICITUD_ID||']. VALIDACION(ES) INVALIDAS DEL MOTOR VALIDACIONES, VALIDAR LA RESPUESTA DEL MOTOR EN LA BASE DE DATOS';
                  RAISE_APPLICATION_ERROR (-20001, l_error);
              END IF;
          ELSE
              l_error := 'SOLICITUD_ID ['||l_reg_solicitud.SOLICITUD_ID||']. ERROR EN LA EJECUCION DEL MOTOR VALIDACIONES, VALIDAR LOG';
              RAISE_APPLICATION_ERROR (-20001, l_error);
          END IF;
     ELSE
         l_error := 'PAGO_ID ['||l_reg_pago.Pago_Id||'] - CUENTA POR PAGAR['||l_reg_cuenta_por_pagar.Cuenta_Por_Pagar_Id||'] EN ESTADO INVALIDO ['||l_reg_cuenta_por_pagar.ESTADO_CXP_ID||']';
         RAISE_APPLICATION_ERROR (-20001, l_error);
     END IF ;
     --ACTUALIZAR NUMERO PAGOS REALIZADOS EN CXP
     ACTUALIZAR_NUMERO_PAGOS(NULL,IN_USUARIO,l_reg_cuenta_por_pagar);

  EXCEPTION WHEN OTHERS THEN
    ROLLBACK;
    OUT_MENSAJE := 'ERROR EN EL METODO [APLICA_PAGO] : '|| SQLERRM || '-' || l_error;
    -- SI EL CODIGO DE RESPUESTA ES 2 SE DETIENE EL PROCESO
    IF(OUT_COD_RESPUESTA = 2) THEN
      OUT_COD_RESPUESTA := 1;
      RAISE_APPLICATION_ERROR (-20002, OUT_MENSAJE);
    ELSE
      OUT_COD_RESPUESTA := 1;
      IF(l_reg_cuenta_por_pagar.ESTADO_CXP_ID NOT IN ('ANULADA','PAGADA'))
      THEN
          ACTUALIZAR_CXP_ESTADO (l_reg_cuenta_por_pagar,'RECHAZADA');
      END IF;
      mpenges.spg_validador_pck.inserta_causal_error(user, l_reg_cuenta_por_pagar.cuenta_por_pagar_id, 'ERROR_APLICA_PAGO');
      --ACTUALIZA ESTADO DETALLES DE PAGO A CON_ERROR
      ACTUALIZAR_DETALLE_PAGO(NULL, NULL, IN_USUARIO, NULL, NULL, NULL,IN_REG_PAGO.PAGO_ID,NULL);
      INSERTA_ERROR('PAGO', l_reg_pago.pago_lote_id, l_reg_cuenta_por_pagar.cuenta_por_pagar_id, OUT_MENSAJE);
    END IF;
  END APLICA_PAGO;