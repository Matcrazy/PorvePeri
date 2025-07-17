PROCEDURE APLICA_PAGO_FONDO (IN_SOLICITUD            MPENGES.SPG_SOLICITUD%ROWTYPE,
                               IN_REG_PAGO             MPENGES.SPG_PAGO%ROWTYPE,
                               IN_REG_CUENTA_POR_PAGAR MPENGES.SPG_CUENTA_POR_PAGAR%ROWTYPE,
                               IN_NOMINA_PENSIONADO_ID MNOMPEN.NOMP_NOMINA_PENSIONADO.NOMINA_PENSIONADO_ID%TYPE,
                               IN_USUARIO IN VARCHAR2,
                               OUT_COD_RESPUESTA OUT NUMBER,
                               OUT_MENSAJE OUT VARCHAR2)
  IS
   l_reg_pago             MPENGES.SPG_PAGO%ROWTYPE;
   l_reg_cuenta_por_pagar MPENGES.SPG_CUENTA_POR_PAGAR%ROWTYPE;
   l_detenerProceso       BOOLEAN := FALSE;

   l_inversion_conf       NUMBER(20);
   l_sumaSaldos           NUMBER(18,8);
   l_unidadesPago         NUMBER(18,8);
   l_consecutivo_pago_id  NUMBER(20);
   l_asiento_contable_id  NUMBER(20);
   l_estadoSaap           NUMBER(5);

   l_descripcionInversion VARCHAR2(200);
   l_error                VARCHAR2(2000);
   l_fechaOperacion       DATE;

   l_codOperacionId         MPENGES.SPG_OPERACION_CONCEPTO.CODIGO_OPERACION_ID%TYPE;
   l_valorUnidadInversion   MGENERAL.GEN_VALOR_UNIDAD.VALOR_UNIDAD%TYPE;
   l_reg_saldo_portafolio   mcuentas.CTA_SALDO_PORTAFOLIO%ROWTYPE;
   l_reg_cuenta_movimiento  mcuentas.cta_cuenta_movimiento%ROWTYPE;
   l_reg_tipo_pago          mpenges.spg_tipo_pago%ROWTYPE;
   l_reg_solicitud          mpenges.spg_solicitud%ROWTYPE;
   l_reg_operacion_concepto mpenges.spg_operacion_concepto%ROWTYPE;
   l_reg_detalle_pago       mpenges.spg_detalle_pago%ROWTYPE;
   l_bloqueoOperacion       MGENERAL.GEN_FONDO.BLOQUEO_OPERACION%TYPE;
   l_bloqueoPagos           MGENERAL.GEN_FONDO.BLOQUEO_PAGOS%TYPE;
   l_bloqueoTransferencia   MGENERAL.GEN_FONDO.BLOQUEO_TRANSFERENCIA%TYPE;
   l_estadoProcesoCierre    MGENERAL.GEN_FONDO.ESTADO_PROCESO_CIERRE%TYPE;

   l_vCodigoError  NUMBER(10);
   l_vMensajeError VARCHAR2(2000);

   CURSOR OBTENER_DETALLE_PAGO (P_PAGO_ID IN NUMBER)
   IS
      SELECT P.DETALLE_PAGO_ID
            , P.PAGO_ID
            , P.OPERACION_CONCEPTO_ID
            , P.VALOR_PESOS
            , P.AFECTA_PAGO
            , P.BENEFICIARIO_ID
            , P.MEDIO_PAGO
            , P.BANCO_ID
            , P.TIPO_CUENTA
            , P.CUENTA_BANCARIA
            , P.OFICINA_PORVENIR_ID
            , P.ESTADO
            , P.ESTADO_SAAP
            , P.CONSECUTIVO_PAGO
            , P.ID_FACTURA
            , P.FECHA_CREACION
            , P.USUARIO_CREACION
            , P.FECHA_ULTIMA_MODIFICACION
            , P.USUARIO_ULTIMA_MODIFICACION
            , P.ESTADO_ERP_ID
            , P.NUMERO_ASIENTO_ID
            , P.PERSONA_PAGO
            , P.DESCUENTO_ADICIONAL_ID
            , P.MODALIDAD_GIRO
            , P.NUM_CHEQUE
            , P.NUM_AUTORIZACION
            , P.CUENTA_POR_COBRAR_ID
            , P.PERIODO
            , P.NUM_PAG_SEGURIDAD_SOCIAL
            , P.MFONDOS_SCN
            FROM  MPENGES.SPG_DETALLE_PAGO P, MPENGES.SPG_OPERACION_CONCEPTO C
            WHERE P.OPERACION_CONCEPTO_ID = C.OPERACION_CONCEPTO_ID
            AND C.TIPO_PAGO_ID= C_COD_OPER_RET_PROGRAMADO
            AND P.PAGO_ID = P_PAGO_ID
            ORDER BY P.OPERACION_CONCEPTO_ID ASC;
  BEGIN
     OUT_COD_RESPUESTA := 0;
     l_reg_solicitud := IN_SOLICITUD;
     l_reg_pago := IN_REG_PAGO;
     l_reg_cuenta_por_pagar := IN_REG_CUENTA_POR_PAGAR;
     g_valorUnidadInversion := 0;

     BEGIN

       SELECT tip.tipo_pago_id, tip.tipo_pago, tip.inversion_id
       INTO l_reg_tipo_pago.tipo_pago_id,
       l_reg_tipo_pago.tipo_pago,
       l_reg_tipo_pago.inversion_id
       FROM mpenges.spg_tipo_pago tip
       WHERE tip.tipo_pago_id=l_reg_cuenta_por_pagar.tipo_pago_id;
    EXCEPTION
       WHEN OTHERS THEN

        l_error := 'CUENTA_POR_PAGAR_ID = ' || l_reg_cuenta_por_pagar.cuenta_por_pagar_id || ', ' || '[CONSULTANDO_TIPO_PAGO] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
        RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

    BEGIN
       SELECT  con.codigo_operacion_id
       INTO  l_codOperacionId
       FROM mpenges.spg_operacion_concepto con
       WHERE con.tipo_pago_id=l_reg_tipo_pago.tipo_pago_id
       AND con.principal = 'S';
     EXCEPTION
      WHEN OTHERS THEN

        l_error := 'TIPO_PAGO_ID = ' || l_reg_tipo_pago.tipo_pago_id || ', ' || '[CONSULTANDO_COD_OPERACION_ID] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
        RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

     BEGIN
         SELECT  per.numero_identificacion,
         (select ti.abreviatura from mgeneral.gen_tipo_identificacion ti where ti.tipo_identificacion_id=per.tipo_identificacion)
         INTO l_numeroIdentificacion, l_tipoIdentificacion
         FROM mpenges.spg_persona per
         WHERE per.persona_id=l_reg_cuenta_por_pagar.persona_id
         AND rownum=1  ;
     EXCEPTION
      WHEN OTHERS THEN

        l_error := 'PAGO_ID = ' || l_reg_pago.pago_id || '-PERSONA_ID = ' || l_reg_cuenta_por_pagar.persona_id ||', ' || '[CONSULTANDO_PERSONA_PAGO] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         RAISE_APPLICATION_ERROR (-20001, l_error);
     END;
     l_nombrePersona:=mpenges.spg_utilidades_persona_pck.obtener_nombres_ben2_fn(NULL, l_reg_cuenta_por_pagar.persona_id,1);

    BEGIN
        SELECT
              (select inv.descripcion from mgeneral.gen_inversion inv
              where inv.inversion_id=sp.inversion_id and inv.fondo_id=sp.fondo_id)descripcion_inversion,
              SP.SALDO_PORTAFOLIO_ID,
              SP.CUENTA_ID,
              SP.FONDO_ID,
              SP.INVERSION_ID,
              SP.SALDO_OBLIGATORIO,
              SP.SALDO_VOL_AFILIADO,
              SP.SALDO_VOL_EMPLEADOR,
              SP.FECHA_CREACION,
              SP.USUARIO_CREACION,
              SP.FECHA_ULTIMA_MODIFICACION,
              SP.USUARIO_ULTIMA_MODIFICACION,
              SP.MFONDOS_SCN,
              SP.RETENCION_CONTINGENTE
         INTO  l_descripcionInversion,
               l_reg_saldo_portafolio.SALDO_PORTAFOLIO_ID,
               l_reg_saldo_portafolio.CUENTA_ID,
               l_reg_saldo_portafolio.FONDO_ID,
               l_reg_saldo_portafolio.INVERSION_ID,
               l_reg_saldo_portafolio.SALDO_OBLIGATORIO,
               l_reg_saldo_portafolio.SALDO_VOL_AFILIADO,
               l_reg_saldo_portafolio.SALDO_VOL_EMPLEADOR,
               l_reg_saldo_portafolio.FECHA_CREACION,
               l_reg_saldo_portafolio.USUARIO_CREACION,
               l_reg_saldo_portafolio.FECHA_ULTIMA_MODIFICACION,
               l_reg_saldo_portafolio.USUARIO_ULTIMA_MODIFICACION,
               l_reg_saldo_portafolio.MFONDOS_SCN,
               l_reg_saldo_portafolio.RETENCION_CONTINGENTE
       FROM   MCUENTAS.CTA_SALDO_PORTAFOLIO SP,
              MCUENTAS.CTA_CUENTA_DISPERSION CD
       WHERE  SP.CUENTA_ID = CD.CUENTA_ID
       AND    SP.INVERSION_ID = CD.INVERSION_ID
       AND    CD.ESTADO = 'ACTIVO'
       AND    CD.CUENTA_ID = l_reg_solicitud.cuenta_afiliado;

     EXCEPTION
      WHEN NO_DATA_FOUND THEN
         l_error := 'CUENTA_ID = ' || l_reg_solicitud.cuenta_afiliado || ', NO SE ENCONTRARON PORTAFOLIOS ACTIVOS ' || '[CONSULTANDO_SALDO_AFILIADO] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         RAISE_APPLICATION_ERROR (-20001, l_error);
      WHEN TOO_MANY_ROWS THEN
        l_error := 'CUENTA_ID = ' || l_reg_solicitud.cuenta_afiliado || ', EL AFILIADO TIENE MAS DE UN PORTAFOLIO ACTIVO,' || '[CONSULTANDO_SALDO_AFILIADO] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         RAISE_APPLICATION_ERROR (-20001, l_error);
      WHEN OTHERS THEN
        l_error := 'CUENTA_ID = ' || l_reg_solicitud.cuenta_afiliado || ', ' || '[CONSULTANDO_SALDO_AFILIADO] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

     l_sumaSaldos := l_reg_saldo_portafolio.SALDO_VOL_EMPLEADOR +  l_reg_saldo_portafolio.SALDO_VOL_AFILIADO + l_reg_saldo_portafolio.SALDO_OBLIGATORIO;

     BEGIN

       SELECT ppi.INVERSION_ID
       INTO l_inversion_conf
       FROM MPENGES.SPG_TIPO_PAGO_POR_INVERSION  ppi
       WHERE ppi.TIPO_PAGO_ID = l_reg_tipo_pago.tipo_pago_id
       AND ppi.inversion_id=l_reg_saldo_portafolio.INVERSION_ID;

     EXCEPTION
       WHEN OTHERS THEN

        l_inversion_conf:=NULL;
        l_error := 'TIPO_PAGO_ID = ' || l_reg_tipo_pago.tipo_pago_id||'INVERSION_ID = ' || l_reg_saldo_portafolio.INVERSION_ID ||  ' INVALIDA PARA EL TIPO DE PAGO, ' ||l_reg_tipo_pago.tipo_pago_id || '[CONSULTANDO_CONFIGURACION_INVERSION] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

    IF(l_inversion_conf IN (4)) THEN
     BEGIN
         SELECT V.VALOR_UNIDAD,
                F.FECHA_OPERACION,
                F.BLOQUEO_OPERACION,
                F.BLOQUEO_PAGOS,
                F.BLOQUEO_TRANSFERENCIA,
                F.ESTADO_PROCESO_CIERRE
         INTO l_valorUnidadInversion,
              l_fechaOperacion,
              l_bloqueoOperacion,
              l_bloqueoPagos,
              l_bloqueoTransferencia,
              l_estadoProcesoCierre
         FROM MGENERAL.GEN_FONDO F,
              MGENERAL.GEN_INVERSION I,
              MGENERAL.GEN_VALOR_UNIDAD V
         WHERE F.FONDO_ID = I.FONDO_ID AND
              I.INVERSION_ID = V.INVERSION_ID AND
              I.FONDO_ID = V.FONDO_ID AND
              TRUNC(V.FECHA) = TRUNC(F.FECHA_OPERACION)
          AND F.FONDO_ID =  C_FONDO_PENSION_OBLIG
          AND I.INVERSION_ID =  l_inversion_conf;

          g_valorUnidadInversion := l_valorUnidadInversion;
     EXCEPTION WHEN OTHERS THEN
        l_error := 'FONDO_ID = ' || l_reg_solicitud.cuenta_afiliado || 'INVERSION_ID = ' || l_inversion_conf ||', ' || '[CONSULTANDO_VALORINVERSION_FECHAOPERACION] Error[' || SQLERRM ||
        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
         RAISE_APPLICATION_ERROR (-20001, l_error);
     END;

     /*
     IF(l_fechaOperacion != IN_REG_PAGO.Periodo) THEN
        l_error := '[VALIDANDO_FECHA_OPERACION] - No existe valor de unidad de inversi¿¿n para el periodo ['||IN_REG_PAGO.Periodo||']' ;
        l_detenerProceso := TRUE;
        RAISE_APPLICATION_ERROR (-20001, l_error);
     END IF ;
     */
     IF(l_bloqueoOperacion      = 'S' OR
        l_bloqueoPagos          = 'S' OR
        l_bloqueoTransferencia  = 'S' OR
        l_estadoProcesoCierre   = 'S' )THEN

        l_error := '[VALIDANDO_ESTADO_FONDO] - ESTADO DEL FONDO CERRADO : BLOQUEO_OPERACION ['||l_bloqueoOperacion||'] '||
                   ' - BLOQUEO_PAGOS ['||l_bloqueoPagos||'] '||
                   ' - BLOQUEO_TRANSFERENCIA ['||l_bloqueoTransferencia||'] '||
                   ' - ESTADO_PROCESO_CIERRE ['||l_estadoProcesoCierre||'] ';
       l_detenerProceso := TRUE;
       RAISE_APPLICATION_ERROR (-20001, l_error);
     END IF;

    ELSE
       l_error := 'INVERSION_ID = ' || l_inversion_conf ||  ' INVALIDA, VALIDA (4), ITEM KEY [' ||l_reg_solicitud.CODIGO_RADICACION_REC||']';
       RAISE_APPLICATION_ERROR (-20001, l_error);
    END IF;

    l_unidadesPago := l_reg_pago.VALOR_PESOS_TOTAL / l_valorUnidadInversion;

    IF l_sumaSaldos < l_unidadesPago AND l_periodo_suspendido = FALSE THEN

        l_error := ' EL SALDO DE LA CUENTA [' || l_sumaSaldos ||  '] ES MENOR DEL VALOR DEL PAGO ['||l_unidadesPago||'], ITEM KEY [' ||l_reg_solicitud.CODIGO_RADICACION_REC||']';
        RAISE_APPLICATION_ERROR (-20001, l_error);

    ELSE
       --OBTENER ID DEL ASIENTO CONTABLE Y CONSECUTIVO DE PAGO
       /* BEGIN
            EXECUTE IMMEDIATE l_sentencia_cons_pago INTO l_consecutivo_pago_id;
            EXECUTE IMMEDIATE l_sentencia_asiento_id INTO l_asiento_contable_id;
        EXCEPTION
          WHEN OTHERS THEN

           l_error :=  'ITEM KEY [' ||l_reg_solicitud.CODIGO_RADICACION_REC||'], ' || '[OBTENIENDO_ASIENTO_CONSECUTIVO_PAGO] Error[' || SQLERRM ||
           '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
           RAISE_APPLICATION_ERROR (-20001, l_error);
        END;*/



        -- Declaración de tipo y colección
TYPE t_detalle_pago IS TABLE OF OBTENER_DETALLE_PAGO%ROWTYPE;
v_detalles_pago t_detalle_pago;

-- Uso de BULK COLLECT
OPEN OBTENER_DETALLE_PAGO(l_reg_pago.pago_id);
FETCH OBTENER_DETALLE_PAGO BULK COLLECT INTO v_detalles_pago;
CLOSE OBTENER_DETALLE_PAGO;

FOR i IN v_detalles_pago.FIRST .. v_detalles_pago.LAST LOOP
    CUR_DETALLE_PAGO := v_detalles_pago(i);
            -- OBTIENE EL REGISTRO DE DETALLE DE PAGO
              l_reg_detalle_pago.DETALLE_PAGO_ID := CUR_DETALLE_PAGO.DETALLE_PAGO_ID            ;
              l_reg_detalle_pago.PAGO_ID := CUR_DETALLE_PAGO.PAGO_ID                    ;
              l_reg_detalle_pago.OPERACION_CONCEPTO_ID := CUR_DETALLE_PAGO.OPERACION_CONCEPTO_ID      ;
              l_reg_detalle_pago.VALOR_PESOS := CUR_DETALLE_PAGO.VALOR_PESOS                ;
              l_reg_detalle_pago.AFECTA_PAGO := CUR_DETALLE_PAGO.AFECTA_PAGO                ;
              l_reg_detalle_pago.BENEFICIARIO_ID := CUR_DETALLE_PAGO.BENEFICIARIO_ID            ;
              l_reg_detalle_pago.MEDIO_PAGO := CUR_DETALLE_PAGO.MEDIO_PAGO                 ;
              l_reg_detalle_pago.BANCO_ID := CUR_DETALLE_PAGO.BANCO_ID                   ;
              l_reg_detalle_pago.TIPO_CUENTA := CUR_DETALLE_PAGO.TIPO_CUENTA                ;
              l_reg_detalle_pago.CUENTA_BANCARIA := CUR_DETALLE_PAGO.CUENTA_BANCARIA            ;
              l_reg_detalle_pago.OFICINA_PORVENIR_ID := CUR_DETALLE_PAGO.OFICINA_PORVENIR_ID        ;
              l_reg_detalle_pago.ESTADO := CUR_DETALLE_PAGO.ESTADO                     ;
              l_reg_detalle_pago.ESTADO_SAAP := CUR_DETALLE_PAGO.ESTADO_SAAP                ;
              l_reg_detalle_pago.CONSECUTIVO_PAGO := CUR_DETALLE_PAGO.CONSECUTIVO_PAGO           ;
              l_reg_detalle_pago.ID_FACTURA := CUR_DETALLE_PAGO.ID_FACTURA                 ;
              l_reg_detalle_pago.FECHA_CREACION := CUR_DETALLE_PAGO.FECHA_CREACION             ;
              l_reg_detalle_pago.USUARIO_CREACION := CUR_DETALLE_PAGO.USUARIO_CREACION           ;
              l_reg_detalle_pago.FECHA_ULTIMA_MODIFICACION := CUR_DETALLE_PAGO.FECHA_ULTIMA_MODIFICACION  ;
              l_reg_detalle_pago.USUARIO_ULTIMA_MODIFICACION := CUR_DETALLE_PAGO.USUARIO_ULTIMA_MODIFICACION;
              l_reg_detalle_pago.ESTADO_ERP_ID := CUR_DETALLE_PAGO.ESTADO_ERP_ID              ;
              l_reg_detalle_pago.NUMERO_ASIENTO_ID := CUR_DETALLE_PAGO.NUMERO_ASIENTO_ID          ;
              l_reg_detalle_pago.PERSONA_PAGO := CUR_DETALLE_PAGO.PERSONA_PAGO               ;
              l_reg_detalle_pago.DESCUENTO_ADICIONAL_ID := CUR_DETALLE_PAGO.DESCUENTO_ADICIONAL_ID     ;
              l_reg_detalle_pago.MODALIDAD_GIRO := CUR_DETALLE_PAGO.MODALIDAD_GIRO             ;
              l_reg_detalle_pago.NUM_CHEQUE := CUR_DETALLE_PAGO.NUM_CHEQUE                 ;
              l_reg_detalle_pago.NUM_AUTORIZACION := CUR_DETALLE_PAGO.NUM_AUTORIZACION           ;
              l_reg_detalle_pago.CUENTA_POR_COBRAR_ID := CUR_DETALLE_PAGO.CUENTA_POR_COBRAR_ID       ;
              l_reg_detalle_pago.PERIODO := CUR_DETALLE_PAGO.PERIODO                    ;
              l_reg_detalle_pago.NUM_PAG_SEGURIDAD_SOCIAL := CUR_DETALLE_PAGO.NUM_PAG_SEGURIDAD_SOCIAL   ;
              l_reg_detalle_pago.MFONDOS_SCN := CUR_DETALLE_PAGO.MFONDOS_SCN  ;

              l_global_pago_id := CUR_DETALLE_PAGO.PAGO_ID;

              BEGIN
                 SELECT  per.numero_identificacion,
                 (select ti.abreviatura from mgeneral.gen_tipo_identificacion ti where ti.tipo_identificacion_id=per.tipo_identificacion)
                 INTO l_numeroIdentificacion, l_tipoIdentificacion
                 FROM mpenges.spg_persona per
                 WHERE per.persona_id= l_reg_detalle_pago.Persona_Pago
                 AND rownum=1  ;
             EXCEPTION
              WHEN OTHERS THEN

                 l_numeroIdentificacion := C_NIT_ENT_APORT_LINEA;
                 l_tipoIdentificacion   := 'NIT';

              END;

        l_nombrePersona:=mpenges.spg_utilidades_persona_pck.obtener_nombres_ben2_fn(NULL, l_reg_detalle_pago.Persona_Pago,1);

        --OBTENER ID DEL ASIENTO CONTABLE Y CONSECUTIVO DE PAGO
        BEGIN
            EXECUTE IMMEDIATE l_sentencia_cons_pago INTO l_consecutivo_pago_id;
            EXECUTE IMMEDIATE l_sentencia_asiento_id INTO l_asiento_contable_id;
        EXCEPTION
          WHEN OTHERS THEN

           l_error :=  'ITEM KEY [' ||l_reg_solicitud.CODIGO_RADICACION_REC||'], ' || '[OBTENIENDO_ASIENTO_CONSECUTIVO_PAGO] Error[' || SQLERRM ||
           '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
           RAISE_APPLICATION_ERROR (-20001, l_error);
        END;

            --sE VALIDA SI SE ENCUENTRA EN ESTADO SUSPENDIDO
            IF (l_reg_detalle_pago.ESTADO IN ('SUSPENDIDO')) THEN

                 PAGOS_SUSPENDIDOS ( l_reg_cuenta_por_pagar,
                                     l_reg_pago,
                                     l_reg_detalle_pago,
                                     IN_USUARIO);

                 ACTUALIZAR_NOMINA_PENSIONADO(IN_REG_PAGO,
                                              IN_NOMINA_PENSIONADO_ID,
                                              IN_USUARIO);


            ELSIF l_reg_detalle_pago.ESTADO IN ('APROBADO') THEN

                  -- DBMS_OUTPUT.PUT_LINE('------- APLICA PAGO FONDO  ESTADO LOTE -------');

                  --OBTENER CTA_MOVIMIENTO BASE
                  l_reg_cuenta_movimiento.cuenta_id                    :=l_reg_solicitud.cuenta_afiliado;
                  l_reg_cuenta_movimiento.id_disponible                :='S';
                  l_reg_cuenta_movimiento.naturaleza                   :='D';
                  l_reg_cuenta_movimiento.usuario_Creacion             := IN_USUARIO;
                  l_reg_cuenta_movimiento.fecha_Creacion               :=SYSDATE;
                  l_reg_cuenta_movimiento.MFONDOS_SCN                  :=0;
                  l_reg_cuenta_movimiento.fecha_Operacion              :=l_fechaOperacion;
                  l_reg_cuenta_movimiento.fondo_Id                     :=C_FONDO_PENSION_OBLIG;
                  l_reg_cuenta_movimiento.inversion_Id                 :=l_reg_saldo_portafolio.inversion_id;
                  l_reg_cuenta_movimiento.tipo_id_nit_pago             :=l_tipoIdentificacion;
                  l_reg_cuenta_movimiento.nit_Pago                     :=l_numeroIdentificacion;
                  l_reg_cuenta_movimiento.fecha_pago                   :=l_fechaOperacion;
                  l_reg_cuenta_movimiento.periodo_Pago                 :=to_number(l_reg_pago.periodo);
                  l_reg_cuenta_movimiento.usuario_ultima_modificacion  := null ;
                  l_reg_cuenta_movimiento.fecha_ultima_modificacion    := null ;
                  l_reg_cuenta_movimiento.consecutivo_pago             :=l_consecutivo_pago_id;
                  l_reg_cuenta_movimiento.Numero_Asiento_Id            :=l_asiento_contable_id;
                  l_reg_cuenta_movimiento.Codigo_Operacion_Id          :=l_codOperacionId;

                  -- OBTENER OPERACI¿¿N CONCEPTO
                  BEGIN
                       SELECT con.operacion_concepto_id,
                              con.descuenta,
                              con.caja_pago,
                              con.codigo_operacion_id,
                              con.operacion,
                              con.principal
                       INTO l_reg_operacion_concepto.Operacion_Concepto_Id,
                       l_reg_operacion_concepto.Descuenta,
                       l_reg_operacion_concepto.Caja_Pago,
                       l_reg_operacion_concepto.Codigo_Operacion_Id,
                       l_reg_operacion_concepto.operacion,
                       l_reg_operacion_concepto.principal
                       FROM mpenges.spg_operacion_concepto con
                       WHERE con.operacion_concepto_id =  l_reg_detalle_pago.OPERACION_CONCEPTO_ID
                       AND  con.tipo_pago_id = l_reg_pago.tipo_pago_id;
                     EXCEPTION
                      WHEN OTHERS THEN
                        l_error := 'PAGO_ID = ' || l_reg_pago.pago_id || ', OPERACION_CONCEPTO_ID = ' || l_reg_detalle_pago.OPERACION_CONCEPTO_ID || '[CONSULTANDO_OPERACION_CONCEPTO] Error[' || SQLERRM ||
                        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
                        RAISE_APPLICATION_ERROR (-20001, l_error);
                     END;

                     IF(l_codOperacionId IN (C_COD_OPER_RET_PROGRAMADO))
                     THEN
                       --l_reg_cuenta_movimiento.consecutivo_pago := l_consecutivo_pago_id;
                       --l_reg_cuenta_movimiento.numero_asiento_id := l_asiento_contable_id;

                         IF(l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_RET_PROGRAMADO,
                                                                               C_OPER_CONCEPTO_DESC_ADICIONAL,
                                                                               C_OPER_CONCEPTO_MES_ADICIONAL,
                                                                               C_OPER_CONCEPTO_CAJA_APORT_VOL
                                                                               )

                           ) THEN

                             GENERAR_MOV_RET_PROGRAMADO(l_reg_cuenta_movimiento,
                                                       l_reg_solicitud,
                                                       l_reg_detalle_pago,
                                                       l_valorUnidadInversion,
                                                       l_reg_operacion_concepto,
                                                       IN_USUARIO,
                                                       l_asiento_contable_id,
                                                       l_vCodigoError,
                                                       l_vMensajeError);

                             IF l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_DESC_ADICIONAL) THEN

                                ACTUALIZAR_NUMERO_PAGOS(l_reg_detalle_pago,IN_USUARIO,NULL);
                             END IF;

                         ELSIF(l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_FSP)) THEN

                            GENERAR_MOVIMIENTO_FSP(l_reg_cuenta_movimiento,
                                                   l_reg_solicitud,
                                                   l_reg_detalle_pago,
                                                   l_valorUnidadInversion,
                                                   l_reg_operacion_concepto,
                                                   IN_USUARIO,
                                                   l_asiento_contable_id,
                                                   l_vCodigoError,
                                                   l_vMensajeError);
                         ELSIF(l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_EPS)) THEN

                            GENERAR_MOVIMIENTO_EPS(l_reg_cuenta_movimiento,
                                                   l_reg_solicitud,
                                                   l_reg_detalle_pago,
                                                   l_valorUnidadInversion,
                                                   l_reg_operacion_concepto,
                                                   IN_USUARIO,
                                                   l_asiento_contable_id,
                                                   l_vCodigoError,
                                                   l_vMensajeError);
                         ELSIF(l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_CAJA)) THEN

                             GENERAR_MOVIMIENTO_CAJA(l_reg_cuenta_movimiento,
                                                   l_reg_solicitud,
                                                   l_reg_detalle_pago,
                                                   l_valorUnidadInversion,
                                                   l_reg_operacion_concepto,
                                                   IN_USUARIO,
                                                   l_asiento_contable_id,
                                                   l_vCodigoError,
                                                   l_vMensajeError);
                         ELSIF(l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_AFP)) THEN

                            GENERAR_MOVIMIENTO_AFP(l_reg_cuenta_movimiento,
                                                   l_reg_solicitud,
                                                   l_reg_detalle_pago,
                                                   l_valorUnidadInversion,
                                                   l_reg_operacion_concepto,
                                                   IN_USUARIO,
                                                   l_asiento_contable_id,
                                                   l_consecutivo_pago_id,
                                                   l_vCodigoError,
                                                   l_vMensajeError);
                         ELSE
                              l_vCodigoError  := 1;
                              l_vMensajeError := 'PAGO_ID['||IN_REG_PAGO.PAGO_ID||'] - OPERACION_CONCEPTO_ID NO VALIDO ['||l_reg_operacion_concepto.operacion_concepto_id||']';
                         END IF  ;
                     ELSE
                         l_error :=  'ITEM KEY [' ||l_reg_solicitud.CODIGO_RADICACION_REC||'], CODIGO_OPERACION_ID [' ||l_codOperacionId|| '] INVALIDO ';
                         RAISE_APPLICATION_ERROR (-20001, l_error);
                     END IF;

                  IF(l_vCodigoError = 0) THEN

                     IF(l_reg_operacion_concepto.operacion_concepto_id NOT IN(C_OPER_CONCEPTO_COMISION_ADM,
                                                                              C_OPER_CONCEPTO_COT_CAJA,
                                                                              C_OPER_CONCEPTO_RETEFUENTE,
                                                                              C_OPER_CONCEPTO_SALUD_UPC,
                                                                              C_OPER_CONCEPTO_CAJA_APORT_VOL))
                     THEN
                         l_estadoSaap := C_ESTADO_PEND_SAAP;
                     ELSE
                         l_estadoSaap := NULL;
                     END IF;

                    ACTUALIZAR_DETALLE_PAGO(l_asiento_contable_id,
                                           l_consecutivo_pago_id,
                                           IN_USUARIO,
                                           l_estadoSaap,
                                           NULL,
                                           l_reg_detalle_pago,
                                           NULL,
                                           C_PARAMETRO_FONDO);

                    ACTUALIZAR_NOMINA_PENSIONADO(IN_REG_PAGO,
                                                 IN_NOMINA_PENSIONADO_ID,
                                                 IN_USUARIO);
                  ELSE
                      RAISE_APPLICATION_ERROR (-20001, l_vMensajeError);
                  END IF;

            ELSIF CUR_DETALLE_PAGO.ESTADO NOT IN ('PAGADO')  THEN
               l_error :=  'ITEM KEY [' ||l_reg_solicitud.CODIGO_RADICACION_REC||'], ' || 'ESTADO DEL PAGO INVALIDO ['||l_reg_detalle_pago.ESTADO||']';
               RAISE_APPLICATION_ERROR (-20001, l_error);
            END IF;

        END LOOP;

   END IF;
  EXCEPTION
    WHEN OTHERS THEN
      IF(l_detenerProceso) THEN
        OUT_COD_RESPUESTA := 2;
      ELSE
        OUT_COD_RESPUESTA := 1;
      END IF;
    OUT_MENSAJE := 'ERROR EN EL METODO [APLICA_PAGO_FONDO] : '|| SQLERRM || '-' || l_error;
    RAISE_APPLICATION_ERROR (-20001, OUT_MENSAJE);
    -- ESCRIBIR EN LA TABLA DE ERRORES;
  END;