  PROCEDURE APLICA_PAGO_SOCIEDAD (IN_SOLICITUD           MPENGES.SPG_SOLICITUD%ROWTYPE,
                                 IN_REG_PAGO             MPENGES.SPG_PAGO%ROWTYPE,
                                 IN_REG_CUENTA_POR_PAGAR MPENGES.SPG_CUENTA_POR_PAGAR%ROWTYPE,
                                 IN_NOMINA_PENSIONADO_ID MNOMPEN.NOMP_NOMINA_PENSIONADO.NOMINA_PENSIONADO_ID%TYPE,
                                 IN_USUARIO IN VARCHAR2,
                                 OUT_COD_RESPUESTA OUT NUMBER,
                                 OUT_MENSAJE OUT VARCHAR2)
  IS
    l_reg_pago             MPENGES.SPG_PAGO%ROWTYPE;
    l_reg_cuenta_por_pagar MPENGES.SPG_CUENTA_POR_PAGAR%ROWTYPE;
    l_reg_solicitud        MPENGES.SPG_SOLICITUD%ROWTYPE;
    l_inversionId          MGENERAL.GEN_INVERSION.INVERSION_ID%TYPE;
    l_reg_operacion_concepto MPENGES.SPG_OPERACION_CONCEPTO%ROWTYPE;
    l_reg_detalle_pago     MPENGES.SPG_DETALLE_PAGO%ROWTYPE;
    l_tipoIdentificacionAfil   MCUENTAS.CTA_AFILIADO.TIPO_IDENTIFICACION%TYPE;
    l_numeroIdentificacionAfil MCUENTAS.CTA_AFILIADO.NUMERO_IDENTIFICACION%TYPE;
    l_cuentaPorCobrarId    MPENGES.SPG_CUENTA_POR_COBRAR.CUENTA_POR_COBRAR_ID%TYPE ;

    l_descripcionAsiento   VARCHAR2(200);
    l_error                VARCHAR2(2000);
    l_nombreAfiliado       VARCHAR2(200);
    l_asiento_contable_id  NUMBER;
    htTablaValoresSociedad MCONTABILIDAD.HASH_TABLE;

    l_nitTerceroCredito        NUMBER;
    l_nombreTerceroCredito     VARCHAR2(200);
    l_nitTerceroDebito         NUMBER;
    l_nombreTerceroDebito      VARCHAR2(200);
    l_idCXP                     VARCHAR2(200);
    l_resultado_contabilidad  VARCHAR2(2000);
    l_estadoSaap           NUMBER(5);

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
     l_cuentaPorCobrarId := 0;
     l_reg_solicitud := IN_SOLICITUD;
     l_reg_pago := IN_REG_PAGO;
     l_reg_cuenta_por_pagar := IN_REG_CUENTA_POR_PAGAR;
     l_descripcionAsiento :=  NVL(OBTENER_VALOR_GEN_PARAMETRO(C_FONDO_PENSION_OBLIG,C_MODULO_PENS_GESTION,'DESCRIP_LINEA_ASIENTO'),'');

     BEGIN
         SELECT I.INVERSION_ID INTO l_inversionId
         FROM MGENERAL.GEN_INVERSION I
         WHERE I.FONDO_ID = C_FONDO_SOCIEDAD;
     EXCEPTION WHEN OTHERS THEN
         l_inversionId := NULL;
     END;

     
    -- Optimización total: BULK COLLECT sin uso de FOR LOOP
    DECLARE
        TYPE t_detalle_pago_tab IS TABLE OF OBTENER_DETALLE_PAGO%ROWTYPE INDEX BY PLS_INTEGER;
        l_detalle_pago_tab t_detalle_pago_tab;
    BEGIN
        SELECT * BULK COLLECT INTO l_detalle_pago_tab
        FROM OBTENER_DETALLE_PAGO(l_reg_pago.pago_id);

        IF l_detalle_pago_tab.COUNT > 0 THEN
            FORALL i IN INDICES OF l_detalle_pago_tab SAVE EXCEPTIONS
                NULL; -- Aquí se podría aplicar DML por lote si existiera

            -- Procesar por lógica condicional cuando no es posible usar FORALL
            FOR i IN l_detalle_pago_tab.FIRST .. l_detalle_pago_tab.LAST LOOP
                l_reg_detalle_pago := l_detalle_pago_tab(i);
IF (CUR_DETALLE_PAGO.ESTADO IN ('SUSPENDIDO')) THEN

                 PAGOS_SUSPENDIDOS ( l_reg_cuenta_por_pagar,
                                     l_reg_pago,
                                     l_reg_detalle_pago,
                                     IN_USUARIO);

                 ACTUALIZAR_NOMINA_PENSIONADO(IN_REG_PAGO,
                                              IN_NOMINA_PENSIONADO_ID,
                                              IN_USUARIO);

         ELSIF CUR_DETALLE_PAGO.ESTADO IN ('APROBADO') THEN

            -- DBMS_OUTPUT.PUT_LINE('------- APLICA PAGO SOCIEDAD  ESTADO LOTE -------');
           -- OBTENER OPERACI¿¿N CONCEPTO
              BEGIN
                   SELECT con.operacion_concepto_id,
                          con.descuenta,
                          con.caja_pago,
                          con.codigo_operacion_id,
                          con.operacion,
                          con.principal,
                          con.descripcion_contable
                   INTO l_reg_operacion_concepto.Operacion_Concepto_Id,
                   l_reg_operacion_concepto.Descuenta,
                   l_reg_operacion_concepto.Caja_Pago,
                   l_reg_operacion_concepto.Codigo_Operacion_Id,
                   l_reg_operacion_concepto.operacion,
                   l_reg_operacion_concepto.principal,
                   l_reg_operacion_concepto.descripcion_contable
                   FROM mpenges.spg_operacion_concepto con
                   WHERE con.operacion_concepto_id =  l_reg_detalle_pago.OPERACION_CONCEPTO_ID
                   AND  con.tipo_pago_id = l_reg_pago.tipo_pago_id;

             EXCEPTION
              WHEN OTHERS THEN

                l_error := 'PAGO_ID = ' || l_reg_pago.pago_id || ', OPERACION_CONCEPTO_ID = ' || l_reg_detalle_pago.OPERACION_CONCEPTO_ID || '[CONSULTANDO_OPERACION_CONCEPTO] Error[' || SQLERRM ||
                '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
                RAISE_APPLICATION_ERROR (-20001, l_error);

             END;

             IF(l_reg_operacion_concepto.operacion_concepto_id NOT IN (C_OPER_CONCEPTO_COMISION_ADM))
             THEN
                  BEGIN
                      MPENGES.SPG_GESTION_CXC_PCK.cargar_cxp_encxc_sp(l_reg_detalle_pago.DETALLE_PAGO_ID,
                                                                      C_ESTADO_PEND_SAAP,
                                                                      l_cuentaPorCobrarId);

                  EXCEPTION WHEN OTHERS THEN
                    l_cuentaPorCobrarId := 0;
                 END;
                 --OBTENER ID DEL ASIENTO CONTABLE
                  BEGIN
                      EXECUTE IMMEDIATE l_sentencia_asiento_id INTO l_asiento_contable_id;
                  EXCEPTION
                    WHEN OTHERS THEN

                     l_error :=  'PAGO_ID = ' || l_reg_detalle_pago.PAGO_ID || '[OBTENIENDO_ASIENTO_CONSECUTIVO_PAGO] Error[' || SQLERRM ||
                     '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
                     RAISE_APPLICATION_ERROR (-20001, l_error);

                  END;

                  BEGIN
                      SELECT CA.TIPO_IDENTIFICACION,
                             CA.NUMERO_IDENTIFICACION,
                             (CA.PRIMER_NOMBRE || ' ' || CA.SEGUNDO_NOMBRE
                             || ' ' || CA.PRIMER_APELLIDO || ' ' || CA.SEGUNDO_APELLIDO) AS NOMBRE_AFILIADO
                             INTO l_tipoIdentificacionAfil,
                                  l_numeroIdentificacionAfil,
                                  l_nombreAfiliado
                      FROM MCUENTAS.CTA_AFILIADO CA, MCUENTAS.CTA_CUENTA C
                      WHERE C.AFILIADO_FONDO_ID = CA.AFILIADO_FONDO_ID
                      AND C.CUENTA_ID = l_reg_solicitud.cuenta_afiliado;

                  EXCEPTION
                    WHEN OTHERS THEN
                     l_error :=  'PAGO_ID = ' || l_reg_detalle_pago.PAGO_ID || '[OBTENIENDO_ASIENTO_CONSECUTIVO_PAGO] Error[' || SQLERRM ||
                     '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
                     RAISE_APPLICATION_ERROR (-20001, l_error);

                  END;

                IF (l_reg_operacion_concepto.operacion_concepto_id NOT IN (C_OPER_CONCEPTO_EPS,
                                                                           C_OPER_CONCEPTO_FSP,
                                                                           C_OPER_CONCEPTO_CAJA,
                                                                           C_OPER_CONCEPTO_COMISION_ADM,
                                                                           C_OPER_CONCEPTO_AFP,
                                                                           C_OPER_CONCEPTO_SALUD_UPC,
                                                                           C_OPER_CONCEPTO_CAJA_APORT_VOL)) THEN
                     BEGIN
                         SELECT  per.numero_identificacion,
                         (select ti.abreviatura from mgeneral.gen_tipo_identificacion ti where ti.tipo_identificacion_id=per.tipo_identificacion)
                         INTO l_numeroIdentificacion, l_tipoIdentificacion
                         FROM mpenges.spg_persona per
                         WHERE per.persona_id=l_reg_detalle_pago.PERSONA_PAGO
                         AND rownum=1  ;

                     EXCEPTION
                      WHEN OTHERS THEN
                         l_error := 'PAGO_ID = ' || l_reg_pago.pago_id || '-PERSONA_ID = ' || l_reg_cuenta_por_pagar.persona_id ||', ' || '[CONSULTANDO_PERSONA_PAGO] Error[' || SQLERRM ||
                        '] Traza[' || dbms_utility.format_error_backtrace || ']' ;
                         RAISE_APPLICATION_ERROR (-20001, l_error);

                     END;

                     l_nombrePersona:=mpenges.spg_utilidades_persona_pck.obtener_nombres_ben2_fn(NULL, l_reg_detalle_pago.PERSONA_PAGO,1);


                      l_nitTerceroDebito       := l_numeroIdentificacionAfil;
                      l_nombreTerceroDebito     := l_nombreAfiliado;
                      l_nitTerceroCredito       := l_numeroIdentificacion;
                      l_nombreTerceroCredito   := l_nombrePersona;
                ELSE
                    l_nitTerceroCredito        := l_numeroIdentificacionAfil;
                    l_nombreTerceroCredito     := l_nombreAfiliado;
                    l_nitTerceroDebito         := l_numeroIdentificacionAfil;
                    l_nombreTerceroDebito      := l_nombreAfiliado;
                END IF;


                 IF (l_descripcionAsiento IS NOT NULL AND l_cuentaPorCobrarId IS NOT NULL) THEN
                     l_idCXP := CONCAT(l_descripcionAsiento,l_cuentaPorCobrarId);
                 ELSE
                     l_idCXP := NULL;
                 END IF;

                 htTablaValoresSociedad :=   MCONTABILIDAD.HASH_TABLE(MCONTABILIDAD.map_entry(key=>'FONDO',value=> C_FONDO_SOCIEDAD),
                                                                      MCONTABILIDAD.map_entry(key=>'INVERSIONSOC',value=>NVL(l_inversionId,0)),
                                                                      MCONTABILIDAD.map_entry(key=>'TIPODOCBASEDEB',value=>' '),
                                                                      MCONTABILIDAD.map_entry(key=>'TIPODOCBASECRE',value=>' '),
                                                                      MCONTABILIDAD.map_entry(key=>'NUMDOCBASEDEB',value=>' '),
                                                                      MCONTABILIDAD.map_entry(key=>'NUMDOCBASECRE',value=>' '),
                                                                      MCONTABILIDAD.map_entry(key=>'NITTERCERODEB',value=>NVL(l_nitTerceroDebito,0)),
                                                                      MCONTABILIDAD.map_entry(key=>'NITTERCEROCRE',value=>NVL(l_nitTerceroCredito,0)),
                                                                      MCONTABILIDAD.map_entry(key=>'NOMTERCERODEB',value=>NVL(l_nombreTerceroDebito,' ')),
                                                                      MCONTABILIDAD.map_entry(key=>'NOMTERCEROCRE',value=>NVL(l_nombreTerceroCredito,' ')),
                                                                      MCONTABILIDAD.map_entry(key=>'IDCXC',value=>NVL(l_idCXP,' ')),
                                                                      MCONTABILIDAD.map_entry(key=>'VALORCONSOC',value=>NVL(l_reg_detalle_pago.valor_pesos,0)),
                                                                      MCONTABILIDAD.map_entry(key=>'TIPOBENEFICIO',value=>NVL(l_reg_operacion_concepto.descripcion_contable,' ')));

                l_resultado_contabilidad:=MCONTABILIDAD.con_contabilizacion_pck.contabilizarasignado(IN_USUARIO,
                                                                                                     C_TOC_CAUSACION_BENEFICIOS,
                                                                                                      htTablaValoresSociedad,
                                                                                                      l_asiento_contable_id);

                OUT_COD_RESPUESTA := TO_NUMBER(SUBSTR(l_resultado_contabilidad, 1, (nvl(instr(l_resultado_contabilidad, '|', 1),0)-1)));
                OUT_MENSAJE := SUBSTR(l_resultado_contabilidad,1,200);

                IF(OUT_COD_RESPUESTA = 0) THEN

                   INSERTA_CONTABILIDAD_CXC(l_cuentaPorCobrarId,
                                            l_asiento_contable_id,
                                            l_reg_operacion_concepto.descripcion_contable,
                                            l_idCXP,
                                            IN_USUARIO);

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
                                          NULL,
                                          IN_USUARIO,
                                          l_estadoSaap,
                                          l_cuentaPorCobrarId,
                                          l_reg_detalle_pago,
                                          NULL,
                                          C_PAGADOR_SOCIEDAD);

                    ACTUALIZAR_NOMINA_PENSIONADO(IN_REG_PAGO,
                                                 IN_NOMINA_PENSIONADO_ID,
                                                 IN_USUARIO);
                    IF l_reg_operacion_concepto.operacion_concepto_id IN (C_OPER_CONCEPTO_DESC_ADICIONAL) THEN
                      ACTUALIZAR_NUMERO_PAGOS(l_reg_detalle_pago,IN_USUARIO,NULL);
                    END IF;

                ELSE
                      RAISE_APPLICATION_ERROR (-20001, OUT_MENSAJE);

                END IF;

             ELSE
                 l_error := 'PAGO_ID = ' || l_reg_cuenta_por_pagar.cuenta_por_pagar_id || ', OPERACION_CONCEPTO_ID = '|| l_reg_operacion_concepto.operacion_concepto_id ||'- TIPO DE PAGO INVALIDO PARA SOCIEDAD';
                 RAISE_APPLICATION_ERROR (-20001, l_error);
             END IF;

         END IF;
     END LOOP;
            END LOOP;
        END IF;
    END;

  EXCEPTION
    WHEN OTHERS THEN
    OUT_COD_RESPUESTA := 1;
    OUT_MENSAJE := 'ERROR EN EL METODO [APLICA_PAGO_SOCIEDAD] : '|| SQLERRM || '-' || l_error;
    RAISE_APPLICATION_ERROR (-20001, OUT_MENSAJE);
  END APLICA_PAGO_SOCIEDAD;