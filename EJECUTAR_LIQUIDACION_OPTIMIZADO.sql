PROCEDURE EJECUTAR_LIQUIDACION(IN_PAGINA_ID IN NUMBER,
                                 IN_USUARIO IN VARCHAR2,
                                 OUT_COD_RESPUESTA OUT NUMBER,
                                 OUT_MENSAJE OUT VARCHAR2)    
  IS 
    l_pagoIdInicio     NUMBER;
    l_pagoIdFinal      NUMBER;
    l_regProcesados    NUMBER :=0;
    l_regConError      NUMBER :=0;
    l_codRespuesta     NUMBER;
    l_mensajeRespuesta VARCHAR2(2000);
    l_regPaginacion    MNOMPEN.NOMP_BAT_PAGINACION%ROWTYPE;
    l_reg_pago         MPENGES.SPG_PAGO%ROWTYPE;
    l_observacion      VARCHAR2(2000);
    l_pagoLoteId       NUMBER;
    l_estadoPagina     MNOMPEN.NOMP_BAT_PAGINACION.ESTADO_PAGINACION_ID%TYPE;
    
    CURSOR CUR_PAGINACION_PAGOS(p_pagoIdInicio IN NUMBER,p_pagoIdFinal IN NUMBER)
    IS
        SELECT P.PAGO_ID,
               P.CUENTA_POR_PAGAR_ID,
               P.TIPO_PAGO_ID,
               P.VALOR_PESOS_TOTAL,
               P.FECHA_PAGO,
               P.EPS_ID,
               P.PORCENTAJE_EPS,
               P.CAJA_COMPENSACION_ID,
               P.ESTADO_OFICINA,
               P.PAGO_LOTE_ID,
               P.SOLICITUD_ID_BENEFICIO,
               P.PERIODO,
               P.PROCESO
        FROM MPENGES.SPG_PAGO P
        WHERE P.PAGO_ID BETWEEN p_pagoIdInicio AND p_pagoIdFinal
        AND P.TIPO_PAGO_ID = C_TIPO_PAGO_RET_PROGRAMADO;
  BEGIN
        DBMS_OUTPUT.PUT_LINE('-------ENTRO EJECUTAR LIQUIDACION PAQUETE ='||to_char(SYSDATE, 'YYYY-MM-DD HH24_MI_SS'));
    OUT_COD_RESPUESTA:=0;
    l_observacion := NULL;
    l_regPaginacion.Paginacion_Id := IN_PAGINA_ID;
    l_regPaginacion.Fecha_Inicio_Exe := SYSDATE;
    l_regPaginacion.Estado_Paginacion_Id := C_ESTADO_PAG_EN_PROCESO;
    
       BEGIN
         SELECT NP.ID_INICIO,NP.ID_FIN,NP.ESTADO_PAGINACION_ID 
         INTO l_pagoIdInicio,l_pagoIdFinal,l_estadoPagina
         FROM MNOMPEN.NOMP_BAT_PAGINACION NP
         WHERE NP.PAGINACION_ID = IN_PAGINA_ID FOR UPDATE;
       EXCEPTION WHEN NO_DATA_FOUND THEN
           RAISE_APPLICATION_ERROR (-20001, 'PAGINACION CON EL ID ['||IN_PAGINA_ID||'] NO EXISTE  : '|| SQLERRM);
       END; 
       
       DBMS_OUTPUT.PUT_LINE(' Estado de la Pagina = '|| l_estadoPagina || ' - Estado correcto a procesar 1 - ' || 'Numero de Paguina' ||IN_PAGINA_ID);
       
       IF(l_estadoPagina = C_ESTADO_PAG_NUEVO) THEN
           --ACTUALIZAR ESTADO DE PAGINA A EN_PROCESO
           ACTUALIZAR_PAGINACION(l_regPaginacion, l_observacion, IN_USUARIO);
          
       -- DBMS_OUTPUT.PUT_LINE('------- ENTRO EJECUTAR LIQUIDACION PAQUETE actualizar estado pagina -------');
           
           --LIQUIDACION DE CADA UNO DE LOS PAGOS DE LA PAGINA SUMINISTRADA
        ---    DBMS_OUTPUT.PUT_LINE(l_pagoIdInicio || '  -  ' || l_pagoIdFinal );
            
           -- OPTIMIZADO CON BULK COLLECT
           TYPE t_pago_tab IS TABLE OF CUR_PAGINACION_PAGOS%ROWTYPE;
           l_pago_tab t_pago_tab;
           BEGIN
               OPEN CUR_PAGINACION_PAGOS(l_pagoIdInicio,l_pagoIdFinal);
               FETCH CUR_PAGINACION_PAGOS BULK COLLECT INTO l_pago_tab;
               CLOSE CUR_PAGINACION_PAGOS;
               FOR i IN 1 .. l_pago_tab.COUNT LOOP
                   REG_PAGO := l_pago_tab(i);
               END LOOP;
           END;
           ACTUALIZAR_ESTADO_LOTE(IN_PAGINA_ID,IN_USUARIO);       
           -- ACTUALIZAR EL ESTADO Y LA FECHA DE FINALIZACION DE LIQUIDACION DE LA PAGINA
           l_regPaginacion.Fecha_Fin_Exe := SYSDATE;
           l_regPaginacion.Estado_Paginacion_Id := C_ESTADO_PAG_TERMINADO;
           ACTUALIZAR_PAGINACION(l_regPaginacion,l_observacion,IN_USUARIO);
       ELSE
         COMMIT; -- FIN DE LA TRANSACCION DEL SELECT FOR UPDATE   
          --  DBMS_OUTPUT.PUT_LINE('------- COMMIT FIN -------');
       END IF;    
       DBMS_OUTPUT.PUT_LINE('-------FIN EJECUTAR LIQUIDACION='||to_char(SYSDATE, 'YYYY-MM-DD HH24_MI_SS'));
  EXCEPTION WHEN OTHERS THEN
    OUT_COD_RESPUESTA := 1;
    OUT_MENSAJE := 'ERROR EN EL METODO [EJECUTAR_LIQUIDACION] : '|| SQLERRM;
    -- ACTUALIZAR EL ESTADO CON ERROR Y LA FECHA DE FINALIZACION DE LIQUIDACION DE LA PAGINA
     l_regPaginacion.Fecha_Fin_Exe := SYSDATE;
     l_regPaginacion.Estado_Paginacion_Id := C_ESTADO_PAG_ERROR_EJECUCION;
     l_observacion := OUT_MENSAJE;
     ACTUALIZAR_PAGINACION(l_regPaginacion,l_observacion, IN_USUARIO);
  END EJECUTAR_LIQUIDACION;