BEGIN
   Migration.StartScript (
      piosScriptName        => 'LOADING_TARIFF_DLL.sql'
      ,piosTrack            => 'ROM'
      ,piosScriptVersion    => '%I%'
      ,piosLevelName        => 'ROM_03'
      ,piosReleaseName      => '%R%'
      ,piosComponentName    => '%C%'
      ,piobReexecutableInd  => FALSE
      ,piosDescription       => 'MAPPING ROAM TARIFAS');
END;
/


GRANT INSERT ON RATE_PACK_ZONE to NKADM;
GRANT INSERT ON MPULKRIM to NKADM;
GRANT INSERT ON  RATE_PACK_PARAMETER_VALUE  to NKADM;
GRANT INSERT ON RATE_PACK_ELEMENT to NKADM;
GRANT SELECT ON MAX_RATE_PACK_ENTRY_SEQ TO NKADM;
GRANT SELECT ON MAX_RP_ELEMENT_ID_SEQ TO NKADM;
GRANT INSERT, UPDATE ON  RATE_PACK_PARAMETER_VALUE_WORK to NKADM;
GRANT SELECT ON MPUSPTAB to NKADM;
GRANT SELECT ON MPURITAB to NKADM;
GRANT SELECT ON RATEPLAN_VERSION to NKADM;
GRANT SELECT ON MPUGVVSD to NKADM;
GRANT SELECT ON mpurivsd to NKADM;
GRANT SELECT ON MPUTWVSD to NKADM;
GRANT SELECT ON rate_pack_parameter_value to NKADM;
GRANT SELECT ON MPUSNTAB to NKADM;
GRANT SELECT ON RATE_PACK_ELEMENT to NKADM;
GRANT SELECT ON udc_rate_type_table to NKADM;
GRANT SELECT ON udc_rate_type_table to NKADM;
GRANT SELECT ON mputttab to NKADM;
GRANT SELECT ON udc_usage_type_table to NKADM;
GRANT SELECT ON mpuzntab to NKADM;
GRANT SELECT,INSERT ON mpulkrim to NKADM;
GRANT SELECT ON mpulktmm to NKADM;
GRANT SELECT ON mpusptab to NKADM;
GRANT SELECT,UPDATE ON rate_pack_parameter_value_work to NKADM;
GRANT SELECT ON rate_pack_element_work to NKADM;
GRANT SELECT ON MPULKRI2 to NKADM;
GRANT SELECT ON rate_pack_zone_work to NKADM;
GRANT SELECT ON RATE_PACK_EXTERNAL_CHARGE_WORK to NKADM;
GRANT SELECT ON MAX_RP_ELEMENT_ID_SEQ to NKADM;
GRANT SELECT ON MAX_RATE_PACK_ENTRY_SEQ to NKADM;

CREATE OR REPLACE PACKAGE BODY NKADM.ROM_LOADING_TARIFFS IS
  function invalidEntriesInVersionWorks(v_file_id  IN NKADM.ROM_tarifas_rp_files.FILE_ID%TYPE) RETURN INTEGER
  IS
    invalidEntries integer := 0;

  BEGIN
    for en in (
     with map as(
      select distinct A.servicio, a.destino, spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
      from   NKADM.ROM_RP_MAPPING A
      )
      select map.servicio, map.destino,  map.spshdes, map.snshdes, map.zndes, map.ttdes, map.usage_type_shdes, map.rate_type_shdes, map.numrow,  nvl(tar.tarifa,parameter_value_float) tar_tariff, parameter_value_float worktariff
        from mpulktmm a, mpusptab sp, mpulkri2 rim, mpuzntab zn, udc_usage_type_table usg , mputttab tt, udc_rate_type_table rtt, rate_pack_element_work rpe,   map, mpusntab sn,
        rate_pack_parameter_value_work rppv, NKADM.ROM_RP_TARIFAS tar
        where a.sncode = sn.sncode
        and map.spshdes = sp.shdes and map.snshdes = sn.shdes and map.zndes = zn.des and map.ttdes = tt.des and usg.usage_type_shname = map.usage_type_shdes and 
        rtt.rate_type_SHNAME = map.rate_type_SHdes and  rppv.parameter_rownum = decode(map.numrow,0,rppv.parameter_rownum, null,rppv.parameter_rownum, map.numrow)
        and rim.rate_type_id = rtt.rate_type_id
        and a.spcode = sp.spcode 
        and zn.zncode = rim.zncode
        and tt.ttcode = rim.ttcode
        and usg.usage_type_id = a.usage_type_id
        and a.vscode = (select max(Vscode) from rateplan_version where tmcode = a.tmcode)
        and rim.ricode = a.ricode and rim.vscode = 0
        and rim.gvvscode = (select max(vscode) from MPUGVVSD where gvcode = rim.gvcode)
        and twvscode  = (select max(vscode) from MPUTWVSD where twcode = rim.twcode )
        and rpe.rate_pack_entry_id = rim.rate_pack_entry_id
        and rppv.rate_pack_element_id = rpe.rate_pack_element_id 
        and decode(rpe.conversion_module_id,3,1,4) = rppv.parameter_seqnum
        and map.servicio = tar.servicio(+) and  map.destino = decode(tar.destino, null,map.destino,tar.destino) 
        and nvl(tar.file_id,v_file_id) = v_file_id and nvl(tar.tarifa,parameter_value_float) != parameter_value_float
    )  
    loop


      update  NKADM.ROM_RP_TARIFAS set status = 30 where servicio = en.servicio and destino = en.destino and status is null;
       DBMS_OUTPUT.PUT('**** ERR: NOT VALID WORK VERSION *** '||en.servicio||'|'|| en.destino||'|'|| en.spshdes||'|'||
        en.snshdes||'|'|| en.zndes||'|'|| en.ttdes||'|'|| en.usage_type_shdes||'|'|| en.rate_type_shdes||
        '|'|| en.numrow||'|'||  en.tar_tariff||'|'|| en.worktariff||':: Updated ::' || sql%rowcount);

        invalidEntries := invalidEntries + sql%rowcount;
    end loop;   
    return invalidEntries; 
  end invalidEntriesInVersionWorks;


  function isUniqueTariffBySourceMapping(v_file_id  IN NKADM.ROM_tarifas_rp_files.FILE_ID%TYPE) RETURN INTEGER
  IS
    invalidEntries integer := 0;
  BEGIN
    for en in (with map as(
        select distinct A.servicio, A.DESTINO, spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
        from   NKADM.ROM_RP_MAPPING A),
      err as (
      select spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
          from NKADM.ROM_RP_TARIFAS tar, map
          where
          map.servicio = tar.servicio(+) and  map.destino = decode(tar.destino, null,map.destino,tar.destino) 
          and nvl(tar.file_id,v_file_id) = v_file_id
          group by spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
          having count(distinct tarifa) > 1
        )
          select distinct map.servicio, map.destino from map, err
          where map.spshdes = err.spshdes and  map.snshdes = err.snshdes and map.zndes = err.zndes and  map.ttdes = err.ttdes and map.usage_type_shdes = err.usage_type_shdes and 
          map.rate_type_shdes= err.rate_type_shdes and map.numrow = err.numrow
        )
    loop
      update  NKADM.ROM_RP_TARIFAS set status = 40 where servicio = en.servicio and destino = en.destino  and status is null;
      invalidEntries := 1;
    end loop;  
    return invalidEntries;
  end isUniqueTariffBySourceMapping;

  function isValidMapping(v_file_id  IN NKADM.ROM_tarifas_rp_files.FILE_ID%TYPE) RETURN INTEGER
  IS
  BEGIN

      update NKADM.ROM_RP_TARIFAS set status = 10 where (file_id, servicio, DESTINO) in (
        select tar.file_id,tar.servicio, tar.DESTINO from  NKADM.ROM_RP_MAPPING MAP, NKADM.ROM_RP_TARIFAS tar
          where map.servicio(+) = tar.servicio and  map.DESTINO(+) = tar.DESTINO 
          and map.servicio is null
          and tar.file_id = V_FILE_ID);
      if SQL%ROWCOUNT > 0 then 
          return 0;
      else
          return 1;
      end if;
  end isValidMapping;



  PROCEDURE applyNewTariffsByFileId(v_file_id  IN NKADM.ROM_tarifas_rp_files.FILE_ID%TYPE, v_mode IN CHAR)
  IS
    v_trace varchar2(256) := '';
    v_vscode MPURIVSD.VSCODE%TYPE;
    v_error Integer;
    v_fecha_efectiva NKADM.ROM_tarifas_rp_files.fecha_efectiva%type;

    cursor c_ricode_action is  with map as(
        select distinct A.servicio, a.destino, spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
        from   NKADM.ROM_RP_MAPPING A
        order by  A.servicio, a.destino,spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow)
            select distinct ri.ricode, ri.vscode, fl.fecha_efectiva, ri.vsdate, case when ri.vsdate < fecha_efectiva then 'ADD' when ri.vsdate > fecha_efectiva then 'ERR' else 'ADD' end ACTION
            from  map, NKADM.ROM_RP_TARIFAS tar, NKADM.ROM_TARIFAS_RP_FILES fl, mpulktmm tmm, mpurivsd ri
            where map.servicio = tar.servicio and  map.destino = tar.destino and  tar.file_id = v_file_id
            and tmm.spcode = (select spcode from mpusptab where shdes = map.spshdes)
            and tmm.sncode = (select sncode from mpusntab where shdes = map.snshdes)
            and tmm.usage_type_id = (select usage_type_id from udc_usage_type_table where  usage_type_shname = map.usage_type_shdes)
            and tmm.vscode = (select max(Vscode) from rateplan_version where tmcode = tmm.tmcode)
            and ri.ricode = tmm.ricode
            and ri.vscode = (select max(vscode) from mpurivsd where ricode = ri.ricode and vscode > 0  )
            and tar.file_id = fl.file_id
    ;

    cursor c_ree (v_ricode MPURiTAB.RICODE%TYPE) is 
    with map as(
          select distinct A.servicio, a.destino, spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
          from   NKADM.ROM_RP_MAPPING A
          --where a.servicio = '123'
            )
            select distinct a.ricode, zn.zncode,  tt.ttcode, rtt.rate_type_id,  rppv.parameter_rownum rownum1, nvl(tar.tarifa,parameter_value_float) rate
                  from mpulktmm a, mpusptab sp, mpulkrim rim, mpuzntab zn, udc_usage_type_table usg , mputttab tt, udc_rate_type_table rtt, rate_pack_element rpe,   map, mpusntab sn,
                  rate_pack_parameter_value rppv, NKADM.ROM_RP_TARIFAS tar
                  where a.sncode = sn.sncode
                  and map.spshdes = sp.shdes and map.snshdes = sn.shdes and map.zndes = zn.des and map.ttdes = tt.des and usg.usage_type_shname = map.usage_type_shdes and 
                  rtt.rate_type_SHNAME = map.rate_type_SHdes and  rppv.parameter_rownum = decode(map.numrow,0,rppv.parameter_rownum, null,rppv.parameter_rownum, map.numrow)
                  and rim.rate_type_id = rtt.rate_type_id
                  and a.spcode = sp.spcode 
                  and zn.zncode = rim.zncode
                  and tt.ttcode = rim.ttcode
                  and usg.usage_type_id = a.usage_type_id
                  and a.vscode = (select max(Vscode) from rateplan_version where tmcode = a.tmcode)
                  and rim.ricode = a.ricode and rim.vscode = (select max(vscode) from mpurivsd where ricode = rim.ricode --and --vsdate <= to_date('ddmmyyyy','01032023')
                  )
                  and rim.gvvscode = (select max(vscode) from MPUGVVSD where gvcode = rim.gvcode)
                  and twvscode  = (select max(vscode) from MPUTWVSD where twcode = rim.twcode )
                  and rpe.rate_pack_entry_id = rim.rate_pack_entry_id
                  and rppv.rate_pack_element_id = rpe.rate_pack_element_id 
                  and decode(rpe.conversion_module_id,3,1,4) = rppv.parameter_seqnum
                  and map.servicio = tar.servicio(+) and  map.destino = decode(tar.destino, null,map.destino,tar.destino) 
                  and a.ricode = v_ricode
                  and nvl(tar.file_id,v_file_id) = v_file_id;
  BEGIN
    select  fecha_efectiva into v_fecha_efectiva from NKADM.ROM_tarifas_rp_files where file_id = v_file_id and status = 2;

    for ri_ac in c_ricode_action
    loop

      if ri_ac.action = 'ERR' then 
        update NKADM.ROM_RP_TARIFAS set status = 20 where (file_id, servicio, destino) in (
         with map as(
                  select distinct A.servicio, a.destino, spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow
                  from   NKADM.ROM_RP_MAPPING A
                  order by  A.servicio, a.destino,spshdes, snshdes, zndes, ttdes, usage_type_shdes, rate_type_shdes, numrow)
                      select  tar.file_id, tar.servicio, tar.destino
                      from  map, NKADM.ROM_RP_TARIFAS tar, mpulktmm tmm
                      where map.servicio = tar.servicio and  map.destino = tar.destino 
                      and tar.file_id = v_file_id
                      and tmm.spcode = (select spcode from mpusptab where shdes = map.spshdes)
                      and tmm.sncode = (select sncode from mpusntab where shdes = map.snshdes)
                      and tmm.usage_type_id = (select usage_type_id from udc_usage_type_table where  usage_type_shname = map.usage_type_shdes)
                      and tmm.vscode = (select max(Vscode) from rateplan_version where tmcode = tmm.tmcode)
                      and tmm.ricode = ri_ac.ricode
                      and nvl(tar.status,2) = 2);

        if v_error is null then
          v_error:= 20;
          v_trace := 'There are latest verios of rates ::' || v_trace;
        end if;
      end if;
      if ri_ac.action = 'ADD' then 

      update mpurivsd set vsdate = v_fecha_efectiva where ricode = ri_ac.ricode and vscode = 0;


        for ree in c_ree(ri_ac.ricode)
        loop
            DBMS_OUTPUT.PUT('**** RATE ANALIZADO *** '||ree.rate_type_id||':: Zone ::'||ree.zncode||':: RICODE ::'||ri_ac.ricode||':: Updated ::' );
            FOR R_IM
                      IN (SELECT *
                          FROM (SELECT rate_pack_element_id,CONVERSION_MODULE_ID,
                                        DECODE (CONVERSION_MODULE_ID, 3, 1, 4) SEQ_PARAM
                                  FROM MPULKRI2 ri2,
                                        rate_pack_element_work rpe
                                WHERE   ree.zncode = ri2.zncode and ri2.ttcode = ree.ttcode
                                  AND ri_ac.ricode = ri2.ricode
                                  AND ree.rate_type_id = ri2.rate_type_id
                                  AND rpe.rate_pack_entry_id =ri2.rate_pack_entry_id))
            LOOP
              UPDATE RATE_PACK_PARAMETER_VALUE_WORK
                SET PARAMETER_VALUE_FLOAT =ree.RATE
              WHERE  RATE_PACK_ELEMENT_ID = R_IM.RATE_PACK_ELEMENT_ID
              AND PARAMETER_SEQNUM = R_IM.SEQ_PARAM
                AND NVL(ree.rownum1, parameter_rownum) = parameter_rownum; 
              DBMS_OUTPUT.PUT(sql%rowcount );
            END LOOP;
        end loop;
      end if;
      v_trace := v_trace || 'RI: '|| ri_ac.ricode||'|'||ri_ac.action||'|'||ri_ac.vsdate;

    end loop;

    if invalidEntriesInVersionWorks(v_file_id) > 0 then
        v_error := 30;
        v_trace := 'InvalidEntries in  Work versions ::' || v_trace;
    end if;
    if v_error is null then 
      for ri in c_ricode_action loop
        if ri.action != 'ERR' then
            select NVL( MAX(vscode), 0 )  + 1 into v_vscode from mpurivsd where ricode = ri.ricode
            and exists (select 1 from mpurivsd where vscode = 0 and vsdate = v_fecha_efectiva);
            if v_vscode = 1 then 
              v_error := 50;
              v_trace := 'No valid Version Code To Be copied ::' || v_trace;
            else
            dbms_output.put_line('+++++RICODE - '||ri.ricode);
              INSERT INTO MPURIVSD (RICODE, VSCODE, VSDATE, STATUS, USERLASTMOD, MODDATE, DROP_INPUT_THRESHOLD, REC_VERSION, --BACKDATE_IND,
                UPCODE, UPVSCODE) 
              select ricode, v_vscode,vsdate,'P','SYSADM',sysdate, DROP_INPUT_THRESHOLD, REC_VERSION, upcode, upvscode 
              from mpurivsd where ricode = ri.ricode and vscode = 0;
              
              INSERT INTO RATE_PACK_ZONE (RICODE, VSCODE, GVCODE, GVVSCODE, ZNCODE, TARIFF_TIME_SPLIT_IND, INTERCONNECT_RATING_MODE, QOS_SPLIT_IND) 
                SELECT RICODE, v_vscode, GVCODE, GVVSCODE, ZNCODE, TARIFF_TIME_SPLIT_IND, INTERCONNECT_RATING_MODE, QOS_SPLIT_IND FROM RATE_PACK_ZONE_WORK WHERE RICODE = ri.ricode;

               DBMS_OUTPUT.put_line('RATE_PACK_ZONE::'||SQL%ROWCOUNT);

           

              INSERT INTO MPULKRIM (RICODE, VSCODE, GVCODE, GVVSCODE, ZNCODE, TWCODE, TWVSCODE, TTCODE, RATE_PACK_ENTRY_ID, RATE_TYPE_ID, REC_VERSION, PERFORM_RATING_IND, EXTERNAL_CHARGE_SCALEFACTOR) 
                SELECT RICODE, v_vscode, GVCODE, GVVSCODE, ZNCODE, TWCODE, TWVSCODE, TTCODE, MAX_RATE_PACK_ENTRY_SEQ.NEXTVAL, RATE_TYPE_ID, REC_VERSION, PERFORM_RATING_IND, EXTERNAL_CHARGE_SCALEFACTOR
                FROM (SELECT DISTINCT RICODE, GVCODE, GVVSCODE, ZNCODE, TWCODE, TWVSCODE, TTCODE, RATE_TYPE_ID, REC_VERSION, PERFORM_RATING_IND, EXTERNAL_CHARGE_SCALEFACTOR 
                FROM MPULKRI2 RI2, RATE_PACK_ELEMENT_WORK RPEW WHERE RI2.RICODE = ri.ricode AND RI2.RATE_PACK_ENTRY_ID = RPEW.RATE_PACK_ENTRY_ID AND 
                ( (RPEW.CONVERSION_MODULE_ID IS NOT NULL) OR ( (RPEW.CONVERSION_MODULE_ID IS NULL) AND RI2.RATE_PACK_ENTRY_ID IN (SELECT RATE_PACK_ENTRY_ID FROM RATE_PACK_EXTERNAL_CHARGE_WORK))));


              DBMS_OUTPUT.put_line('MPULKRIM::'||SQL%ROWCOUNT);      
              INSERT INTO RATE_PACK_ELEMENT (RATE_PACK_ELEMENT_ID, RATE_PACK_ENTRY_ID, CHARGEABLE_QUANTITY_UDMCODE, CONVERSION_MODULE_ID, PRICE_LOGICAL_QUANTITY_CODE, RATE_PACK_IMC_SCALEFACTOR, PRICING_TYPE, PRICING_ALTERNATIVE_ID
              ) SELECT MAX_RP_ELEMENT_ID_SEQ.NEXTVAL,
              RIM.RATE_PACK_ENTRY_ID, CHARGEABLE_QUANTITY_UDMCODE, CONVERSION_MODULE_ID, PRICE_LOGICAL_QUANTITY_CODE, RATE_PACK_IMC_SCALEFACTOR, PRICING_TYPE, PRICING_ALTERNATIVE_ID 
              FROM MPULKRI2 RI2, MPULKRIM RIM, RATE_PACK_ELEMENT_WORK RPEW
              WHERE     RI2.RICODE =ri.ricode
              AND RI2.RATE_PACK_ENTRY_ID = RPEW.RATE_PACK_ENTRY_ID AND RIM.RICODE = RI2.RICODE AND RIM.VSCODE = v_vscode AND RIM.GVCODE = RI2.GVCODE AND RIM.GVVSCODE = RI2.GVVSCODE AND RIM.ZNCODE = RI2.ZNCODE AND RIM.TWCODE = RI2.TWCODE AND RIM.TWVSCODE = RI2.TWVSCODE AND RIM.TTCODE = RI2.TTCODE AND RIM.RATE_TYPE_ID = RI2.RATE_TYPE_ID AND ( (RPEW.CONVERSION_MODULE_ID IS NOT NULL) OR ( (RPEW.CONVERSION_MODULE_ID IS NULL) AND RI2.RATE_PACK_ENTRY_ID IN (SELECT RATE_PACK_ENTRY_ID FROM RATE_PACK_EXTERNAL_CHARGE_WORK)));

              INSERT INTO RATE_PACK_PARAMETER_VALUE (RATE_PACK_ELEMENT_ID, PARAMETER_SEQNUM, PARAMETER_ROWNUM, PARAMETER_VALUE_FLOAT) 
              SELECT RPE.RATE_PACK_ELEMENT_ID, RPPV.PARAMETER_SEQNUM, RPPV.PARAMETER_ROWNUM, RPPV.PARAMETER_VALUE_FLOAT
              FROM MPULKRI2 RI2, RATE_PACK_PARAMETER_VALUE_WORK RPPV, MPULKRIM RIM, RATE_PACK_ELEMENT_WORK RPEW, RATE_PACK_ELEMENT RPE 
              WHERE  RI2.RICODE = ri.ricode AND RPEW.CONVERSION_MODULE_ID IS NOT NULL AND RPEW.RATE_PACK_ELEMENT_ID = RPPV.RATE_PACK_ELEMENT_ID 
                AND RI2.RATE_PACK_ENTRY_ID = RPEW.RATE_PACK_ENTRY_ID AND RIM.RICODE = RI2.RICODE AND RIM.VSCODE = v_vscode AND RIM.GVCODE = RI2.GVCODE 
                AND RIM.GVVSCODE = RI2.GVVSCODE AND RIM.ZNCODE = RI2.ZNCODE AND RIM.TWCODE = RI2.TWCODE AND RIM.TWVSCODE = RI2.TWVSCODE AND RIM.TTCODE = RI2.TTCODE 
                AND RIM.RATE_TYPE_ID = RI2.RATE_TYPE_ID AND RIM.RATE_PACK_ENTRY_ID = RPE.RATE_PACK_ENTRY_ID AND RPEW.CHARGEABLE_QUANTITY_UDMCODE = RPE.CHARGEABLE_QUANTITY_UDMCODE 
                AND RPEW.PRICING_ALTERNATIVE_ID = RPE.PRICING_ALTERNATIVE_ID;
              v_error := 100;
            end if;
        end if;
      end loop;
      if v_error = 100 then
          dbms_output.put_line('+++++Exitoso - '||v_file_id);
        if(v_mode = 'C' or v_mode is null) then
          commit;
        end if;
      else
        dbms_output.put_line('+++++Error - '||v_file_id);
        if(v_mode = 'C' or v_mode is null) then
          rollback;
        end if;
      end if;
    end if;
    update NKADM.ROM_tarifas_rp_files set status =v_error , ERROR_DSC = v_trace
      where file_id = v_file_id;
     dbms_output.put_line('+++++Actualizando Status- '||v_file_id);
    if(v_mode = 'C' or v_mode is null) then
      commit;
    end if;


  EXCEPTION
    WHEN NO_DATA_FOUND THEN 
      ROLLBACK;
      RAISE_APPLICATION_ERROR(-20000,'The file id: '|| v_file_id|| ' does not exist or not have the status 2 (Validated), it is not possible update tariffs.');
    WHEN OTHERS THEN 
      ROLLBACK;
      raise;
  END;



  procedure loadPendingFiles (v_mode IN CHAR)
  IS

    CURSOR cFiles IS select * from (SELECT  file_id, file_name, fecha_efectiva from NKADM.ROM_tarifas_rp_files where status = 0 order by fecha_efectiva) ;
  BEGIN
    for fl in cFiles
    loop
      dbms_output.put_line('Cambiando status filename - '||fl.file_name||' - Fecha Efectiva - '||fl.fecha_efectiva);
      if isValidMapping(fl.file_id) = 0 then
        update NKADM.ROM_tarifas_rp_files set status = 10, error_dsc = 'Hay una o más entradas del archivo sin mapeo' where file_id = fl.file_id;
        dbms_output.put_line('****Error Entradas sin Mapeo - '||fl.file_name||' - Fecha Efectiva - '||fl.fecha_efectiva);
        if(v_mode = 'C' or v_mode is null) then
           commit;
        end if;
      else
        if isUniqueTariffBySourceMapping(fl.file_id) > 0 then 
          update NKADM.ROM_tarifas_rp_files set status = 40, error_dsc = 'Hay un destino con mas de una tarifa.' where file_id = fl.file_id;
          dbms_output.put_line('****Destino Multiple Tarifa - '||fl.file_name||' - Fecha Efectiva - '||fl.fecha_efectiva);
          if(v_mode = 'C' or v_mode is null) then
           commit;
          end if;
        else
          dbms_output.put_line('Actualizando - '||fl.file_name||' - Fecha Efectiva - '||fl.fecha_efectiva);
          update NKADM.ROM_tarifas_rp_files set status = 2 where file_id = fl.file_id;
          if(v_mode = 'C' or v_mode is null) then
           commit;
          end if;
          applyNewTariffsByFileId(fl.file_id,v_mode);
        end if;
      end if;
    end loop;
  END;
END;
/




BEGIN
   Migration.FinishScript (piosScriptName => 'LOADING_TARIFF_DLL.sql');
END;
/
