**
FUNCTION h2str
 PARAMETER zhsz
 PRIVATE fhz
 fhz = CHR(MOD(zhsz, 256))+CHR(zhsz/256)
 RETURN fhz
ENDFUNC
**
FUNCTION openfile
 PARAMETER ndbf, nmode, nalias
 SELECT 0
 IF ISNULL(nalias)
    nalias = ndbf
 ENDIF
 IF nmode
    USE (ndbf) EXCLUSIVE ALIAS &nalias
 ELSE
    IF  .NOT. USED(ndbf)
       USE (ndbf) SHARED ALIAS &nalias
    ELSE
       SELECT &nalias   
    ENDIF
 ENDIF
 RETURN .T.
ENDFUNC
**
FUNCTION closefile
 PARAMETER nalias
 dotnumber = OCCURS(",", nalias)
 dotnumber = dotnumber+1
 FOR closefilenumber = 1 TO dotnumber
    dotposition = AT(",", nalias, 1)
    IF dotposition>0
       closealias = SUBSTR(nalias, 1, (dotposition-1))
       nalias = SUBSTR(nalias, (dotposition+1), (LEN(nalias)-dotposition))
    ELSE
       closealias = nalias
    ENDIF
    SELECT &closealias
    USE
 ENDFOR
 RETURN .T.
ENDFUNC
**
PROCEDURE CloseAllTable
 LOCAL ttables, ncat
 ttables = AUSED(alltables)
 IF ttables>0
    FOR ncat = 1 TO ttables
       closefile(alltables(ncat, 1))
    ENDFOR
 ENDIF
ENDPROC
**
FUNCTION GetErrorMessage
 LOCAL cerrormessage, cerr12, cerr13
 = AERROR(aerrors)
 IF RAT("]", aerrors(1, 2))=LEN(aerrors(1, 2))
    m.cerr12 = aerrors(1, 2)
    m.cerr13 = aerrors(1, 3)
 ELSE
    m.cerr12 = SUBSTR(aerrors(1, 2), RAT("]", aerrors(1, 2))+1)
    m.cerr13 = IIF(ISNULL(aerrors(1, 3)), "", SUBSTR(aerrors(1, 3), RAT("]", aerrors(1, 3))+1))
 ENDIF
 DO CASE
    CASE ALLTRIM(STR(aerrors(1, 1)))="1526"
       chimes = "错误--->连接错误"
    CASE ALLTRIM(STR(aerrors(1, 1)))="1884"
       chimes = "错误--->索引不唯一"
    CASE ALLTRIM(STR(aerrors(1, 1)))="1427" .OR. ALLTRIM(STR(aerrors(1, 1)))="1429"
       chimes = "错误--->OLE ERROR"
    OTHERWISE
       chimes = "错误-->运行时刻错误"
 ENDCASE
 IF m.cerr12=m.cerr13 .OR. EMPTY(m.cerr13)
    m.cerrormessage = "错误代码: "+ALLTRIM(STR(aerrors(1, 1)))+CHR(13)+"错误讯息:"+CHR(13)+m.cerr12+CHR(13)+CHR(13)+"如果你看到此讯息,请抄下并告知软件开发人员.. "+CHR(13)+CHR(13)+chimes
 ELSE
    m.cerrormessage = "错误代码: "+ALLTRIM(STR(aerrors(1, 1)))+CHR(13)+"错误讯息:"+CHR(13)+m.cerr12+CHR(13)+m.cerr13+CHR(13)+CHR(13)+"如果你看到此讯息,请抄下并告知软件开发人员.. "+CHR(13)+CHR(13)+chimes
 ENDIF
 RETURN m.cerrormessage
ENDFUNC
**
PROCEDURE Resulterror
 PARAMETER prg_message
 = MESSAGEBOX(prg_message+CHR(13)+CHR(13)+geterrormessage(), 016, '错误信息视窗')
 = SQLROLLBACK(oenv.mysqldata)
 = SQLEXEC(oenv.mysqldata, 'dump transaction LONGCHENG with truncate_only')
 RETURN
ENDPROC
**
FUNCTION createfile
 PARAMETER fromfile, filemode, crefilename, cre_path
 fromfile = UPPER(fromfile)
 IF LOWER(filemode)<>'cursor'
    IF PCOUNT()>=4
       SET DEFAULT TO &cre_path
    ELSE
       SET DEFAULT TO &temp_path
    ENDIF
 ENDIF
 nalias = ALIAS()
 chkfile = 'allfield'
 IF  .NOT. USED(chkfile)
    = openfile(chkfile, .F., chkfile)
    SET ORDER TO 2
 ENDIF
 tf = .F.
 usefield = ''
 SET EXACT ON
 IF SEEK(PADR(fromfile, 15), chkfile, 2)
    SCAN FOR ALLTRIM(allfield.dbfname)==fromfile
       DO CASE
          CASE UPPER(ALLTRIM(&chkfile..fld_type))='D' OR UPPER(ALLTRIM(&chkfile..fld_type))='T'
             IF !EMPTY(&chkfile..fld_null)
                usefield=usefield+ALLTRIM(&chkfile..fieldname)+' '+ALLTRIM(&chkfile..fld_type)+' NOT NULL,'
             ELSE
                usefield=usefield+ALLTRIM(&chkfile..fieldname)+' '+ALLTRIM(&chkfile..fld_type)+' NULL,'
             ENDIF
          CASE UPPER(ALLTRIM(&chkfile..fld_type))='C'
             IF !EMPTY(&chkfile..fld_null)
                usefield=usefield+ALLTRIM(&chkfile..fieldname)+' '+ALLTRIM(&chkfile..fld_type)+'('+ ALLTRIM(STR(&chkfile..fld_wid))+') NOT NULL,'
             ELSE
                usefield=usefield+ALLTRIM(&chkfile..fieldname)+' '+ALLTRIM(&chkfile..fld_type)+'('+ ALLTRIM(STR(&chkfile..fld_wid))+') NULL,'
             ENDIF
          OTHERWISE
             IF !EMPTY(&chkfile..fld_null)
                usefield=usefield+ALLTRIM(&chkfile..fieldname)+' '+ALLTRIM(&chkfile..fld_type)+'('+ ALLTRIM(STR(&chkfile..fld_wid))+','+ALLTRIM(STR(&chkfile..fld_dec))+') NOT NULL,'
             ELSE
                usefield=usefield+ALLTRIM(&chkfile..fieldname)+' '+ALLTRIM(&chkfile..fld_type)+'('+ ALLTRIM(STR(&chkfile..fld_wid))+','+ALLTRIM(STR(&chkfile..fld_dec))+') NULL,'
             ENDIF
       ENDCASE
    ENDSCAN
    SET EXACT OFF
    usefield = LEFT(usefield, LEN(usefield)-1)
 ELSE
    = MESSAGEBOX('tempfile 建档失败，请确定来源档案名称('+fromfile+')是否正确？', 064, 'woring')
 ENDIF
 IF  .NOT. EMPTY(usefield)
    IF LOWER(filemode)=LOWER('cursor')
       CREATE &filemode. &crefilename. (&usefield.)
    ELSE
       CREATE &filemode. &crefilename. FREE (&usefield.)
    ENDIF
    tf = .T.
 ENDIF
 = closefile(chkfile)
 SET DEFAULT TO &default_path
 IF  .NOT. EMPTY(nalias)
    SELECT &nalias
 ENDIF
 RETURN tf
 RELEASE filemode, usealias, usefield, usefieldname, tf
ENDFUNC
**
PROCEDURE CreateIndex
 PARAMETER ci_table, index_exp, ci_cnt
 LOCAL ci_pos, ci_index, ci_index_tag
 SELECT &ci_table
 FOR ci_i = 1 TO ci_cnt
    ci_pos = AT(",", index_exp)
    IF (ci_pos=0)
       ci_pos = LEN(index_exp)+1
    ENDIF
    ci_index = SUBSTR(index_exp, 1, ci_pos-1)
    ci_index_tag = ci_table+ALLTRIM(STR(ci_i))
    index_exp = SUBSTR(index_exp, ci_pos+1, LEN(index_exp))
    INDEX ON &ci_index TAG &ci_index_tag
 ENDFOR
 RETURN
ENDPROC
**
FUNCTION CheckFieldEmpty
 PARAMETER cfe_file, cfe_fieldstring, cfe_cnt
 cfe_file = UPPER(cfe_file)
 cfe_fieldstring = UPPER(cfe_fieldstring)
 nalias = ALIAS()
 IF  .NOT. USED('allfield')
    = openfile('allfield', .F., 'allfield')
 ENDIF
 LOCAL cfe_msg, cfe_ok, cfe_fpos, cfe_mpos, cfe_field, cfe_message
 cfe_msg = ''
 cfe_ok = .T.
 FOR cfe_i = 1 TO cfe_cnt
    cfe_fpos = AT(",", cfe_fieldstring)
    IF (cfe_fpos=0)
       cfe_fpos = LEN(cfe_fieldstring)+1
    ENDIF
    cfe_field = SUBSTR(cfe_fieldstring, 1, cfe_fpos-1)
    cfe_fieldstring = SUBSTR(cfe_fieldstring, cfe_fpos+1, LEN(cfe_fieldstring))
    IF SEEK(PADR(cfe_file, 15)+PADR(cfe_field, 10), 'allfield', 1)
       cfe_message = ALLTRIM(cfe_field)
    ELSE
       cfe_message = '？'
    ENDIF
    IF EMPTY( &cfe_file..&cfe_field )
       cfe_msg = cfe_msg+'【'+cfe_message+'】'
       cfe_ok = .F.
    ENDIF
 ENDFOR
 IF  .NOT. cfe_ok
    MESSAGEBOX(cfe_msg+"不可空白,请重新输入", 016, "错误信息")
 ENDIF
 = closefile('allfield')
 RETURN cfe_ok
ENDFUNC
**
FUNCTION getbasedata
 PARAMETER sqlfile, sqlfield, localname
 IF EMPTY(localname)
    localname = sqlfile
 ENDIF
 WAIT WINDOW NOCLEAR NOWAIT localname+' 资料载入中.....'
 IF SQLEXEC(oenv.mysqldata,'select &sqlfield from &sqlfile','&localname')<0
    =resulterror('select &sqlfile Error')
    selectok = .F.
 ELSE
    selectok = .T.
 ENDIF
 WAIT CLEAR
 RETURN selectok
ENDFUNC
**
FUNCTION getsize
 PARAMETER mmsize
 LOCAL mm10
 mm10 = 0
 IF mmsize>0
    sqlstat = 'select * from size'
    result = SQLEXEC(oenv.mysqldata, sqlstat, 'wsize')
    IF result<0
       = resulterror('尺寸表读取错误！')
    ELSE
       IF RECCOUNT('wsize')>0
          SELECT wsize
          INDEX ON wsizemm TAG wid
          SET ORDER TO 1
          GOTO TOP
          LOCATE FOR wsize.wsizemm>mmsize
          IF FOUND()
             mm10 = wsize.wsizemm
          ELSE
             mm10 = mmsize*10
          ENDIF
       ENDIF
    ENDIF
 ENDIF
 RETURN mm10
ENDFUNC
**
FUNCTION getmaxsize
 LOCAL mm
 mm = 0
 sqlstat = 'select * from size'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'wsize')
 IF result<0
    = resulterror('尺寸表读取错误！')
 ELSE
    IF RECCOUNT('wsize')>0
       SELECT wsize
       INDEX ON wsizemm TAG wid
       SET ORDER TO 1
       GOTO BOTTOM
       mm = wsize.wsizemm/10
    ENDIF
 ENDIF
 IF USED('wsize')
    = closefile('wsize')
 ENDIF
 RETURN mm
ENDFUNC
**
FUNCTION getminsize
 LOCAL mm
 mm = 0
 sqlstat = 'select * from size'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'wsize')
 IF result<0
    = resulterror('尺寸表读取错误！')
 ELSE
    IF RECCOUNT('wsize')>0
       SELECT wsize
       INDEX ON wsizemm TAG wid
       SET ORDER TO 1
       GOTO TOP
       mm = wsize.wsizemm/10
    ENDIF
 ENDIF
 IF USED('wsize')
    = closefile('wsize')
 ENDIF
 RETURN mm
ENDFUNC
**
FUNCTION getbpcsweight
 PARAMETER lp1, lp2, lp3, lp4
 LOCAL bmaterial, bf, bwidth, blength, pwei, pcswei
 bmaterial = ALLTRIM(lp1)
 bf = ALLTRIM(lp2)
 bwidth = lp3
 blength = lp4
 pcswei = 0
 pwei = getstweight(bmaterial, bf)
 pcswei = ROUND(bwidth*blength*pwei/1000000, oenv.cwdp)
 RETURN pcswei
ENDFUNC
**
FUNCTION getboardweight
 PARAMETER lp0, lp1, lp2, lp3, lp4
 LOCAL bsup, bmaterial, bf, bwidth, blength, bnum, bgive, pwei, pcswei
 bsup = ALLTRIM(lp0)
 bmaterial = ALLTRIM(lp1)
 bf = ALLTRIM(lp2)
 bwidth = lp3
 blength = lp4
 pcswei = 0
 sqlstat = 'select unitweight from buypbprice where pbsupplier=?bsup and pbmaterial=?bmaterial and pbcorr=?bf'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'buypbprice1')
 IF result>0
    IF RECCOUNT('buypbprice1')>0
       pwei = buypbprice1.unitweight
       IF pwei>0
          pcswei = ROUND(bwidth*blength*pwei/1000000, oenv.cwdp)
       ELSE
          pwei = getstweight(bmaterial, bf)
          pcswei = ROUND(bwidth*blength*pwei/1000000, oenv.cwdp)
       ENDIF
    ELSE
       pwei = getstweight(bmaterial, bf)
       pcswei = ROUND(bwidth*blength*pwei/1000000, oenv.cwdp)
    ENDIF
 ELSE
    = resulterror('纸板材价表重量栏读取错误！')
 ENDIF
 IF USED('buypbprice1')
    = closefile('buypbprice1')
 ENDIF
 RETURN pcswei
ENDFUNC
**
FUNCTION getstweight
 PARAMETER pb1, pb2
 LOCAL pbmaterial, pbflute, stmark, pwei
 pwei = 0
 stmark = 'Y'
 pbmaterial = pb1
 pbflute = pb2
 IF EMPTY(pbflute)
    IF LEN(ALLTRIM(pbmaterial))>3
       pbflute = 'BC'
    ELSE
       pbflute = 'B'
    ENDIF
 ENDIF
 result = SQLEXEC(oenv.mysqldata, 'select ccode,crate from corrugate', 'cor2')
 INDEX ON ccode TAG coid
 sqlstat = 'select papercode,unitweight from paper where companyid=?oApp.companyid and isdefine=?stmark'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'paper1')
 IF result>0
    IF RECCOUNT('paper1')>0
       INDEX ON papercode TAG pid
       mn = LEN(pbmaterial)
       mc = LEN(pbflute)
       IF mc>0
          DIMENSION cc[mc]
          cc = 1.0 
       ELSE
          DIMENSION cc[1]
          cc[1] = 1.0 
       ENDIF
       FOR k = 1 TO mc
          wf = SUBSTR(pbflute, k, 1)
          SELECT cor2
          SET ORDER TO 1
          IF SEEK(wf)
             cc[k] = cor2.crate
          ELSE
             cc[k] = 1.0 
          ENDIF
       ENDFOR
       DIMENSION pw[mn]
       pw = 0
       FOR i = 1 TO mn
          pcode = SUBSTR(pbmaterial, i, 1)
          SELECT paper1
          SET ORDER TO 1
          IF SEEK(pcode)
             pw[i] = paper1.unitweight
          ELSE
             pw[i] = 0
          ENDIF
       ENDFOR
       DO CASE
          CASE mn=1
             pbwei = ROUND(pw(1)*cc(1)/1000, oenv.cwdp)
          CASE mn=2
             pbwei = ROUND((pw(1)+pw(2)*cc(1))/1000, oenv.cwdp)
          CASE mn=3
             pbwei = ROUND((pw(1)+pw(2)*cc(1)+pw(3))/1000, oenv.cwdp)
          CASE mn=4
             pbwei = ROUND((pw(1)*cc(1)+pw(2)+pw(3)*cc(2)+pw(4))/1000, oenv.cwdp)
          CASE mn=5
             pbwei = ROUND((pw(1)+pw(2)*cc(1)+pw(3)+pw(4)*cc(2)+pw(5))/1000, oenv.cwdp)
          CASE mn=6
             pbwei = ROUND((pw(1)*cc(1)+pw(2)+pw(3)*cc(2)+pw(4)+pw(5)*cc(3)+pw(6))/1000, oenv.cwdp)
          CASE mn=7
             pbwei = ROUND((pw(1)+pw(2)*cc(1)+pw(3)+pw(4)*cc(2)+pw(5)+pw(6)*cc(3)+pw(7))/1000, oenv.cwdp)
       ENDCASE
    ELSE
       = resulterror('原纸设置表无对应栏位或原纸计算默认开关未打开！')
    ENDIF
 ELSE
    = resulterror('原纸设置表读取错误！')
 ENDIF
 IF USED('paper1')
    = closefile('paper1')
 ENDIF
 IF USED('cor2')
    = closefile('cor2')
 ENDIF
 RETURN pbwei
ENDFUNC
**
FUNCTION getpweight
 PARAMETER pb1, pb2, pb3
 LOCAL pcode, stmark, pwei
 pwei = 0.00 
 stmark = 'Y'
 pcode = pb1
 sqlstat = 'select papercode,unitweight from paper where companyid=?oApp.companyid and papercode=?pcode and isdefine=?stmark'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'paper1')
 IF result>0
    IF RECCOUNT('paper1')>0
       SELECT paper1
       pwei = ROUND(paper1.unitweight*pb2*pb3/1000000, 2)
    ELSE
       = resulterror('原纸设置表无对应栏位或原纸计算默认开关未打开！')
    ENDIF
 ELSE
    = resulterror('原纸设置表读取错误！')
 ENDIF
 IF USED('paper1')
    = closefile('paper1')
 ENDIF
 RETURN pwei
ENDFUNC
**
FUNCTION getsqmweight
 PARAMETER pb1, ply1, ply2, ply3, ply4, ply5, ply6, ply7
 LOCAL pbflute, sqmweight, layer1w, layer2w, layer3w, layer4w, layer5w, layer6w, layer7w
 sqmweight = 0.0 
 layer1w = 0.0 
 layer2w = 0.0 
 layer3w = 0.0 
 layer4w = 0.0 
 layer5w = 0.0 
 layer6w = 0.0 
 layer7w = 0.0 
 pbflute = pb1
 IF PCOUNT()>1
    result = SQLEXEC(oenv.mysqldata, 'select ccode,crate from corrugate', 'cor1')
    INDEX ON ccode TAG coid
    sqlstat = 'select paperid,bassweight from paper where companyid=?oApp.companyid'
    result = SQLEXEC(oenv.mysqldata, sqlstat, 'paper1')
    IF result>0
       IF RECCOUNT('paper1')>0
          INDEX ON paperid TAG pid
          mc = LEN(pbflute)
          IF mc>0
             DIMENSION cc[mc]
             cc = 1.0 
          ELSE
             DIMENSION cc[1]
             cc[1] = 1.0 
          ENDIF
          FOR k = 1 TO mc
             wf = SUBSTR(pbflute, k, 1)
             SELECT cor1
             SET ORDER TO 1
             IF SEEK(wf)
                cc[k] = cor1.crate
             ELSE
                cc[k] = 1.0 
             ENDIF
          ENDFOR
          IF  .NOT. EMPTY(ply1)
             mn = 1
          ENDIF
          IF  .NOT. EMPTY(ply2)
             mn = 2
          ENDIF
          IF  .NOT. EMPTY(ply3)
             mn = 3
          ENDIF
          IF  .NOT. EMPTY(ply4)
             mn = 4
          ENDIF
          IF  .NOT. EMPTY(ply5)
             mn = 5
          ENDIF
          IF  .NOT. EMPTY(ply6)
             mn = 6
          ENDIF
          IF  .NOT. EMPTY(ply7)
             mn = 7
          ENDIF
          DIMENSION pw[mn]
          pw = 0
          DO CASE
             CASE mn=1
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                sqmweight = ROUND(pw(1)*cc(1)/1000, oenv.cwdp)
             CASE mn=2
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                IF SEEK(ply2)
                   pw[2] = paper1.bassweight
                ELSE
                   pw[2] = 0
                ENDIF
                sqmweight = ROUND((pw(1)+pw(2)*cc(1))/1000, oenv.cwdp)
             CASE mn=3
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                IF SEEK(ply2)
                   pw[2] = paper1.bassweight
                ELSE
                   pw[2] = 0
                ENDIF
                IF SEEK(ply3)
                   pw[3] = paper1.bassweight
                ELSE
                   pw[3] = 0
                ENDIF
                sqmweight = ROUND((pw(1)+pw(2)*cc(1)+pw(3))/1000, oenv.cwdp)
             CASE mn=4
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                IF SEEK(ply2)
                   pw[2] = paper1.bassweight
                ELSE
                   pw[2] = 0
                ENDIF
                IF SEEK(ply3)
                   pw[3] = paper1.bassweight
                ELSE
                   pw[3] = 0
                ENDIF
                IF SEEK(ply4)
                   pw[4] = paper1.bassweight
                ELSE
                   pw[4] = 0
                ENDIF
                sqmweight = ROUND((pw(1)*cc(1)+pw(2)+pw(3)*cc(2)+pw(4))/1000, oenv.cwdp)
             CASE mn=5
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                IF SEEK(ply2)
                   pw[2] = paper1.bassweight
                ELSE
                   pw[2] = 0
                ENDIF
                IF SEEK(ply3)
                   pw[3] = paper1.bassweight
                ELSE
                   pw[3] = 0
                ENDIF
                IF SEEK(ply4)
                   pw[4] = paper1.bassweight
                ELSE
                   pw[4] = 0
                ENDIF
                IF SEEK(ply5)
                   pw[5] = paper1.bassweight
                ELSE
                   pw[5] = 0
                ENDIF
                sqmweight = ROUND((pw(1)+pw(2)*cc(1)+pw(3)+pw(4)*cc(2)+pw(5))/1000, oenv.cwdp)
             CASE mn=6
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                IF SEEK(ply2)
                   pw[2] = paper1.bassweight
                ELSE
                   pw[2] = 0
                ENDIF
                IF SEEK(ply3)
                   pw[3] = paper1.bassweight
                ELSE
                   pw[3] = 0
                ENDIF
                IF SEEK(ply4)
                   pw[4] = paper1.bassweight
                ELSE
                   pw[4] = 0
                ENDIF
                IF SEEK(ply5)
                   pw[5] = paper1.bassweight
                ELSE
                   pw[5] = 0
                ENDIF
                IF SEEK(ply6)
                   pw[6] = paper1.bassweight
                ELSE
                   pw[6] = 0
                ENDIF
                sqmweight = ROUND((pw(1)*cc(1)+pw(2)+pw(3)*cc(2)+pw(4)+pw(5)*cc(3)+pw(6))/1000, oenv.cwdp)
             CASE mn=7
                SELECT paper1
                SET ORDER TO 1
                IF SEEK(ply1)
                   pw[1] = paper1.bassweight
                ELSE
                   pw[1] = 0
                ENDIF
                IF SEEK(ply2)
                   pw[2] = paper1.bassweight
                ELSE
                   pw[2] = 0
                ENDIF
                IF SEEK(ply3)
                   pw[3] = paper1.bassweight
                ELSE
                   pw[3] = 0
                ENDIF
                IF SEEK(ply4)
                   pw[4] = paper1.bassweight
                ELSE
                   pw[4] = 0
                ENDIF
                IF SEEK(ply5)
                   pw[5] = paper1.bassweight
                ELSE
                   pw[5] = 0
                ENDIF
                IF SEEK(ply6)
                   pw[6] = paper1.bassweight
                ELSE
                   pw[6] = 0
                ENDIF
                IF SEEK(ply7)
                   pw[7] = paper1.bassweight
                ELSE
                   pw[7] = 0
                ENDIF
                sqmweight = ROUND((pw(1)+pw(2)*cc(1)+pw(3)+pw(4)*cc(2)+pw(5)+pw(6)*cc(3)+pw(7))/1000, oenv.cwdp)
          ENDCASE
       ELSE
          = resulterror('原纸资料表无对应栏位！')
       ENDIF
    ELSE
       = resulterror('原纸资料表读取错误！')
    ENDIF
    IF USED('paper1')
       = closefile('paper1')
    ENDIF
    IF USED('cor1')
       = closefile('cor1')
    ENDIF
 ENDIF
 RETURN sqmweight
ENDFUNC
**
FUNCTION getbsvalue
 PARAMETER lp11, lp12, lp13
 LOCAL bcid, bmaterial, bf, svalue
 bcid = ALLTRIM(lp11)
 bmaterial = ALLTRIM(lp12)
 bf = ALLTRIM(lp13)
 svalue = 0.00 
 sqlstat = 'select factprice,dipnum from specialsale where companyid=?oApp.companyid and custid=?bcid and material=?bmaterial and wtype=?bf'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'bsprice')
 IF result>0
    IF RECCOUNT('bsprice')>0
       svalue = bsprice.factprice
       oenv.dip1 = bsprice.dipnum
    ENDIF
 ENDIF
 IF USED('bsprice')
    = closefile('bsprice')
 ENDIF
 RETURN svalue
ENDFUNC
**
FUNCTION getbpcsvalue
 PARAMETER lp0, lp1, lp2, lp3, lp4
 LOCAL bcid, bmaterial, bf, bwidth, blength, bnum, bgive, pcost, pcscost
 bcid = ALLTRIM(lp0)
 bmaterial = ALLTRIM(lp1)
 bf = ALLTRIM(lp2)
 bwidth = lp3
 blength = lp4
 sqlstat = 'select factprice from specialsale where companyid=?oApp.companyid and custid=?bcid and material=?bmaterial and wtype=?bf'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'bsprice')
 IF result>0
    IF RECCOUNT('bsprice')>0
       pcost = bsprice.factprice
       IF pcost>0
          IF oenv.saleunit='1'
             pcscost = ROUND(1.55 *bwidth*blength*pcost/1000000, oenv.bcvdp)
          ELSE
             pcscost = ROUND(bwidth*blength*pcost/1000000, oenv.bcvdp)
          ENDIF
       ELSE
          pcost = getstcost(bmaterial, bf)
          IF oenv.saleunit='1'
             pcscost = ROUND(1.55 *bwidth*blength*pcost/1000000, oenv.bcvdp)
          ELSE
             pcscost = ROUND(bwidth*blength*pcost/1000000, oenv.bcvdp)
          ENDIF
       ENDIF
    ELSE
       pcost = getstcost(bmaterial, bf)
       IF oenv.saleunit='1'
          pcscost = ROUND(1.55 *bwidth*blength*pcost/1000000, oenv.bcvdp)
       ELSE
          pcscost = ROUND(bwidth*blength*pcost/1000000, oenv.bcvdp)
       ENDIF
    ENDIF
 ELSE
    = resulterror('纸板材价表单价栏读取错误！')
 ENDIF
 IF USED('bsprice')
    = closefile('bsprice')
 ENDIF
 RETURN pcscost
ENDFUNC
**
FUNCTION getboardcost
 PARAMETER lp0, lp1, lp2, lp3, lp4
 LOCAL bsup, bmaterial, bf, bwidth, blength, bnum, bgive, pcost, pcscost
 bsup = ALLTRIM(lp0)
 bmaterial = ALLTRIM(lp1)
 bf = ALLTRIM(lp2)
 bwidth = lp3
 blength = lp4
 sqlstat = 'select unitprice,disrate from buypbprice where pbsupplier=?bsup and pbmaterial=?bmaterial and pbcorr=?bf'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'buypbprice1')
 IF result>0
    IF RECCOUNT('buypbprice1')>0
       pcost = buypbprice1.unitprice*buypbprice1.disrate/100
       IF pcost>0
          IF oenv.saleunit='1'
             pcscost = ROUND(1.55 *bwidth*blength*pcost/1000000, oenv.cvdp)
          ELSE
             pcscost = ROUND(bwidth*blength*pcost/1000000, oenv.cvdp)
          ENDIF
       ELSE
          pcost = getstcost(bmaterial, bf)
          pcscost = ROUND(bwidth*blength*pcost/1000000, oenv.cvdp)
       ENDIF
    ELSE
       pcost = getstcost(bmaterial, bf)
       pcscost = ROUND(bwidth*blength*pcost/1000000, oenv.cvdp)
    ENDIF
 ELSE
    = resulterror('纸板材价表单价栏读取错误！')
 ENDIF
 IF USED('buypbprice1')
    = closefile('buypbprice1')
 ENDIF
 RETURN pcscost
ENDFUNC
**
FUNCTION getstcost
 PARAMETER pb1, pb2
 LOCAL pbmaterial, pbflute, stmark, pvalue, addv
 pvalue = 0
 stmark = 'Y'
 pbmaterial = ALLTRIM(pb1)
 pbflute = ALLTRIM(pb2)
 addv = 0
 result = SQLEXEC(oenv.mysqldata, 'select * from bansale where companyid=?oApp.companyid and material=?pbmaterial and wtype=?pbflute', 'bansale1')
 IF result>0
    IF RECCOUNT('bansale1')>0
       pvalue = bansale1.unitprice
    ELSE
       layer7 = ''
       result = SQLEXEC(oenv.mysqldata, 'select bchange from corrugate where ccode=?pbflute', 'cor1')
       IF result>0
          IF RECCOUNT('cor1')>0
             SELECT cor1
             addv = cor1.bchange
          ELSE
             addv = 0
          ENDIF
       ENDIF
       sqlstat = 'select unitcode,mfv,bfv,cfv,efv,afv,ffv,isdefine from paper where companyid=?oApp.companyid'
       result = SQLEXEC(oenv.mysqldata, sqlstat, 'paper1')
       IF result>0
          IF RECCOUNT('paper1')>0
             INDEX ON unitcode TAG puid
             mc = LEN(pbflute)
             DIMENSION wc[mc]
             FOR k = 1 TO mc
                wc[k] = SUBSTR(pbflute, k, 1)
             ENDFOR
             mn = LEN(pbmaterial)
             DIMENSION pp[mn]
             FOR i = 1 TO mn
                pp[i] = SUBSTR(pbmaterial, i, 1)
             ENDFOR
             DO CASE
                CASE mn=1
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='-'
                            pvalue = paper1.mfv
                         CASE wc(1)='B'
                            pvalue = paper1.bfv
                         CASE wc(1)='C'
                            pvalue = paper1.cfv
                         CASE wc(1)='E'
                            pvalue = paper1.efv
                         CASE wc(1)='A'
                            pvalue = paper1.afv
                         CASE wc(1)='F'
                            pvalue = paper1.ffv
                         OTHERWISE
                            pvalue = paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='-'
                               pvalue = paper1.mfv
                            CASE wc(1)='B'
                               pvalue = paper1.bfv
                            CASE wc(1)='C'
                               pvalue = paper1.cfv
                            CASE wc(1)='E'
                               pvalue = paper1.efv
                            CASE wc(1)='A'
                               pvalue = paper1.afv
                            CASE wc(1)='F'
                               pvalue = paper1.ffv
                            OTHERWISE
                               pvalue = paper1.mfv
                         ENDCASE
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                CASE mn=2
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         pvalue = paper1.mfv
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(2) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='-'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='H'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(1)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(1)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(1)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(1)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(2)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='-'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='H'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(1)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(1)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(1)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(1)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                CASE mn=3
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         pvalue = paper1.mfv
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(3) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(3)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(2) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='H'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(1)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(1)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(1)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(1)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(2)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='H'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(1)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(1)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(1)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(1)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                CASE mn=4
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(2) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(2)
                      IF FOUND()
                         pvalue = paper1.mfv
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(4) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(4)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='H'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(1)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(1)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(1)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(1)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='H'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(1)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(1)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(1)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(1)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(3) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(2)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(2)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(2)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(2)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(2)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(3)
                      IF FOUND()
                         DO CASE
                            CASE wc(2)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(2)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(2)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(2)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(2)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   pvalue = pvalue+oenv.lowestqty
                CASE mn=5
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         pvalue = paper1.mfv
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(3) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(3)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(5) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(5)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(2) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='H'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(1)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(1)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(1)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(1)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(2)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='H'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(1)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(1)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(1)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(1)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(4) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(2)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(2)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(2)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(2)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(2)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(4)
                      IF FOUND()
                         DO CASE
                            CASE wc(2)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(2)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(2)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(2)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(2)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                CASE mn=6
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(2) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(2)
                      IF FOUND()
                         pvalue = paper1.mfv
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(4) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(4)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(6) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(6)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='H'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(1)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(1)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(1)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(1)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='H'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(1)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(1)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(1)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(1)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(3) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(2)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(2)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(2)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(2)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(2)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(3)
                      IF FOUND()
                         DO CASE
                            CASE wc(2)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(2)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(2)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(2)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(2)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(5) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(3)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(3)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(3)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(3)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(3)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(5)
                      IF FOUND()
                         DO CASE
                            CASE wc(3)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(3)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(3)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(3)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(3)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                CASE mn=7
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(1) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(1)
                      IF FOUND()
                         pvalue = paper1.mfv
                      ELSE
                         pvalue = 0
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(3) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(3)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(5) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(5)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(7) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      pvalue = pvalue+paper1.mfv
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(7)
                      IF FOUND()
                         pvalue = pvalue+paper1.mfv
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(2) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(1)='H'
                            pvalue = pvalue+paper1.mfv
                         CASE wc(1)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(1)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(1)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(1)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(1)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(2)
                      IF FOUND()
                         DO CASE
                            CASE wc(1)='H'
                               pvalue = pvalue+paper1.mfv
                            CASE wc(1)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(1)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(1)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(1)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(1)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(4) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(2)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(2)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(2)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(2)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(2)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(4)
                      IF FOUND()
                         DO CASE
                            CASE wc(2)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(2)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(2)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(2)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(2)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
                   SELECT paper1
                   GOTO TOP
                   LOCATE FOR paper1.unitcode=pp(6) .AND. paper1.isdefine=stmark
                   IF FOUND()
                      DO CASE
                         CASE wc(3)='B'
                            pvalue = pvalue+paper1.bfv
                         CASE wc(3)='C'
                            pvalue = pvalue+paper1.cfv
                         CASE wc(3)='E'
                            pvalue = pvalue+paper1.efv
                         CASE wc(3)='A'
                            pvalue = pvalue+paper1.afv
                         CASE wc(3)='F'
                            pvalue = pvalue+paper1.ffv
                         OTHERWISE
                            pvalue = pvalue+paper1.mfv
                      ENDCASE
                   ELSE
                      SELECT paper1
                      GOTO TOP
                      LOCATE FOR paper1.unitcode=pp(6)
                      IF FOUND()
                         DO CASE
                            CASE wc(3)='B'
                               pvalue = pvalue+paper1.bfv
                            CASE wc(3)='C'
                               pvalue = pvalue+paper1.cfv
                            CASE wc(3)='E'
                               pvalue = pvalue+paper1.efv
                            CASE wc(3)='A'
                               pvalue = pvalue+paper1.afv
                            CASE wc(3)='F'
                               pvalue = pvalue+paper1.ffv
                            OTHERWISE
                               pvalue = pvalue+paper1.mfv
                         ENDCASE
                      ENDIF
                   ENDIF
             ENDCASE
             pvalue = pvalue+addv
          ELSE
             = resulterror('原纸资料表无对应栏位！')
          ENDIF
       ELSE
          = resulterror('原纸资料表读取错误！')
       ENDIF
       IF USED('paper1')
          = closefile('paper1')
       ENDIF
       IF USED('cor1')
          = closefile('cor1')
       ENDIF
    ENDIF
 ENDIF
 IF USED('bansale1')
    = closefile('bansale1')
 ENDIF
 RETURN pvalue
ENDFUNC
**
FUNCTION getpapercost
 PARAMETER pb1, pb2, pb3, pb4
 LOCAL pid, pflute, bwidth, blength, pcscost, c1, papercost, basew
 pcscost = 0.0 
 papercost = 0.0 
 pid = ALLTRIM(pb1)
 pflute = ALLTRIM(pb2)
 bwidth = pb3
 blength = pb4
 c1 = 1.0 
 basew = 0
 IF  .NOT. EMPTY(pflute)
    result = SQLEXEC(oenv.mysqldata, 'select crate from corrugate where ccode=?pflute', 'cor1')
    IF RECCOUNT('cor1')>0
       SELECT cor1
       c1 = cor1.crate
    ELSE
       c1 = 1.0 
    ENDIF
 ELSE
    c1 = 1.0 
 ENDIF
 sqlstat = 'select bassweight,cost from paper where companyid=?oApp.companyid and paperid=?pid'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'paper1')
 IF result>0
    IF RECCOUNT('paper1')>0
       SELECT paper1
       basew = paper1.bassweight
       papercost = paper1.cost
       pcscost = ROUND(papercost*c1*bwidth*blength*basew/1000000000, oenv.cvdp)
    ELSE
       = resulterror('原纸编码表无对应栏位！')
    ENDIF
 ELSE
    = resulterror('原纸编码表读取错误！')
 ENDIF
 IF USED('paper1')
    = closefile('paper1')
 ENDIF
 IF USED('cor1')
    = closefile('cor1')
 ENDIF
 RETURN pcscost
ENDFUNC
**
FUNCTION get_min_numdisc
 LOCAL min_v
 min_v = 0
 sqlstat = 'select ordernum from numfavor where companyid=?oApp.companyid'
 result = SQLEXEC(oenv.mysqldata, sqlstat, 'nfavor')
 IF result>0
    IF RECCOUNT('nfavor')>0
       SELECT nfavor
       INDEX ON ordernum TAG numid
       GOTO TOP
       min_v = nfavor.ordernum
    ENDIF
 ENDIF
 IF USED('nfavor')
    = closefile('nfavor')
 ENDIF
 RETURN min_v
ENDFUNC
**
FUNCTION getnumdiscount
 PARAMETER onum, bw
 LOCAL buynum, wf, oldvalue, disvalue
 buynum = onum
 wf = bw
 disvalue = 0.0 
 IF buynum>0
    sqlstat = 'select ordernum,discount1,discount2 from numfavor where companyid=?oApp.companyid'
    result = SQLEXEC(oenv.mysqldata, sqlstat, 'nfavor')
    IF result>0
       IF RECCOUNT('nfavor')>0
          SELECT nfavor
          INDEX ON ordernum TAG numid
          GOTO TOP
          LOCATE FOR nfavor.ordernum>buynum
          IF FOUND()
             IF LEN(wf)>1
                disvalue = nfavor.discount2
             ELSE
                disvalue = nfavor.discount1
             ENDIF
          ENDIF
       ENDIF
    ENDIF
 ENDIF
 IF USED('nfavor')
    = closefile('nfavor')
 ENDIF
 RETURN disvalue
ENDFUNC
**
FUNCTION getotherdiscount
 PARAMETER dis1, dis2
 LOCAL oldvalue, disvalue
 dcode = ALLTRIM(dis1)
 oldvalue = dis2
 disvalue = 0.0 
 mn = LEN(dcode)
 IF mn>0
    sqlstat = 'select discode,disrange,disvalue,actioncode from otherdis where companyid=?oApp.companyid'
    result = SQLEXEC(oenv.mysqldata, sqlstat, 'otherdis')
    IF result>0
       INDEX ON discode TAG dcid
       DO CASE
          CASE mn=2
             code1 = ALLTRIM(dcode)
             SELECT otherdis
             SET ORDER TO 1
             IF SEEK(code1)
                IF otherdis.actioncode='-'
                   IF otherdis.disrange>0
                      disvalue1 = ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue1 = otherdis.disvalue
                   ENDIF
                ELSE
                   IF otherdis.disrange>0
                      disvalue1 = 0.0 -ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue1 = 0.0 -otherdis.disvalue
                   ENDIF
                ENDIF
             ENDIF
             disvalue = disvalue1
          CASE mn=4
             code1 = SUBSTR(dcode, 1, 2)
             code2 = SUBSTR(dcode, 3, 2)
             disvalue1 = 0.0 
             disvalue2 = 0.0 
             SELECT otherdis
             SET ORDER TO 1
             IF SEEK(code1)
                IF otherdis.actioncode='-'
                   IF otherdis.disrange>0
                      disvalue1 = ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue1 = otherdis.disvalue
                   ENDIF
                ELSE
                   IF otherdis.disrange>0
                      disvalue1 = 0.0 -ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue1 = 0.0 -otherdis.disvalue
                   ENDIF
                ENDIF
             ENDIF
             IF SEEK(code2)
                IF otherdis.actioncode='-'
                   IF otherdis.disrange>0
                      disvalue2 = ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue2 = otherdis.disvalue
                   ENDIF
                ELSE
                   IF otherdis.disrange>0
                      disvalue2 = 0.0 -ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue2 = 0.0 -otherdis.disvalue
                   ENDIF
                ENDIF
             ENDIF
             disvalue = disvalue1+disvalue2
          CASE mn=6
             code1 = SUBSTR(dcode, 1, 2)
             code2 = SUBSTR(dcode, 3, 2)
             code3 = SUBSTR(dcode, 5, 2)
             disvalue1 = 0.0 
             disvalue2 = 0.0 
             disvalue3 = 0.0 
             SELECT otherdis
             SET ORDER TO 1
             IF SEEK(code1)
                IF otherdis.actioncode='-'
                   IF otherdis.disrange>0
                      disvalue1 = ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue1 = otherdis.disvalue
                   ENDIF
                ELSE
                   IF otherdis.disrange>0
                      disvalue1 = 0.0 -ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue1 = 0.0 -otherdis.disvalue
                   ENDIF
                ENDIF
             ENDIF
             IF SEEK(code2)
                IF otherdis.actioncode='-'
                   IF otherdis.disrange>0
                      disvalue2 = ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue2 = otherdis.disvalue
                   ENDIF
                ELSE
                   IF otherdis.disrange>0
                      disvalue2 = 0.0 -ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue2 = 0.0 -otherdis.disvalue
                   ENDIF
                ENDIF
             ENDIF
             IF SEEK(code3)
                IF otherdis.actioncode='-'
                   IF otherdis.disrange>0
                      disvalue3 = ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue3 = otherdis.disvalue
                   ENDIF
                ELSE
                   IF otherdis.disrange>0
                      disvalue3 = 0.0 -ROUND(oldvalue*otherdis.disrange/100, 2)
                   ELSE
                      disvalue3 = 0.0 -otherdis.disvalue
                   ENDIF
                ENDIF
             ENDIF
             disvalue = disvalue1+disvalue2+disvalue3
       ENDCASE
    ENDIF
 ENDIF
 IF USED('otherdis')
    = closefile('otherdis')
 ENDIF
 RETURN disvalue
ENDFUNC
**
FUNCTION getbactvalue
 PARAMETER lp0, lp1, lp2, lp3, lp4, lp5, lp6, lp7, lp8, lp9
 LOCAL bcid, bmaterial, bf, bwidth, blength, bnum, bgive, pcost, pbactvalue, bpcsvalue
 bcid = ALLTRIM(lp0)
 bmaterial = ALLTRIM(lp1)
 bf = ALLTRIM(lp2)
 bwidth = lp3
 blength = lp4
 bnum = lp7
 discode = ALLTRIM(lp9)
 bpcsvalue = 0.0 
 pbactvalue = 0.0 
 IF lp5=1
    bpcsvalue = getbpcsvalue(bcid, bmaterial, bf, bwidth, blength)
    pbactvalue = bpcsvalue
 ELSE
    IF oenv.saleunit='1'
       bpcsvalue = ROUND(1.55 *bwidth*blength*getstcost(bmaterial, bf)/1000000, oenv.bcvdp)
    ELSE
       bpcsvalue = ROUND(bwidth*blength*getstcost(bmaterial, bf)/1000000, oenv.bcvdp)
    ENDIF
    pbactvalue = bpcsvalue
 ENDIF
 IF lp6=1
    pbactvalue = pbactvalue-ROUND(bwidth*blength*getnumdiscount(bnum, bf)/1000000, oenv.bcvdp)
 ENDIF
 IF lp8=1
    pbactvalue = pbactvalue-getotherdiscount(discode, bpcsvalue, bwidth, blength)
 ENDIF
 IF pbactvalue<0
    pbactvalue = 0.00 
 ENDIF
 RETURN pbactvalue
ENDFUNC
**
FUNCTION getpaperstore
 PARAMETER pb1, pb2
 LOCAL pid, paperwidth, paperlength
 paperlength = 0.0 
 pid = ALLTRIM(pb1)
 paperwidth = pb2
 IF  .NOT. EMPTY(pid)
    result = SQLEXEC(oenv.mysqldata, 'select bassweight,prweight from paperstore where paperid=?pid and pwidth=?paperwidth', 'ppstore')
    IF RECCOUNT('ppstore')>0
       IF oenv.saleunit='1'
          SELECT ppstore
          paperlength = 10000000*ppstore.prweight/(ppstore.bassweight*pb2*254)
       ELSE
          SELECT ppstore
          paperlength = 1000000*ppstore.prweight/(ppstore.bassweight*pb2)
       ENDIF
    ELSE
       paperlength = 0
    ENDIF
 ELSE
    paperlength = 0
 ENDIF
 IF USED('ppstore')
    = closefile('ppstore')
 ENDIF
 RETURN paperlength
ENDFUNC
**
FUNCTION getpaperlength
 PARAMETER pb1, pb2, pb3
 LOCAL pflute, cutlength, bnum, c1, paperlength
 paperlength = 0.0 
 pflute = ALLTRIM(pb1)
 cutlength = pb2
 bnum = pb3
 c1 = 1.0 
 IF  .NOT. EMPTY(pflute)
    result = SQLEXEC(oenv.mysqldata, 'select crate from corrugate where ccode=?pflute', 'cor1')
    IF RECCOUNT('cor1')>0
       SELECT cor1
       c1 = cor1.crate
    ELSE
       c1 = 1.0 
    ENDIF
 ELSE
    c1 = 1.0 
 ENDIF
 paperlength = ROUND(c1*cutlength*bnum/1000, 1)
 IF USED('cor1')
    = closefile('cor1')
 ENDIF
 RETURN paperlength
ENDFUNC
**
FUNCTION getdelino
 PARAMETER deliname, custcode, saleno, cmode
 LOCAL itemcnt, nowno
 sheetid = ''
 DIMENSION maxno[1]
 DIMENSION nowcnt[1]
 IF EMPTY(cmode)
    custmode = 'A'
 ELSE
    custmode = cmode
 ENDIF
 IF custmode<>'B'
    SELECT MAX(dsheetid) FROM &deliname WHERE custid=custcode INTO ARRAY maxno
 ELSE
    SELECT MAX(dsheetid) FROM &deliname WHERE custid=custcode AND custorder=saleno INTO ARRAY maxno
 ENDIF
 IF _TALLY<>0
    nowno = maxno(1)
    SELECT COUNT(dsheetid) FROM  &deliname WHERE dsheetid=nowno AND custid=custcode INTO ARRAY nowcnt
    IF _TALLY<>0
       itemcnt = nowcnt(1)
    ELSE
       itemcnt = 0
    ENDIF
 ELSE
    nowno = ''
    itemcnt = 0
 ENDIF
 IF  .NOT. EMPTY(nowno)
    DO CASE
       CASE custmode='A'
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('xdeliitem', 'dsheetid', '3')
          ENDIF
       CASE custmode='B'
          IF itemcnt=0
             sheetid = oapp.incrid('xdeliitem', 'dsheetid', '3')
          ENDIF
          IF itemcnt>=1 .AND. itemcnt<6
             sheetid = nowno
          ENDIF
          IF itemcnt>=6
             sheetid = oapp.incrid('xdeliitem', 'dsheetid', '3')
          ENDIF
       CASE custmode='C'
          sheetid = oapp.incrid('xdeliitem', 'dsheetid', '3')
       OTHERWISE
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('xdeliitem', 'dsheetid', '3')
          ENDIF
    ENDCASE
 ELSE
    sheetid = oapp.incrid('xdeliitem', 'dsheetid', '3')
 ENDIF
 RETURN sheetid
ENDFUNC
**
FUNCTION getbdelino
 PARAMETER deliname, custcode, saleno, cmode
 LOCAL itemcnt, nowno
 sheetid = ''
 DIMENSION maxno[1]
 DIMENSION nowcnt[1]
 IF EMPTY(cmode)
    custmode = 'A'
 ELSE
    custmode = cmode
 ENDIF
 SELECT &deliname
 DO CASE
    CASE custmode='B'
       SELECT MAX(dsheetid) FROM &deliname WHERE custid=custcode AND custorder=saleno INTO ARRAY maxno
    CASE custmode='X'
       SELECT MAX(dsheetid) FROM &deliname WHERE delicname=custcode AND custid=saleno INTO ARRAY maxno
    CASE custmode='Y'
       SELECT MAX(dsheetid) FROM &deliname WHERE delicname=custcode AND custorder=saleno INTO ARRAY maxno
    OTHERWISE
       SELECT MAX(dsheetid) FROM &deliname WHERE custid=custcode AND EMPTY(delicname) INTO ARRAY maxno
 ENDCASE
 IF _TALLY<>0
    nowno = maxno(1)
    DO CASE
       CASE custmode='X'
          SELECT COUNT(dsheetid) FROM  &deliname WHERE dsheetid=nowno AND delicname=custcode AND custid=saleno INTO ARRAY nowcnt
       CASE custmode='Y'
          SELECT COUNT(dsheetid) FROM  &deliname WHERE dsheetid=nowno AND delicname=custcode AND custorder=saleno INTO ARRAY nowcnt
       OTHERWISE
          SELECT COUNT(dsheetid) FROM  &deliname WHERE dsheetid=nowno AND custid=custcode AND EMPTY(delicname) INTO ARRAY nowcnt      
    ENDCASE
    IF _TALLY<>0
       itemcnt = nowcnt(1)
    ELSE
       itemcnt = 0
    ENDIF
 ELSE
    nowno = ''
    itemcnt = 0
 ENDIF
 IF  .NOT. EMPTY(nowno)
    DO CASE
       CASE custmode='A'
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('bdeliitem', 'dsheetid', '3')
          ENDIF
       CASE custmode='B'
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('bdeliitem', 'dsheetid', '3')
          ENDIF
       CASE custmode='C'
          sheetid = oapp.incrid('bdeliitem', 'dsheetid', '3')
       OTHERWISE
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('bdeliitem', 'dsheetid', '3')
          ENDIF
    ENDCASE
 ELSE
    sheetid = oapp.incrid('bdeliitem', 'dsheetid', '3')
 ENDIF
 RETURN sheetid
ENDFUNC
**
FUNCTION getcdelino
 PARAMETER deliname, custcode, saleno, cmode
 LOCAL itemcnt, nowno
 sheetid = ''
 DIMENSION maxno[1]
 DIMENSION nowcnt[1]
 IF EMPTY(cmode)
    custmode = 'A'
 ELSE
    custmode = cmode
 ENDIF
 IF custmode<>'B'
    SELECT MAX(dsheetid) FROM &deliname WHERE custid=custcode INTO ARRAY maxno
 ELSE
    SELECT MAX(dsheetid) FROM &deliname WHERE custid=custcode AND custorder=saleno INTO ARRAY maxno
 ENDIF
 IF _TALLY<>0
    nowno = maxno(1)
    SELECT COUNT(dsheetid) FROM  &deliname WHERE dsheetid=nowno AND custid=custcode INTO ARRAY nowcnt
    IF _TALLY<>0
       itemcnt = nowcnt(1)
    ELSE
       itemcnt = 0
    ENDIF
 ELSE
    nowno = ''
    itemcnt = 0
 ENDIF
 IF  .NOT. EMPTY(nowno)
    DO CASE
       CASE custmode='A'
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('cdeliitem', 'dsheetid', '3')
          ENDIF
       CASE custmode='B'
          IF itemcnt=0
             sheetid = oapp.incrid('cdeliitem', 'dsheetid', '3')
          ENDIF
          IF itemcnt>=1 .AND. itemcnt<6
             sheetid = nowno
          ENDIF
          IF itemcnt>=6
             sheetid = oapp.incrid('cdeliitem', 'dsheetid', '3')
          ENDIF
       CASE custmode='C'
          sheetid = oapp.incrid('cdeliitem', 'dsheetid', '3')
       OTHERWISE
          IF itemcnt<6
             sheetid = nowno
          ELSE
             sheetid = oapp.incrid('cdeliitem', 'dsheetid', '3')
          ENDIF
    ENDCASE
 ELSE
    sheetid = oapp.incrid('cdeliitem', 'dsheetid', '3')
 ENDIF
 RETURN sheetid
ENDFUNC
**
FUNCTION getsystime
 result = SQLEXEC(oenv.mysqldata, 'select getdate() as dt', 'dtb')
 IF result>0
    SELECT dtb
    dtime = dtb.dt
 ELSE
    dtime = DATETIME()
 ENDIF
 RETURN dtime
ENDFUNC
**
FUNCTION getneedsecond
 PARAMETER pno, sheetlength
 LOCAL needsec
 needsec = 0
 result = SQLEXEC(oenv.mysqldata, 'select orderlength,speedv from bplspeed where companyid=?oApp.companyid and plcode=?pno', 'bspeed')
 IF result>0
    IF RECCOUNT('bspeed')>0
       INDEX ON orderlength TAG ol
       SET ORDER TO ol
       GOTO TOP
       LOCATE FOR bspeed.orderlength>=sheetlength
       IF FOUND()
          needsec = CEILING(60*sheetlength/bspeed.speedv)
       ELSE
          needsec = 0
       ENDIF
    ELSE
       needsec = 0
    ENDIF
 ENDIF
 IF USED('bspeed')
    = closefile('bspeed')
 ENDIF
 RETURN needsec
ENDFUNC
**
FUNCTION getprintfee
 PARAMETER psize1, psize2, pnum, mode
 printfee = 0.00 
 minsize = MIN(psize1, psize2)
 maxsize = MAX(psize1, psize2)
 srid = ''
 result = SQLEXEC(oenv.mysqldata, 'select pfsizeid,pfsizea,pfsizeb from pfee2 where pfcode=?mode', 'psizetable')
 IF result>0
    IF mode='1'
       SELECT psizetable
       INDEX ON pfsizea+pfsizeb TAG pfsid
       GOTO TOP
       LOCATE FOR psizetable.pfsizea>=minsize .AND. psizetable.pfsizeb>=maxsize
       IF FOUND()
          srid = psizetable.pfsizeid
       ELSE
          srid = ''
       ENDIF
    ELSE
       SELECT psizetable
       INDEX ON pfsizea+pfsizeb TAG pfsid
       GOTO TOP
       LOCATE FOR psizetable.pfsizea>=maxsize .AND. psizetable.pfsizeb>=minsize
       IF FOUND()
          srid = psizetable.pfsizeid
       ELSE
          srid = ''
       ENDIF
    ENDIF
 ENDIF
 IF  .NOT. EMPTY(srid)
    result = SQLEXEC(oenv.mysqldata, 'select pfcount,pfee from pfee3 where pfcode=?mode and pfsizeid=?srid', 'pfee11')
    IF result>0
       SELECT pfee11
       INDEX ON pfcount TAG pnid
       GOTO TOP
       LOCATE FOR pfee11.pfcount>pnum
       IF FOUND()
          IF pnum>=1000
             printfee = pfee11.pfee*INT(pnum/1000+0.5 )
          ELSE
             printfee = pfee11.pfee
          ENDIF
       ELSE
          printfee = 0.00 
       ENDIF
    ENDIF
 ENDIF
 IF USED('pfee11')
    = closefile('pfee11')
 ENDIF
 IF USED('psizetable')
    = closefile('psizetable')
 ENDIF
 RETURN printfee
ENDFUNC
**
FUNCTION getzhfee
 PARAMETER zhmode, dmcount, pnum
 zhfee = 0.00 
 zhid = ''
 result = SQLEXEC(oenv.mysqldata, 'select zfsizeid,zhms from zfee2 where zfcode=?zhmode', 'zmstable')
 IF result>0
    SELECT zmstable
    INDEX ON zhms TAG zsid
    GOTO TOP
    LOCATE FOR zmstable.zhms>=dmcount
    IF FOUND()
       srid = zmstable.zfsizeid
    ELSE
       srid = ''
    ENDIF
 ENDIF
 IF  .NOT. EMPTY(srid)
    result = SQLEXEC(oenv.mysqldata, 'select zfcount,zfee from zfee3 where zfcode=?zhmode and zfsizeid=?srid', 'zfee11')
    IF result>0
       SELECT zfee11
       INDEX ON zfcount TAG znid
       GOTO TOP
       LOCATE FOR zfee11.zfcount>pnum
       IF FOUND()
          IF pnum>=1000
             zhfee = zfee11.zfee*pnum
          ELSE
             zhfee = zfee11.zfee
          ENDIF
       ELSE
          zhfee = 0.00 
       ENDIF
    ENDIF
 ENDIF
 IF USED('zfee11')
    = closefile('zfee11')
 ENDIF
 IF USED('zmstable')
    = closefile('zmstable')
 ENDIF
 RETURN zhfee
ENDFUNC
**
FUNCTION get_percent
 PARAMETER totno, prno
 nowpercent = prno/totno
 RETURN ALLTRIM(STR(ROUND(prno/totno, 4)*100, 7, 2))+"%"
ENDFUNC
**
FUNCTION CaseMoney
 PARAMETER namount, isdw
 ndzs = STRTRAN(ALLTRIM(STR(namount, 18, 2)), ".", "")
 chzdx = "零壹贰叁肆伍陆柒捌玖"
 cdw = "分角元拾佰仟万拾佰仟亿拾佰仟万拾佰仟亿"
 crmbdx = ""
 ncd = LEN(ndzs)
 FOR i = 1 TO LEN(ndzs)
    cnumbers = SUBSTRC(chzdx, INT(VAL(SUBSTR(ndzs, i, 1))+1), 1)
    IF isdw=1
       cdws = SUBSTRC(cdw, ncd, 1)
    ELSE
       cdws = SPACE(0)
    ENDIF
    crmbdx = crmbdx+cnumbers+cdws
    ncd = ncd-1
 ENDFOR
 FOR i = LEN(ndzs)+1 TO 6
    crmbdx = SPACE(2)+crmbdx
 ENDFOR
 RETURN crmbdx
ENDFUNC
**
FUNCTION btodhigh
 LPARAMETERS c1
 LOCAL bit1, binary
 bit1 = ASC(c1)
 binary = ''
 FOR i = 7 TO 0 STEP -1
    binary = binary+IIF(BITTEST(bit1, i), '1', '0')
 ENDFOR
 n1 = 0
 n1 = VAL(SUBSTR(binary, 1, 1))*32768+VAL(SUBSTR(binary, 2, 1))*16384+VAL(SUBSTR(binary, 3, 1))*8192+VAL(SUBSTR(binary, 4, 1))*4096+VAL(SUBSTR(binary, 5, 1))*2048+VAL(SUBSTR(binary, 6, 1))*1024+VAL(SUBSTR(binary, 7, 1))*512+VAL(SUBSTR(binary, 8, 1))*256
 RETURN n1
ENDFUNC
**
FUNCTION btodhigh16
 LPARAMETERS c1
 LOCAL bit1, binary
 bit1 = ASC(c1)
 binary = ''
 FOR i = 7 TO 0 STEP -1
    binary = binary+IIF(BITTEST(bit1, i), '1', '0')
 ENDFOR
 n1 = 0
 n1 = VAL(SUBSTR(binary, 1, 1))*2147483648 +VAL(SUBSTR(binary, 2, 1))*1073741824+VAL(SUBSTR(binary, 3, 1))*536870912+VAL(SUBSTR(binary, 4, 1))*268435456+VAL(SUBSTR(binary, 5, 1))*134217728+VAL(SUBSTR(binary, 6, 1))*67108864+VAL(SUBSTR(binary, 7, 1))*33554432+VAL(SUBSTR(binary, 8, 1))*16777216
 RETURN n1
ENDFUNC
**
FUNCTION btodlow
 LPARAMETERS c1
 LOCAL bit1, binary
 bit1 = ASC(c1)
 binary = ''
 FOR i = 7 TO 0 STEP -1
    binary = binary+IIF(BITTEST(bit1, i), '1', '0')
 ENDFOR
 n1 = 0
 n1 = VAL(SUBSTR(binary, 1, 1))*128+VAL(SUBSTR(binary, 2, 1))*64+VAL(SUBSTR(binary, 3, 1))*32+VAL(SUBSTR(binary, 4, 1))*16+VAL(SUBSTR(binary, 5, 1))*8+VAL(SUBSTR(binary, 6, 1))*4+VAL(SUBSTR(binary, 7, 1))*2+VAL(SUBSTR(binary, 8, 1))
 RETURN n1
ENDFUNC
**
FUNCTION btodlow16
 LPARAMETERS c1
 LOCAL bit1, binary
 bit1 = ASC(c1)
 binary = ''
 FOR i = 7 TO 0 STEP -1
    binary = binary+IIF(BITTEST(bit1, i), '1', '0')
 ENDFOR
 n1 = 0
 n1 = VAL(SUBSTR(binary, 1, 1))*8388608+VAL(SUBSTR(binary, 2, 1))*4194304+VAL(SUBSTR(binary, 3, 1))*2097152+VAL(SUBSTR(binary, 4, 1))*1048576+VAL(SUBSTR(binary, 5, 1))*524288+VAL(SUBSTR(binary, 6, 1))*262144+VAL(SUBSTR(binary, 7, 1))*131072+VAL(SUBSTR(binary, 8, 1))*65536
 RETURN n1
ENDFUNC
**
FUNCTION dtob16
 LPARAMETERS c1
 LOCAL bit1, binary
 num1 = c1
 binary = 0
 FOR i = 0 TO 7
    IF MOD(num1, 2)=1
       binary = BITSET(binary, i)
    ELSE
       binary = BITCLEAR(binary, i)
    ENDIF
    num1 = INT(num1/2)
 ENDFOR
 n1 = CHR(binary)
 binary = 0
 FOR i = 0 TO 7
    IF MOD(num1, 2)=1
       binary = BITSET(binary, i)
    ELSE
       binary = BITCLEAR(binary, i)
    ENDIF
    num1 = INT(num1/2)
 ENDFOR
 n2 = CHR(binary)
 binary = 0
 mstr = n1+n2
 RETURN mstr
ENDFUNC
**
FUNCTION dtob32
 LPARAMETERS c1
 LOCAL bit1, binary
 num1 = c1
 binary = 0
 FOR i = 0 TO 7
    IF MOD(num1, 2)=1
       binary = BITSET(binary, i)
    ELSE
       binary = BITCLEAR(binary, i)
    ENDIF
    num1 = INT(num1/2)
 ENDFOR
 n1 = CHR(binary)
 binary = 0
 FOR i = 0 TO 7
    IF MOD(num1, 2)=1
       binary = BITSET(binary, i)
    ELSE
       binary = BITCLEAR(binary, i)
    ENDIF
    num1 = INT(num1/2)
 ENDFOR
 n2 = CHR(binary)
 binary = 0
 FOR i = 0 TO 7
    IF MOD(num1, 2)=1
       binary = BITSET(binary, i)
    ELSE
       binary = BITCLEAR(binary, i)
    ENDIF
    num1 = INT(num1/2)
 ENDFOR
 n3 = CHR(binary)
 binary = 0
 FOR i = 0 TO 7
    IF MOD(num1, 2)=1
       binary = BITSET(binary, i)
    ELSE
       binary = BITCLEAR(binary, i)
    ENDIF
    num1 = INT(num1/2)
 ENDFOR
 n4 = CHR(binary)
 binary = 0
 mstr = n1+n2+n3+n4
 RETURN mstr
ENDFUNC
**
