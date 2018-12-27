*** 
*** ReFox XI+  #GL305681  Administrator  EMBRACE [VFP60]
***
 PUBLIC defa_path, dbf_path, forms_path, reports_path, bmp_path, libs_path, bottom_path, progs_path, sys_path
 PUBLIC nowpercent, chimes, sysmsg, sqllinkstr
 STORE 0 TO nowpercent
 SET SYSMENU OFF
 defa_path = SYS(2003)
 defa_path = SYS(2027, defa_path)
 SET DEFA TO &defa_path
 dbf_path = defa_path+'\Include'
 dbf_path = SYS(2027, dbf_path)
 forms_path = defa_path+'\forms'
 forms_path = SYS(2027, forms_path)
 bmp_path = defa_path+'\Graphics'
 bmp_path = SYS(2027, bmp_path)
 libs_path = defa_path+'\Libs'
 libs_path = SYS(2027, libs_path)
 reports_path = defa_path+'\Reports'
 reports_path = SYS(2027, reports_path)
 bottom_path = defa_path+'\Bottom'
 bottom_path = SYS(2027, bottom_path)
 progs_path = defa_path+'\Progs'
 progs_path = SYS(2027, progs_path)
 sys_path = defa_path+'\system'
 sys_path = SYS(2027, progs_path)
 sqllinkstr = ''
 SET PATH TO &dbf_path;&progs_path;&reports_path;&bmp_path;&libs_path;&forms_path;&bottom_path;&sys_path
 IF SET("talk")=="ON"
    SET TALK OFF
    m.gcoldtalk = "ON"
 ELSE
    m.gcoldtalk = "OFF"
 ENDIF
 SET DEBUG OFF
 m.gntimebegin = SECONDS()
 m.gcoldclas = SET("classlib")
 SET CLASSLIB TO BASEAPP, BASEFORM
 oenv = CREATEOBJECT("ENV")
 oenv.cleanout()
 oenv.savesets()
 oenv.envcheck()
 WITH _SCREEN
    .left = 0
    .top = 0
    .height = INT(525*SYSMETRIC(2)/600)
    .width = SYSMETRIC(1)
    .borderstyle = 2
    .closable = .F.
    .controlbox = .F.
    .maxbutton = .F.
    .minbutton = .T.
    .movable = .T.
    .caption = "龙成纸业生管电脑系统"
    .icon = "eye.ico"
 ENDWITH
 _SCREEN.addobject("image1", "BackImage")
 _SCREEN.show
 DO FORM mnx1
 SET PROCEDURE TO sysfunc, zerrtrap
 oapp = CREATEOBJECT("APP")
 oapp.readini()
 oapp.login()
 IF  .NOT. oapp.permit
    oenv.dosets()
    oapp.it()
    oenv.restoresets
    IF oapp.lev=.T.
       oenv.shutdown()
       RELEASE oapp
       RELEASE oenv
       _SCREEN.removeobject("image1")
       RELEASE backimage
       SET SYSMENU ON
       SET SYSMENU TO DEFAULT
       RETURN
    ELSE
       oenv.shutdown()
       RELEASE oapp
       RELEASE oenv
       _SCREEN.removeobject("image1")
       RELEASE backimage
       DO it.prg
    ENDIF
 ELSE
    oenv.shutdown()
    RELEASE oapp
    RELEASE oenv
    _SCREEN.removeobject("image1")
    RELEASE backimage
    WAIT WINDOW NOWAIT "请验证密码，下次再见！"
    SET SYSMENU ON
    SET SYSMENU TO DEFAULT
    RETURN
 ENDIF
ENDPROC
**
DEFINE CLASS BackImage AS Image
 top = -30
 left = 0
 picture = "bg_res1.bmp"
 backstyle = 0
 visible = .T.
ENDDEFINE
**
*** 
*** ReFox - all is not lost 
***
