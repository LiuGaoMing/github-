     !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Ԑ�~�q �U�@z�%~      ������i��	��Ӳb�l����      8    winspool Epson LQ-300K LPT1:                            �Epson LQ-300K                    � @ n� 	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
DEVICE=Epson LQ-300K
OUTPUT=LPT1:
ORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2970
PAPERWIDTH=2100
DEFAULTSOURCE=8
PRINTQUALITY=180
COLOR=2
YRESOLUTION=180
TTOPTION=1
       custid                         oApp.cnauser                                                  
���շ�����                     
���շ�����                                                    "�Ʊ���"                       
���շ�����                     "��ˣ�"                       
���շ�����                                                    "δ�տ������"                 
���շ�����                     e"******************************************(��  ��  ��  ��)*****************************************"                             !"��"+ ALLTRIM(STR(_PAGENO))+"ҳ "                                                              ����                           xdeli.dsheetid                                                
���շ�����                     
���շ�����                     "�ϼƱ�����"                   xdeli.delinum                                                 
���շ�����                     
���շ�����                     "�ͻ�������"                   
���շ�����                     "Ӧ�ս�"                   xdeli.moneyname                                               
���շ�����                     Wround(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2)                                         
���ռ���                     "@Z"                           &iif(xdeli.badflag<>'0',xdeli.badnum,0)                                                         
���շ�����                     
���շ�����                     "�˻�������"                   
���շ�����                                                    
"δ�ս��"                     oiif(xdeli.accok<>'1',round(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2),0)                                                  
���ռ���                     "@Z"                           
���շ�����                     "δ�ս�"                   oiif(xdeli.accok<>'1',round(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2),0)                                                  
���ռ���                     "@Z"                           
���շ�����                                                    "�ͻ���"                       9substr(xdeli.dsheetid,5,2)+'/'+substr(xdeli.dsheetid,7,2)                                      
���ռ���                     
���շ�����                     "���ڷ�Χ��"                   
���շ�����                     "�Ʊ����ڣ�"                   
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "����"                         
���շ�����                     "����"                         
���շ�����                     
"�ͻ�����"                     
���շ�����                     "�� ��"                        
���շ�����                     
"�������"                     
���շ�����                                                    
"Ӧ�ս��"                     
���շ�����                     "�˻���"                       
���շ�����                     "������"                       substr(dtos(date()),1,4)                                      
���շ�����                     substr(dtos(date()),5,2)                                      
���շ�����                     substr(dtos(date()),7,2)                                      
���շ�����                     
���շ�����                                                    
"ת������"                     substr(dtos(zx2),1,4)                                         
���շ�����                     substr(dtos(zx2),5,2)                                         
���շ�����                     substr(dtos(zx2),7,2)                                         
���շ�����                     substr(dtos(zx3),1,4)                                         
���շ�����                     substr(dtos(zx3),5,2)                                         
���շ�����                     substr(dtos(zx3),7,2)                                         
���շ�����                     substr(xdeli.dsheetid,9,4)                                                                     
���ռ���                     xdeli.custorder                                               
���ռ���                     xdeli.material                                                
���ռ���                     xdeli.delinum                                                 
���ռ���                     "@Z"                           xdeli.badnum                                                  
���ռ���                     xdeli.badflag<>'0'             "@Z"                           xdeli.unitprice                                               
���ռ���                     "@Z"                           Wround(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2)                                         
���ռ���                     "@Z"                           5int(xdeli.pcsweight*(xdeli.delinum-xdeli.badnum)+0.5)                                          
���ռ���                     "@Z"                           
���շ�����                     "��"                           xdeli.wtype                                                   
���ռ���                     
���շ�����                     oEnv.saleunit='1'                                             "ǧ���"                       2iif(oEnv.saleunit='1',xdeli.widthin,xdeli.widthmm)                                             
���շ�����                     "@Z"                           
���շ�����                     :iif(oEnv.saleunit='1',xdeli.lengthin>0.1,xdeli.lengthmm>0)                                     "*"                            4iif(oEnv.saleunit='1',xdeli.lengthin,xdeli.lengthmm)                                           
���շ�����                     "@Z"                           siif(oEnv.saleunit='1',int(xdeli.ordernum*xdeli.lengthin*254/10000+0.5),int(xdeli.ordernum*xdeli.lengthmm/1000+0.5))                                              
���ռ���                     "@Z"                           
���շ�����                     oEnv.saleunit='2'                                             "ƽ�׼�"                       
���շ�����                     "�ͻ����ţ�"                   xdeli.custid                                                  
���ռ���                     :lookup(customer.custname,xdeli.custid,customer.customerid)                                     ����                           
���շ�����                     "�ͻ����ƣ�"                                                  "..\graphics\lc1.bmp"          
���շ�����                                                    "����ֽҵ(����)���޹�˾"       
���շ�����                     /"�绰��0512-57668888-97  ���棺0512-57668898-9"                 
���շ�����                     )"ͨѶ��ַ������ʡ��ɽ��������339ʡ������"                       ����                           
���շ�����                     
���շ�����                     
���շ�����                     ����                           
���շ�����                     
���ռ���                     
���շ�����                     
���ռ���                     
���շ�����                     
���ռ���                     
���ռ���                     ����                           
���շ�����                     
���շ�����                     dataenvironment                JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
                        