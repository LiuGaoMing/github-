  !   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
��w8�C���y)      ����8-���}pp���?pJ>�A@      8    winspool Epson LQ-300K LPT1:                            �Epson LQ-300K                    � @ n� 	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
DEVICE=Epson LQ-300K
OUTPUT=LPT1:
ORIENTATION=1
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
���շ�����                                                    "�ͻ���"                       9substr(xdeli.dsheetid,5,2)+'/'+substr(xdeli.dsheetid,7,2)                                      
���ռ���                     
���շ�����                                                    "�ͻ��տ������"               
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
���շ�����                     "Ʒ ��"                        
���շ�����                     "�� ��"                        
���շ�����                     "����"                         
���շ�����                                                    
"Ӧ�ս��"                     
���շ�����                     "����"                         
���շ�����                     "�˻���"                       
���շ�����                     "������"                       substr(dtos(date()),1,4)                                      
���շ�����                     substr(dtos(date()),5,2)                                      
���շ�����                     substr(dtos(date()),7,2)                                      
���շ�����                     
���շ�����                     �"**********************************************************(��  ��  ��  ��)*********************************************************"                              !"��"+ ALLTRIM(STR(_PAGENO))+"ҳ "                                                              ����                           
���շ�����                     ,"�绰��0755-27099952-3  ���棺0755-27099766"                    
���շ�����                     ("ͨѶ��ַ�������б������ɸ��򶫷���ҵ��"                        substr(dtos(zx2),1,4)                                         
���շ�����                     substr(dtos(zx2),5,2)                                         
���շ�����                     substr(dtos(zx2),7,2)                                         
���շ�����                     substr(dtos(zx3),1,4)                                         
���շ�����                     substr(dtos(zx3),5,2)                                         
���շ�����                     substr(dtos(zx3),7,2)                                         
���շ�����                     substr(xdeli.dsheetid,9,4)                                                                     
���ռ���                     xdeli.custorder                                               
���ռ���                     xdeli.pname                                                   
���ռ���                     
���շ�����                     empty(xdeli.ordersize)=.t.                                                                     "*"                            xdeli.boxhigh                                                 
���շ�����                     empty(xdeli.ordersize)=.t.                                      xdeli.boxlength                                               
���շ�����                     empty(xdeli.ordersize)=.t.                                      "@Z"                           
���շ�����                     empty(xdeli.ordersize)=.t.                                                                     "*"                            xdeli.boxwidth                                                
���շ�����                     empty(xdeli.ordersize)=.t.                                      xdeli.ordersize                                               
���ռ���                     xdeli.delinum                                                 
���ռ���                     "@Z"                           xdeli.badnum                                                  
���ռ���                     xdeli.badflag<>'0'             "@Z"                           xdeli.pcsvalue                                                
���ռ���                     "@Z"                           xdeli.moneysign                                               
���ռ���                     xdeli.ordernum                                                
���ռ���                     "@Z"                           Wround(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2)                                         
���ռ���                     "@Z"                           xdeli.dsheetid                                                
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
"���ս��"                     Wround(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2)                                         
���ռ���                     xdeli.accok='1'                "@Z"                           
���շ�����                     "���ս�"                   niif(xdeli.accok='1',round(xdeli.pcsvalue*iif(xdeli.badflag='0',xdeli.delinum,xdeli.delinum-xdeli.badnum),2),0)                                                   
���ռ���                     "@Z"                           
���շ�����                     "�ͻ����ţ�"                   xdeli.custid                                                  
���ռ���                     :lookup(customer.custname,xdeli.custid,customer.customerid)                                     ����                           
���շ�����                     "�ͻ����ƣ�"                                                  "..\graphics\lc1.bmp"          
���շ�����                                                    "����ֽҵ(����)���޹�˾"       
���շ�����                     /"�绰��0512-57668888-97  ���棺0512-57668898-9"                 
���շ�����                     )"ͨѶ��ַ������ʡ��ɽ��������339ʡ������"                       ����                           
���շ�����                     
���շ�����                     
���ռ���                     
���շ�����                     
���շ�����                     
���շ�����                     ����                           
���շ�����                     
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