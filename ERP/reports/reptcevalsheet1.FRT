   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        �  & {00020420-0      ����0-000000000046}V   �       8    winspool Epson LQ-300K LPT1:                            �Epson LQ-300K                    � @ n� 	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
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
       biao.custid                    ak2                                                           ����                           "@D"                           
���շ�����                     
"�ͻ����"                     
���շ�����                     "���"                         
���շ�����                     
"��ʼ����"                     substr(biao.sheetid,5,8)                                      
���շ�����                     biao.custid                                                   
���շ�����                     
biao.ptype                                                    
���շ�����                     biao.ordernum                                                 
���շ�����                     biao.sheetvalue                                               
���շ�����                     
���շ�����                     "�ϼƣ�"                       k1                                                            
���շ�����                     k2                                                            
���շ�����                     "round(biao.sheetvalue*biao.rate,2)                                                             
���շ�����                     
���շ�����                     
"��������"                     ak3                                                           ����                           "@D"                           "��"+ALLTRIM(STR(_PAGENO))+"ҳ"                                                                
���շ�����                     
���շ�����                     
"�������"                     
���շ�����                     "����"                                                        "..\graphics\lc1.bmp"          
���շ�����                     
"�ͻ��Ϻ�"                     biao.custmcode                                                
���շ�����                     
���շ�����                     "(�ͻ�)"                       
���շ�����                     
"�ͻ�����"                     biao.custname                                                 
���շ�����                     
���շ�����                     
"�������"                     biao.ordersize                                                
���շ�����                     biao.moneyname                                                
���շ�����                     oApp.cnauser                                                  
���շ�����                     
���շ�����                                                    "�Ʊ���"                       
���շ�����                     "��ˣ�"                       
���շ�����                     
"�ܺϼƣ�"                     biao.sheetid                                                  
���շ�����                     biao.ordernum                                                 
���շ�����                     "round(biao.sheetvalue*biao.rate,2)                                                             
���շ�����                     ;lookup(currsys.moneyname,oEnv.naturalcur,currsys.moneycode)                                                                     
���շ�����                     ;lookup(currsys.moneyname,oEnv.naturalcur,currsys.moneycode)                                                                     
���շ�����                     biao.makesize                                                 
���շ�����                     
���շ�����                     "Ʒ��"                         
���շ�����                     "��ӡ���������"               
���շ�����                     
"���۵���"                     
���շ�����                     "������"                       
biao.pname                                                    
���շ�����                     
���շ�����                     
"�������"                     
���շ�����                     
"���۱���"                     
���շ�����                     
"��������"                     
���շ�����                     
"���۽��"                     
���շ�����                     
"���۱���"                     
���շ�����                     
"��������"                     
���շ�����                     
"���۽��"                     
���շ�����                                                    "����ֽҵ(����)���޹�˾"       k1                             biao.sheetid                   0                              k2                             biao.ordernum                  0                              k4                             biao.sheetwt                   0                              ����                           ����                           
���շ�����                     
���շ�����                     
���շ�����                     
���շ�����                     
���շ�����                     
���շ�����                     dataenvironment                JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
                        