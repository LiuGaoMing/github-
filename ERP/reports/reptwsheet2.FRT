   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        9C3-0000-0000-C0      ����046}   0-00  
 WrapF      8    winspool Epson LQ-300K LPT1:                            �Epson LQ-300K                    � @ n� 	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
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
       biao.salesmanid                ak2                                                           ����                           "@D"                           
���շ�����                     
"ҵ����"                     
���շ�����                     
"ֽ��Ʒ��"                     
���շ�����                     "����"                         
���շ�����                     "����"                         
���շ�����                     "ֽ���ܶ������"               
���շ�����                     
"��ʼ����"                     
���շ�����                     
"������ˮ"                     
���շ�����                     "������"                       substr(biao.wsheetid,5,8)                                     
���շ�����                     biao.salesmanid                                               
���շ�����                     
biao.pname                                                    
���շ�����                     biao.stylecode                                                
���շ�����                     biao.material1                                                
���շ�����                     biao.ordernum                                                 
���շ�����                     biao.wvalue                                                   
���շ�����                     
���շ�����                     
"�ͻ�����"                     biao.custorder                                                
���շ�����                     
���շ�����                     "�ϼƣ�"                       
���շ�����                     
"��������"                     k1                                                            
���շ�����                     
���շ�����                     
"��������"                     k2                                                            
���շ�����                     
���շ�����                     
"�������"                     round(biao.wvalue*biao.rate,2)                                                                 
���շ�����                     
���շ�����                     
"�ܶ�����"                     k4                                                            
���շ�����                     
���շ�����                     "KG"                           
���շ�����                     
"��������"                     ak3                                                           ����                           "@D"                           
���շ�����                     
"��������"                     "��"+ALLTRIM(STR(_PAGENO))+"ҳ"                                                                
���շ�����                     
���շ�����                     "�������(��X��X�� mm)"        biao.boxlength                                                
���շ�����                     "@I"                           
���շ�����                     biao.boxwidth>0                                               "*"                            biao.boxwidth                                                 
���շ�����                     "@I"                           
���շ�����                     biao.boxhigh>0                                                "*"                            biao.boxhigh                                                  
���շ�����                     
���շ�����                     "����"                                                        "..\graphics\lc1.bmp"          
���շ�����                     
"�ͻ��Ϻ�"                     biao.custmcode                                                
���շ�����                     
���շ�����                     "(ҵ��)"                       
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
"�ܺϼƣ�"                     
���շ�����                     
"��������"                     biao.wsheetid                                                 
���շ�����                     
���շ�����                     
"��������"                     biao.ordernum                                                 
���շ�����                     
���շ�����                     
"�������"                     
���շ�����                     
"�ܶ�����"                     biao.wweight                                                  
���շ�����                     
���շ�����                     "KG"                           round(biao.wvalue*biao.rate,2)                                                                 
���շ�����                     ;lookup(currsys.moneyname,oEnv.naturalcur,currsys.moneycode)                                                                     
���շ�����                     ;lookup(currsys.moneyname,oEnv.naturalcur,currsys.moneycode)                                                                     
���շ�����                     
���շ�����                                                    "����ֽҵ(����)���޹�˾"       k1                             biao.custid                    0                              k2                             biao.ordernum                  0                              k4                             biao.wweight                   0                              ����                           ����                           
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