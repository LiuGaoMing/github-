   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        dno=0
create cu      ����       (jobno   int,;      8    winspool Epson LQ-300K LPT1:                            �Epson LQ-300K                    � @ n� 	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
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
       
���շ�����                     "ֽ�����Ƿ����"               
���շ�����                     
"���Ƶ���"                     substr(biao.wsheetid,5,8)                                     
���ռ���                     
���շ�����                     
"�ͻ�����"                     biao.custorder                                                
���ռ���                     
���շ�����                     
"��������"                     	ttod(zx4)                                                     ����                           "@D"                           "��"+ALLTRIM(STR(_PAGENO))+"ҳ"                                                                
���շ�����                     
���շ�����                     
"�ۼ����"                     biao.ordernum-biao.tinsnum                                                                     
���ռ���                     "@Z"                                                          "..\graphics\lc1.bmp"          biao.custname                                                 
���ռ���                     oApp.cnauser                                                  
���շ�����                     
���շ�����                                                    "�Ʊ���"                       
���շ�����                     "��ˣ�"                       
���շ�����                     
"�ܺϼƣ�"                     
���շ�����                     
"Ƿ������"                     biao.wsheetid                                                 
���շ�����                     
���շ�����                     
"Ƿ������"                     biao.ordernum-biao.tinsnum                                                                     
���շ�����                     
���շ�����                     
"�ͻ�����"                     
���շ�����                     "��Ƿ��"                       biao.tinsnum                                                  
���ռ���                     "@Z"                           
���շ�����                                                    "����ֽҵ(����)���޹�˾"       
���շ�����                     
"ֽ��Ʒ��"                     
biao.pname                                                    
���շ�����                     
���շ�����                     
"�������"                     biao.boxlength                                                
���շ�����                     "@I"                           
���շ�����                     biao.boxwidth>0                                               "*"                            biao.boxwidth                                                 
���շ�����                     "@I"                           
���շ�����                     biao.boxhigh>0                                                "*"                            biao.boxhigh                                                  
���շ�����                     
���շ�����                     
"�������"                     biao.ordersize                                                
���շ�����                     ����                           
���շ�����                     
���շ�����                     
���ռ���                     ����                           
���շ�����                     
���շ�����                     
���շ�����                     
���շ�����                     dataenvironment                JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
                        