   �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        62(http://67.159      ����b,\u8650\u5F85,\u6027      8    winspool yaLQ-300K \\Ya\lq-630                          �yaLQ-300K                        � @ n� 	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
DEVICE=yaLQ-300K
OUTPUT=\\Ya\lq-630
ORIENTATION=0
PAPERSIZE=9
PAPERLENGTH=2970
PAPERWIDTH=2100
DEFAULTSOURCE=8
PRINTQUALITY=180
COLOR=2
YRESOLUTION=180
TTOPTION=1
                                      
���շ�����                     
"ֽ��Ʒ��"                     
���շ�����                     "ֽ��ӡˢ���������"           
���շ�����                     
"������ˮ"                     
���շ�����                     "������"                       substr(biao.xpwid,5,8)                                        
���շ�����                     
biao.pname                                                    
���շ�����                     biao.ordernum                                                 
���շ�����                     
���շ�����                     
"�Ʊ�����"                     date()                                                        ����                           "@D"                           "��"+ALLTRIM(STR(_PAGENO))+"ҳ"                                                                
���շ�����                     
���շ�����                     "Ҫ�����ʱ��"                                                "..\graphics\lc1.bmp"          
���շ�����                     
"�ͻ�����"                     biao.custname                                                 
���շ�����                     oApp.cnauser                                                  
���շ�����                     
���շ�����                                                    "�Ʊ���"                       
���շ�����                     
"�ܺϼƣ�"                     
���շ�����                     
"�ų̱���"                     	biao.xpid                                                     
���շ�����                     
���շ�����                     
"�ų�����"                     biao.ordernum                                                 
���շ�����                     
���շ�����                     "����"                         	biao.xpno                                                     
���շ�����                     
���շ�����                     "����"                          substr(ttoc(biao.delidate1),6,5)                                                               
���շ�����                     
���շ�����                     "��Ʒ"                         	biao.give                                                     
���շ�����                     
���շ�����                     "�ų���"                       biao.makenum                                                  
���շ�����                     biao.planftime                                                
���շ�����                     
���շ�����                     "�����"                       biao.goodnum1                                                 
���շ�����                     
���շ�����                     
"�ų����"                     "round(biao.makenum*biao.pcsarea,1)                                                             
���շ�����                     
���շ�����                     "����"                         biao.maketeam1                                                
���շ�����                     
���շ�����                     "����"                         biao.badnum1                                                  
���շ�����                     
���շ�����                     "������"                       biao.inputnum1                                                
���շ�����                     "@Z"                           
���շ�����                     
"ӡˢ����"                     iif(biao.goodnum1>0,1,0)                                      
���շ�����                     
���շ�����                     
"ӡˢ����"                     $iif(biao.goodnum1>0,biao.goodnum1,0)                                                           
���շ�����                     
���շ�����                     
"ӡˢ���"                     <iif(biao.goodnum1>0,round(biao.goodnum1*biao.pcsarea,1),0.0)                                                                    
���շ�����                     
���շ�����                     "ƽ����"                       
���շ�����                     "ƽ����"                       
���շ�����                                                    "����ֽҵ(����)���޹�˾"       
���շ�����                     "�ӹ���̨��"                   
biao.mname                                                    
���շ�����                     
���շ�����                     
"��������"                     biao.inputnum1                                                
���շ�����                     
���շ�����                     
"��������"                     biao.badnum1                                                  
���շ�����                     
���շ�����                     
"�������"                     $round(biao.inputnum1*biao.pcsarea,1)                                                           
���շ�����                     
���շ�����                     "ƽ����"                       
���շ�����                     
"�������"                     "round(biao.badnum1*biao.pcsarea,1)                                                             
���շ�����                     
���շ�����                     "ƽ����"                       
���շ�����                     "����������"                   round(100*k1/k2,1)                                            
���շ�����                     
���շ�����                     "%"                            
���շ�����                     "���������"                   round(100*k3/k4,1)                                            
���շ�����                     
���շ�����                     "%"                            k1                             biao.badnum1                   0                              k2                             biao.inputnum1                 0                              k4                             $round(biao.inputnum1*biao.pcsarea,1)                            0                              k3                             "round(biao.badnum1*biao.pcsarea,1)                              0                              ����                           
���շ�����                     
���շ�����                     
���շ�����                     ����                           
���շ�����                     
���շ�����                     
���շ�����                     
���շ�����                     dataenvironment                JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
                        