  �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        GIF89a	 	 �           ����   ,    	 	  ��9�      8    winspool yaLQ-300K \\Ya\lq-630                          �yaLQ-300K                        � @ n  	 �4    �   �                                                                                @ MSUDEpson LQ-300K                   �              d                      �DRIVER=winspool
DEVICE=yaLQ-300K
OUTPUT=\\Ya\lq-630
ORIENTATION=1
PAPERSIZE=9
PAPERLENGTH=2970
PAPERWIDTH=2100
DEFAULTSOURCE=8
PRINTQUALITY=180
COLOR=1
YRESOLUTION=180
TTOPTION=1
                                      biao.bsupcode                  oApp.cnauser                                                  
���շ�����                     
���շ�����                                                    "�Ʊ���"                       
���շ�����                     "��ˣ�"                       
���շ�����                     "ֽ��ɹ�������ϸ��"           
���շ�����                     
"�Ʊ�����"                     DATE()                                                        ����                           
���շ�����                     ."ע����ˮǰ��*��Ϊ������5�ս���Ϊ��ֽ��ɹ���"                  ����                           1biao.tinsnum=0 and sysdate-ttod(biao.delidate1)>5                                              "*"                            
���շ�����                     "�������ڣ�"                   
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           
���շ�����                     "��"                           substr(dtos(aa2),1,4)                                         ����                           substr(dtos(aa2),5,2)                                         ����                           substr(dtos(aa2),7,2)                                         ����                           substr(dtos(aa3),1,4)                                         ����                           substr(dtos(aa3),5,2)                                         ����                           substr(dtos(aa3),7,2)                                         ����                           
���շ�����                     
"��������"                     
���շ�����                     
"��������"                     
���շ�����                     
"������ˮ"                     
���շ�����                     "����"                         substr(biao.xbsheetid,5,8)                                                                     
���շ�����                     biao.bsupplier                                                
���շ�����                     biao.material2                                                
���շ�����                     biao.wslitcount                                               
���շ�����                     "��"+ALLTRIM(STR(_PAGENO))+"ҳ"                                                                
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "����kg"                       
���շ�����                     "�����"                       
���շ�����                     "������"                       
���շ�����                     "������inch"                    substr(dtoc(biao.delidate1),6,5)                                                               
���շ�����                     int(biao.sheetweight)                                         
���շ�����                     5int(biao.tinsnum/biao.wslitcount/biao.lslitcount+0.5)                                          
���շ�����                     5int(biao.toutnum/biao.wslitcount/biao.lslitcount+0.5)                                          
���շ�����                     
���շ�����                     "��ע"                         
���շ�����                     "��Ƿ��"                       
���շ�����                     "ֽ�乤����"                   biao.xwsheetid                                                
���շ�����                     =iif(biao.tinsnum>=biao.boardnum,0,biao.boardnum-biao.tinsnum)                                                                   
���շ�����                     "@Z"                           biao.notecode                                                 
���շ�����                     
���շ�����                     
"ֽ�����"                     Bround(biao.board1num*biao.board1width*biao.board1length/1000000,2)                                                              
���շ�����                     "@Z"                           
���շ�����                     
"���ʽ��"                     biao.sheetvalue                                               
���շ�����                     "@Z"                           biao.board2width                                              
���շ�����                     "@Z"                           biao.board2length                                             
���շ�����                     "@Z"                           
���շ�����                     biao.board2length>0                                           "*"                                                           "..\graphics\lc1.bmp"          
���շ�����                     "�ұ�"                         biao.moneysign                                                
���շ�����                     
���շ�����                     "�ϼƣ�"                       
���շ�����                     
"�ܶ�����"                     biao.xbsheetid                                                
���շ�����                     
���շ�����                     "����"                         biao.boardnum                                                 
���շ�����                     
���շ�����                     "�����"                       
���շ�����                     "ƽ����"                       Bround(biao.board1width*biao.board1length*biao.board1num/1000000,2)                                                              
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "kg"                           int(biao.sheetweight)                                         
���շ�����                     
���շ�����                     "�ܽ��"                       "round(biao.sheetvalue*biao.rate,2)                                                             
���շ�����                     7lookup(biao.moneyname,oEnv.naturalcur,biao.moneycode_a)                                        
���շ�����                     
���շ�����                     
"�ѵ�����"                     iif(biao.tinsnum>0,1,0)                                       
���շ�����                     
���շ�����                     "����"                         biao.tinsnum                                                  
���շ�����                     
���շ�����                     
"δ������"                     
���շ�����                     "����"                         biao.board1num-biao.tinsnum                                                                    
���շ�����                     
���շ�����                     "�ܼƣ�"                       
���շ�����                     "������inch"                   
���շ�����                     "���"                         biao.wtype2                                                   
���շ�����                     
���շ�����                     "������"                       biao.board1num                                                
���շ�����                     iif(biao.tinsnum=0,1,0)                                       
���շ�����                     
���շ�����                     
"�ܶ�����"                     biao.xbsheetid                                                
���շ�����                     
���շ�����                     "����"                         biao.board1num                                                
���շ�����                     
���շ�����                     "�����"                       
���շ�����                     "ƽ����"                       Bround(biao.board1width*biao.board1length*biao.board1num/1000000,2)                                                              
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "kg"                           int(biao.sheetweight)                                         
���շ�����                     
���շ�����                     "�ܽ��"                       "round(biao.sheetvalue*biao.rate,2)                                                             
���շ�����                     7lookup(biao.moneyname,oEnv.naturalcur,biao.moneycode_a)                                        
���շ�����                     
���շ�����                     
"�ѵ�����"                     iif(biao.tinsnum>0,1,0)                                       
���շ�����                     
���շ�����                     "����"                         biao.tinsnum                                                  
���շ�����                     
���շ�����                     
"δ������"                     
���շ�����                     "����"                         biao.board1num-biao.tinsnum                                                                    
���շ�����                     iif(biao.tinsnum=0,1,0)                                       
���շ�����                     
���շ�����                                                    "����ֽҵ(����)���޹�˾"       
���շ�����                     "�����"                       
���շ�����                     "ƽ����"                       >round(biao.boardwidth*biao.boardlength*biao.tinsnum/1000000,2)                                                                  
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "kg"                           0int(biao.tinsnum*biao.sheetweight/biao.boardnum)                                               
���շ�����                     
���շ�����                     "�ܽ��"                       =round(biao.tinsnum*biao.sheetvalue*biao.rate/biao.boardnum,2)                                                                   
���շ�����                     7lookup(biao.moneyname,oEnv.naturalcur,biao.moneycode_a)                                        
���շ�����                     
���շ�����                     "�����"                       
���շ�����                     "ƽ����"                       Qround(biao.board1width*biao.board1length*(biao.board1num-biao.tinsnum)/1000000,2)                                               
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "kg"                           eiif(biao.boardnum>biao.tinsnum,int((biao.boardnum-biao.tinsnum)*biao.sheetweight/biao.boardnum),0.00)                                                            
���շ�����                     
���շ�����                     "�ܽ��"                       riif(biao.boardnum>biao.tinsnum,round((biao.boardnum-biao.tinsnum)*biao.sheetvalue*biao.rate/biao.boardnum,2),0.00)                                               
���շ�����                     7lookup(biao.moneyname,oEnv.naturalcur,biao.moneycode_a)                                        
���շ�����                     
���շ�����                     "�����"                       
���շ�����                     "ƽ����"                       >round(biao.boardwidth*biao.boardlength*biao.tinsnum/1000000,2)                                                                  
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "kg"                           0int(biao.tinsnum*biao.sheetweight/biao.boardnum)                                               
���շ�����                     
���շ�����                     "�ܽ��"                       =round(biao.tinsnum*biao.sheetvalue*biao.rate/biao.boardnum,2)                                                                   
���շ�����                     7lookup(biao.moneyname,oEnv.naturalcur,biao.moneycode_a)                                        
���շ�����                     
���շ�����                     "�����"                       
���շ�����                     "ƽ����"                       Nround(biao.boardwidth*biao.boardlength*(biao.boardnum-biao.tinsnum)/1000000,2)                                                  
���շ�����                     
���շ�����                     "������"                       
���շ�����                     "kg"                           eiif(biao.boardnum>biao.tinsnum,int((biao.boardnum-biao.tinsnum)*biao.sheetweight/biao.boardnum),0.00)                                                            
���շ�����                     
���շ�����                     "�ܽ��"                       riif(biao.boardnum>biao.tinsnum,round((biao.boardnum-biao.tinsnum)*biao.sheetvalue*biao.rate/biao.boardnum,2),0.00)                                               
���շ�����                     7lookup(biao.moneyname,oEnv.naturalcur,biao.moneycode_a)                                        
���շ�����                     ����                           
���շ�����                     
���շ�����                     
���շ�����                     ����                           
���շ�����                     ����                           
���շ�����                     
���շ�����                     
���շ�����                     dataenvironment                JLeft = 1
Top = 220
Width = 520
Height = 200
Name = "Dataenvironment"
                        