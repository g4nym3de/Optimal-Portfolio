# -*- coding: utf-8 -*-
"""
Created on Fri Mar 03 11:45:37 2017

@author: simonkc
"""

import pandas_datareader.data as web
import datetime
import csv
key = 'Adj Close'
# stock = ['0001.hk',	'0002.hk',	'0003.hk',	'0004.hk',	'0005.hk',	'0006.hk',	'0007.hk',	'0008.hk',	'0009.hk',	'0010.hk',	'0011.hk',	'0012.hk',	'0014.hk',	'0015.hk',	'0016.hk',	'0017.hk',	'0018.hk',	'0019.hk',	'0020.hk',	'0021.hk',	'0022.hk',	'0023.hk',	'0024.hk',	'0025.hk',	'0026.hk',	'0027.hk',	'0028.hk',	'0029.hk',	'0030.hk',	'0031.hk',	'0032.hk',	'0033.hk',	'0034.hk',	'0035.hk',	'0036.hk',	'0037.hk',	'0038.hk',	'0039.hk',	'0040.hk',	'0041.hk',	'0042.hk',	'0043.hk',	'0044.hk',	'0045.hk',	'0046.hk',	'0047.hk',	'0048.hk',	'0050.hk',	'0051.hk',	'0052.hk',	'0053.hk',	'0054.hk',	'0055.hk',	'0056.hk',	'0057.hk',	'0058.hk',	'0059.hk',	'0060.hk',	'0061.hk',	'0062.hk',	'0063.hk',	'0064.hk',	'0065.hk',	'0066.hk',	'0067.hk',	'0068.hk',	'0069.hk',	'0070.hk',	'0071.hk',	'0072.hk',	'0073.hk',	'0075.hk',	'0076.hk',	'0077.hk',	'0078.hk',	'0079.hk',	'0081.hk',	'0082.hk',	'0083.hk',	'0084.hk',	'0085.hk',	'0086.hk',	'0087.hk',	'0088.hk',	'0089.hk',	'0090.hk',	'0091.hk',	'0092.hk',	'0093.hk',	'0094.hk',	'0095.hk',	'0096.hk',	'0097.hk',	'0098.hk',	'0099.hk',	'0100.hk',	'0101.hk',	'0102.hk',	'0103.hk',	'0104.hk',	'0105.hk',	'0106.hk',	'0107.hk',	'0108.hk',	'0109.hk',	'0110.hk',	'0111.hk',	'0112.hk',	'0113.hk',	'0114.hk',	'0115.hk',	'0116.hk',	'0117.hk',	'0118.hk',	'0119.hk',	'0120.hk',	'0121.hk',	'0122.hk',	'0123.hk',	'0124.hk',	'0125.hk',	'0126.hk',	'0127.hk',	'0128.hk',	'0129.hk',	'0130.hk',	'0131.hk',	'0132.hk',	'0135.hk',	'0136.hk',	'0137.hk',	'0138.hk',	'0139.hk',	'0141.hk',	'0142.hk',	'0143.hk',	'0144.hk',	'0145.hk',	'0146.hk',	'0147.hk',	'0148.hk',	'0149.hk',	'0151.hk',	'0152.hk',	'0153.hk',	'0154.hk',	'0155.hk',	'0156.hk',	'0157.hk',	'0158.hk',	'0159.hk',	'0160.hk',	'0161.hk',	'0162.hk',	'0163.hk',	'0164.hk',	'0165.hk',	'0166.hk',	'0167.hk',	'0168.hk',	'0169.hk',	'0171.hk',	'0172.hk',	'0173.hk',	'0174.hk',	'0175.hk',	'0176.hk',	'0177.hk',	'0178.hk',	'0179.hk',	'0180.hk',	'0181.hk',	'0182.hk',	'0183.hk',	'0184.hk',	'0185.hk',	'0186.hk',	'0187.hk',	'0188.hk',	'0189.hk',	'0190.hk',	'0191.hk',	'0193.hk',	'0194.hk',	'0195.hk',	'0196.hk',	'0197.hk',	'0198.hk',	'0199.hk',	'0200.hk',	'0201.hk',	'0202.hk',	'0205.hk',	'0206.hk',	'0207.hk',	'0208.hk',	'0209.hk',	'0210.hk',	'0211.hk',	'0212.hk',	'0213.hk',	'0214.hk',	'0215.hk',	'0216.hk',	'0217.hk',	'0218.hk',	'0219.hk',	'0220.hk',	'0221.hk',	'0222.hk',	'0223.hk',	'0224.hk',	'0225.hk',	'0226.hk',	'0227.hk',	'0228.hk',	'0229.hk',	'0230.hk',	'0231.hk',	'0232.hk',	'0233.hk',	'0234.hk',	'0235.hk',	'0236.hk',	'0237.hk',	'0238.hk',	'0239.hk',	'0240.hk',	'0241.hk',	'0242.hk',	'0243.hk',	'0244.hk',	'0245.hk',	'0246.hk',	'0247.hk',	'0248.hk',	'0250.hk',	'0251.hk',	'0252.hk',	'0253.hk',	'0254.hk',	'0255.hk',	'0256.hk',	'0257.hk',	'0258.hk',	'0259.hk',	'0260.hk',	'0261.hk',	'0262.hk',	'0263.hk',	'0264.hk',	'0265.hk',	'0266.hk',	'0267.hk',	'0268.hk',	'0269.hk',	'0270.hk',	'0271.hk',	'0272.hk',	'0273.hk',	'0274.hk',	'0275.hk',	'0276.hk',	'0277.hk',	'0278.hk',	'0279.hk',	'0280.hk',	'0281.hk',	'0282.hk',	'0283.hk',	'0285.hk',	'0286.hk',	'0287.hk',	'0288.hk',	'0289.hk',	'0290.hk',	'0291.hk',	'0292.hk',	'0293.hk',	'0294.hk',	'0295.hk',	'0296.hk',	'0297.hk',	'0298.hk',	'0299.hk',	'0300.hk',	'0303.hk',	'0305.hk',	'0306.hk',	'0307.hk',	'0308.hk',	'0309.hk',	'0311.hk',	'0312.hk',	'0313.hk',	'0315.hk',	'0316.hk',	'0317.hk',	'0318.hk',	'0319.hk',	'0320.hk',	'0321.hk',	'0322.hk',	'0323.hk',	'0326.hk',	'0327.hk',	'0328.hk',	'0329.hk',	'0330.hk',	'0331.hk',	'0332.hk',	'0333.hk',	'0334.hk',	'0335.hk',	'0336.hk',	'0337.hk',	'0338.hk',	'0340.hk',	'0341.hk',	'0342.hk',	'0343.hk',	'0345.hk',	'0346.hk',	'0347.hk',	'0348.hk',	'0351.hk',	'0352.hk',	'0353.hk',	'0354.hk',	'0355.hk',	'0357.hk',	'0358.hk',	'0359.hk',	'0360.hk',	'0361.hk',	'0362.hk',	'0363.hk',	'0364.hk',	'0365.hk',	'0366.hk',	'0367.hk',	'0368.hk',	'0369.hk',	'0370.hk',	'0371.hk',	'0372.hk',	'0373.hk',	'0374.hk',	'0375.hk',	'0376.hk',	'0377.hk',	'0378.hk',	'0379.hk',	'0380.hk',	'0381.hk',	'0382.hk',	'0383.hk',	'0384.hk',	'0385.hk',	'0386.hk',	'0387.hk',	'0388.hk',	'0389.hk',	'0390.hk',	'0391.hk',	'0392.hk',	'0393.hk',	'0395.hk',	'0396.hk',	'0397.hk',	'0398.hk',	'0399.hk',	'0400.hk',	'0401.hk',	'0402.hk',	'0403.hk',	'0404.hk',	'0406.hk',	'0408.hk',	'0410.hk',	'0411.hk',	'0412.hk',	'0413.hk',	'0416.hk',	'0417.hk',	'0418.hk',	'0419.hk',	'0420.hk',	'0422.hk',	'0423.hk',	'0425.hk',	'0426.hk',	'0430.hk',	'0431.hk',	'0432.hk',	'0433.hk',	'0434.hk',	'0436.hk',	'0438.hk',	'0439.hk',	'0440.hk',	'0442.hk',	'0444.hk',	'0445.hk',	'0449.hk',	'0450.hk',	'0451.hk',	'0455.hk',	'0456.hk',	'0458.hk',	'0459.hk',	'0460.hk',	'0462.hk',	'0464.hk',	'0465.hk',	'0467.hk',	'0468.hk',	'0469.hk',	'0471.hk',	'0472.hk',	'0474.hk',	'0475.hk',	'0476.hk',	'0479.hk',	'0480.hk',	'0482.hk',	'0483.hk',	'0484.hk',	'0485.hk',	'0486.hk',	'0487.hk',	'0488.hk',	'0489.hk',	'0491.hk',	'0493.hk',	'0494.hk',	'0495.hk',	'0496.hk',	'0497.hk',	'0498.hk',	'0499.hk',	'0500.hk',	'0503.hk',	'0505.hk',	'0506.hk',	'0508.hk',	'0509.hk',	'0510.hk',	'0511.hk',	'0512.hk',	'0513.hk',	'0515.hk',	'0517.hk',	'0518.hk',	'0519.hk',	'0520.hk',	'0521.hk',	'0522.hk',	'0524.hk',	'0525.hk',	'0526.hk',	'0527.hk',	'0528.hk',	'0529.hk',	'0530.hk',	'0531.hk',	'0532.hk',	'0533.hk',	'0535.hk',	'0536.hk',	'0538.hk',	'0539.hk',	'0540.hk',	'0542.hk',	'0543.hk',	'0544.hk',	'0546.hk',	'0547.hk',	'0548.hk',	'0549.hk',	'0550.hk',	'0551.hk',	'0552.hk',	'0553.hk',	'0554.hk',	'0555.hk',	'0556.hk',	'0557.hk',	'0558.hk',	'0559.hk',	'0560.hk',	'0563.hk',	'0564.hk',	'0565.hk',	'0566.hk',	'0567.hk',	'0568.hk',	'0569.hk',	'0570.hk',	'0571.hk',	'0572.hk',	'0573.hk',	'0574.hk',	'0575.hk',	'0576.hk',	'0577.hk',	'0578.hk',	'0579.hk',	'0580.hk',	'0581.hk',	'0582.hk',	'0583.hk',	'0585.hk',	'0586.hk',	'0587.hk',	'0588.hk',	'0589.hk',	'0590.hk',	'0591.hk',	'0592.hk',	'0593.hk',	'0595.hk',	'0596.hk',	'0598.hk',	'0599.hk',	'0600.hk',	'0601.hk',	'0602.hk',	'0603.hk',	'0604.hk',	'0605.hk',	'0606.hk',	'0607.hk',	'0608.hk',	'0609.hk',	'0610.hk',	'0611.hk',	'0613.hk',	'0616.hk',	'0617.hk',	'0618.hk',	'0619.hk',	'0620.hk',	'0621.hk',	'0622.hk',	'0623.hk',	'0626.hk',	'0627.hk',	'0628.hk',	'0629.hk',	'0630.hk',	'0631.hk',	'0632.hk',	'0633.hk',	'0635.hk',	'0636.hk',	'0637.hk',	'0638.hk',	'0639.hk',	'0640.hk',	'0641.hk',	'0643.hk',	'0645.hk',	'0646.hk',	'0647.hk',	'0648.hk',	'0650.hk',	'0651.hk',	'0653.hk',	'0655.hk',	'0656.hk',	'0657.hk',	'0658.hk',	'0659.hk',	'0660.hk',	'0661.hk',	'0662.hk',	'0663.hk',	'0665.hk',	'0668.hk',	'0669.hk',	'0670.hk',	'0672.hk',	'0673.hk',	'0674.hk',	'0675.hk',	'0676.hk',	'0677.hk',	'0678.hk',	'0679.hk',	'0680.hk',	'0681.hk',	'0682.hk',	'0683.hk',	'0684.hk',	'0685.hk',	'0686.hk',	'0687.hk',	'0688.hk',	'0689.hk',	'0690.hk',	'0691.hk',	'0692.hk',	'0693.hk',	'0694.hk',	'0695.hk',	'0696.hk',	'0697.hk',	'0698.hk',	'0699.hk',	'0700.hk',	'0701.hk',	'0702.hk',	'0703.hk',	'0704.hk',	'0705.hk',	'0706.hk',	'0707.hk',	'0708.hk',	'0709.hk',	'0710.hk',	'0711.hk',	'0712.hk',	'0713.hk',	'0715.hk',	'0716.hk',	'0717.hk',	'0718.hk',	'0719.hk',	'0720.hk',	'0722.hk',	'0723.hk',	'0724.hk',	'0725.hk',	'0726.hk',	'0727.hk',	'0728.hk',	'0729.hk',	'0730.hk',	'0731.hk',	'0732.hk',	'0733.hk',	'0735.hk',	'0736.hk',	'0737.hk',	'0738.hk',	'0743.hk',	'0745.hk',	'0746.hk',	'0747.hk',	'0750.hk',	'0751.hk',	'0752.hk',	'0753.hk',	'0754.hk',	'0755.hk',	'0756.hk',	'0757.hk',	'0758.hk',	'0759.hk',	'0760.hk',	'0761.hk',	'0762.hk',	'0763.hk',	'0764.hk',	'0765.hk',	'0766.hk',	'0767.hk',	'0769.hk',	'0771.hk',	'0775.hk',	'0776.hk',	'0777.hk',	'0787.hk',	'0789.hk',	'0794.hk',	'0798.hk',	'0799.hk',	'0800.hk',	'0801.hk',	'0802.hk',	'0803.hk',	'0805.hk',	'0806.hk',	'0809.hk',	'0811.hk',	'0812.hk',	'0813.hk',	'0814.hk',	'0815.hk',	'0816.hk',	'0817.hk',	'0818.hk',	'0819.hk',	'0821.hk',	'0822.hk',	'0825.hk',	'0826.hk',	'0827.hk',	'0828.hk',	'0829.hk',	'0830.hk',	'0831.hk',	'0832.hk',	'0833.hk',	'0834.hk',	'0835.hk',	'0836.hk',	'0837.hk',	'0838.hk',	'0840.hk',	'0841.hk',	'0842.hk',	'0844.hk',	'0845.hk',	'0846.hk',	'0847.hk',	'0848.hk',	'0850.hk',	'0851.hk',	'0852.hk',	'0853.hk',	'0854.hk',	'0855.hk',	'0856.hk',	'0857.hk',	'0858.hk',	'0859.hk',	'0860.hk',	'0861.hk',	'0862.hk',	'0863.hk',	'0864.hk',	'0865.hk',	'0866.hk',	'0867.hk',	'0868.hk',	'0869.hk',	'0871.hk',	'0872.hk',	'0873.hk',	'0874.hk',	'0875.hk',	'0876.hk',	'0877.hk',	'0878.hk',	'0880.hk',	'0881.hk',	'0882.hk',	'0883.hk',	'0884.hk',	'0885.hk',	'0886.hk',	'0887.hk',	'0888.hk',	'0889.hk',	'0891.hk',	'0893.hk',	'0894.hk',	'0895.hk',	'0896.hk',	'0897.hk',	'0898.hk',	'0899.hk',	'0900.hk',	'0902.hk',	'0903.hk',	'0904.hk',	'0906.hk',	'0907.hk',	'0908.hk',	'0909.hk',	'0910.hk',	'0911.hk',	'0912.hk',	'0914.hk',	'0915.hk',	'0916.hk',	'0918.hk',	'0919.hk',	'0921.hk',	'0922.hk',	'0923.hk',	'0925.hk',	'0926.hk',	'0927.hk',	'0928.hk',	'0929.hk',	'0931.hk',	'0932.hk',	'0933.hk',	'0934.hk',	'0935.hk',	'0936.hk',	'0938.hk',	'0939.hk',	'0940.hk',	'0941.hk',	'0943.hk',	'0945.hk',	'0947.hk',	'0948.hk',	'0950.hk',	'0951.hk',	'0952.hk',	'0953.hk',	'0954.hk',	'0956.hk',	'0958.hk',	'0959.hk',	'0960.hk',	'0963.hk',	'0966.hk',	'0967.hk',	'0968.hk',	'0969.hk',	'0970.hk',	'0973.hk',	'0974.hk',	'0975.hk',	'0976.hk',	'0978.hk',	'0979.hk',	'0980.hk',	'0981.hk',	'0982.hk',	'0983.hk',	'0984.hk',	'0985.hk',	'0986.hk',	'0987.hk',	'0988.hk',	'0989.hk',	'0990.hk',	'0991.hk',	'0992.hk',	'0993.hk',	'0995.hk',	'0996.hk',	'0997.hk',	'0998.hk',	'0999.hk',	'1000.hk',	'1001.hk',	'1002.hk',	'1003.hk',	'1004.hk',	'1005.hk',	'1006.hk',	'1007.hk',	'1008.hk',	'1009.hk',	'1010.hk',	'1011.hk',	'1013.hk',	'1019.hk',	'1020.hk',	'1021.hk',	'1022.hk',	'1023.hk',	'1026.hk',	'1027.hk',	'1028.hk',	'1029.hk',	'1030.hk',	'1031.hk',	'1033.hk',	'1035.hk',	'1036.hk',	'1037.hk',	'1038.hk',	'1039.hk',	'1041.hk',	'1043.hk',	'1044.hk',	'1045.hk',	'1046.hk',	'1047.hk',	'1048.hk',	'1049.hk',	'1050.hk',	'1051.hk',	'1052.hk',	'1053.hk',	'1055.hk',	'1057.hk',	'1058.hk',	'1059.hk',	'1060.hk',	'1061.hk',	'1063.hk',	'1064.hk',	'1065.hk',	'1066.hk',	'1068.hk',	'1069.hk',	'1070.hk',	'1071.hk',	'1072.hk',	'1073.hk',	'1075.hk',	'1076.hk',	'1079.hk',	'1080.hk',	'1082.hk',	'1083.hk',	'1085.hk',	'1086.hk',	'1087.hk',	'1088.hk',	'1089.hk',	'1090.hk',	'1091.hk',	'1093.hk',	'1094.hk',	'1096.hk',	'1097.hk',	'1098.hk',	'1099.hk',	'1100.hk',	'1101.hk',	'1102.hk',	'1103.hk',	'1104.hk',	'1105.hk',	'1106.hk',	'1107.hk',	'1108.hk',	'1109.hk',	'1110.hk',	'1111.hk',	'1112.hk',	'1113.hk',	'1114.hk',	'1115.hk',	'1116.hk',	'1117.hk',	'1118.hk',	'1120.hk',	'1121.hk',	'1122.hk',	'1123.hk',	'1124.hk',	'1125.hk',	'1126.hk',	'1127.hk',	'1128.hk',	'1129.hk',	'1130.hk',	'1131.hk',	'1132.hk',	'1133.hk',	'1135.hk',	'1136.hk',	'1137.hk',	'1138.hk',	'1139.hk',	'1141.hk',	'1142.hk',	'1143.hk',	'1145.hk',	'1146.hk',	'1148.hk',	'1149.hk',	'1150.hk',	'1151.hk',	'1152.hk',	'1155.hk',	'1157.hk',	'1159.hk',	'1161.hk',	'1163.hk',	'1164.hk',	'1165.hk',	'1166.hk',	'1168.hk',	'1169.hk',	'1170.hk',	'1171.hk',	'1172.hk',	'1173.hk',	'1174.hk',	'1175.hk',	'1176.hk',	'1177.hk',	'1178.hk',	'1180.hk',	'1181.hk',	'1182.hk',	'1184.hk',	'1185.hk',	'1186.hk',	'1187.hk',	'1188.hk',	'1189.hk',	'1190.hk',	'1191.hk',	'1192.hk',	'1193.hk',	'1194.hk',]
stock =['0001.hk',
'0002.hk',
'0003.hk',
'0004.hk',
'0005.hk',
'0006.hk',
'0011.hk',
'0012.hk',
'0016.hk',
'0017.hk',
'0019.hk',
'0023.hk',
'0027.hk',
'0066.hk',
'0083.hk',
'0101.hk',
'0135.hk',
'0144.hk',
'0151.hk',
'0267.hk',
'0293.hk',
'0386.hk',
'0388.hk',
'0494.hk',
'0688.hk',
'0700.hk',
'0762.hk',
'0823.hk',
'0836.hk',
'0857.hk',
'0883.hk',
'0939.hk',
'0941.hk',
'0992.hk',
'1038.hk',
'1044.hk',
'1088.hk',
'1109.hk',
'1113.hk',
'1299.hk',
'1398.hk',
'1880.hk',
'1928.hk',
'2018.hk',
'2318.hk',
'2319.hk',
'2388.hk',
'2628.hk',
'3328.hk',
'3988.hk']
start = datetime.datetime(2016, 11, 1)
end = datetime.datetime(2017, 4, 1)
f = web.DataReader(stock, 'yahoo', start, end)

df = f.ix[key]


# dates =[]
# for x in range(len(df)):
#     newdate = str(df.index[x])
#     newdate = newdate[0:10]
#     dates.append(newdate)
#
# df['dates'] = dates

print df.head()
print df.tail()

# df.describe()
df.to_csv('hsi_1611-1704.csv')


