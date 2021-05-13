#!/bin/sh
if [ ! -d "../models" ]; then
  mkdir ../models
fi
cd ../models                            && \
if [ ! -f "nuc15_1.mx" ]; then
    curl -L -o nuc15_1.mx -k https://www.dropbox.com/s/7g8ahvryjkwtf3l/UNETdswResGradientOut_Countnuc_objSz15_crop256Mon_25_May_2020_19-24-04.mx?dl=1
fi
if [ ! -f "nuc7_1.mx" ]; then
    curl -L -o nuc7_1.mx -k https://www.dropbox.com/s/2b4qi1zrn8fc2nc/UNETdswResGradientOut_Countnuc_objSz7_crop128Fri_22_May_2020_13-01-34.mx?dl=1
fi
if [ ! -f "nucSZUP_1.mx" ]; then
    curl -L -o nucSZUP_1.mx -k https://www.dropbox.com/s/7n73anxxue3ik12/SFCNNSizeUp_CountNuc__crop208Thu_28_May_2020_07-56-18.mx?dl=1
fi
if [ ! -f "cell30_1.mx" ]; then
    curl -L -o cell30_1.mx -k https://www.dropbox.com/s/rm7kz8fke1xp9m8/cell30_1.mx?dl=1
fi
if [ ! -f "cell15_1.mx" ]; then
    curl -L -o cell15_1.mx -k https://www.dropbox.com/s/bf3mu0nlmmuhfba/UNETdswResGradientOut_CellLiner_objSz15_crop128Tue_26_May_2020_07-03-45.mx?dl=1
fi
if [ ! -f "cellSZUP_1.mx" ]; then
    curl -L -o cellSZUP_1.mx -k https://www.dropbox.com/s/jqwglvgt26h9fei/SFCNNSizeUp_CellLiner__crop144Thu_28_May_2020_04-00-08.mx?dl=1
fi