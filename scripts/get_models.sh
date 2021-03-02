#!/bin/sh
if [ ! -d "../models" ]; then
  mkdir ../models
fi
cd ../models                            && \
if [ ! -f "nuc15_1.mx" ]; then
    curl -L -o nuc15_1.mx -k https://www.dropbox.com/s/7g8ahvryjkwtf3l/UNETdswResGradientOut_Countnuc_objSz15_crop256Mon_25_May_2020_19-24-04.mx?dl=1
fi
if [ ! -f "nucSZUP_1.mx" ]; then
    curl -L -o nucSZUP_1.mx -k https://www.dropbox.com/s/7n73anxxue3ik12/SFCNNSizeUp_CountNuc__crop208Thu_28_May_2020_07-56-18.mx?dl=1
fi
if [ ! -f "cell30_1.mx" ]; then
    curl -L -o cell30_1.mx -k https://www.dropbox.com/s/zwt9kg58wldg8fw/UNETdswResGradientOut_CellLiner_objSz30_crop256Sun_24_May_2020_07-28-47.mx?dl=1
fi
if [ ! -f "cellSZUP_1.mx" ]; then
    curl -L -o cellSZUP_1.mx -k https://www.dropbox.com/s/jqwglvgt26h9fei/SFCNNSizeUp_CellLiner__crop144Thu_28_May_2020_04-00-08.mx?dl=1
fi



