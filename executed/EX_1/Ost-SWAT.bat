@echo off
copy TxtInOut_tpl\000010004.gw TxtInOut\000010004.gw
copy TxtInOut_tpl\000010005.gw TxtInOut\000010005.gw
copy TxtInOut_tpl\000020004.gw TxtInOut\000020004.gw
copy TxtInOut_tpl\000020005.gw TxtInOut\000020005.gw
copy TxtInOut_tpl\000030003.gw TxtInOut\000030003.gw
copy TxtInOut_tpl\000030004.gw TxtInOut\000030004.gw
copy TxtInOut_tpl\000010001.gw TxtInOut\000010001.gw
copy TxtInOut_tpl\000010002.gw TxtInOut\000010002.gw
copy TxtInOut_tpl\000020002.gw TxtInOut\000020002.gw
copy TxtInOut_tpl\000020003.gw TxtInOut\000020003.gw
copy TxtInOut_tpl\000030001.gw TxtInOut\000030001.gw
copy TxtInOut_tpl\000030002.gw TxtInOut\000030002.gw
copy TxtInOut_tpl\000010003.gw TxtInOut\000010003.gw
copy TxtInOut_tpl\000020001.gw TxtInOut\000020001.gw
copy TxtInOut_tpl\000030005.gw TxtInOut\000030005.gw
copy TxtInOut_tpl\000030001.hru TxtInOut\000030001.hru
copy TxtInOut_tpl\000030002.hru TxtInOut\000030002.hru
copy TxtInOut_tpl\000010001.mgt TxtInOut\000010001.mgt
copy TxtInOut_tpl\000010002.mgt TxtInOut\000010002.mgt
copy TxtInOut_tpl\000010003.mgt TxtInOut\000010003.mgt
copy TxtInOut_tpl\000010004.mgt TxtInOut\000010004.mgt
copy TxtInOut_tpl\000010005.mgt TxtInOut\000010005.mgt
copy TxtInOut_tpl\000020001.mgt TxtInOut\000020001.mgt
copy TxtInOut_tpl\000020002.mgt TxtInOut\000020002.mgt
copy TxtInOut_tpl\000020003.mgt TxtInOut\000020003.mgt
copy TxtInOut_tpl\000020004.mgt TxtInOut\000020004.mgt
copy TxtInOut_tpl\000020005.mgt TxtInOut\000020005.mgt
copy TxtInOut_tpl\000030001.mgt TxtInOut\000030001.mgt
copy TxtInOut_tpl\000030002.mgt TxtInOut\000030002.mgt
copy TxtInOut_tpl\000030003.mgt TxtInOut\000030003.mgt
copy TxtInOut_tpl\000030004.mgt TxtInOut\000030004.mgt
copy TxtInOut_tpl\000030005.mgt TxtInOut\000030005.mgt
copy TxtInOut_tpl\000030006.mgt TxtInOut\000030006.mgt
copy TxtInOut_tpl\000030007.mgt TxtInOut\000030007.mgt
copy TxtInOut_tpl\000030008.mgt TxtInOut\000030008.mgt
copy TxtInOut_tpl\000030009.mgt TxtInOut\000030009.mgt
copy TxtInOut_tpl\000030010.mgt TxtInOut\000030010.mgt
copy TxtInOut_tpl\000030011.mgt TxtInOut\000030011.mgt
copy TxtInOut_tpl\000010001.sol TxtInOut\000010001.sol
copy TxtInOut_tpl\000010002.sol TxtInOut\000010002.sol
copy TxtInOut_tpl\000010003.sol TxtInOut\000010003.sol
copy TxtInOut_tpl\000010004.sol TxtInOut\000010004.sol
copy TxtInOut_tpl\000010005.sol TxtInOut\000010005.sol
copy TxtInOut_tpl\000020001.sol TxtInOut\000020001.sol
copy TxtInOut_tpl\000020002.sol TxtInOut\000020002.sol
copy TxtInOut_tpl\000020003.sol TxtInOut\000020003.sol
copy TxtInOut_tpl\000020004.sol TxtInOut\000020004.sol
copy TxtInOut_tpl\000020005.sol TxtInOut\000020005.sol
copy TxtInOut_tpl\000030001.sol TxtInOut\000030001.sol
copy TxtInOut_tpl\000030002.sol TxtInOut\000030002.sol
copy TxtInOut_tpl\000030003.sol TxtInOut\000030003.sol
copy TxtInOut_tpl\000030004.sol TxtInOut\000030004.sol
copy TxtInOut_tpl\000030005.sol TxtInOut\000030005.sol
copy TxtInOut_tpl\000030006.sol TxtInOut\000030006.sol
copy TxtInOut_tpl\000030007.sol TxtInOut\000030007.sol
copy TxtInOut_tpl\000030008.sol TxtInOut\000030008.sol
copy TxtInOut_tpl\000030009.sol TxtInOut\000030009.sol
copy TxtInOut_tpl\000030010.sol TxtInOut\000030010.sol
copy TxtInOut_tpl\000030011.sol TxtInOut\000030011.sol
copy TxtInOut_tpl\basins.bsn TxtInOut\basins.bsn

cd TxtInOut

C:\OSTRICH_SWAT\exe\Rev_692_64rel.exe

cd ..

"C:\Program Files\R\R-4.4.3\bin\Rscript.exe" "C:\OSTRICH_SWAT\executed\EX_1\source\performance.R"
