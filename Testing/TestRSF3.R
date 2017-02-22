library(RSF3)

# A test of Minnesota data

## Tranche 1
mtbs <- InputTranche1('./config/MTBS.R')
mtbs <- readRDS('mtbs.RDS')
mtbs <- SubsetByState(mtbs, 'Minnesota')
geomac <- InputGeomac('./config/GeoMac.R')
geomac <- SubsetByState(geomac, 'Minnesota')
MN_WF <- InputTranche1('./config/MN_WF.R')
MN_RX <- InputTranche1('./config/MN_RX.R')
T1 <- ProcessTranche(list(MN_WF, MN_RX, mtbs, geomac), 1)

## Tranche 2
FACTS <- InputTranche2('./config/FACTS.R', T1)
FACTS <- SubsetByState(FACTS, 'Minnesota')
MN_DNR_WF <- InputTranche2('./config/MN_DNR_WF.R', T1)
NASF <- InputTranche2('./config/NASF.R', T1)
NASF <- SubsetByState(NASF, 'Minnesota')
MN_FWS <- InputTranche2('./config/MN_FWS.R', T1)
T2 <- ProcessTranche(list(MN_DNR_WF, MN_FWS, NASF, FACTS), 2)
T1.T2 <- MergeTranches(T1, T2)

## Tranche 3
HMS <- InputHMS('./config/HMS.R', T1.T2)
VIIRSi <- InputTranche3('./config/VIIRSi.R', T1.T2)
T3 <- ProcessTranche(list(VIIRSi, HMS), 3)

All <- MergeTranches(T1.T2, T3)

## Write Some output (will fail if existing)
writeOGR(T1, dsn = './FinalData', 'T1_MN', 'ESRI Shapefile')
writeOGR(T2, dsn = './FinalData', 'T2_MN', 'ESRI Shapefile')
writeOGR(T3, dsn = './FinalData', 'T3_MN', 'ESRI Shapefile')
writeOGR(All, dsn = './FinalData', 'All_MN', 'ESRI Shapefile')
