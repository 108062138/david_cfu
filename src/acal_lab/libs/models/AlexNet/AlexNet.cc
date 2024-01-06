
#include "acal_lab/includes/models/AlexNet/AlexNet.h"

void acal_lab::alexNet(convInfo *cInfo, gemmInfo *gInfo, mxPlInfo *mInfo, tensorInfo *tInfo, quantiInfo *qInfo)
{
    acal_lab::Conv(&tInfo[1], &tInfo[0], &cInfo[0], &qInfo[0], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[2], &tInfo[1], GENERAL).execute();
    acal_lab::MxPl(&tInfo[3], &tInfo[2], &mInfo[0], GENERAL).execute();
    acal_lab::Conv(&tInfo[4], &tInfo[3], &cInfo[1], &qInfo[1], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[5], &tInfo[4], GENERAL).execute();
    acal_lab::MxPl(&tInfo[6], &tInfo[5], &mInfo[1], GENERAL).execute();
    acal_lab::Conv(&tInfo[7], &tInfo[6], &cInfo[2], &qInfo[2], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[8], &tInfo[7], GENERAL).execute();
    acal_lab::Conv(&tInfo[9], &tInfo[8], &cInfo[3], &qInfo[3], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[10], &tInfo[9], GENERAL).execute();
    acal_lab::Conv(&tInfo[11], &tInfo[10], &cInfo[4], &qInfo[4], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[12], &tInfo[11], GENERAL).execute();
    acal_lab::MxPl(&tInfo[13], &tInfo[12], &mInfo[2], GENERAL).execute();
    acal_lab::Gemm(&tInfo[14], &tInfo[13], &gInfo[0], &qInfo[6], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[15], &tInfo[14], GENERAL).execute();
    acal_lab::Gemm(&tInfo[16], &tInfo[15], &gInfo[1], &qInfo[7], PER_OPERATION_NAIVE_QUANTI).execute();
    acal_lab::ReLU(&tInfo[17], &tInfo[16], GENERAL).execute();
    acal_lab::Gemm(&tInfo[18], &tInfo[17], &gInfo[2], &qInfo[8], PER_OPERATION_NAIVE_QUANTI).execute();
}
