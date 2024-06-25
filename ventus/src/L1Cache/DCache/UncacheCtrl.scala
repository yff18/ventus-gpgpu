package L1Cache.DCache

import L1Cache._
import SRAMTemplate.SRAMBundleA
import chisel3._
import chisel3.util._
import top.parameters._

class UncacheCtrl(set: Int, way: Int, tagBits: Int)extends Module {
  val io = IO(new Bundle {
    //From coreReq_pipe0
    val coreReq_Ctrl_st0 =  Flipped(Decoupled(new DCacheControl))
    //From coreReq_pipe1
    val coreReq_Ctrl_st1 = Flipped(Decoupled(new DCacheControl))
    //From MSHR
    val MSHR_empty_st0 = Input(Bool())
    //From WSHR
    val WSHR_empty_st0 = Input(Bool())
    //From tagAccess
    val tagAccess_hitStatus_st1 = Input(new hitStatus(way, tagBits))
    //From pipe Reg
    val coreReq_setIdx_st1 = Input(UInt(log2Up(set).W))

    //uncache busy, unable to handle new req, will block coreReq st0 and st1
    val coreReq_ready_st0 = Output(Bool())
    val coreReq_ready_st1 = Output(Bool())

    val uncacheReadHit_st1 = Output(Bool())
    val uncacheReadMiss_st1 = Output(Bool())
    val uncacheWriteHit_st1 = Output(Bool())
    val uncacheWriteMiss_st1 = Output(Bool())

    val uncacheCoreRsp_valid_st1 = Output(Bool())
    val uncacheMemReq_valid_st1 = Output(Bool())
    val uncacheMemRsp_valid_st0 = Input(Bool())
  })


}