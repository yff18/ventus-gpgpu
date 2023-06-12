package L1Cache

import L1Cache.DCache._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFile
import config.config.Parameters

class DCacheWraper extends Module{
  val param = (new MyConfig).toInstance

  val io = IO(new Bundle{
    val coreReq = Flipped(DecoupledIO(new DCacheCoreReq()(param)))
    val coreRsp = DecoupledIO(new DCacheCoreRsp()(param))
    val memReq_ready = Input(Bool())
    val dummy = Output(Bool())
  })

  val DCache = Module(new DataCache()(param))
  io.coreReq <> DCache.io.coreReq
  io.coreRsp <> DCache.io.coreRsp
  val L2 = Module(new L2ROM()(param))
  L2.io.memRsp <> DCache.io.memRsp
  L2.io.memReq <> DCache.io.memReq
  L2.io.memReq_ready := io.memReq_ready

  io.dummy := Cat(L2.io.memReq_data).orR ^ Cat(L2.io.memReq_mask).orR
}

class L2ROM(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle{
    val memReq = Flipped(DecoupledIO(new DCacheMemReq))
    val memRsp= DecoupledIO(new DCacheMemRsp)

    //ports under tb control
    val memReq_ready = Input(Bool())
    val memReq_data = Output(Vec(BlockWords,UInt(WordLength.W)))
    val memReq_mask = Output(Vec(BlockWords,Bool()))
  })
  val memory = Mem(4096*BlockWords,UInt(WordLength.W))
  loadMemoryFromFile(memory,"./L2Image.txt")
  val raw_vec = Wire(Vec(BlockWords,UInt(WordLength.W)))
  for (i<- 0 until BlockWords){//do not include blockWords
    raw_vec(i) := memory.read(Cat(get_blockAddr(io.memReq.bits.a_addr),i.U(BlockOffsetBits.W)))
  }
  io.memReq.ready := io.memReq_ready
  val data_out = Wire(Vec(BlockWords,UInt(WordLength.W)))
  data_out := raw_vec

  val instrIdx_out1 = RegEnable(io.memReq.bits.a_source,io.memReq.fire())
  val data_out1 = RegEnable(data_out,io.memReq.fire())
  val addr_out1 = RegEnable(Cat(get_blockAddr(io.memReq.bits.a_addr),
    Fill(32-(TagBits+SetIdxBits),0.U(1.W))),io.memReq.fire())
  val fire_out1 = RegNext(io.memReq.fire()&(io.memReq.bits.a_opcode===TLAOp_Get))

  val instrIdx_out2 = RegNext(instrIdx_out1)
  val data_out2 = RegNext(data_out1)
  val addr_out2 = RegNext(addr_out1)
  val fire_out2 = RegNext(fire_out1)

  val instrIdx_out3 = RegNext(instrIdx_out2)
  val data_out3 = RegNext(data_out2)
  val addr_out3 = RegNext(addr_out2)
  val fire_out3 = RegNext(fire_out2)

  val instrIdx_out4 = RegNext(instrIdx_out3)
  val data_out4 = RegNext(data_out3)
  val addr_out4 = RegNext(addr_out3)
  val fire_out4 = RegNext(fire_out3)
  //
  val mem_rsp_Q = Module(new Queue(new DCacheMemRsp, 6))
  mem_rsp_Q.io.enq.valid := fire_out4
  mem_rsp_Q.io.enq.bits.d_source := instrIdx_out4
  mem_rsp_Q.io.enq.bits.d_addr := addr_out4
  mem_rsp_Q.io.enq.bits.d_data := data_out4
  //should be safe to float enq ready, as this fifo is deep enough TODO: add ready for L2ROM mem_rsp_Q

  io.memRsp <> mem_rsp_Q.io.deq

  io.memReq_data := io.memReq.bits.a_data
  io.memReq_mask := io.memReq.bits.a_mask
}
