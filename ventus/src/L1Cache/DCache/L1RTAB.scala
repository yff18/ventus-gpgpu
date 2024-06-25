package L1Cache.DCache

import L1Cache._
import chisel3._
import chisel3.util._
import config.config.Parameters

class L1RTAB(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    //to coreReq_pipe0
    val coreReq_replay = DecoupledIO(new DCacheCoreReq) // insert core Req to pipe line st0
    //to coreReq-io
    val RTAB_full = Output(Bool())
    //From coreReq_pipe1
    // write miss hit in MSHR:
    // include 1 - probestatus = 3, secondary full, read/write miss, hit in mshr, need to wait for missrspin
    //         2- probestatus = 2/3 write miss, hit in mshr
    val Hitmshr_st1 = Flipped(ValidIO(new MSHRprobeOut(NMshrEntry, NMshrSubEntry)))
    // read miss hit in WSHR (NoC may cause MC problem)
    val readMiss_Hitwshr_st1 = Flipped(ValidIO(UInt(log2Up(NWshrEntry).W)))
    // Mshr is Full
    val MshrFull = Input(Bool())
    val checkRTAB = Flipped(ValidIO(UInt(bABits.W)))
    // coreReq_st1
    val coreReq_st1 = Flipped(ValidIO(new DCacheCoreReq))
    // to coreReqst1
    val checkRTABhit = Output(Bool())
    // update RTAB
    val mshrMissRsp = Flipped(ValidIO(new MSHRmissRspIn(NMshrEntry)))
    val wshrPopReq = Flipped(ValidIO(UInt(log2Up(NWshrEntry).W)))
  })
  val Req_access = Reg(Vec(NRTABs, new DCacheCoreReq))
  val hit_mshr   = RegInit(VecInit(Seq.fill(NRTABs)(false.B)))
  val mshr_full  = RegInit(VecInit(Seq.fill(NRTABs)(false.B)))
  val mshr_idx   = RegInit(VecInit(Seq.fill(NRTABs)(0.U(log2Up(NMshrEntry)))))
  val hit_wshr   = RegInit(VecInit(Seq.fill(NRTABs)(false.B)))
  val wshr_idx   = RegInit(VecInit(Seq.fill(NRTABs)(0.U(log2Up(NWshrEntry)))))
  val hit_RTAB   = RegInit(VecInit(Seq.fill(NRTABs)(false.B)))
  val EntryValid = RegInit(VecInit(Seq.fill(NRTABs)(false.B)))
  val ptr = RegInit(0.U(log2Up(NRTABs).W))
  val seq_Q = Module(new Queue(UInt(log2Up(NRTABs).W),NRTABs,false,false)) // hold the new ptr idx for pop req
  io.RTAB_full := !seq_Q.io.enq.ready

  val ptr_w = Wire(UInt(log2Up(NRTABs).W))
  ptr_w := PriorityEncoder(Reverse(Cat(EntryValid.map(!_))))
  val ptrEnqValid = Wire(Bool())

  val blockAddrMatch = Wire(Vec(NRTABs, Bool()))
  val blockAddrMatchPop = Wire(Vec(NRTABs, Bool()))
  val mshrIdxMatch = Wire(Vec(NRTABs, Bool()))
  val wshrIdxMatch = Wire(Vec(NRTABs, Bool()))

  for(i<-0 until NRTABs){
    blockAddrMatch(i) := (Cat(Req_access(i).tag,Req_access(i).setIdx) === io.checkRTAB.bits)  && EntryValid(i)
      //(Req_access(i).tag === io.coreReq_st1.bits.tag) && (Req_access(i).setIdx === io.coreReq_st1.bits.setIdx) &&  EntryValid(i)
    mshrIdxMatch(i) := (mshr_idx(i) === io.mshrMissRsp.bits.instrId) && hit_mshr(i) && EntryValid(i)
    wshrIdxMatch(i) := (wshr_idx(i) === io.wshrPopReq.bits) && hit_wshr(i) && EntryValid(i)
    blockAddrMatchPop(i) := (Req_access(i).tag === io.coreReq_replay.bits.tag) && (Req_access(i).setIdx === io.coreReq_replay.bits.setIdx) &&  EntryValid(i)
  }
  val MshrHit_Full = (io.Hitmshr_st1.bits.probeStatus === 3.U)
  val MshrWriteHit = (io.coreReq_st1.bits.opcode === 2.U) && (io.Hitmshr_st1.bits.probeStatus === 3.U || io.Hitmshr_st1.bits.probeStatus === 2.U)
  val bAMatch = Cat(blockAddrMatch).orR
  val bAMatchIdx = OHToUInt(Cat(blockAddrMatch))

  io.checkRTABhit := bAMatch
  seq_Q.io.enq.valid := ptrEnqValid
  seq_Q.io.enq.bits := ptr_w

  ptrEnqValid := false.B
  // RTAB push req
  when(io.coreReq_st1.valid){
    //mshr hit case
    when(io.Hitmshr_st1.valid && (MshrHit_Full || MshrWriteHit)){
      EntryValid(ptr_w) := true.B
      Req_access(ptr_w) := io.coreReq_st1.bits
      hit_mshr(ptr_w):= true.B
      mshr_idx(ptr_w) := io.Hitmshr_st1.bits.a_source
      ptrEnqValid := true.B
    }.elsewhen(io.MshrFull){
      // mshr full case
      EntryValid(ptr_w) := true.B
      Req_access(ptr_w) := io.coreReq_st1.bits
      mshr_full(ptr_w) := true.B
      ptrEnqValid := true.B
    }.elsewhen(io.readMiss_Hitwshr_st1.valid ){
      EntryValid(ptr_w) := true.B
      Req_access(ptr_w) := io.coreReq_st1.bits
      hit_wshr(ptr_w) := true.B
      wshr_idx(ptr_w) := io.readMiss_Hitwshr_st1.bits
      ptrEnqValid := true.B
    }.elsewhen(io.checkRTAB.valid && bAMatch){
      EntryValid(ptr_w) := true.B
      Req_access(ptr_w) := io.coreReq_st1.bits
      hit_mshr(ptr_w) := hit_mshr(bAMatchIdx)
      mshr_full(ptr_w) := mshr_full(bAMatchIdx)
      mshr_idx(ptr_w) := mshr_idx(bAMatchIdx)
      hit_wshr(ptr_w) := hit_wshr(bAMatchIdx)
      wshr_idx(ptr_w) := wshr_idx(bAMatchIdx)
      hit_RTAB(ptr_w) := true.B
      EntryValid(ptr_w) := true.B
    }
  }

  // RTAB clear req
  for(i<-0 until NRTABs){
    when(io.mshrMissRsp.valid && mshrIdxMatch(i)){
      mshr_idx(i) := 0.U
      hit_mshr(i) := false.B
    }.elsewhen(io.mshrMissRsp.valid && mshr_full(i)){
      mshr_full(i) := false.B
    }
    when(io.wshrPopReq.valid  && wshrIdxMatch(i)){
      wshr_idx(i) := 0.U
      hit_wshr(i) := false.B
    }
    when(io.coreReq_replay.fire() && blockAddrMatchPop(i)){
      hit_RTAB(i) := false.B
    }
  }
  //replay request
  val popPtr = seq_Q.io.deq.bits
  val popReqAllClear = !(hit_mshr(popPtr) || hit_wshr(popPtr) || hit_RTAB(popPtr) || mshr_full(popPtr)) && EntryValid(popPtr)

  io.coreReq_replay.valid := seq_Q.io.deq.valid && popReqAllClear
  io.coreReq_replay.bits := Req_access(popPtr)
  seq_Q.io.deq.ready := io.coreReq_replay.fire()

  when(io.coreReq_replay.fire()){
    EntryValid(popPtr) := false.B
  }
}