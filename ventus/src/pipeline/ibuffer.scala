package pipeline

import chisel3._
import chisel3.util._
import parameters._
import utils.QueueWithFlush



class instbuffer extends Module{
  val io = IO(new Bundle{
    val in=Flipped(DecoupledIO(new CtrlSigs))
    val flush=Flipped(ValidIO(UInt(depth_warp.W)))
    val out=Vec(num_warp,Decoupled(new CtrlSigs))
    val ibuffer_ready=Output(Vec(num_warp,Bool()))
  })
  //val fifo=VecInit(Seq.fill(num_warp)(Module(new QueueWithFlush((new CtrlSigs),2,hasFlush = true)).io))
  io.in.ready:=false.B
  val fifo=(0 until num_warp).map(i=>{
    val x_single=Module(new QueueWithFlush(new CtrlSigs,num_ibuffer,hasFlush=true))
      io.ibuffer_ready(i):=x_single.io.enq.ready
      x_single.io.enq.bits:=io.in.bits
      x_single.io.enq.valid:=Mux((i.asUInt()===io.in.bits.wid),io.in.valid,false.B)
      when(i.asUInt()===io.in.bits.wid){io.in.ready:=x_single.io.enq.ready}
      io.out(i)<>x_single.io.deq
      x_single.flush:=io.flush.valid&(i.asUInt()===io.flush.bits)
    x_single
  })
  //val arbiter=Module(new arbiter_m2o(3))
}
class ibuffer2issue extends Module{
  val io = IO(new Bundle{
    val in=Flipped(Vec(num_warp,Decoupled(new CtrlSigs)))
    val out=Decoupled(new CtrlSigs)
    val out_sel=Output(UInt(depth_warp.W))
  })
  val rrarbit=Module(new RRArbiter(new CtrlSigs(),num_warp))
  rrarbit.io.in<>io.in
  io.out<>rrarbit.io.out
/*
  rrarbit.io.out.ready:=false.B
  io.out.bits:=io.in(0).bits
  io.out.valid:=io.in(0).valid
  io.in.foreach(_.ready:=false.B)
  io.in(0).ready:=io.out.ready
*/
  io.out_sel:=rrarbit.io.chosen
  //input:ibuffer output: issue exe

}