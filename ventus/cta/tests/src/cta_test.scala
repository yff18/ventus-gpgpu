package cta_test

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import cta_scheduler._
import org.scalatest.freespec.AnyFreeSpec

import scala.util.Random

class DecoupledIO_monitor[T <: Data](gen: T) extends Bundle {
  val valid = Bool()
  val bits = gen
  val ready = Bool()
}

class test1 extends AnyFreeSpec with ChiselScalatestTester {

  println("class test1 begin")

  class wg_buffer_wrapper extends Module {
    val io = IO(new Bundle{
      val host_wg_new = Flipped(DecoupledIO(new io_host2cta))             // Get new wg from host
      val alloc_wg_new = DecoupledIO(new io_buffer2alloc(16))             // Request allocator to determine if the given wg is ok to allocate
      val alloc_result = Flipped(DecoupledIO(new io_alloc2buffer()))      // Determination result from allocator
      val cuinterface_wg_new = DecoupledIO(new io_buffer2cuinterface)
      val host_wg_new_r = Output(new DecoupledIO_monitor(new io_host2cta))
      val alloc_wg_new_r = Output(new DecoupledIO_monitor(new io_buffer2alloc(16)))
      val alloc_result_r = Output(new DecoupledIO_monitor(new io_alloc2buffer()))
      val cuinterface_wg_new_r = Output(new DecoupledIO_monitor(new io_buffer2cuinterface))
    })
    val inst = Module{new wg_buffer()}
    inst.io.host_wg_new <> io.host_wg_new
    inst.io.alloc_wg_new <> io.alloc_wg_new
    inst.io.alloc_result <> io.alloc_result
    inst.io.cuinterface_wg_new <> io.cuinterface_wg_new
    io.host_wg_new_r.valid := RegNext(io.host_wg_new.valid)
    io.host_wg_new_r.bits  := RegNext(io.host_wg_new.bits)
    io.host_wg_new_r.ready := RegNext(io.host_wg_new.ready)
    io.alloc_wg_new_r.valid := RegNext(io.alloc_wg_new.valid)
    io.alloc_wg_new_r.bits  := RegNext(io.alloc_wg_new.bits )
    io.alloc_wg_new_r.ready := RegNext(io.alloc_wg_new.ready)
    io.alloc_result_r.valid := RegNext(io.alloc_result.valid)
    io.alloc_result_r.bits  := RegNext(io.alloc_result.bits )
    io.alloc_result_r.ready := RegNext(io.alloc_result.ready)
    io.cuinterface_wg_new_r.valid := RegNext(io.cuinterface_wg_new.valid)
    io.cuinterface_wg_new_r.bits  := RegNext(io.cuinterface_wg_new.bits )
    io.cuinterface_wg_new_r.ready := RegNext(io.cuinterface_wg_new.ready)
  }

  /*
  "Test1: wg_buffer" in {
    test(new wg_buffer_wrapper).withAnnotations(Seq()) { dut =>

      dut.io.host_wg_new.initSource().setSourceClock(dut.clock)
      dut.io.alloc_wg_new.initSink().setSinkClock(dut.clock)
      dut.io.alloc_result.initSource().setSourceClock(dut.clock)
      dut.io.cuinterface_wg_new.initSink().setSinkClock(dut.clock)

      val testlen = 200
      val testIn = Seq.tabulate(testlen){i => (Random.nextInt().abs, Random.nextInt(1024))}

      val testSeqIn = Seq.tabulate(testlen){i => (new io_host2cta).Lit(
        _.wg_id -> i.U,
        _.csr_kernel-> testIn(i)._1.U(32.W),
        _.num_lds -> testIn(i)._2.U(10.W),
        _.gds_base -> 0.U,
        _.num_wf -> 0.U,
        _.num_sgpr -> 0.U,
        _.num_vgpr -> 0.U,
        _.num_sgpr_per_wf -> 0.U,
        _.num_vpgr_per_wf -> 0.U,
        _.num_thread_per_wf -> 0.U,
        _.num_gds -> 0.U,
        _.pds_base -> 0.U,
        _.start_pc -> 0.U,
        _.num_wg_x -> 0.U,
        _.num_wg_y -> 0.U,
        _.num_wg_z -> 0.U,
      ) }

      val testOut = new Array[(BigInt, BigInt)](testlen)

      var alloc_cnt = 0
      var cu_cnt = 0
      var wg_id = 0
      var data = 0

      dut.io.alloc_wg_new.ready.poke(false.B)
      dut.io.cuinterface_wg_new.ready.poke(false.B)
      dut.clock.step(5)


      fork{
        dut.clock.step(5)
        dut.io.host_wg_new.enqueueSeq(testSeqIn)
      } .fork {
        while(alloc_cnt < testlen){
          if(dut.io.alloc_wg_new_r.valid.peek().litToBoolean && dut.io.alloc_wg_new_r.ready.peek().litToBoolean) {
            wg_id = dut.io.alloc_wg_new_r.bits.wg_id.peek.litValue.toInt;
            data = dut.io.alloc_wg_new_r.bits.num_lds.peek.litValue.toInt;
            val addr = dut.io.alloc_wg_new_r.bits.wgram_addr.peek.litValue;
            dut.io.alloc_wg_new.ready.poke(false.B)

            dut.clock.step(Random.nextInt(5))
            val accept = Random.nextBoolean
            if(accept){
              alloc_cnt = alloc_cnt + 1
            }

            dut.io.alloc_result.valid.poke(true.B)
            dut.io.alloc_result.bits.poke(
              new(io_alloc2buffer).Lit(
                _.accept -> accept.B,
                _.wgram_addr -> addr.U,
              )
            )
            do {
              dut.clock.step(1)
            } while(!dut.io.alloc_result_r.ready.peek.litToBoolean)
            dut.io.alloc_result.valid.poke(false.B)
          }
          if(alloc_cnt > cu_cnt){
            dut.io.alloc_wg_new.ready.poke(false.B)
          } else {
            dut.io.alloc_wg_new.ready.poke(Random.nextBoolean.B)
          }
          dut.clock.step(1)
        }
      } .fork {
        while(cu_cnt < testlen){
          if(dut.io.cuinterface_wg_new_r.valid.peek.litToBoolean && dut.io.cuinterface_wg_new_r.ready.peek.litToBoolean){
            dut.io.cuinterface_wg_new.ready.poke(false.B)
            testOut(wg_id) = (dut.io.cuinterface_wg_new_r.bits.csr_kernel.peek.litValue, data)
          }
          if(alloc_cnt > cu_cnt && dut.io.cuinterface_wg_new_r.valid.peek.litToBoolean){
            dut.io.cuinterface_wg_new.ready.poke(true.B)
            cu_cnt = cu_cnt + 1
          }
          dut.clock.step(1)
        }
        if(dut.io.cuinterface_wg_new_r.valid.peek.litToBoolean && dut.io.cuinterface_wg_new_r.ready.peek.litToBoolean){
          dut.io.cuinterface_wg_new.ready.poke(false.B)
          testOut(wg_id) = (dut.io.cuinterface_wg_new_r.bits.csr_kernel.peek.litValue, data)
        }
      }.join()

      for(i <- 0 until testlen){
        assert(testOut(i)._1 == testIn(i)._1)
        assert(testOut(i)._2 == testIn(i)._2)
      }
    }
  }
  */

  "Test2: CTA_scheduler" in {
    test(new cta_scheduler_top()).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) { dut =>
    //test(new cta_scheduler_top()).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(1000)
      dut.io.host_wg_new.initSource().setSourceClock(dut.clock)
      dut.io.host_wg_done.initSink().setSinkClock(dut.clock)

      val testlen = 2000
      val testIn_csr = Seq.tabulate(testlen){i => Random.nextInt().abs}
      val testIn_lds = Seq.tabulate(testlen){i =>  Random.nextInt(CONFIG.WG.NUM_LDS_MAX  / 3)}
      val testIn_sgpr = Seq.tabulate(testlen){i => Random.nextInt(CONFIG.WG.NUM_SGPR_MAX / 3)}
      val testIn_vgpr = Seq.tabulate(testlen){i => Random.nextInt(CONFIG.WG.NUM_VGPR_MAX / 3)}
      //val testIn = Seq.tabulate(testlen){i => (i, i)}
      val testOut_cu = new Array[Int](testlen)
      val testOut_csr = new Array[Int](testlen)
      val testOut_lds = new Array[Int](testlen)
      val testOut_sgpr = new Array[Int](testlen)
      val testOut_vgpr = new Array[Int](testlen)
      val testSeqIn = Seq.tabulate(testlen){i => (new io_host2cta).Lit(
        _.wg_id -> i.U,
        _.csr_kernel-> testIn_csr(i).U(CONFIG.GPU.MEM_ADDR_WIDTH),
        _.num_lds -> testIn_lds(i).U,
        _.num_sgpr -> testIn_sgpr(i).U,
        _.num_vgpr -> testIn_vgpr(i).U,
        _.num_wf -> 0.U,
        _.gds_base -> 0.U,
        _.num_sgpr_per_wf -> 0.U,
        _.num_vpgr_per_wf -> 0.U,
        _.num_thread_per_wf -> 0.U,
        _.num_gds -> 0.U,
        _.pds_base -> 0.U,
        _.start_pc -> 0.U,
        _.num_wg_x -> 0.U,
        _.num_wg_y -> 0.U,
        _.num_wg_z -> 0.U,
      ) }

      dut.io.host_wg_done.ready.poke(false.B)
      dut.io.host_wg_new.valid.poke(false.B)
      dut.clock.step(5)

      var cnt = 0
      fork{
        dut.io.host_wg_new.enqueueSeq(testSeqIn)
      } .fork {
        dut.clock.step(70)
        while(cnt < testlen){
          dut.io.host_wg_done.ready.poke((scala.util.Random.nextBoolean() && scala.util.Random.nextBoolean()).B)
          if(dut.io.host_wg_done.valid.peek.litToBoolean && dut.io.host_wg_done.ready.peek.litToBoolean) {
            val wg_id = dut.io.host_wg_done.bits.wg_id.peek.litValue.toInt
            testOut_cu(wg_id) = dut.io.host_wg_done.bits.cu_id.peek.litValue.toInt
            testOut_csr(wg_id) = dut.io.host_wg_done.bits.csr_kernel.peek.litValue.toInt
            testOut_lds(wg_id) = dut.io.host_wg_done.bits.lds_base.peek.litValue.toInt
            testOut_sgpr(wg_id) = dut.io.host_wg_done.bits.sgpr_base.peek.litValue.toInt
            testOut_vgpr(wg_id) = dut.io.host_wg_done.bits.vgpr_base.peek.litValue.toInt
            assert(testOut_csr(wg_id) == testIn_csr(wg_id))
            println(s"WG ${dut.io.host_wg_done.bits.wg_id.peek.litValue} finished: " +
              s"CU = ${testOut_cu(wg_id)}, " +
              s"LDS = [${testOut_lds(wg_id)}, ${testOut_lds(wg_id)+testIn_lds(wg_id)-1}], " +
              s"SGPR = [${testOut_sgpr(wg_id)}, ${testOut_sgpr(wg_id)+testIn_sgpr(wg_id)-1}], " +
              s"VGPR = [${testOut_vgpr(wg_id)}, ${testOut_vgpr(wg_id)+testIn_vgpr(wg_id)-1}], "
            )
            cnt = cnt + 1
          }
          dut.clock.step()
        }
      }.join

      dut.clock.step(100)
    }
  }
}
