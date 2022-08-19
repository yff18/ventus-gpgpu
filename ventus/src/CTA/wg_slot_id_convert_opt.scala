package CTA

import chisel3._

class wg_slot_id_convert_opt(val NUMBER_CU: Int, val CU_ID_WIDTH: Int, val WG_ID_WIDTH: Int, val WG_SLOT_ID_WIDTH: Int) extends Module {
    val io = IO(new Bundle{
        val wg_id = Input(UInt(WG_ID_WIDTH.W))
        val cu_id = Input(UInt(CU_ID_WIDTH.W))
        val wg_slot_id_gen = Output(UInt(WG_SLOT_ID_WIDTH.W))
        val wg_slot_id_find = Output(UInt(WG_SLOT_ID_WIDTH.W))
        val find_and_cancel = Input(Bool())
        val generate = Input(Bool())
    })
    val found_wg_id = Wire(UInt(WG_ID_WIDTH.W))
    val wg_slot_id_find_i = RegInit(0.U(WG_SLOT_ID_WIDTH.W))
    io.wg_slot_id_find := wg_slot_id_find_i
    val SLOT_ID_NUM = 1 << WG_SLOT_ID_WIDTH
    val wg_slot_id_gen_i = RegInit(0.U(WG_SLOT_ID_WIDTH.W))
    io.wg_slot_id_gen := wg_slot_id_gen_i
    val wg_slot_id_bitmap = RegInit{
        val bitmap = Wire(Vec(NUMBER_CU, Vec(SLOT_ID_NUM, Bool())))
        for(i <- 0 until NUMBER_CU){
            for(j <- 0 until (1 << WG_SLOT_ID_WIDTH)){
                bitmap(i)(j) := false.B
            }
        }
        bitmap
    }
    val first_slot_id = Wire(UInt(WG_SLOT_ID_WIDTH.W))
    val first_slot_id_valid = Wire(Bool())
    first_slot_id_valid := false.B
    first_slot_id := 0.U
    for(i <- SLOT_ID_NUM - 1 to 0 by -1){
        when(~wg_slot_id_bitmap(io.cu_id)(i)){
            first_slot_id := i.U
            first_slot_id_valid := true.B
        }
    }
    wg_slot_id_gen_i := first_slot_id
    when(io.generate && first_slot_id_valid){
        wg_slot_id_bitmap(io.cu_id)(first_slot_id) := true.B
    }
    /*
    val wg_slot_id_find_ram = Module(new RAM(WG_SLOT_ID_WIDTH, WG_ID_WIDTH, 1 << WG_ID_WIDTH))
    wg_slot_id_find_ram.io.wr_en := io.generate && first_slot_id_valid
    wg_slot_id_find_ram.io.wr_addr := io.wg_id
    wg_slot_id_find_ram.io.wr_word := first_slot_id
    wg_slot_id_find_ram.io.rd_en := io.find_and_cancel
    wg_slot_id_find_ram.io.rd_addr := io.wg_id
    io.wg_slot_id_find := wg_slot_id_find_ram.io.rd_word
    */
    val wg_slot_id_find_ram_cam = RegInit{
        val data = Wire(Vec(NUMBER_CU, Vec(SLOT_ID_NUM, UInt(WG_ID_WIDTH.W))))
        for(i <- 0 until NUMBER_CU){
            for(j <- 0 until SLOT_ID_NUM){
                data(i)(j) := 0.U
            }
        }
        data
    }
    found_wg_id := 0.U
    for(i <- 0 until NUMBER_CU){
        for(j <- 0 until SLOT_ID_NUM){
            when(wg_slot_id_find_ram_cam(i)(j) === io.wg_id && wg_slot_id_bitmap(i)(j)){
                found_wg_id := j.U
            }
        }
    }
    wg_slot_id_find_i := found_wg_id
    when(io.generate && first_slot_id_valid){
        wg_slot_id_find_ram_cam(io.cu_id)(first_slot_id) := io.wg_id
    }


    var cu_id_cancel = RegInit(0.U(CU_ID_WIDTH.W))
    var cancel_valid = RegInit(false.B)
    cu_id_cancel := io.cu_id
    cancel_valid := io.find_and_cancel
    
    when(cancel_valid){
        wg_slot_id_bitmap(cu_id_cancel)(wg_slot_id_find_i) := false.B
    }
    //printf(p"wg_slot_id_bitmap: ${wg_slot_id_bitmap(io.cu_id)}\n")
}