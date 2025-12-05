package com.app.backend.responses.thongke;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AdminSummaryStatsResponse {

    // người dùng
    @JsonProperty("tong_nguoi_dung")
    private long tongNguoiDung;

    // trận đấu
    @JsonProperty("tong_tran_dau")
    private long tongTranDau;
    @JsonProperty("tran_dang_cho")
    private long tranDangCho;
    @JsonProperty("tran_dang_dien_ra")
    private long tranDangDienRa;
    @JsonProperty("tran_da_ket_thuc")
    private long tranDaKetThuc;

    // bộ câu hỏi / câu hỏi
    @JsonProperty("tong_bo_cau_hoi")
    private long tongBoCauHoi;
    @JsonProperty("tong_cau_hoi")
    private long tongCauHoi;
}
