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

    // ========== NGƯỜI DÙNG ==========
    @JsonProperty("tong_nguoi_dung")
    private long tongNguoiDung;
    
    @JsonProperty("nguoi_dung_active")
    private long nguoiDungActive;
    
    @JsonProperty("nguoi_dung_blocked")
    private long nguoiDungBlocked;
    
    @JsonProperty("nguoi_dung_deleted")
    private long nguoiDungDeleted;
    
    @JsonProperty("so_admin")
    private long soAdmin;
    
    @JsonProperty("nguoi_dung_moi_hom_nay")
    private long nguoiDungMoiHomNay;

    // ========== TRẬN ĐẤU ==========
    @JsonProperty("tong_tran_dau")
    private long tongTranDau;
    
    @JsonProperty("tran_dang_cho")
    private long tranDangCho;
    
    @JsonProperty("tran_dang_dien_ra")
    private long tranDangDienRa;
    
    @JsonProperty("tran_da_ket_thuc")
    private long tranDaKetThuc;
    
    @JsonProperty("tran_da_huy")
    private long tranDaHuy;
    
    @JsonProperty("tran_hom_nay")
    private long tranHomNay;

    // ========== BỘ CÂU HỎI / CÂU HỎI ==========
    @JsonProperty("tong_bo_cau_hoi")
    private long tongBoCauHoi;
    
    @JsonProperty("bo_cau_hoi_da_duyet")
    private long boCauHoiDaDuyet;
    
    @JsonProperty("bo_cau_hoi_cho_duyet")
    private long boCauHoiChoDuyet;
    
    @JsonProperty("bo_cau_hoi_tu_choi")
    private long boCauHoiTuChoi;
    
    @JsonProperty("tong_cau_hoi")
    private long tongCauHoi;

    // ========== KHÓA HỌC ==========
    @JsonProperty("tong_khoa_hoc")
    private long tongKhoaHoc;
    
    @JsonProperty("khoa_hoc_published")
    private long khoaHocPublished;
    
    @JsonProperty("khoa_hoc_draft")
    private long khoaHocDraft;
}
