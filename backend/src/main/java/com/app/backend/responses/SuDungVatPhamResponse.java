package com.app.backend.responses;

import com.app.backend.models.enums.LoaiVatPham;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

/**
 * Response khi sử dụng vật phẩm trong trận đấu
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class SuDungVatPhamResponse {

    @JsonProperty("thanh_cong")
    private boolean thanhCong;

    @JsonProperty("loai_vat_pham")
    private LoaiVatPham loaiVatPham;

    @JsonProperty("ten_vat_pham")
    private String tenVatPham;

    @JsonProperty("thong_bao")
    private String thongBao;

    /**
     * Hiệu ứng áp dụng (x2 điểm, các đáp án bị loại, etc.)
     */
    @JsonProperty("hieu_ung")
    private HieuUngVatPham hieuUng;

    /**
     * Số lượng còn lại sau khi sử dụng
     */
    @JsonProperty("so_luong_con_lai")
    private Integer soLuongConLai;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class HieuUngVatPham {
        /**
         * Hệ số nhân điểm (x2, x3...)
         */
        @JsonProperty("he_so_diem")
        private Double heSoDiem;

        /**
         * Thời gian thêm (giây)
         */
        @JsonProperty("thoi_gian_them_giay")
        private Integer thoiGianThemGiay;

        /**
         * Danh sách đáp án bị loại (cho 50-50)
         */
        @JsonProperty("dap_an_bi_loai")
        private List<String> dapAnBiLoai;

        /**
         * Có được bảo vệ combo không
         */
        @JsonProperty("bao_ve_combo")
        private Boolean baoVeCombo;

        /**
         * Hiển thị đáp án đúng
         */
        @JsonProperty("dap_an_dung")
        private String dapAnDung;

        /**
         * Bỏ qua câu hỏi thành công
         */
        @JsonProperty("bo_qua_thanh_cong")
        private Boolean boQuaThanhCong;
    }
}
