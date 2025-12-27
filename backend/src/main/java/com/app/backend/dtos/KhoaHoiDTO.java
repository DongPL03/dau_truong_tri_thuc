package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

@Data
public class KhoaHoiDTO {

    @NotBlank
    @JsonProperty("tieu_de")
    private String tieuDe;

    @JsonProperty("mo_ta")
    private String moTa;

    @JsonProperty("hinh_anh")
    private String hinhAnh;

    @NotNull
    @JsonProperty("chu_de_id")
    private Long chuDeId;

    @JsonProperty("trang_thai")
    private String trangThai; // DRAFT, PUBLISHED, ARCHIVED

    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa = 0L;

    @JsonProperty("thu_tu")
    private Integer thuTu = 0;

    @JsonProperty("danh_sach_bo_cau_hoi")
    private List<BoCauHoiTrongKhoaDTO> danhSachBoCauHoi;

    @Data
    public static class BoCauHoiTrongKhoaDTO {
        @NotNull
        @JsonProperty("bo_cau_hoi_id")
        private Long boCauHoiId;

        @NotNull
        @JsonProperty("thu_tu")
        private Integer thuTu;

        @JsonProperty("is_bat_buoc")
        private Boolean isBatBuoc = true;

        @JsonProperty("diem_toi_thieu")
        private Integer diemToiThieu = 0;
    }
}

