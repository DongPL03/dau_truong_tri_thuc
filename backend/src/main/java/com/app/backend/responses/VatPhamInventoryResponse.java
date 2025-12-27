package com.app.backend.responses;

import com.app.backend.models.enums.LoaiVatPham;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

/**
 * Response cho thông tin vật phẩm trong inventory của user
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VatPhamInventoryResponse {

    @JsonProperty("vat_pham_id")
    private Long vatPhamId;

    @JsonProperty("ten")
    private String ten;

    @JsonProperty("mo_ta")
    private String moTa;

    @JsonProperty("loai")
    private LoaiVatPham loai;

    @JsonProperty("icon")
    private String icon;

    @JsonProperty("mau_sac")
    private String mauSac;

    @JsonProperty("do_hiem")
    private String doHiem;

    @JsonProperty("so_luong")
    private Integer soLuong;

    @JsonProperty("gia_tri_hieu_ung")
    private Double giaTriHieuUng;

    @JsonProperty("thoi_gian_hieu_luc_giay")
    private Integer thoiGianHieuLucGiay;
}
