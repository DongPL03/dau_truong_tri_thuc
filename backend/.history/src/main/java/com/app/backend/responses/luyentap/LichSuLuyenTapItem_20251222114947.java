package com.app.backend.responses.luyentap;

import com.app.backend.models.PhienLuyenTap;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;
import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LichSuLuyenTapItem {
    @JsonProperty("phien_id")
    private Long phienId;

    @JsonProperty("bo_cau_hoi")
    private String boCauHoi;

    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("khoa_hoc")
    private KhoaHocInfo khoaHoc;

    @JsonProperty("tong_cau_hoi")
    private Integer tongCauHoi;

    @JsonProperty("so_cau_dung")
    private Integer soCauDung;

    @JsonProperty("diem_so")
    private Integer diemSo;

    @JsonProperty("do_chinh_xac")
    private BigDecimal doChinhXac;

    @JsonProperty("thoi_gian_tb_ms")
    private Integer thoiGianTbMs;

    @JsonProperty("ngay_tao")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ngayTao;

    @JsonProperty("nguoi_dung")
    private UserInfo nguoiDung;

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class UserInfo {
        @JsonProperty("id")
        private Long id;
        @JsonProperty("ho_ten")
        private String hoTen;
    }

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class KhoaHocInfo {
        @JsonProperty("id")
        private Long id;
        @JsonProperty("ten")
        private String ten;
    }

    public static LichSuLuyenTapItem from(PhienLuyenTap p) {
        return LichSuLuyenTapItem.builder()
                .phienId(p.getId())
                .boCauHoi(p.getBoCauHoi().getTieuDe())
                .tongCauHoi(p.getTongCauHoi())
                .soCauDung(p.getSoCauDung())
                .diemSo(p.getDiemSo())
                .doChinhXac(p.getDoChinhXac())
                .thoiGianTbMs(p.getThoiGianTbMs())
                .ngayTao(p.getTaoLuc())
                .nguoiDung(UserInfo.builder()
                        .id(p.getNguoiDung().getId())
                        .hoTen(p.getNguoiDung().getHoTen())
                        .build())
                .build();
    }

}
