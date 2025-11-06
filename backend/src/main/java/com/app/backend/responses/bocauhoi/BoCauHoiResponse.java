package com.app.backend.responses.bocauhoi;

import com.app.backend.models.BoCauHoi;
import com.app.backend.models.NguoiDung;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BoCauHoiResponse {

    private Long id;

    @JsonProperty("tieu_de")
    private String tieuDe;

    @JsonProperty("mo_ta")
    private String moTa;

    @JsonProperty("che_do_hien_thi")
    private String cheDoHienThi;

    @JsonProperty("trang_thai")
    private String trangThai;

    @JsonProperty("ly_do_tu_choi")
    private String lyDoTuChoi;

    @JsonProperty("chu_de")
    private String chuDe;

    @JsonProperty("chu_de_id")
    private Long chuDeId;

    @JsonProperty("nguoi_tao")
    private String nguoiTao;

    @JsonProperty("nguoi_tao_id")
    private Long nguoiTaoId;

    @JsonProperty("tao_luc")
    private LocalDateTime taoLuc;

    @JsonProperty("cap_nhat_luc")
    private LocalDateTime capNhatLuc;

    // ✅ static mapper
    public static BoCauHoiResponse from(BoCauHoi entity) {
        NguoiDung taoBoi = entity.getTaoBoi();
        return BoCauHoiResponse.builder()
                .id(entity.getId())
                .tieuDe(entity.getTieuDe())
                .moTa(entity.getMoTa())
                .cheDoHienThi(entity.getCheDoHienThi() != null ? entity.getCheDoHienThi() : null)
                .trangThai(entity.getTrangThai())
                .lyDoTuChoi(entity.getLyDoTuChoi())
                .chuDe(entity.getChuDe() != null ? entity.getChuDe().getTen() : null)
                .chuDeId(entity.getChuDe() != null ? entity.getChuDe().getId() : null)
                .nguoiTao(taoBoi != null ? taoBoi.getHoTen() : null)
                .nguoiTaoId(taoBoi != null ? taoBoi.getId() : null)
                .taoLuc(entity.getTaoLuc())
                .capNhatLuc(entity.getCapNhatLuc())
                .build();
    }
}
