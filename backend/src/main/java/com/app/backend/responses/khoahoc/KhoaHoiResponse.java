package com.app.backend.responses.khoahoc;

import com.app.backend.models.KhoaHoc;
import com.app.backend.models.NguoiDung;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KhoaHoiResponse {

    private Long id;

    @JsonProperty("tieu_de")
    private String tieuDe;

    @JsonProperty("mo_ta")
    private String moTa;

    @JsonProperty("hinh_anh")
    private String hinhAnh;

    @JsonProperty("chu_de")
    private String chuDe;

    @JsonProperty("chu_de_id")
    private Long chuDeId;

    @JsonProperty("nguoi_tao")
    private String nguoiTao;

    @JsonProperty("nguoi_tao_id")
    private Long nguoiTaoId;

    @JsonProperty("trang_thai")
    private String trangThai;

    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa;

    @JsonProperty("thu_tu")
    private Integer thuTu;

    @JsonProperty("so_bo_cau_hoi")
    private Integer soBoCauHoi;

    @JsonProperty("co_quyen_sua")
    private Boolean coQuyenSua;

    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @JsonProperty("cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    public static KhoaHoiResponse from(KhoaHoc entity) {
        NguoiDung taoBoi = entity.getTaoBoi();
        boolean la_admin_tao = taoBoi != null && taoBoi.getVaiTro() != null && "admin".equals(taoBoi.getVaiTro().getTenVaiTro());
        return KhoaHoiResponse.builder()
                .id(entity.getId())
                .tieuDe(entity.getTieuDe())
                .moTa(entity.getMoTa())
                .hinhAnh(entity.getHinhAnh())
                .chuDe(entity.getChuDe() != null ? entity.getChuDe().getTen() : null)
                .chuDeId(entity.getChuDe() != null ? entity.getChuDe().getId() : null)
                .nguoiTao(taoBoi != null ? taoBoi.getHoTen() : null)
                .nguoiTaoId(taoBoi != null ? taoBoi.getId() : null)
                .trangThai(entity.getTrangThai())
                .giaMoKhoa(entity.getGiaMoKhoa())
                .thuTu(entity.getThuTu())
                .soBoCauHoi(entity.getDanhSachBoCauHoi() != null ? entity.getDanhSachBoCauHoi().size() : 0)
                .coQuyenSua(la_admin_tao)
                .taoLuc(entity.getTaoLuc())
                .capNhatLuc(entity.getCapNhatLuc())
                .build();
    }
}

