package com.app.backend.responses.bocauhoi;

import com.app.backend.models.BoCauHoi;
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

    @JsonProperty("is_official")
    private Boolean isOfficial;

    @JsonProperty("loai_su_dung")
    private String loaiSuDung;

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

    @JsonProperty("co_quyen_sua")
    private Boolean coQuyenSua;

    @JsonProperty("gia_mo_khoa")
    private Long giaMoKhoa;

    @JsonProperty("da_mo_khoa")
    private Boolean daMoKhoa;

    @JsonProperty("can_mo_khoa")
    private Boolean canMoKhoa;

    // Thông tin liên kết khóa học (nếu bộ câu hỏi thuộc một khóa học nào đó)
    @JsonProperty("thuoc_khoa_hoc")
    private Boolean thuocKhoaHoc;

    @JsonProperty("khoa_hoc_id")
    private Long khoaHocId;

    @JsonProperty("khoa_hoc_ten")
    private String khoaHocTen;

    @JsonProperty("tao_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant taoLuc;

    @JsonProperty("cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    // ✅ static mapper
    public static BoCauHoiResponse from(BoCauHoi entity) {
        NguoiDung taoBoi = entity.getTaoBoi();
        boolean la_admin_tao = taoBoi != null && taoBoi.getVaiTro() != null && "admin".equals(taoBoi.getVaiTro().getTenVaiTro());
        return BoCauHoiResponse.builder()
                .id(entity.getId())
                .tieuDe(entity.getTieuDe())
                .moTa(entity.getMoTa())
                .cheDoHienThi(entity.getCheDoHienThi() != null ? entity.getCheDoHienThi() : null)
                .trangThai(entity.getTrangThai())
                .isOfficial(entity.getIsOfficial())
                .loaiSuDung(entity.getLoaiSuDung())
                .lyDoTuChoi(entity.getLyDoTuChoi())
                .chuDe(entity.getChuDe() != null ? entity.getChuDe().getTen() : null)
                .chuDeId(entity.getChuDe() != null ? entity.getChuDe().getId() : null)
                .nguoiTao(taoBoi != null ? taoBoi.getHoTen() : null)
                .nguoiTaoId(taoBoi != null ? taoBoi.getId() : null)
                .coQuyenSua(la_admin_tao)
                // Các field mở khoá cũng cần trả về cho danh sách chung
                .canMoKhoa(entity.getCanMoKhoa())
                .thuocKhoaHoc(false)
                .giaMoKhoa(entity.getGiaMoKhoa())
                // Ở danh sách chung chưa có thông tin theo user, mặc định false
                .daMoKhoa(false)
                .taoLuc(entity.getTaoLuc())
                .capNhatLuc(entity.getCapNhatLuc())
                .build();
    }

    public static BoCauHoiResponse from(BoCauHoi entity, boolean daMoKhoa) {
        NguoiDung taoBoi = entity.getTaoBoi();
        boolean la_admin_tao = taoBoi != null && taoBoi.getVaiTro() != null && "admin".equals(taoBoi.getVaiTro().getTenVaiTro());
        return BoCauHoiResponse.builder()
                .id(entity.getId())
                .tieuDe(entity.getTieuDe())
                .moTa(entity.getMoTa())
                .cheDoHienThi(entity.getCheDoHienThi() != null ? entity.getCheDoHienThi() : null)
                .trangThai(entity.getTrangThai())
                .isOfficial(entity.getIsOfficial())
                .loaiSuDung(entity.getLoaiSuDung())
                .lyDoTuChoi(entity.getLyDoTuChoi())
                .chuDe(entity.getChuDe() != null ? entity.getChuDe().getTen() : null)
                .chuDeId(entity.getChuDe() != null ? entity.getChuDe().getId() : null)
                .nguoiTao(taoBoi != null ? taoBoi.getHoTen() : null)
                .nguoiTaoId(taoBoi != null ? taoBoi.getId() : null)
                .coQuyenSua(la_admin_tao)
                .taoLuc(entity.getTaoLuc())
                .canMoKhoa(entity.getCanMoKhoa())
                .giaMoKhoa(entity.getGiaMoKhoa())
                .daMoKhoa(daMoKhoa)
                .thuocKhoaHoc(false)
                .capNhatLuc(entity.getCapNhatLuc())
                .build();
    }
}
